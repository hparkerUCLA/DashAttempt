library(tidyverse)
library(shiny)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(Polychrome)
library(RColorBrewer)
library(viridis)
library(shinythemes)

# options(shiny.port = 3002)

enga <- read.csv('./CEILSEngagementByEvent.csv', header = TRUE)

#transform tenureship role variable
enga <- enga %>% 
  rename("Tenureship_Role" = TT..non.TT..etc.)
enga$Tenureship_Role[enga$Tenureship_Role=="TT: tenured"]<-"Tenured Faculty"
enga$Tenureship_Role[enga$Tenureship_Role=="Future faculty"]<-"Postdocs & \n Grad Students"
enga$Tenureship_Role[enga$Tenureship_Role=="TT: pre-tenure"]<-"Pre-tenure Faculty"
enga$Tenureship_Role[enga$Tenureship_Role=="Non-TT faculty"]<-"Non-tenure Track \n Faculty"

#transform Division variable
enga <- enga %>% 
  rename("Division" = Division..Institution)

#transform Department variable
enga <- enga %>% 
  rename("Department" = Department..necessary.for.PS.and.LS..optional.otherwise.)

#transform Event Name variable
enga <- enga %>% 
  rename("Specific.Event" = Event.Name.W.Yr)

#specify the choices for the dropdown menus
# choiceList <- names(enga)
choiceList <- c("Position","Tenureship_Role","Division","Department",
                "Specific.Event","Event.Name","Academic.Year","Event.category")


# Define UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      pickerInput("filter1","Select filters for Position", choices = unique(enga$Position), options = list(`actions-box` = TRUE),multiple = T),
      pickerInput("filter2","Select filters for Tenureship", choices = unique(enga$Tenureship_Role), options = list(`actions-box` = TRUE),multiple = T),
      pickerInput("filter3","Select filters for Division", choices = unique(enga$Division), options = list(`actions-box` = TRUE),multiple = T),
      pickerInput("filter4","Select filters for Department", choices = unique(enga$Department), options = list(`actions-box` = TRUE),multiple = T),
      pickerInput("filter5","Select filters for Event", choices = unique(enga$Event.Name), options = list(`actions-box` = TRUE),multiple = T),
      # pickerInput("filter6","Select filters for Event and Year", choices = unique(enga$Event.Name.W.Yr), options = list(`actions-box` = TRUE),multiple = T),
      pickerInput("filter7","Select filters for Academic Year", choices = unique(enga$Academic.Year), options = list(`actions-box` = TRUE),multiple = T),
      pickerInput("filter8","Select filters for Event Category", choices = unique(enga$Event.category), options = list(`actions-box` = TRUE),multiple = T),
      actionButton("exportBtn", "Export Filtered Data to CSV"),
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Bar Graphs",
          selectInput("x_var", "Select X Variable", choices = choiceList, selected = "Academic.Year"),
          # selectInput("y_var", "Select Y Variable", choices = names(enga), selected = "Hours"),
          selectInput("fill_var", "Select Fill Variable", choices = choiceList, selected = "Position"),
          plotOutput("barplot")
        ),
        tabPanel(
          "Unique Participant Bar Graphs",
          selectInput("uniqy_var", "Select Y Variable", choices = choiceList, selected = "Division"),
          selectInput("uniqfill_var", "Select Fill Variable", choices = choiceList, selected = "Position"),
          plotOutput("uniquebarplot")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    enga %>%
      filter(
        if (!is.null(input$filter1)) Position %in% input$filter1 else TRUE,
        if (!is.null(input$filter2)) Tenureship_Role %in% input$filter2 else TRUE,
        if (!is.null(input$filter3)) Division %in% input$filter3 else TRUE,
        if (!is.null(input$filter4)) Department %in% input$filter4 else TRUE,
        if (!is.null(input$filter5)) Event.Name %in% input$filter5 else TRUE,
        # if (!is.null(input$filter6)) Event.Name.W.Yr %in% input$filter6 else TRUE,
        if (!is.null(input$filter7)) Academic.Year %in% input$filter7 else TRUE,
        if (!is.null(input$filter8)) Event.category %in% input$filter8 else TRUE
      )
  })
  
  
  # observe({
  #   unique_counts <- length(unique(filtered_data()[[input$uniqy_var]]))
  #   output$uniqueCount <- renderText({
  #     paste("Number of Unique Observations:", unique_counts)
  #   })
  # })
  
  
  output$barplot <- renderPlot({
    # Check if there is filtered data
    if (nrow(filtered_data()) > 0) {
      
      nb.cols <- length(unique(filtered_data()[[input$fill_var]]))
      mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)
      
      # Plot bar graph based on selected variables
      fig.width=8
      fig.height=8
      ggplot(filtered_data(), aes(x = !!sym(input$x_var), y = filtered_data()$Hours, fill = !!sym(input$fill_var))) +
        geom_bar(stat = "identity", position = "stack") +
        labs(x = input$x_var, y = "Hours") +
        scale_fill_manual(values = mycolors) +
        theme_minimal(base_size = 14) + theme(text=element_text(size=20),axis.text.x= element_text(angle=90,vjust=0.5,hjust=1)) #+ scale_fill_brewer(palette = hcl.colors(8, "viridis"))
    } else {
      # Display a message if no data passes the filters
      plot(0, type = "n", axes = FALSE, xlab = "", ylab = "", main = "No data matches the selected filters.")
      text(0.5, 0.5, "Adjust filters in the sidebar to see bar graphs.", cex = 1.2, col = "gray")
    }
  })
  
  output$uniquebarplot <- renderPlot({
    # Check if there is filtered data
    if (nrow(filtered_data()) > 0) {
      
      sum1<- filtered_data() %>%
        group_by(!!sym(input$uniqy_var), !!sym(input$uniqfill_var)) %>%
        summarize(distinct_participants = n_distinct(Name.1))
      
      nb.cols <- length(unique(sum1[[input$uniqfill_var]]))#nrow(sum1)
      mycolors2 <- colorRampPalette(brewer.pal(8, "Dark2"))(nb.cols)
      
      # # Generate bar graph
      fig.width=8
      fig.height=8
      ggplot(sum1, aes(x = distinct_participants, 
                       y = reorder(!!sym(input$uniqy_var),+distinct_participants),
                       fill=!!sym(input$uniqfill_var))) +
        geom_bar(stat = "identity") +
        labs(title = "Number of Unique Participants", x = "Unique Count", y="Variable") +
        scale_fill_manual(values = mycolors2) +
        theme_minimal(base_size = 14)
      
    } else {
      # Display a message if no data passes the filters
      plot(0, type = "n", axes = FALSE, xlab = "", ylab = "", main = "No data matches the selected filters.")
      text(0.5, 0.5, "Adjust filters in the sidebar to see bar graphs.", cex = 1.2, col = "gray")
    }
  })
  
  # # not currently looking at percentage of hours
  #  output$phbarplot <- renderPlot({
  #   # Check if there is filtered data
  #   if (nrow(filtered_data()) > 0) {
  #     
  #     filtered_dataframe <- filtered_data()
  #     percenthours = c()
  #     catlist = unique(filtered_dataframe$Department..necessary.for.PS.and.LS..optional.otherwise.)
  #     hours=input$Hours
  #     
  #     for(i in 1:length(catlist)){
  #       aa <- filter(filtered_dataframe,filtered_dataframe$Department..necessary.for.PS.and.LS..optional.otherwise. == catlist[i])
  #       percenthours = c(percenthours,(sum(filtered_dataframe$Hours[filtered_dataframe$Department..necessary.for.PS.and.LS..optional.otherwise. == catlist[i]])/sum(filtered_dataframe$Hours))*100)
  #     }
  #     
  #     Percentage <- (filtered_data()$Hours / sum(filtered_data()$Hours)) * 100
  #     catlistpct <- paste(catlist, round(percenthours, digits=2))
  #     # add percents to labels
  #     catlistpct <- paste(catlistpct,"%",sep="") # ad % to labels
  #     
  #     middf <- data.frame(catlist,percenthours)
  #     ggplot(middf, aes(x = percenthours, y = catlist)) +
  #       geom_bar(stat = "identity", position = "stack") +
  #       labs(title = "Bar Graph Based on Selected Variables", x = input$x_var, y = input$y_var) +
  #       theme_minimal() + theme(text=element_text(size=20),axis.text.x= element_text(angle=90,vjust=0.5,hjust=1)) #+ scale_fill_brewer(palette = 'Dark2')
  #     
  #   } else {
  #     # Display a message if no data passes the filters
  #     plot(0, type = "n", axes = FALSE, xlab = "", ylab = "", main = "No data matches the selected filters.")
  #     text(0.5, 0.5, "Adjust filters in the sidebar to see bar graphs.", cex = 1.2, col = "gray")
  #   }
  # })
  
  # Export data to CSV on button click
  observeEvent(input$exportBtn, {
    write.csv(filtered_data(), "exported_data.csv", row.names = FALSE)
    showModal(modalDialog(
      title = "Export Successful",
      "Data has been exported to 'exported_data.csv'.",
      easyClose = TRUE
    ))
  })
}

# Run the application
shinyApp(ui, server)