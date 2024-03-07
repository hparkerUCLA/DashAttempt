library(tidyverse)
library(shiny)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(Polychrome)
library(RColorBrewer)
library(viridis)
library(shinythemes)
library(janitor)

options(shiny.port = 3003)

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

#transform event name variable
enga <- enga %>% 
  rename("Event" = `Event.name`)

#transform Event Name with year variable
enga <- enga %>% 
  rename("Specific.Event" = Event.Name.W.Yr)

#specify the choices for the dropdown menus
# choiceList <- names(enga)
choiceList <- c("Position","Tenureship_Role","Division","Department",
                "Specific.Event","Event","Academic.Year","Event.category")


# Define UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      pickerInput("filter1","Select filters for Position", choices = unique(enga$Position), options = list(`actions-box` = TRUE),multiple = T),
      pickerInput("filter2","Select filters for Tenureship", choices = unique(enga$Tenureship_Role), options = list(`actions-box` = TRUE),multiple = T),
      pickerInput("filter3","Select filters for Division", choices = unique(enga$Division), options = list(`actions-box` = TRUE),multiple = T),
      pickerInput("filter4","Select filters for Department", choices = unique(enga$Department), options = list(`actions-box` = TRUE),multiple = T),
      pickerInput("filter5","Select filters for Event", choices = unique(enga$Event), options = list(`actions-box` = TRUE),multiple = T),
      # pickerInput("filter6","Select filters for Event and Year", choices = unique(enga$Event.Name.W.Yr), options = list(`actions-box` = TRUE),multiple = T),
      pickerInput("filter7","Select filters for Academic Year", choices = sort(unique(enga$Academic.Year)), options = list(`actions-box` = TRUE),multiple = T),
      pickerInput("filter8","Select filters for Event Category", choices = unique(enga$Event.category), options = list(`actions-box` = TRUE),multiple = T),
      # actionButton("exportBtn", "Export Filtered Data to CSV"),
      # actionButton("exportBtn_agg", "Export Aggregated Data to CSV"),
      downloadButton("filtDwnld","Download Filtered Data"),
      downloadButton("aggDwnld","Download Aggregated Data"),
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Bar Graphs",
          selectInput("x_var", "Select X Variable", choices = choiceList, selected = "Academic.Year"),
          # selectInput("y_var", "Select Y Variable", choices = names(enga), selected = "Hours"),
          selectInput("fill_var", "Select Fill Variable", choices = choiceList, selected = "Tenureship_Role"),
          plotOutput("barplot"),
        ),
        tabPanel(
          "Unique Participant Bar Graphs",
          selectInput("uniqy_var", "Select Y Variable", choices = choiceList, selected = "Division"),
          selectInput("uniqfill_var", "Select Fill Variable", choices = choiceList, selected = "Tenureship_Role"),
          plotOutput("uniquebarplot"),#, click="plot_click2"),
          dataTableOutput("dynamic")
        ),
        tabPanel(
          "Bubble Plot Participants",
          selectInput("uniqbubble_var", "Select Bubble Variable", choices = choiceList, selected = "Department"),
          # selectInput("uniqfill_var", "Select Fill Variable", choices = choiceList, selected = "Tenureship_Role"),
          plotOutput("bubbleplot", click="plot_click"), 
          tableOutput("bubbledata")
        ),
        tabPanel(
          "Bubble Plot Percent",
          # selectInput("uniqbubble_var", "Select Bubble Variable", choices = choiceList, selected = "Department"),
          # selectInput("uniqfill_var", "Select Fill Variable", choices = choiceList, selected = "Tenureship_Role"),
          plotOutput("bubbleplot_prc", click="plot_clickbbl"), 
          tableOutput("bubbledata_prc")
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
        if (!is.null(input$filter5)) Event %in% input$filter5 else TRUE,
        # if (!is.null(input$filter6)) Event.Name.W.Yr %in% input$filter6 else TRUE,
        if (!is.null(input$filter7)) Academic.Year %in% input$filter7 else TRUE,
        if (!is.null(input$filter8)) Event.category %in% input$filter8 else TRUE
      )
  })
  
  agg_df <- reactive({
    filtered_data() %>% group_by(Name.1,
                                         Email,
                                         Position,Tenureship_Role,
                                         Division,
                                         Department) %>% summarize(totalhours=sum(Hours),
                                                                   distinct_years = n_distinct(Academic.Year),
                                                                   distinct_eventcat= n_distinct(Event.category),
                                                                   distinct_event= n_distinct(Event),
                                                                   distinct_specificevent= n_distinct(Specific.Event))
      
  })
  
  output$barplot <- renderPlot({
    # Check if there is filtered data
    if (nrow(filtered_data()) > 0) {

      # Plot bar graph based on selected variables
      fig.width=8
      fig.height=8
      ggplot(filtered_data(), aes(x = !!sym(input$x_var), y = filtered_data()$Hours, fill = !!sym(input$fill_var), color = !!sym(input$fill_var))) +
        geom_bar(stat = "identity", position = "stack") +
        labs(x = input$x_var, y = "Hours") +
        # scale_fill_manual(values = mycolors) +
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
        labs(title = "Number of Unique Participants", x = "Unique Count", y=input$uniqy_var) +
        scale_fill_manual(values = mycolors2) +
        theme_minimal(base_size = 14)
      
    } else {
      # Display a message if no data passes the filters
      plot(0, type = "n", axes = FALSE, xlab = "", ylab = "", main = "No data matches the selected filters.")
      text(0.5, 0.5, "Adjust filters in the sidebar to see bar graphs.", cex = 1.2, col = "gray")
    }
  })
  
  output$dynamic <- renderDataTable(sum1<- filtered_data() %>%
                                      group_by(!!sym(input$uniqy_var), !!sym(input$uniqfill_var)) %>%
                                      summarize(distinct_participants = n_distinct(Name.1)),
                                    options = list(pageLength = 10), 
                                    )
  
  output$bubbleplot <- renderPlot({
    # Check if there is filtered data
    if (nrow(filtered_data()) > 0) {
      
      sum2<- filtered_data() %>%
        group_by(!!sym(input$uniqbubble_var)) %>%
        summarize(distinct_participants = n_distinct(Name.1), total_hours=sum(Hours),ave_hours_per_participant=total_hours/distinct_participants)
    
      
      # calculate the percentage of faculty engaged
      # sum2$perc_faculty <- (sum2$distinct_participants/max(sum2$distinct_participants))*100
      sum2$sizevec <- ((log(sum2$distinct_participants)+1)/(max(log(sum2$distinct_participants)+1)/6))*3
      # sum2$sizevec <- (((sum2$distinct_participants)+1)/(max((sum2$distinct_participants)+1)/3))*10

      nb.cols <- length(unique(sum2[[input$uniqbubble_var]]))
      mycolors2 <- colorRampPalette(brewer.pal(8, "Dark2"))(nb.cols)
      
      # Plot bar graph based on selected variables
      fig.width=8
      fig.height=8
      # Create scatter plot
      
      ggplot(sum2, aes(x = distinct_participants, 
                       y = ave_hours_per_participant)) +
        geom_point(size=sum2$sizevec, shape=16,color=mycolors2) +
        geom_text_repel(aes(label = .data[[input$uniqbubble_var]]), max.overlaps = nb.cols+1) +
        labs(title = "Engagement Bubble Plot", x = "Distinct Participants", y="Average Hours of Engagement") +
        theme_minimal(base_size = 14)
      
      
    } else {
      # Display a message if no data passes the filters
      plot(0, type = "n", axes = FALSE, xlab = "", ylab = "", main = "No data matches the selected filters.")
      text(0.5, 0.5, "Adjust filters in the sidebar to see bar graphs.", cex = 1.2, col = "gray")
    }
  })
  
  output$bubbledata <- renderTable({
    
    sum2<- filtered_data() %>%
      group_by(!!sym(input$uniqbubble_var)) %>%
      summarize(distinct_participants = n_distinct(Name.1), total_hours=sum(Hours),ave_hours_per_participant=total_hours/distinct_participants)
    
    # calculate the percentage of faculty engaged
    sum2$perc_faculty <- (sum2$distinct_participants/max(sum2$distinct_participants))*100
    sum2$sizevec <- ((log(sum2$distinct_participants)+1)/(max(log(sum2$distinct_participants)+1)/6))*3
    req(input$plot_click)
    nearPoints(sum2, input$plot_click)
  })
  
  output$bubbleplot_prc <- renderPlot({
    # Check if there is filtered data
    if (nrow(filtered_data()) > 0) {
      
      sum2<- filtered_data() %>%
        group_by(!!sym(input$uniqbubble_var)) %>%
        summarize(distinct_participants = n_distinct(Name.1), total_hours=sum(Hours),ave_hours_per_participant=total_hours/distinct_participants)
      
      
      # calculate the percentage of faculty engaged
      # sum2$perc_faculty <- (sum2$distinct_participants/max(sum2$distinct_participants))*100
      # sum2$sizevec <- ((log(sum2$distinct_participants)+1)/(max(log(sum2$distinct_participants)+1)/6))*3
      
      # hardcoding the departmental data in

      Department <- c('Atmospheric and Oceanic Sciences','Biomedical Research',
                        'Chemistry and Biochemistry','Earth, Planetary and Space Sciences',
                        'Ecology and Evolutionary Biology',
                        'Institute of the Environment and Sustainability',
                        'Institute of Society and Genetics',
                        'Integrative Biology and Physiology','LS Core',
                        'Mathematics','Microbiology, Immunology, and Molecular Genetics',
                        'Molecular, Cell, and Developmental Biology','Neuroscience',
                        'Physics and Astronomy','Program in Computing','Psychology',
                        'Statistics')
      TT <- c(19,0,51,27,25,14,11,19,0,51,27,25,0,60,0,64,18)
      nonTT <- c(1,3,24,1,8,7,1,6,11,28,0,3,2,3,1,10,10)
      facultynumbers <- data.frame(Department,TT,nonTT)
      perc_enga <- merge(sum2,facultynumbers,by='Department')
      perc_enga$perc_faculty <- (perc_enga$distinct_participants/(perc_enga$TT+perc_enga$nonTT))*100
      perc_enga$sizevec <- ((log(perc_enga$distinct_participants)+1)/(max(log(perc_enga$distinct_participants)+1)/6))*3
      
      nb.cols <- length(unique(perc_enga[["Department"]]))
      mycolors2 <- colorRampPalette(brewer.pal(8, "Dark2"))(nb.cols)
      
      # Plot bar graph based on selected variables
      fig.width=8
      fig.height=8
      # Create scatter plot
      
      ggplot(perc_enga, aes(x = perc_faculty, 
                       y = ave_hours_per_participant)) +
        geom_point(size=perc_enga$sizevec, shape=16,color=mycolors2) +
        geom_text_repel(aes(label = Department), max.overlaps = nb.cols+1) +
        labs(title = "Engagement Bubble Plot", x = "% of faculty", y="Average Hours of Engagement") +
        theme_minimal(base_size = 14)
      
      
    } else {
      # Display a message if no data passes the filters
      plot(0, type = "n", axes = FALSE, xlab = "", ylab = "", main = "No data matches the selected filters.")
      text(0.5, 0.5, "Adjust filters in the sidebar to see bar graphs.", cex = 1.2, col = "gray")
    }
  })
  
  output$bubbledata_prc <- renderTable({
    
    sum2<- filtered_data() %>%
      group_by(!!sym(input$uniqbubble_var)) %>%
      summarize(distinct_participants = n_distinct(Name.1), total_hours=sum(Hours),ave_hours_per_participant=total_hours/distinct_participants)
    
    Department <- c('Atmospheric and Oceanic Sciences','Biomedical Research',
                    'Chemistry and Biochemistry','Earth, Planetary and Space Sciences',
                    'Ecology and Evolutionary Biology',
                    'Institute of the Environment and Sustainability',
                    'Institute of Society and Genetics',
                    'Integrative Biology and Physiology','LS Core',
                    'Mathematics','Microbiology, Immunology, and Molecular Genetics',
                    'Molecular, Cell, and Developmental Biology','Neuroscience',
                    'Physics and Astronomy','Program in Computing','Psychology',
                    'Statistics')
    TT <- c(19,0,51,27,25,14,11,19,0,51,27,25,0,60,0,64,18)
    nonTT <- c(1,3,24,1,8,7,1,6,11,28,0,3,2,3,1,10,10)
    facultynumbers <- data.frame(Department,TT,nonTT)
    perc_enga <- merge(sum2,facultynumbers,by='Department')
    perc_enga$perc_faculty <- (perc_enga$distinct_participants/(perc_enga$TT+perc_enga$nonTT))*100
    perc_enga$sizevec <- ((log(perc_enga$distinct_participants)+1)/(max(log(perc_enga$distinct_participants)+1)/6))*3
    
    # req(input$plot_click)
    nearPoints(perc_enga, input$plot_clickbbl)
  })
  
  output$filtDwnld <- downloadHandler(
    filename = function() {
      paste("filtered_data_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
    output$aggDwnld <- downloadHandler(
    filename = function() {
      paste("Aggregated_data_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(agg_df(), file, row.names = FALSE)
    }
  )
  
}

# Run the application
shinyApp(ui, server)
