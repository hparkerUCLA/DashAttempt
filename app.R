library(tidyverse)
library(shiny)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
options(shiny.port = 3001)

enga <- read.csv('CEILSEngagementByEvent.csv', header = TRUE)
rawcolnam <- names(enga)

for (col_name in rawcolnam) {
  assign(col_name, enga[[col_name]])
}

# get unique options for each column name
# unique(enga[[rawcolnam[1]]])
# colnam <- lapply(colnames(enga[,sapply(enga,is.character)]),tolower)
colnam <- lapply(colnames(enga),tolower)


# Define UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      pickerInput("filter1","Select filters for Position", choices = unique(enga$Position), options = list(`actions-box` = TRUE),multiple = T),
      pickerInput("filter2","Select filters for Tenureship", choices = unique(enga$TT..non.TT..etc.), options = list(`actions-box` = TRUE),multiple = T),
      pickerInput("filter3","Select filters for Division", choices = unique(enga$Division..Institution), options = list(`actions-box` = TRUE),multiple = T),
      pickerInput("filter4","Select filters for Department", choices = unique(enga$Department..necessary.for.PS.and.LS..optional.otherwise.), options = list(`actions-box` = TRUE),multiple = T),
      pickerInput("filter5","Select filters for Event", choices = unique(enga$Event.Name), options = list(`actions-box` = TRUE),multiple = T),
      pickerInput("filter6","Select filters for Event and Year", choices = unique(enga$Event.Name.W.Yr), options = list(`actions-box` = TRUE),multiple = T),
      pickerInput("filter7","Select filters for Academic Year", choices = unique(enga$Academic.Year), options = list(`actions-box` = TRUE),multiple = T),
      pickerInput("filter8","Select filters for Event Category", choices = unique(enga$Event.category), options = list(`actions-box` = TRUE),multiple = T),
      actionButton("exportBtn", "Export Filtered Data to CSV"),
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Bar Graphs",
          selectInput("x_var", "Select X Variable", choices = names(enga), selected = "Position"),
          selectInput("y_var", "Select Y Variable", choices = names(enga), selected = "Hours"),
          plotOutput("barplot")
        ),
        tabPanel("Boxplots", plotOutput("boxplots"))
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
        if (!is.null(input$filter2)) TT..non.TT..etc. %in% input$filter2 else TRUE,
        if (!is.null(input$filter3)) Division..Institution %in% input$filter3 else TRUE,
        if (!is.null(input$filter4)) Department..necessary.for.PS.and.LS..optional.otherwise. %in% input$filter4 else TRUE,
        if (!is.null(input$filter5)) Event.Name %in% input$filter5 else TRUE,
        if (!is.null(input$filter6)) Event.Name.W.Yr %in% input$filter6 else TRUE,
        if (!is.null(input$filter7)) Academic.Year %in% input$filter7 else TRUE,
        if (!is.null(input$filter8)) Event.category %in% input$filter8 else TRUE
      )
    
  })
  
  
  # Create reactive boxplots based on the filtered data
  output$boxplots <- renderPlot({
    # Check if there is filtered data
    if (nrow(filtered_data()) > 0) {
      # Plot boxplots for each category
      boxplot(
        filtered_data()$Hours,
        main = "Boxplots of Selected Filters",
        col = rainbow(3)
      )
    } else {
      # Display a message if no data passes the filters
      plot(0, type = "n", axes = FALSE, xlab = "", ylab = "", main = "No data matches the selected filters.")
      text(0.5, 0.5, "Adjust filters in the sidebar to see boxplots.", cex = 1.2, col = "gray")
    }
  })
  
  output$barplot <- renderPlot({
    # Check if there is filtered data
    if (nrow(filtered_data()) > 0) {
      # Plot bar graph based on selected variables
      ggplot(filtered_data(), aes(x = !!sym(input$x_var), y = !!sym(input$y_var), fill = !!sym(input$x_var))) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Bar Graph Based on Selected Variables", x = input$x_var, y = input$y_var) +
        theme(axis.text.x= element_text(angle=90,vjust=0.5,hjust=1))
    } else {
      # Display a message if no data passes the filters
      plot(0, type = "n", axes = FALSE, xlab = "", ylab = "", main = "No data matches the selected filters.")
      text(0.5, 0.5, "Adjust filters in the sidebar to see bar graphs.", cex = 1.2, col = "gray")
    }
  })
  
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
