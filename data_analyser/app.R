library(shiny)
library(data.table)

data_input <- data.table()

about_page <- tabPanel(
  title = "About",
  titlePanel("About"),
  "Fill in about section"
)

main_page <- tabPanel(
  title = "Analysis",
  titlePanel("Analysis"),
  sidebarLayout(
    sidebarPanel(
      title = "Inputs",
      fileInput(inputId = "csv_input", label = "Select file to import", accept = ".csv"), 
      # selectInput(inputId = "column_1", label = "Column 1", choices = c("Placeholder 1", "Placeholder 2")),
      # selectInput(inputId = "column_2", label = "Column 2", choices = c("Placeholder 3", "Placeholder 4")),
      uiOutput("dyn_ui_1"),
      uiOutput("dyn_ui_2"),
      br(),
      actionButton(inputId = "run_button", label = "Run Analysis")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Plot", 
          plotOutput("plot")
        ),
        tabPanel(
          title = "Statistics", 
          tableOutput("table")
        )
      )
    )
  )
)



ui <- navbarPage(
  title = "Data Analyser", 
  main_page, 
  about_page
)

server <- function(input, output){
  data_input <- reactive({fread(input$csv_input$datapath)})
  
  output$dyn_ui_1 <- renderUI({
    selectInput(inputId = "column_1", label = "Column 1", choices = colnames(data_input()))
  })

  output$table <- renderTable({colnames(data_input())})
}

shinyApp(ui = ui, server = server)



