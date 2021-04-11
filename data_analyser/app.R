library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)

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
      fileInput("csv_input", "Select CSV File to Import", accept = ".csv"), 
      selectInput("num_var_1", "Numerical Variable 1", choices = c("Not Selected")),
      selectInput("num_var_2", "Numerical Variable 2", choices = c("Not Selected")),
      selectInput("fact_var", "Factor Variable", choices = c("Not Selected")),
      br(),
      actionButton("run_button", "Run Analysis", icon = icon("play"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Plots", 
          plotOutput("plot_1"), 
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
  theme = shinytheme('united'),
  main_page, 
  about_page
)

server <- function(input, output){
  
  data_input <- reactive({
    req(input$csv_input)
    fread(input$csv_input$datapath)
  })
  
  observeEvent(data_input(),{
    choices <- c("Not Selected",names(data_input()))
    updateSelectInput(inputId = "num_var_1", choices = choices)
    updateSelectInput(inputId = "num_var_2", choices = choices)
    updateSelectInput(inputId = "fact_var", choices = choices)
  })
}

shinyApp(ui = ui, server = server)



