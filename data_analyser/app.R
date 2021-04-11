library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)

about_page <- tabPanel(
  title = "About",
  titlePanel("About"),
  "Fill in about section"
)

not_sel <- "Not Selected"

main_page <- tabPanel(
  title = "Analysis",
  titlePanel("Analysis"),
  sidebarLayout(
    sidebarPanel(
      title = "Inputs",
      fileInput("csv_input", "Select CSV File to Import", accept = ".csv"), 
      selectInput("num_var_1", "Numerical Variable 1", choices = c(not_sel)),
      selectInput("num_var_2", "Numerical Variable 2", choices = c(not_sel)),
      selectInput("fact_var", "Factor Variable", choices = c(not_sel)),
      br(),
      actionButton("run_button", "Run Analysis", icon = icon("play"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Plot", 
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
    choices <- c(not_sel,names(data_input()))
    updateSelectInput(inputId = "num_var_1", choices = choices)
    updateSelectInput(inputId = "num_var_2", choices = choices)
    updateSelectInput(inputId = "fact_var", choices = choices)
  })
  
  num_var_1 <- eventReactive(input$run_button,input$num_var_1)
  
  num_var_2 <- eventReactive(input$run_button,input$num_var_2)
  
  fact_var <- eventReactive(input$run_button,input$fact_var)
  
  plot_1 <- eventReactive(input$run_button,{
    if(num_var_1() != not_sel & num_var_2() != not_sel & fact_var() != not_sel){
      ggplot(data = data_input(), 
             aes_string(x = num_var_1(), y = num_var_2(), color = fact_var())) + 
        geom_point() 
    } 
    else if(num_var_1() != not_sel & num_var_2() != not_sel & fact_var() == not_sel){
      ggplot(data = data_input(), 
             aes_string(x = num_var_1(), y = num_var_2())) + 
        geom_point()  
    }
    else if(num_var_1() != not_sel & num_var_2() == not_sel & fact_var() != not_sel){
      ggplot(data = data_input(), 
             aes_string(x = fact_var(), y = num_var_1())) + 
        geom_violin()
    }
    else if(num_var_1() == not_sel & num_var_2() != not_sel & fact_var() != not_sel){
      ggplot(data = data_input(), 
             aes_string(x = fact_var(), y = num_var_2())) + 
        geom_violin()
    }
    else if(num_var_1() != not_sel & num_var_2() == not_sel & fact_var() == not_sel){
      ggplot(data = data_input(), 
             aes_string(x = num_var_1())) + 
        geom_histogram()
    }
    else if(num_var_1() == not_sel & num_var_2() != not_sel & fact_var() == not_sel){
      ggplot(data = data_input(), 
             aes_string(x = num_var_2())) + 
        geom_histogram()
    }
    else if(num_var_1() == not_sel & num_var_2() == not_sel & fact_var() != not_sel){
      ggplot(data = data_input(), 
             aes_string(x = fact_var())) + 
        geom_bar()
    }    
  })
  
  output$plot_1 <- renderPlot(plot_1())
}

shinyApp(ui = ui, server = server)



