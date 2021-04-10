library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)

data_input <- data.table(
  "NA" = c(NA)
)

plot_1 <- ggplot()

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
      fileInput(inputId = "csv_input", label = "Select CSV File to Import", accept = ".csv"), 
      # selectInput(inputId = "column_1", label = "Variable 1", choices = c("Placeholder 1")),
      # selectInput(inputId = "column_2", label = "Variable 2", choices = c("Placeholder 2")),
      uiOutput("dyn_ui_select_1"),
      uiOutput("dyn_ui_var_type_1"),
      uiOutput("dyn_ui_select_2"),
      uiOutput("dyn_ui_var_type_2"),
      br(),
      actionButton(inputId = "run_button", label = "Run Analysis", icon = icon("play"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Plots", 
          plotOutput("plot_1"), 
          wellPanel(
            checkboxInput(inputId = "log_x", label = "Log scale on axis X"),
            checkboxInput(inputId = "log_y", label = "Log scale on axis Y")  
          )
          
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
  
  output$dyn_ui_select_1 <- renderUI({
    selectInput(inputId = "var_1", label = "Variable 1", choices = colnames(data_input()))
  })
  
  output$dyn_ui_var_type_1 <- renderUI({
    req(data_input())
    selectInput(inputId = "var_1_type", label = "Variable 1 Type", choices = c("Factor","Numerical"))
  })
  
  output$dyn_ui_select_2 <- renderUI({
    selectInput(inputId = "var_2", label = "Variable 2", choices = colnames(data_input()))
  })
  
  output$dyn_ui_var_type_2 <- renderUI({
    req(data_input())
    selectInput(inputId = "var_2_type", label = "Variable 2 Type", choices = c("Factor","Numerical"))
  })
  
  observeEvent(input$run_button,{
    
    var_1 <- renderText({input$var_1})
    var_1_type <- renderText({input$var_1_type})
    var_2 <- renderText({input$var_2})
    var_2_type <- renderText({input$var_2_type})
    
      
    if(var_1_type() == "Numerical" & var_2_type() == "Numerical"){
      plot_1 <- ggplot(data = data_input(), aes_string(x = var_1(), y = var_2())) + geom_point()
    }
    else if(var_1_type() == "Numerical" & var_2_type() == "Factor"){
      plot_1 <- ggplot(data = data_input(), aes_string(x = var_1(), color = var_2())) + geom_density()
    }
    
    if(input$log_x == TRUE){plot_1 <- plot_1 + scale_x_log10()}
    if(input$log_y == TRUE){plot_1 <- plot_1 + scale_y_log10()}

  })
  
  output$plot_1 <- renderPlot({plot_1})
  
  
}

shinyApp(ui = ui, server = server)



