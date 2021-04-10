library(shiny)
library(data.table)
library(ggplot2)

data_input <- data.table(
  "NA" = c(NA)
)

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
      uiOutput("dyn_ui_factor_1"),
      uiOutput("dyn_ui_select_2"),
      uiOutput("dyn_ui_factor_2"),
      br(),
      actionButton(inputId = "run_button", label = "Run Analysis")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Plots", 
          plotOutput("plot_1"), 
          checkboxInput(inputId = "log_x", label = "Log scale on axis X"),
          checkboxInput(inputId = "log_y", label = "Log scale on axis Y")
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
  
  data_input <- reactive({
    req(input$csv_input)
    fread(input$csv_input$datapath)
    })
  
  output$dyn_ui_select_1 <- renderUI({
    selectInput(inputId = "var_1", label = "Variable 1", choices = colnames(data_input()))
  })
  
  output$dyn_ui_factor_1 <- renderUI({
    req(data_input())
    selectInput(inputId = "var_1_factor", label = "Variable 1 Type", choices = c("Factor","Numerical"))
  })
  
  output$dyn_ui_select_2 <- renderUI({
    selectInput(inputId = "var_2", label = "Variable 2", choices = colnames(data_input()))
  })
  
  output$dyn_ui_factor_2 <- renderUI({
    req(data_input())
    selectInput(inputId = "var_2_factor", label = "Variable 2 Type", choices = c("Factor","Numerical"))
  })


  
  observeEvent(input$run_button,{
    
    var_1 <- renderText({input$var_1})
    
    var_2 <- renderText({input$var_2})
    
    
    output$plot_1 <- renderPlot({
      base_plot <- ggplot(data = data_input(), aes_string(x = var_1(), y = var_2()))
      plot_1 <- base_plot + geom_point()
      if(input$log_x == TRUE){plot_1 <- plot_1 + scale_x_log10()}
      if(input$log_y == TRUE){plot_1 <- plot_1 + scale_y_log10()}
      plot_1
    })
  })
  
  
}

shinyApp(ui = ui, server = server)



