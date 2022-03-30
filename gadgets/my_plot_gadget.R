
# PLAN ########################################

# input: data.table
# left bar: 3 dropdowns: variable to show on x / y axis, and a red-amber-green-deselect dropdown
# on the plot, with the brush tool, whatever color is selected, the points are updated accordingly

# LIBRARIES ###################################

library(shiny)
library(miniUI)
library(ggplot2)
library(data.table)

# GADGET ###############################

my_plot_gadget <- function(data, x_axis_variable = NULL, y_axis_variable = NULL){
  
  column_names <- colnames(data)
  colours <- c("red", "amber", "green", "deselect")
  
  draw_main_plot <- function(data, x_axis_variable, y_axis_variable){
    ggplot(data = data, aes_string(x = x_axis_variable, y = y_axis_variable)) + 
      geom_point()
  }
  
  ui <- miniPage(
    gadgetTitleBar("Group Observations"), 
    miniContentPanel(
      fillRow(flex = c(NA, 1), # NA: the space it naturally needs
              fillCol(flex = c(NA, NA, NA), width = "150px", 
                      selectInput("colour", "Colour", colours),
                      selectInput("x_axis_variable", "X Axis Variable", column_names, 
                                  selected = ifelse(is.null(x_axis_variable), 
                                                    column_names[1], x_axis_variable)),
                      selectInput("y_axis_variable", "Y Axis Variable", column_names, 
                                  selected = ifelse(is.null(y_axis_variable), 
                                                    column_names[2], y_axis_variable))
                      ),
              plotOutput("main_plot", height = "100%")
              )
      )
    )
  
  server <- function(input, output){
    
    # x_axis_variable <- reactive(input$x_axis_variable)
    # y_axis_variable <- reactive(input$y_axis_variable)
    # 
    # main_plot <- reactive(draw_main_plot(data, x_axis_variable(), y_axis_variable()))
    # 
    main_plot <- reactive(
      draw_main_plot(
        data, input$x_axis_variable, input$y_axis_variable))
    
    output$main_plot <- renderPlot(main_plot())
    
    # handle cancel and done
    observeEvent(input$cancel, {
      stopApp(NULL)
    })
    
  }
  
  runGadget(ui, server)
  
}




# TEST #################################

# iris_data <- data.table(iris)

my_plot_gadget(iris_data)

my_plot_gadget(iris_data, "Sepal.Length", "Sepal.Width")

colnames(iris_data)

# Notes
# might need a toggle button that determines if we are on the log scale

