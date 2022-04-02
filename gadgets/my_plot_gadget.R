
# LIBRARIES ###################################

library(shiny)
library(miniUI)
library(ggplot2)

# GADGET ###############################

my_plot_gadget <- function(data, x_axis_variable = NULL, y_axis_variable = NULL){
  
  column_names <- colnames(data)
  colours <- c("red", "amber", "green", "none")
  
  red_colourcode <- "#D2222D"
  amber_colourcode <- "#FFBF00"
  green_colourcode <- "#238823"
  none_colourcode <- "grey70"
  
  draw_main_plot <- function(data, x_axis_variable, y_axis_variable, colourcodes){
    ggplot(data = data[colourcodes == "none",], 
           aes_string(x = x_axis_variable, y = y_axis_variable)) + 
      geom_point(colour = none_colourcode) + 
      geom_point(data = data[colourcodes == "red",], colour = red_colourcode) + 
      geom_point(data = data[colourcodes == "amber",], colour = amber_colourcode) + 
      geom_point(data = data[colourcodes == "green",], colour = green_colourcode)
  }
  
  ui <- miniPage(
    gadgetTitleBar("Group Observations"), 
    miniContentPanel(
      fillRow(flex = c(NA, 1),
              fillCol(flex = c(NA, NA, NA), width = "150px", 
                      selectInput("colour", "Colour", colours),
                      selectInput("x_axis_variable", "X Axis Variable", column_names, 
                                  selected = ifelse(is.null(x_axis_variable), 
                                                    column_names[1], x_axis_variable)),
                      selectInput("y_axis_variable", "Y Axis Variable", column_names, 
                                  selected = ifelse(is.null(y_axis_variable), 
                                                    column_names[2], y_axis_variable))
                      ),
              plotOutput("main_plot", height = "100%", brush = "main_plot_brush")
              )
      )
    )
  
  server <- function(input, output, session){
    
    results <- reactiveValues(
      colourcodes = rep("none", nrow(data)))
        
    # when there is a brush event, update the colourcodes accordingly
    # also clears the brush, otherwise it just lingers there
    observeEvent(input$main_plot_brush,{
      results$colourcodes[
        brushedPoints(data, input$main_plot_brush, allRows = TRUE)$selected_] <- input$colour
    })
    
    # the plot updates every time the variables or the colourcode changes
    main_plot <- reactive(
      draw_main_plot(
        data, input$x_axis_variable, input$y_axis_variable, results$colourcodes))
    output$main_plot <- renderPlot(main_plot())
    
    observeEvent(main_plot(), {
      session$resetBrush("main_plot_brush")  
    })
    
    # handle cancel and done
    observeEvent(input$cancel, {
      stopApp(NULL)
    })
    observeEvent(input$done, {
      stopApp(results$colourcodes)
    })
  }
  runGadget(ui, server)
}


# HOW TO USE #################################

results <- my_plot_gadget(iris)
results <- my_plot_gadget(iris, "Petal.Length", "Sepal.Width")



