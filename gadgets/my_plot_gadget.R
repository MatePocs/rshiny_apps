
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

my_plot_gadget <- function(data){
  
  column_names <- c("test1", "test2")
  colours <- c("red", "amber", "green", "deselect")
  
  ui <- miniPage(
    gadgetTitleBar("Group Observations"), 
    miniContentPanel(
      fillRow(flex = c(NA, 1), #NA: the space it naturally needs
              fillCol(flex = c(NA, NA, NA),  
                      selectInput("colour", "Colour", colours),
                      selectInput("x_axis_variable", "X Axis Variable", column_names),
                      selectInput("y_axis_variable", "Y Axis Variable", column_names)
                      ),
              plotOutput("main_plot", height = "100%")
              )
      )
    )
  
  server <- function(input, output){
    
  }
  
  runGadget(ui, server)
  
}




# TEST #################################

iris_data <- data.table(iris)

my_plot_gadget(iris_data)

# Notes
# might need a toggle button that determines if we are on the log scale

