# following tutorial here: 
# https://shiny.rstudio.com/articles/gadgets.html

library(shiny)
library(miniUI)
library(ggplot2)
library(leaflet)
library(DT)

# SOURCES ####################################

# summary: 
# https://shiny.rstudio.com/articles/gadgets.html
# https://shiny.rstudio.com/articles/gadget-ui.html

# good video: 
# https://www.rstudio.com/resources/webinars/introducing-shiny-gadgets-interactive-tools/

# examples (more complicated than basic tutorials)
# https://github.com/rstudio/miniUI/tree/master/examples
# one does the miniTabStripPanel, the other is for the miniButtonBlock

# EXAMPLES #########################################

## 1 - base tutorial as written ---------------------------------------------

ggbrush <- function(data, xvar, yvar) {
  
  ui <- miniPage(
    gadgetTitleBar("Drag to select points"),
    miniContentPanel(
      # The brush="brush" argument means we can listen for
      # brush events on the plot using input$brush.
      plotOutput("plot", height = "100%", brush = "brush")
    )
  )
  
  server <- function(input, output, session) {
    
    # Render the plot
    output$plot <- renderPlot({
      # Plot the data with x/y vars indicated by the caller.
      ggplot(data, aes_string(xvar, yvar)) + geom_point()
    })
    
    # Handle the Done button being pressed.
    observeEvent(input$done, {
      # Return the brushed points. See ?shiny::brushedPoints.
      stopApp(brushedPoints(data, input$brush))
    })
  }
  
  runGadget(ui, server)
}

ggbrush(mtcars, "hp", "mpg")

## 2) - my modifications ------------------------------------
# based on design bit

ggbrush2 <- function(data, xvar, yvar) {
  
  ui <- miniPage(
    # gadgetTitleBar("Drag to select points"), # this automatically includes the Done and Cancel buttons
    
    gadgetTitleBar("my gadget title", 
                   left = miniTitleBarButton("select", "Select", primary = FALSE),
                   right = miniTitleBarButton("done", "Done", primary = FALSE)),
    
    miniContentPanel(
      # The brush="brush" argument means we can listen for
      # brush events on the plot using input$brush.
      plotOutput("plot", height = "100%", brush = "brush"), padding = 0
    )
  )
  
  server <- function(input, output, session) {
    
    # Render the plot
    output$plot <- renderPlot({
      # Plot the data with x/y vars indicated by the caller.
      ggplot(data, aes_string(xvar, yvar)) + geom_point()
    })
    
    # Handle the Done button being pressed.
    observeEvent(input$done, {
      # Return the brushed points. See ?shiny::brushedPoints.
      stopApp(brushedPoints(data, input$brush))
    })
    
    observeEvent(input$cancel, {
      stopApp(NULL)
    })
    
  }
  
  runGadget(ui, server, stopOnCancel = FALSE, viewer = paneViewer(minHeight = 500)) # to render in viewer but set height to min
  # runGadget(ui, server, viewer = dialogViewer("ggbrush")) # to open a popup window
  # runGadget(ui, server, stopOnCancel = FALSE, viewer = browserViewer()) # in a browser window
}

ggbrush2(mtcars, "hp", "mpg")

## 3 - miniTabstripPanel --------------------------

# note: these are completely independent of each other, weird example
# point: there is a navigation bar in the bottom

ui <- miniPage(
  gadgetTitleBar("Shiny gadget example"),
  miniTabstripPanel(
    miniTabPanel("Parameters", icon = icon("sliders"),
                 miniContentPanel(
                   sliderInput("year", "Year", 1978, 2010, c(2000, 2010), sep = "")
                 )
    ),
    miniTabPanel("Visualize", icon = icon("area-chart"),
                 miniContentPanel(
                   plotOutput("cars", height = "100%")
                 )
    ),
    miniTabPanel("Map", icon = icon("map-o"),
                 miniContentPanel(padding = 0,
                                  leafletOutput("map", height = "100%")
                 ),
                 miniButtonBlock(
                   actionButton("resetMap", "Reset")
                 )
    ),
    miniTabPanel("Data", icon = icon("table"),
                 miniContentPanel(
                   DT::dataTableOutput("table")
                 )
    )
  )
)

server <- function(input, output, session) {
  output$cars <- renderPlot({
    require(ggplot2)
    ggplot(cars, aes(speed, dist)) + geom_point()
  })
  
  output$map <- renderLeaflet({
    force(input$resetMap)
    
    leaflet(quakes, height = "100%") %>% addTiles() %>%
      addMarkers(lng = ~long, lat = ~lat)
  })
  
  output$table <- DT::renderDataTable({
    diamonds
  })
  
  observeEvent(input$done, {
    stopApp(TRUE)
  })
}

runGadget(shinyApp(ui, server), viewer = paneViewer())

## 4 - miniButtonBlock -----------------------

# this is quite straightforward, you can put buttons in a small block





# 5 - linear regression example 

# https://gist.github.com/wch/c4b857d73493e6550cba


lmGadget <- function(data, xvar, yvar) {

  ui <- miniPage(
    gadgetTitleBar("Interactive lm"),
    miniContentPanel(
      fillRow(flex = c(NA, 1),
              fillCol(width = "100px",
                      selectInput("degree", "Polynomial degree", c(1, 2, 3, 4))
              ),
              plotOutput("plot1",
                         height = "100%",
                         click = "plot1_click",
                         brush = brushOpts(
                           id = "plot1_brush"
                         )
              )
      )
    ),
    miniButtonBlock(
      actionButton("exclude_toggle", "Toggle points"),
      actionButton("exclude_reset", "Reset")
    )
  )
  
  server <- function(input, output) {
    # For storing which rows have been excluded
    vals <- reactiveValues(
      keeprows = rep(TRUE, nrow(data))
    )
    
    output$plot1 <- renderPlot({
      req(input$degree)
      formula <- as.formula(paste0("y ~ poly(x, degree = ", input$degree, ")"))
      
      # Plot the kept and excluded points as two separate data sets
      keep    <- data[ vals$keeprows, , drop = FALSE]
      exclude <- data[!vals$keeprows, , drop = FALSE]
      
      ggplot(keep, aes_string(xvar, yvar)) + geom_point() +
        geom_smooth(method = lm, formula = formula, fullrange = TRUE, color = "gray50") +
        geom_point(data = exclude, fill = NA, color = "black", alpha = 0.25) +
        coord_cartesian(xlim = range(data[[xvar]]), ylim = range(data[[yvar]])) +
        theme_bw(base_size = 14)
    })
    
    # Toggle points that are clicked
    observeEvent(input$plot1_click, {
      res <- nearPoints(data, input$plot1_click, allRows = TRUE)
      
      vals$keeprows <- xor(vals$keeprows, res$selected_)
    })
    
    # Toggle points that are brushed, when button is clicked
    observeEvent(input$exclude_toggle, {
      res <- brushedPoints(data, input$plot1_brush, allRows = TRUE)
      
      vals$keeprows <- xor(vals$keeprows, res$selected_)
    })
    
    # Reset all points
    observeEvent(input$exclude_reset, {
      vals$keeprows <- rep(TRUE, nrow(data))
    })
    
    # Handle the Done button being pressed.
    observeEvent(input$done, {
      # Replace x and y in the formula with the values in xvar and yvar
      formula <- as.formula(paste0(yvar, " ~ poly(", xvar, ", degree = ", input$degree, ")"))
      keep_data <- data[vals$keeprows, , drop = FALSE]
      
      # Return the kept points.
      stopApp(
        list(
          data = keep_data,
          model = lm(formula, keep_data)
        )
      )
    })
    
  }
  
  runGadget(ui, server)
}

lmGadget(iris, "Sepal.Length", "Sepal.Width")
