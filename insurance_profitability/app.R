# INSURANCE PROFITABILITY ANALYSIS

library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)

not_sel <- "Not Selected"

original_features <- c("area", "region", "driving_restriction", 
                       "make", "fuel", "transmission", "num_veh_seats",
                       "ph_employment_status", "ph_business_desc", 
                       "ph_occupation_name", "owner_type")

grouped_features <- c("grp_unplugged_journeys", "grp_num_unplugs", "grp_num_journeys", 
                      "grp_total_miles", "grp_ncd", "grp_engine_size", "grp_veh_age", 
                      "grp_years_owned", "grp_veh_value", "grp_ph_age", "grp_ph_licence_years")

features_list <- c(not_sel, original_features, grouped_features)

dt <- fread('data/data_cleaned.csv')

about_page <- tabPanel(
  title = "About",
  titlePanel("About"),
  "Created with R Shiny",
  br(),
  "Mate Pocs",
  br(),
  "2021 November"
)

main_page <- tabPanel(
  title = "Profitability",
  titlePanel("Analysis"),
  sidebarLayout(
    sidebarPanel(
      title = "Inputs",
      selectInput("col_to_analyse", "feature to analyse", choices = features_list),
      numericInput("exposure_limit", "exposure threshold", value = 0),
      numericInput("count_limit", "count threshold", value = 0),
      numericInput("claim_max", "individual claim max", value = 0),
      sliderInput("slider_green", "green profitability range", min = -1.0, max = 1.0, value = c(-0.1, 0.1), step = 0.05),
      sliderInput("slider_amber", "amber profitability range", min = -1.0, max = 1.0, value = c(-0.3, 0.3), step = 0.05),
      br(),
      actionButton("run_button", "Run Analysis", icon = icon("play"), style = "width:150px"),
      br(),
      br(),
      actionButton("delete_button", "Clear Analysis", icon = icon("trash"), style = "width:150px")
    ),
    mainPanel(
      tabsetPanel(id = "main_tabset",
        tabPanel(
          title = "Plots",value = "plots",
          plotOutput("plot_1", click = "plot_click"),
          plotOutput("plot_2"),
          plotOutput("plot_3")
        ),
        tabPanel(
          title = "Table",value = "table",
          tableOutput("table_1")
        ),
        tabPanel(
          title = "Details",value = "details",
          textOutput("category_value"),
          plotOutput("plot_4"),
          plotOutput("plot_5")
        )
      )
    )
  )
)

create_od_dt <- function(
  data_input, col_to_analyse, exposure_limit, 
  count_limit, claim_max, low_exposure_name,
  amber_lower, amber_upper, green_lower, green_upper){
  
  od_dt <- copy(data_input)
  
  # claims maximised if input is not 0
  if(claim_max > 0){
    od_dt[,claims_incurred := pmin(claims_incurred, claim_max)]
  }
  
  od_dt[,eval(col_to_analyse) := as.character(get(col_to_analyse))]
  
  od_dt <- od_dt[,.(
    count = .N, 
    exposure = sum(exposure),
    claims_incurred = sum(claims_incurred), 
    premium_gross = sum(premium_gross),
    premium = sum(premium_gross * exposure)
  ), 
  by = eval(col_to_analyse)]
  
  # create LOW_EXPOSURE group under threshold
  
  od_dt[count < count_limit | exposure < exposure_limit, eval(col_to_analyse) := low_exposure_name]
  
  od_dt <- od_dt[,.(
    count = sum(count), 
    exposure = sum(exposure),
    claims_incurred = sum(claims_incurred), 
    premium_gross = sum(premium_gross),
    premium = sum(premium)
  ), 
  by = eval(col_to_analyse)]
  
  od_dt[,profit_ratio := (premium - claims_incurred) / (premium)]
  
  # re-order analysed column so it's in the correct order in a factor
  od_dt <- rbind(
    od_dt[get(col_to_analyse) != low_exposure_name,][order(profit_ratio)],
    od_dt[get(col_to_analyse) == low_exposure_name,])
  # put low_exposure name at the end
  factor_levels <- c(od_dt[get(col_to_analyse) != low_exposure_name,get(col_to_analyse)],low_exposure_name)
  # TODO 
  # right now, we assume there will be a low_exposure name, make it conditional
  od_dt[,eval(col_to_analyse) := factor(get(col_to_analyse), levels = factor_levels)]
  
  # add RAG
  od_dt[profit_ratio < amber_lower, RAG := "R"]
  od_dt[profit_ratio >= amber_lower & profit_ratio < green_lower, RAG := "A"]
  od_dt[profit_ratio >= green_lower & profit_ratio <= green_upper, RAG := "G"]
  od_dt[profit_ratio > green_upper & profit_ratio <= amber_upper, RAG := "A"]
  od_dt[profit_ratio > amber_upper, RAG := "R"]
  
  od_dt[,RAG := factor(RAG, levels = c("R", "A", "G"))]

  return(od_dt)
}

draw_empty_plot <- function(){
  return(ggplot())
}

draw_plot_1 <- function(data_input, col_to_analyse){
  p1 <- ggplot(data = data_input, aes(x = get(col_to_analyse), y = profit_ratio, fill = RAG)) + 
    geom_bar(stat = 'identity', color = "grey70") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.x=element_blank(), legend.position = "none") + 
    labs(y = "profit ratio") + 
    scale_fill_manual(values = c("#D2222D", "#FFBF00", "#238823"), breaks = c("R", "A", "G"))
  return(p1)
}

draw_plot_2 <- function(data_input, col_to_analyse){
  plot_tbl <- data_input[,.(get(col_to_analyse),premium, claims_incurred)]
  setnames(plot_tbl, old = "V1", new = col_to_analyse)
  plot_tbl <- melt.data.table(plot_tbl, measure_vars = c("claims_incurred", "premium"), id.vars = c(col_to_analyse))
  p2 <- ggplot(data = plot_tbl, aes(x = get(col_to_analyse), y = value, fill  = variable)) + 
    geom_bar(position = 'dodge', stat = 'identity', color = "grey70") + 
    scale_fill_manual(values = c("dodgerblue4", "red4")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "top",axis.title.x=element_blank()) + 
    labs(y = "GBP")
  return(p2)
}

draw_plot_3 <- function(data_input, col_to_analyse){
  p3 <- ggplot(data = data_input, aes(x = get(col_to_analyse), y = exposure)) + 
    geom_bar(stat = 'identity', fill = "salmon2", color = "grey70") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.x=element_blank()) + 
    labs(y = "exposure")
  return(p3)
}

# TODO
# technically, you can pass on LOW EXPOSURE to the function below
# it won't crash
# but probably should drop a warning message / don't activate Details tab

draw_plot_4 <- function(data_input, col_to_analyse, category_value){
  # premium_plot
  plot_tbl <- data_input[get(col_to_analyse) == category_value,]
  p4 <- ggplot(data = plot_tbl, aes(x = premium)) + 
    geom_histogram(fill = "dodgerblue4", color = "grey70") + 
    scale_y_continuous(breaks = pretty_breaks()) + 
    scale_x_continuous(breaks = pretty_breaks()) + 
    labs(title = "Premium histogram", subtitle = "all policies, net") + 
    xlab("net premium (GBP)") + 
    ylab("count") + 
    theme(plot.margin=unit(c(0.1,0.75,0.1,0.1),"cm"))
  return(p4)
}

draw_plot_5 <- function(data_input, col_to_analyse, category_value){
  # claims plot
  plot_tbl <- data_input[get(col_to_analyse) == category_value & claims_incurred > 0,]
  p5 <- ggplot(data = plot_tbl, aes(x = claims_incurred)) + 
    geom_histogram(fill = "red4", color = "grey70") + 
    scale_y_continuous(breaks = pretty_breaks()) + 
    scale_x_continuous(label = comma, breaks = breaks_pretty()) +
    labs(title = "Claims incurred histogram", subtitle = "positive claims only") +
    xlab("claims incurred (GBP)") + 
    ylab("count") + 
    theme(plot.margin=unit(c(0.1,0.75,0.1,0.1),"cm"))
  return(p5)
}

get_category_value_from_plot <- function(data_input, col_to_analyse, x){
  return(levels(data_input[,get(col_to_analyse)])[x])
}


ui <- navbarPage(
  title = "Car Insurance Company",
  theme = shinytheme('united'),
  main_page,
  about_page
)

server <- function(input, output, session){
  
  options(shiny.maxRequestSize=10*1024^2) 

  # input fields
  
  col_to_analyse <- eventReactive(input$run_button,input$col_to_analyse)
  exposure_limit <- eventReactive(input$run_button,input$exposure_limit)
  count_limit <- eventReactive(input$run_button,input$count_limit)
  claim_max <- eventReactive(input$run_button,input$claim_max)
  green_lower <- eventReactive(input$run_button,input$slider_green[1])
  green_upper <- eventReactive(input$run_button,input$slider_green[2])
  amber_lower <- eventReactive(input$run_button,input$slider_amber[1])
  amber_upper <- eventReactive(input$run_button,input$slider_amber[2])
  
  plot_click_x <- eventReactive(input$plot_click, round(input$plot_click$x))
  
  # table
  
  od_dt <- eventReactive(input$run_button,{
    create_od_dt(data_input = dt, col_to_analyse = col_to_analyse(), 
                 exposure_limit = exposure_limit(),count_limit = count_limit(),
                 claim_max = claim_max(), 
                 green_lower = green_lower(), green_upper = green_upper(),
                 amber_lower = amber_lower(), amber_upper = amber_upper(),
                 low_exposure_name = "LOW EXPOSURE")
  })
  output$table_1 <- renderTable(od_dt(),colnames = TRUE)
  
  # plots
  
  plot_1 <- eventReactive(input$run_button,{
    draw_plot_1(data_input = od_dt(), col_to_analyse = col_to_analyse())
  })
  output$plot_1 <- renderPlot(plot_1())
  
  plot_2 <- eventReactive(input$run_button,{
    draw_plot_2(data_input = od_dt(), col_to_analyse = col_to_analyse())
  })
  output$plot_2 <- renderPlot(plot_2())
  
  plot_3 <- eventReactive(input$run_button,{
    draw_plot_3(data_input = od_dt(), col_to_analyse = col_to_analyse())
  })
  output$plot_3 <- renderPlot(plot_3())
  
  # details tab
  # this is a bit wobbly now, assume we have an od_dt
  category_value <- eventReactive(input$plot_click, {
    get_category_value_from_plot(data_input = od_dt(), col_to_analyse = col_to_analyse(), x = plot_click_x())
  })
  output$category_value <- renderText(paste0(col_to_analyse(), " = " ,category_value()))
  
  plot_4 <- eventReactive(input$plot_click,{
    draw_plot_4(data_input = dt, col_to_analyse = col_to_analyse(), category_value = category_value())
  })
  output$plot_4 <- renderPlot(plot_4())
  
  plot_5 <- eventReactive(input$plot_click,{
    draw_plot_5(data_input = dt, col_to_analyse = col_to_analyse(), category_value = category_value())
  })
  output$plot_5 <- renderPlot(plot_5())
  
  # note: this needs a session
  # after click, activate
  observeEvent(input$plot_click,{
    updateTabsetPanel(session, "main_tabset", selected = "details")
  })
  
  # delete button
  observeEvent(input$delete_button,{
    output$plot_1 <- renderPlot(NULL)
    output$plot_2 <- renderPlot(NULL)
    output$plot_3 <- renderPlot(NULL)
    output$table_1 <- renderTable(NULL)
  })
  
}

shinyApp(ui = ui, server = server)
