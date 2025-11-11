# UI ----------------------------------------------------------------------

map_UI <- function(id) {
  ns <- NS(id)
  
  dashboardPage(
    dashboardHeader(title = "PMI QC REPORT"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Inputs", tabName = "inputs", icon = icon("sliders-h")),
        selectInput(ns("retailer"), "Retailer", choices = c("EXXON")),
        selectInput(ns("report_name"), "Report Name", choices = names(data_by_group), selected = "PMG"),
        numericInput(ns("year"), "Year", value = 2025),
        selectInput(ns("week"), "Week", choices = NULL), #dynamic
        # Show data button
        actionButton(ns("show_data"), "Show Data", icon = icon("table")),
        hr(),
        selectizeInput(ns("selected_weeks"), "Selected Multiple Weeks", choices = sprintf("%02d", 1:52), multiple = TRUE, 
                       options = list(placeholder = "Choose one or more weeks...",
                                      plugins = list("Reset"), maxItems = 10)),
        actionButton(ns("show_report"), "Show Report", icon = icon("chart-line"))
      )
    ),
    
    dashboardBody(
      tabBox(
        id = "tabs", width = 12,
        
        # --- Data table + Value Box ---
        
        tabPanel("Overview",
                 # Show data
                 conditionalPanel(
                   condition = sprintf("input['%s'] > 0 ", ns("show_data")),
                   fluidRow(
                     column(6, valueBoxOutput(ns("kpi_sales"))),
                     column(6, valueBoxOutput(ns("kpi_revenue"))),
                     column(6, valueBoxOutput(ns("kpi_items"))),
                     column(6, valueBoxOutput(ns("kpi_stores")))
                    )
                 ),
                 hr(),
                 h4("Weekly Data Summary"),
                 dataTableOutput(ns("table_data"))
          )
        ),
                 
        # --- Heatmap + Trend charts
        tabPanel("Weekly Report",
                 # Show report
                 conditionalPanel(
                   condition = sprintf("input['%s'] >0 ", ns("show_report")),
                 h4("Weekly QC Trend"),
                 fluidRow(
                   column(4, box(title = "Heatmap", width = NULL, plotlyOutput(ns("heatmap")))),
                   column(9,
                          fluidRow(
                            column(5, box(title = "Sales Trend", width = NULL, plotlyOutput(ns("plot_sales")))),
                            column(5, box(title = "Revenue Trend", width = NULL, plotlyOutput(ns("plot_revenue"))))
                          ),
                          fluidRow(
                            column(5, box(title = "Stores Count", width = NULL, plotlyOutput(ns("plot_stores_count")))),
                            column(5, box(title = "Item Count", width = NULL, plotlyOutput(ns("plot_item_count"))))
                          )
                   )
                 )
                )
        )
    )
  )
}

# SERVER ------------------------------------------------------------------

map_SV <- function (id, data_by_group) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- REPORT NAME: Update week list after selecting report_name
    observeEvent(input$report_name, { # this function used for re-run, when user selects new
      req(input$report_name)
      
      # List of weeks of selected input report name
      weeks <- names(data_by_group[[input$report_name]])
      
      # If week is valid, then update UI
      if (!is.null(weeks) && length(weeks) > 0) {
      # # PMG Data
      # if(input$report_name == "PMG"){ # e.x. User choose PMG
      #   
      #   # List of weeks of data_by_group$PMG
      #   weeks <- names(data_by_group[["PMG"]])
        
        # Update dropdown "Week" by PMG List, select single week used for KPI Box value
        updateSelectInput(session, "week", choices = weeks, selected = weeks[1])
        
        # Update multiple weeks selector, used for Trends
        updateSelectizeInput(session, "selected_weeks", choices = weeks, selected = head(weeks,2))
        
      } else { # If week is invalid or no data, then reset input
        
        updateSelectInput(session, "week", choices = character(0))
        updateSelectizeInput(session, "selected_weeks", choices = character(0))
        
      }
    })
    
    # Here can be developed to adjust data performance by each report (if...eles). Because EAN Level only in PMG and Missing Item.
    # OTP has no Total Sales, but Total G
    
    
    # --- KPI Box data of single week
    selected_data <- eventReactive(input$show_data, {
      req(input$report_name, input$week)
      df <- data_by_group[[input$report_name]][[as.character(input$week)]] 
      
      if (input$report_name == "PMG") {
        df <- setNames(df, c("FileName", "Date", "Store", "ProductNr", "Description", "Sales", "Sticks", "Revenue", "Month")) 
      } else if (input$report_name == "CIG") {
        df <- setNames(df, c("FileName", "Date", "Store", "Sticks", "Revenue", "Month")) 
      } else if (input$report_name == "ECIG_9961") {
        df <- setNames(df, c("FileName", "Date", "Store", "Sales", "Revenue", "Month"))
      } else if (input$report_name == "ECIG_9962") {
        df <- setNames(df, c("FileName", "Date", "Store", "Sales", "Revenue", "Month"))
      } else if (input$report_name == "ECIG_9963") {
        df <- setNames(df, c("FileName", "Date", "Store", "Sales", "Revenue", "Month"))
      } else if (input$report_name == "OTP") {
        df <- setNames(df, c("FileName", "Date", "Store", "Total_G", "Revenue", "Month"))
      } else {
        df <- setNames(df, c("FileName", "Store", "ProductNr", "Description", "WGR", "Sales", "Revenue"))
      }
        
    })
    
    output$kpi_sales <- renderValueBox({
      df <- selected_data() 
      valueBox(sum(df$Sales, na.rm = TRUE), "Total Sales", color = "blue")
    })
    
    output$kpi_revenue <- renderValueBox({
      df <- selected_data() 
      valueBox(sum(df$Revenue, na.rm = TRUE), "Total Revenue", color = "green")
    })
    
    output$kpi_item <- renderValueBox({
      df <- selected_data() 
      valueBox(n_distinct(df$ProductNr), "Total Item", color = "purple")
    })
    
    output$kpi_store <- renderValueBox({
      df <- selected_data() 
      valueBox(n_distinct(df$Stores), "Stores count", color = "orange")
    })
    
    output$table_data <- renderDataTable({
      selected_data()
    })
    
    # --- TREND Charts with multiple weeks
    multi_week_data <- eventReactive(input$show_report, {
      req(input$report_name, input$selected_weeks)
      weeks <- as.character(input$selected_weeks)
      data_lst <- data_by_group[[input$report_name]][weeks]
      data_lst <- data_lst[!sapply(data_lst, is.null)]
      
      df_summary <- lapply(names(data_lst), function(wk) {
        df <- data_lst[[wk]]
        tibble(
          Week = as.numeric(wk),
          Total_Sales = sum(df$Sales, na.rm = TRUE),
          Total_Revenue = sum(df$Revenue, na.rm = TRUE),
          Item_count = n_distinct(df$ProductNr),
          Stores_count = n_distinct(df$Store),
          
        )
      }) %>% dplyr::bind_rows()
      
      df_summary
    })
    
    # --- Heatmap ----
    output$heatmap <- renderPlotly({
      
    })
    
    # --- Trend plots ---
    output$plot_sales <- renderPlotly({
      df <- multi_week_data()
      p <- ggplot(df, aes(x = Week, y = Total_Sales)) + geom_line() + geom_point() + theme_minimal() + labs(y = "Total Sales", x = "Week")
      plotly::ggplotly(p)
    })
  
    output$plot_revenue <- renderPlotly({
      df <- multi_week_data()
      p <- ggplot(df, aes(x = Week, y = Total_Revenue)) + geom_line() + geom_point() + theme_minimal() + labs(y = "Total Revenue", x = "Week")
      plotly::ggplotly(p)
    })
    
    output$plot_stores_count <- renderPlotly({
      df <- multi_week_data()
      p <- ggplot(df, aes(x = Week, y = Stores_count)) + geom_line() + geom_point() + theme_minimal() + labs(y = "Store count", x = "Week")
      plotly::ggplotly(p)
    })
    
    output$plot_item_count <- renderPlotly({
      df <- multi_week_data()
      p <- ggplot(df, aes(x = Week, y = Item_count)) + geom_line() + geom_point() + theme_minimal() + labs(y = "Total Item", x = "Week")
      plotly::ggplotly(p)
    })
    
  })
}
