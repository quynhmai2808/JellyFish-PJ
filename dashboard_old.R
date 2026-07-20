# UI ----------------------------------------------------------------------

map_UI <- function(id, data_by_group = NULL) {
  ns <- NS(id)

  dashboardPage(
    # skin = "blue",
    dashboardHeader(title = "PMI QC REPORT"),

    dashboardSidebar(
      sidebarMenu(
        menuItem("Inputs", tabName = "inputs", icon = icon("sliders-h")),
        # retailer choices are taken from provided data_by_group
        selectInput(ns("retailer"), "Retailer", 
                    # choices = c("EXXON")
                    choices = if (!is.null(data_by_group)) names(data_by_group) else character(0)),
        # report_name choices are taken from provided data_by_group
        selectInput(ns("report_name"), "Report Name",
                    choices = NULL),
                    # choices = if (!is.null(data_by_group)) names(data_by_group) else character(0),
                    # selected = if (!is.null(data_by_group) && "PMG" %in% names(data_by_group)) "PMG" else NULL),
        numericInput(ns("year"), "Year", value = 2025),
        selectInput(ns("week"), "Week", choices = NULL), # dynamic
        # Show data button
        actionButton(ns("show_data"), "Show Data", icon = icon("table")),
        hr(),
        selectizeInput(ns("selected_weeks"), "Selected Multiple Weeks", choices = sprintf("%02d", 1:52), multiple = TRUE,
                       options = list(placeholder = "Choose one or more weeks...",
                                      maxItems = 10)),
        actionButton(ns("show_report"), "Show Report", icon = icon("chart-line"))
      )
    ),

    dashboardBody(
      # tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
      tabBox(
        id = "tabs", width = 12,

        # --- Data table + Value Box ---
        tabPanel("Overview",
                 # Show data
                 conditionalPanel(
                   condition = sprintf("input['%s'] > 0 ", ns("show_data")),
                   fluidRow(
                     uiOutput(ns("kpi_row"))
                     # column(6, valueBoxOutput(ns("kpi_sales"))),
                     # column(6, valueBoxOutput(ns("kpi_revenue"))),
                     # column(6, valueBoxOutput(ns("kpi_items"))),
                     # column(6, valueBoxOutput(ns("kpi_stores")))
                   )
                 ),
                 hr(),
                 h4("Weekly Data Summary"),
                 dataTableOutput(ns("table_data"))
        ),

        # --- Heatmap + Trend charts
        tabPanel("Weekly Report",
                 # Show report
                 conditionalPanel(
                   condition = sprintf("input['%s'] >0 ", ns("show_report")),
                   h4("Weekly QC Trend"),
                   fluidRow(
                     # column(12,
                            # fluidRow(
                            # column(4, box(title = "Heatmap", width = NULL, plotlyOutput(ns("heatmap")))),
                            box(title = "Week-over-Week Summary", width = NULL, DT::dataTableOutput(ns("summary_table")))),
                   hr(),
                   # fluidRow(
                   #   column(12,
                   #          fluidRow(
                   #            column(6, box(title = "Sales Trend", width = NULL, plotlyOutput(ns("plot_sales"), height = "250px"))),
                   #            column(6, box(title = "Revenue Trend", width = NULL, plotlyOutput(ns("plot_revenue"), height = "250px")))
                   #          ),
                   #          fluidRow(
                   #            column(6, box(title = "Stores Count", width = NULL, plotlyOutput(ns("plot_stores_count"), height = "250px"))),
                   #            column(6, box(title = "Item Count", width = NULL, plotlyOutput(ns("plot_item_count"), height = "250px")))
                   #          )))
                   
                   fluidRow(
                     column(12,
                            fluidRow(
                              column(3, box(title = "Sales Trend", width = NULL, plotlyOutput(ns("plot_sales"), height = "250px"))),
                              column(3, box(title = "Revenue Trend", width = NULL, plotlyOutput(ns("plot_revenue"), height = "250px"))),
                              column(3, box(title = "Stores Count", width = NULL, plotlyOutput(ns("plot_stores_count"), height = "250px"))),
                              column(3, box(title = "Item Count", width = NULL, plotlyOutput(ns("plot_item_count"), height = "250px")))
                            )))
                   

                 )
        )
      )
    )
  )
}
#   
# # SERVER ------------------------------------------------------------------

map_SV <- function (id, data_by_group) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- RETAILER: Update report type list after selecting retailer
    
    observeEvent(input$retailer, {
      req(input$retailer)
      
      type_groups <- names(data_by_group[[input$retailer]])
      updateSelectInput(session, "report_name", choices = type_groups)
    })
    

    # --- REPORT NAME: Update week list after selecting retailer + report_name
    observeEvent(input$report_name, {
      req(input$report_name)

      # List of weeks of selected input report name (if available)
      weeks <- NULL
      if (!is.null(data_by_group) && input$report_name %in% names(data_by_group[[input$retailer]])) {
        weeks <- names(data_by_group[[input$retailer]][[input$report_name]])
      }
      
      # sort weeks, newest week as first
      weeks_sorted <- sort(as.numeric(weeks))
      latest_week <- sprintf("%02d", max(weeks_sorted))

      if (!is.null(weeks) && length(weeks) > 0) {
        updateSelectInput(session, "week", choices = weeks_sorted, selected = latest_week)
        updateSelectizeInput(session, "selected_weeks", choices = weeks_sorted, selected = weeks_sorted[length(weeks_sorted) - 5])
      } else {
        updateSelectInput(session, "week", choices = character(0))
        updateSelectizeInput(session, "selected_weeks", choices = character(0))
      }
    }, ignoreNULL = TRUE)

    
    # ---- Reactive: Get data of selected week, after action Show Data ----
    selected_data <- eventReactive(input$show_data, {
      req(input$retailer, input$report_name, input$week)
      df <- data_by_group[[input$retailer]][[input$report_name]][[input$week]]
      return(df)
    })
    
    
    # ---- Calculated KPI ----
    kpis <- reactive({
      df <- selected_data()
      list(
        Total_Sales = if ("Total_Sales" %in% names(df)) sum(df[["Total_Sales"]], na.rm = TRUE) else NA,
        Total_Revenue = if ("Total_Revenue" %in% names(df)) sum(df[["Total_Revenue"]], na.rm = TRUE) else NA,
        Unique_Stores = if ("StoreNr" %in% names(df)) dplyr::n_distinct(df[["StoreNr"]]) else NA,
        Unique_Products = if ("Product_Nr" %in% names(df)) dplyr::n_distinct(df[["Product_Nr"]]) else NA
      )
    })
    
    
    # ---- Render KPI active ----
  
    # This option isn't allowed by R shiny. Though defined .small-box.bg-white and .small-box.bg-lightpurple in CSS, Shiny doesn’t know about those custom classes because valueBox() 
    output$kpi_row <- renderUI({
      kpi_vals <- kpis()
      fluidRow(
        if (!is.na(kpi_vals$Total_Sales)) column(6, valueBox(format(kpi_vals$Total_Sales, big.mark=","), "Sales Volumn", icon = icon("shopping-cart"), color = "blue")),
        if (!is.na(kpi_vals$Total_Revenue)) column(6, valueBox(format(kpi_vals$Total_Revenue, big.mark=","), "Total Revenue", icon = icon("dollar-sign"), color = "green")),
        if (!is.na(kpi_vals$Unique_Stores)) column(6, valueBox(kpi_vals$Unique_Stores, "Stores", icon = icon("store"), color = "orange")),
        if (!is.na(kpi_vals$Unique_Products)) column(6, valueBox(kpi_vals$Unique_Products, "Products", icon = icon("boxes"), color = "purple"))
      )
    })
    
    # Alternative UI for design in style css
    # output$kpi_row <- renderUI({
    #   kpi_vals <- kpis()
    #   fluidRow(
    #     if (!is.na(kpi_vals$Total_Sales)) column(6, div(class = "small-box bg-purple", div(class = "inner",
    #                                                                                           h3(format(kpi_vals$Total_Sales, big.mark=",")), p("Sales Volume")),
    #                                                     div(class = "icon", icon("shopping-cart")))),   
    #     
    #     if (!is.na(kpi_vals$Total_Revenue)) column(6, div(class = "small-box bg-black", div(class = "inner",
    #                                                                                           h3(format(kpi_vals$Total_Revenue, big.mark=",")), p("Total Revenue")),
    #                                                     div(class = "icon", icon("dollar-sign")))),
    #     
    #     if (!is.na(kpi_vals$Unique_Stores)) column(6, div(class = "small-box bg-white", div(class = "inner",
    #                                                                                          h3(format(kpi_vals$Unique_Stores, big.mark=",")), p("Stores")),
    #                                                       div(class = "icon", icon("store")))),
    #     
    #     if (!is.na(kpi_vals$Unique_Products)) column(6, div(class = "small-box bg-lightpurple", div(class = "inner",
    #                                                                                          h3(format(kpi_vals$Unique_Products, big.mark=",")), p("Products")),
    #                                                       div(class = "icon", icon("boxes")))),
    #     
    #   )
    # })
    
    
    
    output$table_data <- renderDataTable({
      selected_data()
    })
    

    # --- TREND Charts with multiple weeks
    
    selected_weeks_data <- eventReactive(input$show_report, {
      req(input$retailer, input$report_name, input$selected_weeks)
      
      # List of weeks
      weeks <- input$selected_weeks
      
      # Get data from data_by_group by retailer + report_name
      df_list <- data_by_group[[input$retailer]][[input$report_name]][weeks]
      
      # Gộp tất cả tuần
      combined_df <- bind_rows(df_list, .id = "Week")  # .id for keeping week name
      combined_df$Week <- factor(combined_df$Week, levels = weeks)  # sort of week is saved
      
      return(combined_df)
      
    })
    
    # --- Render Sales Trend plots ---
    
    output$plot_sales <- renderPlotly({
      
      # If there are no data, req will be stop rendering without throwing exception.
      req(nrow(selected_weeks_data()) > 0)
      # Optional: Informative
      validate(need(nrow(selected_weeks_data()) > 0, "No data available for this report"))
      
      df <- selected_weeks_data()
      # print(df)
      
      if ("Total_Sales" %in% names(df)) {
        
        df_summary <- df %>%
          group_by(Week) %>%
          summarise(Total_Sales = sum(Total_Sales))
                    
        p <- ggplot(df_summary, aes(x = Week, y = Total_Sales, group = 1)) +
          geom_line(color = "blue", size = 1.2) +
          geom_point(color = "blue", size = 3) +
          labs(title = "Sales Trend", y = "Sales Volumn", x = "Week") +
          theme_minimal()
        ggplotly(p)
      }
      
    })

    # --- Render Revenue Trend plots ---
    output$plot_revenue <- renderPlotly({
      
      # If there are no data, req will be stop rendering without throwing exception.
      req(nrow(selected_weeks_data()) > 0)
      # Optional: Informative
      validate(need(nrow(selected_weeks_data()) > 0, "No data available for this report"))
      
      df <- selected_weeks_data()
      if ("Total_Revenue" %in% names(df)) {
        
        df_summary <- df %>%
          group_by(Week) %>%
          summarise(Total_Revenue = sum(Total_Revenue))
        
        p <- ggplot(df_summary, aes(x = Week, y = Total_Revenue, group = 1)) +
          geom_line(color = "green", size = 1.2) +
          geom_point(color = "green", size = 3) +
          labs(title = "Revenue Trend", y = "Total Revenue", x = "Week") +
          theme_minimal()
        ggplotly(p)
      }
      
      
    })
    
    # --- Render Store count Trend plots ---

    output$plot_stores_count <- renderPlotly({
      
      # If there are no data, req will be stop rendering without throwing exception.
      req(nrow(selected_weeks_data()) > 0)
      # Optional: Informative
      validate(need(nrow(selected_weeks_data()) > 0, "No data available for this report"))
      
      df <- selected_weeks_data()
      if ("StoreNr" %in% names(df)) {
        store_count <- df %>%
          group_by(Week) %>%
          summarise(Stores = n_distinct(StoreNr))
        
        p <- ggplot(store_count, aes(x = Week, y = Stores, group = 1)) +
          geom_line(color = "orange", size = 1.2) +
          geom_point(color = "orange", size = 3) +
          labs(title = "Stores Count Trend", y = "Number of Stores", x = "Week") +
          theme_minimal()
        ggplotly(p)
      }
        
    })

    # --- Render Item count Trend plots ---
    output$plot_item_count <- renderPlotly({
      
      # If there are no data, req will be stop rendering without throwing exception.
      req(nrow(selected_weeks_data()) > 0)
      # Optional: Informative
      validate(need(nrow(selected_weeks_data()) > 0, "No data available for this report"))
      
      df <- selected_weeks_data()
      if ("Product_Nr" %in% names(df)) {
        item_count <- df %>%
          group_by(Week) %>%
          summarise(Items = n_distinct(Product_Nr))
        
        p <- ggplot(item_count, aes(x = Week, y = Items, group = 1)) +
          geom_line(color = "purple", size = 1.2) +
          geom_point(color = "purple", size = 3) +
          labs(title = "Item Count Trend", y = "Number of Items", x = "Week") +
          theme_minimal()
        ggplotly(p)
      }

    })
    
    # --- Heatmap ----
    # output$heatmap <- renderPlotly({
    #   
    #   req(input$retailer, input$report_name, input$selected_weeks)
    #   
    #   selected_weeks <- sort(input$selected_weeks)
    #   
    #   # Get data for selected weeks
    #   data_compare <- purrr::map(selected_weeks, function(w) {
    #     raw_df <- data_by_group[[input$retailer]][[input$report_name]][[w]]
    #     
    #     df <- raw_df %>%
    #       summarise(Total_Sales = sum(Total_Sales, na.rm = TRUE),
    #                 Total_Revenue = sum(Total_Revenue, na.rm = TRUE))
    #     
    #     df$week <- w
    #     
    #     return (df)
    #     
    #   }) %>% bind_rows()
    #   
    #   # Create table data for comparing week over week
    #   
    #   diff_list <- list()
    #   
    #   for (i in 2:length(selected_weeks)) {
    #     current_week <- selected_weeks[i]
    #     prev_week <- selected_weeks[i - 1]
    #     
    #     current <- data_compare %>% filter(week == current_week)
    #     prev <- data_compare %>% filter(week == prev_week)
    #     
    #     diff_sales <- (current$Total_Sales - prev$Total_Sales) / prev$Total_Sales * 100
    #     diff_revenue <- (current$Total_Revenue - prev$Total_Revenue) / prev$Total_Revenue * 100
    #     
    #     diff_list[[current_week]] <- tibble(
    #       week = current_week,
    #       diff_sales_pct = round(diff_sales, 2),
    #       diff_revenue_pct = round(diff_revenue, 2)
    #     )
    #   }
    #   
    #   
    #   heatmap_data <- bind_rows(diff_list) %>%
    #     tidyr::pivot_longer(cols = starts_with("diff"), names_to = "metric", values_to = "diff")
    #   
    #   # Render heatmap plot
    #   
    #   zmin_val <- min(heatmap_data$diff, na.rm = TRUE)
    #   zmax_val <- max(heatmap_data$diff, na.rm = TRUE)
    #   
    #   
    #   p <- plot_ly(
    #     data = heatmap_data,
    #     x = ~week,
    #     y = ~metric,
    #     z = ~diff,
    #     type = "heatmap",
    #     zmin = zmin_val,
    #     zmax = zmax_val,
    #     colorscale = list(
    #       c(0, "#FF0000"),    
    #       c(0.2, "#FF7F00"), 
    #       c(0.4, "#FFFF00"),  
    #       c(0.6, "#ADFF2F"),  
    #       c(0.8, "#32CD32"),  
    #       c(1, "#006400")     
    #     ),
    #     showscale = TRUE,
    #     height = 500,
    #     width = 700
    #   ) %>%
    #     add_annotations(
    #       text = paste0(round(heatmap_data$diff, 1), "%"),
    #       x = heatmap_data$week,
    #       y = heatmap_data$metric,
    #       showarrow = FALSE,
    #       font = list(color = "black", size = 14)
    #     ) %>%
    #     layout(
    #       title = list(text = "Week-over-Week Difference", font = list(size = 20)),
    #       yaxis = list(title = "", tickfont = list(size = 12)),
    #       xaxis = list(title = "Current Week", tickfont = list(size = 12)),
    #       margin = list(l = 80, r = 40, t = 60, b = 80),  
    #       plot_bgcolor = "#F9F9F9",  
    #       paper_bgcolor = "#FFFFFF"  
    #     )
    # }) 
    
    #--- Summary Data ----
    
  output$summary_table <- DT::renderDataTable({
    req(input$retailer, input$report_name, input$selected_weeks)

    selected_weeks <- sort(input$selected_weeks)

    diff_list <- list()
    for (i in 2:length(selected_weeks)) {
      current_week <- selected_weeks[i]
      prev_week <- selected_weeks[i - 1]

      current <- data_by_group[[input$retailer]][[input$report_name]][[current_week]] %>%
        summarise(Total_Sales = sum(Total_Sales, na.rm = TRUE),
                  Total_Revenue = sum(Total_Revenue, na.rm = TRUE),
                  Items = n_distinct(Product_Nr),
                  Stores = n_distinct(StoreNr))

      prev <- data_by_group[[input$retailer]][[input$report_name]][[prev_week]] %>%
        summarise(Total_Sales = sum(Total_Sales, na.rm = TRUE),
                  Total_Revenue = sum(Total_Revenue, na.rm = TRUE),
                  Items = n_distinct(Product_Nr),
                  Stores = n_distinct(StoreNr))

      diff_sales <- (current$Total_Sales - prev$Total_Sales) / prev$Total_Sales * 100
      diff_revenue <- (current$Total_Revenue - prev$Total_Revenue) / prev$Total_Revenue * 100
      diff_stores <- (current$Stores - prev$Stores) / prev$Stores * 100
      diff_items <- (current$Items - prev$Items) / prev$Items * 100

      diff_list[[current_week]] <- tibble(
        `Current Week` = current_week,
        `Previous Week` = prev_week,
        `Sales (Current)` = current$Total_Sales,
        `Sales (Previous)` = prev$Total_Sales,
        `Revenue (Current)` = current$Total_Revenue,
        `Revenue (Previous)` = prev$Total_Revenue,
        `Stores (Current)` = current$Stores,
        `Stores (Previous)` = prev$Stores,
        `Items (Current)` = current$Items,
        `Items (Previous)` = prev$Items,
        `Sales Diff (%)` = round(diff_sales, 2),
        `Revenue Diff (%)` = round(diff_revenue, 2),
        `Stores Diff (%)` = round(diff_stores, 2),
        `Items Diff (%)` = round(diff_items, 2)

      )
    }

    summary_df <- bind_rows(diff_list)

    DT::datatable(summary_df, options = list(pageLength = 10)) %>%
      DT::formatStyle(
        'Sales Diff (%)',
        backgroundColor = DT::styleInterval(c(-4, -2, 2, 4), c('red', 'yellow', 'lightgreen', 'yellow', 'red')),
        color = 'black'
      ) %>%
      DT::formatStyle(
        'Revenue Diff (%)',
        backgroundColor = DT::styleInterval(c(-4, -2, 2, 4), c('red', 'yellow', 'lightgreen', 'yellow', 'red')),
        color = 'black'
      ) %>%
      DT::formatStyle(
        'Stores Diff (%)',
        backgroundColor = DT::styleInterval(c(-4, -2, 2, 4), c('red', 'yellow', 'lightgreen', 'yellow', 'red')),
        color = 'black'
      ) %>%
      DT::formatStyle(
        'Items Diff (%)',
        backgroundColor = DT::styleInterval(c(-4, -2, 2, 4), c('red', 'yellow', 'lightgreen', 'yellow', 'red')),
        color = 'black'
      )



    })

  })
}
 
#-------4 KPI Box, but solid, not reactived ----

# output$kpi_sales <- renderValueBox({
#   df <- selected_data()
#   valueBox(
#     subtitle = "Total Sales",
#     value = if (!is.null(df)) safe_sum(df, "Sales") else NA,
#     color = "blue",
#     icon = icon("shopping-cart")
#   )
# })
# 
# output$kpi_revenue <- renderValueBox({
#   df <- selected_data()
#   valueBox(
#     subtitle = "Total Revenue",
#     value = if (!is.null(df)) safe_sum(df, "Revenue") else NA,
#     color = "green",
#     icon = icon("dollar-sign")
#   )
# })
# 
# output$kpi_items <- renderValueBox({
#   df <- selected_data()
#   valueBox(
#     subtitle = "Total Item",
#     value = if (!is.null(df)) safe_n_distinct(df, "ProductNr") else NA,
#     color = "purple",
#     icon = icon("box")
#   )
# })
# 
# output$kpi_stores <- renderValueBox({
#   df <- selected_data()
#   # server uses "Store" as normalized name above
#   valueBox(
#     subtitle = "Stores count",
#     value = if (!is.null(df)) safe_n_distinct(df, "Store") else NA,
#     color = "orange",
#     icon = icon("store")
#   )
# })


