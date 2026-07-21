# =============================================================================
# dashboard.R
# -----------------------------------------------------------------------------
# data.R must be sourced before this file, as map_UI() calls get_retailer()
# during UI initialization (once when the application starts).  
# =============================================================================

# Threshold (%) used for week-over-week table highlighting (can be adjusted if needed).
RED_THRESHOLD <- 4      # |diff%| > 4 -> RED
YELLOW_THRESHOLD <- 2   # 2 < |diff%| <= 4 -> YELLOW
                        # |diff%| <= 2 -> GREEN

# UI ----------------------------------------------------------------------

map_UI <- function(id, data_by_group = NULL) {
  ns <- NS(id)

  dashboardPage(
    # skin = "blue",
    dashboardHeader(title = "PMI QC REPORT"),

    dashboardSidebar(
      sidebarMenu(
        menuItem("Inputs", tabName = "inputs", icon = icon("sliders-h")),
        
        # Retailer: Scan dynamic data from the entire data directory (see data.R).
        # FIX #3: ten ham dung la get_retailers() (so nhieu), truoc day goi
        # nham get_retailer() (so it) -> khong ton tai, khien map_UI() loi
        # ngay luc app khoi dong.
        selectInput(ns("retailer"), "Retailer", choices = get_retailers()),
                    # choices = c("EXXON")
                    # choices = if (!is.null(data_by_group)) names(data_by_group) else character(0)),
        
        # Report Name: This list will be updated dynamically based on the selected retailer.
        selectInput(ns("report_name"), "Report Name", choices = NULL),
                    # choices = if (!is.null(data_by_group)) names(data_by_group) else character(0),
                    # selected = if (!is.null(data_by_group) && "PMG" %in% names(data_by_group)) "PMG" else NULL),
        
        # Year: This list will be updated dynamically based on the selected retailer + report name
        selectInput(ns("year"), "Year", choices = NULL),
        
        selectInput(ns("week"), "Week", choices = NULL), # dynamic, depend on year
        
        # Show data button
        actionButton(ns("show_data"), "Show Data", icon = icon("table")),
        hr(),
        
        selectizeInput(ns("selected_weeks"), "Selected Multiple Weeks", 
                       # choices = sprintf("%02d", 1:52), multiple = TRUE,
                       choices = NULL, multiple = TRUE,
                       options = list(placeholder = "Choose one or more weeks...",
                                      plugins = list("remove_button"), maxItems = 10)),
        actionButton(ns("show_report"), "Show Report", icon = icon("chart-line"))
      )
    ),

    dashboardBody(
      tabBox(
        id = "tabs", width = 12,

        # --- Overview: KPI + Data table ---
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

        # --- Weekly Report: Table Week-over-Week + Trend chart ---
        tabPanel("Weekly Report",
                 # Show report
                 conditionalPanel(
                   condition = sprintf("input['%s'] >0 ", ns("show_report")),
                   
                   h4("Weekly Comparison"),
                   fluidRow(
                     # FIX #8: id truoc day la "wow_table_data" nhung server
                     # lai render vao output$wow_table -> KHONG KHOP, bang
                     # nay se khong bao gio hien thi. Doi lai cho dung id.
                     box(title = "Week-over-Week Summary", width = NULL, DT::dataTableOutput(ns("wow_table")))),
                            
                   hr(),
                   h4("Weekly Trend"),
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
# SERVER ------------------------------------------------------------------

map_SV <- function (id, data_by_group) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- RETAILER: Update report type list after selecting retailer
    
    observeEvent(input$retailer, {
      req(input$retailer)
      
      # type_groups <- names(data_by_group[[input$retailer]])
      types <- get_report_types(input$retailer)
      # updateSelectInput(session, "report_name", choices = type_groups)
      updateSelectInput(session, "report_name", choices = types,
                        selected = if("PMG" %in% types) "PMG" else types[1])
    }, ignoreNULL = TRUE)
    

    # --- RETAILER + REPORT NAME: Update year list after selecting retailer + report_name
    # Year shows only available year belong to retailer + report name
    observeEvent(list(input$retailer,input$report_name), {
      req(input$retailer,input$report_name)
      years <- get_available_years(input$retailer,input$report_name)
      
      if (length(years) > 0) {
        updateSelectInput (session, "year", choices = years, selected = tail(years,1))
      } else {
        updateSelectInput (session, "year", choices = character(0))
      }
    }, ignoreNULL = TRUE)
    
    # --- RETAILER + REPORT NAME + YEAR: Update week list after selecting retailer + report_name + year
    # Showing only available weeks
    observeEvent(list(input$retailer,input$report_name,input$year), {
      req(input$retailer,input$report_name,input$year)
      weeks <- get_available_weeks(input$retailer,input$report_name,input$year)
      
      # FIX #6: dieu kien truoc day kiem tra bien "years" (khong ton tai
      # trong scope nay, con lai tu observer khac) thay vi "weeks" -> loi
      # "object 'years' not found" moi khi chon xong Year.
      if (length(weeks) > 0) {
        updateSelectInput (session, "week", choices = weeks, selected = tail(weeks,1))
        updateSelectizeInput (session, "selected_weeks", choices = weeks,
                              selected = tail(weeks, min(6, length(weeks))))
      } else {
        updateSelectInput (session, "week", choices = character(0))
        # FIX #7: dong nay truoc day truyen "choices" 2 LAN
        # (choices = weeks, choices = character(0)) -> R bao loi
        # "formal argument matched by multiple actual arguments".
        updateSelectizeInput (session, "selected_weeks", choices = character(0))
      }
    }, ignoreNULL = TRUE)
    
    
    # ---- Normalized "Volume" column based on report name (Sales/Sticks/Total_G)
    volume_col <- reactive({
      req(input$report_name)
      get_volume_column(input$report_name)
    })
    
    # ---- Reactive: Get data of selected input (only 1 Data table for 1 selected week), after action Show Data ---
    selected_data <- eventReactive(input$show_data, {
      req(input$retailer, input$report_name, input$year, input$week)
      # df <- data_by_group[[input$retailer]][[input$report_name]][[input$week]]
      df <- read_report_data(input$retailer, input$report_name, input$year, input$week)
      validate(need(!is.null(df), "Not found data"))                                                                                                                            
      df
    })
    
    
    # ---- Calculated KPI ----
    #  (1) The calculation logic (sum, n_distinct) is separated from the HTML rendering logic.
    #  (2) This makes the code reusable in the future (e.g., for an Export button or displaying the KPI elsewhere) without having to duplicate the formulas.
    #  (3) It also makes testing/debugging easier (you can call kpis() independently without running the UI).
    
    kpis <- reactive({
      df <- selected_data()
      vol <- sum(df[[volume_col()]], na.rm = TRUE)
      rev <- sum(df$Revenue, na.rm = TRUE)
      
      list(
        sales_volume = vol,
        total_revenue = rev,
        avg_price = if(vol == 0) NA else round(rev/vol,3),
        stores = n_distinct(df$Store),
        has_products = "ProductNr" %in% names(df),
        products = if("ProductNr" %in% names(df)) n_distinct(df$ProductNr) else NA_integer_
      )
    })
    
    # ---- Render KPI active ----
  
    # This option isn't allowed by R shiny. Though defined .small-box.bg-white and .small-box.bg-lightpurple in CSS, Shiny doesn’t know about those custom classes because valueBox() 
    output$kpi_row <- renderUI({
      kpi_vals <- kpis()
      
      kpi_sales <- valueBox(format(kpi_vals$sales_volume, big.mark=","), "Sales Volumn", icon = icon("shopping-cart"), color = "blue", width = 12)
      kpi_revenue <- valueBox(format(round(kpi_vals$total_revenue), big.mark=","), "Total Revenue", icon = icon("dollar-sign"), color = "green", width = 12)
      kpi_price <- valueBox(format(kpi_vals$avg_price, big.mark=","), "Average Price", icon = icon("tag"), color = "yellow", width = 12)
      kpi_stores <- valueBox(kpi_vals$stores, "Stores", icon = icon("store"), color = "orange", width = 12)
      
      if (kpi_vals$has_products) {
        kpi_items <- valueBox(kpi_vals$products, "Products", icon = icon("box"), color = "purple", width = 12)
        
        tagList(
          fluidRow(
            column(4, kpi_sales), column(4, kpi_revenue), column(4, kpi_price)
          ),
          fluidRow(
            column(6, kpi_stores), column(4, kpi_items)
          )
        )
      } else {
        # No Product by Report OTP,CIG,ECIG -> All KPIs in 1 row
        tagList(
          fluidRow(
            column(3, kpi_sales), column(3,kpi_revenue), column(3,kpi_price), column(3,kpi_stores)
          )
        )
        
      }
     
    })
    
  
    output$table_data <- renderDataTable({
      selected_data()
    }, options = list(pageLength = 10, scrollX = TRUE))
    

    # --- TREND Charts with multiple weeks
    # Aggregated data across multiple weeks (lazy loading: read the data only once for all selected weeks, 
    # since they all belong to the same selected year)
    
    multi_weeks_data <- eventReactive(input$show_report, {
      req(input$retailer, input$report_name, input$year, input$selected_weeks)
      
      vcol <- volume_col()
      df_raw <- read_report_data(input$retailer, input$report_name, input$year, input$selected_weeks)
      validate(need(!is.null(df_raw), "There are no data for selected weeks"))
      
      # List of weeks
      weeks_present <- sort(unique(df_raw$Week))
      
      rows <- lapply(weeks_present, function(wk) {
        sub_df <- df_raw[df_raw$Week == wk, ]
        tibble(
          Week = wk,
          Week_Label = sub_df$Week_Label[1],
          Total_Volume = sum(sub_df[[vcol]], na.rm = TRUE),
          Total_Revenue = sum(sub_df$Revenue, na.rm = TRUE),
          Stores_count = n_distinct(sub_df$Store),
          Item_count = if ("ProductNr" %in% names(sub_df)) n_distinct(sub_df$ProductNr) else NA_integer_
        )
      })
      
      # FIX #9: truoc day goi dplyr::bind_rows() KHONG co tham so, luon tra
      # ve data rong -> toan bo bieu do/bang tuan phia sau se trong.
      dplyr::bind_rows(rows)
      
    })
    #--- Week-over-Week Data Table (current week vs previous weeks) ----
    wow_table_data <- reactive({
      df <- multi_weeks_data()
      req(nrow(df) >= 2)
      
      pct_diff <- function(cur, prev) {
        ifelse(prev == 0 | is.na(prev), NA, round((cur-prev)/prev*100,2))
      }
      
      purrr_rows <- lapply(2:nrow(df), function(i){
        cur <- df [i, ];
        prev <- df [i-1, ]
        
        # FIX #10: ten ham go nham "tiblle" -> dung phai la "tibble"
        # (ham cua package tibble), gay loi "could not find function".
        # FIX #11: cot du lieu goc la "Total_Volume", truoc day go nham
        # thanh "Tota_Volume" (thieu chu "l") o ca 2 dong Sales Current/Previous.
        tibble(
          `Current Week` = cur$Week_Label,
          `Previous Week` = prev$Week_Label,
          `Sales (Current)` = cur$Total_Volume,
          `Sales (Previous)` = prev$Total_Volume,
          `Revenue (Current)` = cur$Total_Revenue,
          `Revenue (Previous)` = prev$Total_Revenue,
          `Stores (Current)` = cur$Stores_count,
          `Stores (Previous)` = prev$Stores_count,
          `Items (Current)` = cur$Item_count,
          `Items (Previous)` = prev$Item_count,
          `Sales Diff (%)` = pct_diff(cur$Total_Volume, prev$Total_Volume),
          `Revenue Diff (%)` = pct_diff(cur$Total_Revenue, prev$Total_Revenue),
          `Stores Diff (%)` = pct_diff(cur$Stores_count, prev$Stores_count),
          `Items Diff (%)` = pct_diff(cur$Item_count, prev$Item_count)
        )
      })
      
      # FIX #12: truoc day co 1 dong "dplyr::bind_rows()" KHONG THAM SO nam
      # LAC ben trong lapply (phia sau tibble(...), nen khong bao gio chay
      # toi vi tibble() da la gia tri return cua function con) - va quan
      # trong hon la KET QUA CUOI CUNG cua reactive nay chua bao gio duoc
      # gop lai tu list "purrr_rows" thanh 1 data.frame. Sua bang cach goi
      # dplyr::bind_rows(purrr_rows) LA DONG CUOI CUNG cua reactive.
      dplyr::bind_rows(purrr_rows)
    })
    
    output$wow_table <- DT::renderDT({
      df <- wow_table_data()
      # FIX #13: ten cot khai bao o day ("Volume Diff (%)",...) truoc day
      # KHONG KHOP voi ten cot thuc te duoc tao trong wow_table_data()
      # ("Sales Diff (%)",...) -> DT::formatStyle() khong tim thay cot nao
      # de to mau. Sua lai cho dung ten cot thuc te.
      diff_cols <- c("Sales Diff (%)", "Revenue Diff (%)", "Stores Diff (%)", "Items Diff (%)")
      
      dt <- DT::datatable(df,rownames = FALSE,
                          options = list(pageLength = 10, scrollX = TRUE))
      
      for (col in diff_cols){
        dt <- DT::formatStyle(dt, col,
                              backgroundColor = DT::styleInterval(
                                cuts = c(-RED_THRESHOLD,-YELLOW_THRESHOLD,YELLOW_THRESHOLD,RED_THRESHOLD),
                                values = c("#f8d7da","#fff3cd","#d4edda","#fff3cd","#f8d7da")
                              ))
      }
      
      dt
      
    })
  
    
    # --- Render Sales Trend plots ---
    
    output$plot_sales <- renderPlotly({
      
      # If there are no data, req will be stop rendering without throwing exception.
      req(nrow(multi_weeks_data()) > 0)
      # Optional: Informative
      validate(need(nrow(multi_weeks_data()) > 0, "No data available for this report"))
      
      df <- multi_weeks_data()
                    
      p <- ggplot(df, aes(x = Week_Label, y = Total_Volume, group = 1)) +
        geom_line(color = "blue", size = 1.2) +
        geom_point(color = "blue", size = 3) +
        labs(title = "Sales Trend", y = "Sales Volumn", x = "Week") +
        theme_minimal()
      
      plotly::ggplotly(p)
      
    })

    # --- Render Revenue Trend plots ---
    output$plot_revenue <- renderPlotly({
      
      # If there are no data, req will be stop rendering without throwing exception.
      req(nrow(multi_weeks_data()) > 0)
      # Optional: Informative
      validate(need(nrow(multi_weeks_data()) > 0, "No data available for this report"))
      
      df <- multi_weeks_data()
        
      p <- ggplot(df, aes(x = Week_Label, y = Total_Revenue, group = 1)) +
        geom_line(color = "green", size = 1.2) +
        geom_point(color = "green", size = 3) +
        labs(title = "Revenue Trend", y = "Total Revenue", x = "Week") +
        theme_minimal()
      
      plotly::ggplotly(p)
      
    })
    
    # --- Render Store count Trend plots ---

    output$plot_stores_count <- renderPlotly({
      
      # If there are no data, req will be stop rendering without throwing exception.
      req(nrow(multi_weeks_data()) > 0)
      # Optional: Informative
      validate(need(nrow(multi_weeks_data()) > 0, "No data available for this report"))
      
      df <- multi_weeks_data()
      
      p <- ggplot(df, aes(x = Week_Label, y = Stores_count, group = 1)) +
        geom_line(color = "orange", size = 1.2) +
        geom_point(color = "orange", size = 3) +
        labs(title = "Stores Count Trend", y = "Number of Stores", x = "Week") +
        theme_minimal()
      
      plotly::ggplotly(p)
        
    })

    # --- Render Item count Trend plots ---
    output$plot_item_count <- renderPlotly({
      
      # If there are no data, req will be stop rendering without throwing exception.
      req(nrow(multi_weeks_data()) > 0)
      # Optional: Informative
      validate(need(nrow(multi_weeks_data()) > 0, "No data available for this report"))
      
      df <- multi_weeks_data()
        
      p <- ggplot(df, aes(x = Week_Label, y = Item_count, group = 1)) +
        geom_line(color = "purple", size = 1.2) +
        geom_point(color = "purple", size = 3) +
        labs(title = "Item Count Trend", y = "Number of Items", x = "Week") +
        theme_minimal()
      
      plotly::ggplotly(p)

    })
    
  })
  
}
