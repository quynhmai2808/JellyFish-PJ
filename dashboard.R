# Shiny Dashboard: Report Management (Template)
# Save this file as Shiny_Dashboard_app.R and run: shiny::runApp('Shiny_Dashboard_app.R')

library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(lubridate)
library(plotly)
library(DT)

# ---------- Helper: load data ----------
# Adjust `DATA_PATH` for your environment. The app will try to read all CSV files
# under DATA_PATH matching pattern "MRBW" (or adjust pattern as needed).

DATA_PATH <- "./data" # <-- change to your path or keep for sample
FILE_PATTERN <- "MRBW" # adjust if filenames contain a different identifier

load_reports <- function(path = DATA_PATH, pattern = FILE_PATTERN) {
  if (!dir.exists(path)) {
    # fallback: generate sample data
    message("Data folder not found: generating sample data")
    set.seed(123)
    n <- 500
    df <- data.frame(
      customer = sample(c("Kunde A","Kunde B","Kunde C"), n, TRUE),
      store = paste0("Store_", sample(1:20, n, TRUE)),
      product_number = sample(sprintf("P%04d", 1:200), n, TRUE),
      artikel = sample(c("Artikel X","Artikel Y","Artikel Z","Artikel Q"), n, TRUE),
      total_sticks = sample(1:100, n, TRUE),
      total_revenue = round(runif(n, 5, 500), 2),
      month = sample(seq(ymd('2024-01-01'), ymd('2025-12-01'), by='1 month'), n, TRUE),
      report_type = sample(c("Mau A","Mau B","Mau C"), n, TRUE),
      stringsAsFactors = FALSE
    )
    return(df)
  }

  files <- list.files(path = path, pattern = pattern, full.names = TRUE, recursive = TRUE)
  if (length(files) == 0) {
    stop('No files found in ', path, ' with pattern ', pattern)
  }

  # Try to read each file as CSV; if fails, skip and warn
  read_single <- function(f) {
    tryCatch({
      df <- read_csv(f, show_col_types = FALSE)
      # standardize column names expected by app
      # Expecting: store, product_number, artikel, total_sticks, total_revenue, month, customer (optional), report_type (optional)
      names(df) <- tolower(names(df))
      # ensure required cols exist
      if (!"month" %in% names(df)) df$month <- lubridate::today()
      df
    }, error = function(e){
      warning('Failed to read ', f, ': ', e$message)
      NULL
    })
  }

  dfs <- lapply(files, read_single)
  dfs <- dfs[!sapply(dfs, is.null)]
  if (length(dfs) == 0) stop('No readable files')
  df <- bind_rows(dfs)

  # Try to coerce types
  if ("month" %in% names(df)) {
    df$month <- suppressWarnings(ymd(df$month))
    if (all(is.na(df$month))) df$month <- parse_date_time(df$month, orders = c('ym','Y-m','Y'))
  }
  df
}

# ---------- UI ----------
ui <- dashboardPage(
  dashboardHeader(title = "Report Management Dashboard"),
  dashboardSidebar(
    width = 300,
    selectInput("customer", "Customer:", choices = NULL, multiple = TRUE),
    selectInput("report_type", "Report Type:", choices = c("Mau A","Mau B","Mau C"), multiple = TRUE),
    dateRangeInput("date_range", "Month range:", start = Sys.Date() - months(6), end = Sys.Date()),
    actionButton("refresh", "Load / Refresh Data"),
    hr(),
    checkboxInput("show_sample", "Use sample data (if data folder missing)", value = TRUE),
    br(),
    downloadButton("download_filtered", "Download filtered CSV")
  ),
  dashboardBody(
    fluidRow(
      valueBoxOutput("vb_revenue", width = 4),
      valueBoxOutput("vb_sticks", width = 4),
      valueBoxOutput("vb_stores", width = 4)
    ),
    fluidRow(
      box(title = "Monthly Revenue Trend", status = "primary", solidHeader = TRUE, width = 6,
          plotlyOutput("plot_revenue", height = "300px")
      ),
      box(title = "Top Products by Revenue", status = "primary", solidHeader = TRUE, width = 6,
          plotlyOutput("plot_top_products", height = "300px")
      )
    ),
    fluidRow(
      box(title = "Detailed Data", width = 12, solidHeader = TRUE,
          DTOutput("table_data")
      )
    )
  )
)

# ---------- Server ----------
server <- function(input, output, session) {
  # Reactive store for data
  data_all <- reactiveVal(NULL)

  observeEvent(input$refresh, {
    df <- tryCatch({
      if (!input$show_sample) {
        load_reports()
      } else {
        # Try load from path, fallback to sample
        tryCatch({
          load_reports()
        }, error = function(e){
          message('Falling back to sample data: ', e$message)
          load_reports(path = tempfile())
        })
      }
    }, error = function(e){
      showNotification(paste('Error loading data:', e$message), type = 'error')
      NULL
    })
    data_all(df)

    # update selectors
    if (!is.null(df)) {
      updateSelectInput(session, "customer", choices = unique(df$customer), selected = unique(df$customer)[1])
      updateSelectInput(session, "report_type", choices = unique(df$report_type), selected = unique(df$report_type))
    }
  }, ignoreNULL = FALSE)

  filtered <- reactive({
    df <- data_all()
    req(df)
    # filter by customer
    if (!is.null(input$customer) && length(input$customer) > 0) {
      df <- df %>% filter(customer %in% input$customer)
    }
    # report type
    if (!is.null(input$report_type) && length(input$report_type) > 0) {
      df <- df %>% filter(report_type %in% input$report_type)
    }
    # date range by month
    if (!is.null(input$date_range)) {
      start <- as_date(input$date_range[1])
      end <- as_date(input$date_range[2])
      if ("month" %in% names(df)) {
        df <- df %>% filter(!is.na(month) & month >= start & month <= end)
      }
    }
    df
  })

  # KPI boxes
  output$vb_revenue <- renderValueBox({
    df <- filtered()
    total_rev <- ifelse(nrow(df)==0, 0, sum(df$total_revenue, na.rm = TRUE))
    valueBox(format(round(total_rev,2), big.mark = ","), "Total Revenue", icon = icon("euro-sign"), color = "green")
  })

  output$vb_sticks <- renderValueBox({
    df <- filtered()
    total_sticks <- ifelse(nrow(df)==0, 0, sum(df$total_sticks, na.rm = TRUE))
    valueBox(format(total_sticks, big.mark = ","), "Total Sticks", icon = icon("box"), color = "blue")
  })

  output$vb_stores <- renderValueBox({
    df <- filtered()
    stores_n <- ifelse(nrow(df)==0, 0, n_distinct(df$store))
    valueBox(stores_n, "Active Stores", icon = icon("store"), color = "yellow")
  })

  # Plots
  output$plot_revenue <- renderPlotly({
    df <- filtered()
    req(df)
    # aggregate by month
    if (!"month" %in% names(df)) return(NULL)
    df2 <- df %>% group_by(month = floor_date(month, 'month')) %>% summarize(rev = sum(total_revenue, na.rm = TRUE)) %>% arrange(month)
    p <- plot_ly(df2, x = ~month, y = ~rev, type = 'scatter', mode = 'lines+markers') %>% layout(yaxis = list(title = 'Revenue'))
    p
  })

  output$plot_top_products <- renderPlotly({
    df <- filtered()
    req(df)
    df2 <- df %>% group_by(product_number, artikel) %>% summarize(rev = sum(total_revenue, na.rm = TRUE)) %>% arrange(desc(rev)) %>% slice_head(n = 10)
    p <- plot_ly(df2, x = ~rev, y = ~paste0(product_number, ' - ', artikel), type = 'bar', orientation = 'h') %>% layout(xaxis = list(title = 'Revenue'), yaxis = list(title = 'Product'))
    p
  })

  # Data table
  output$table_data <- renderDT({
    df <- filtered()
    req(df)
    datatable(df, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })

  # Download
  output$download_filtered <- downloadHandler(
    filename = function() {
      paste0('filtered_reports_', Sys.Date(), '.csv')
    },
    content = function(file) {
      df <- filtered()
      write_csv(df, file)
    }
  )
}

---------------------------------------------------------------------------------------------------
mainUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    # Sidebar Inputs
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("retailer"), "Retailer", choices = c("Metro")),
        selectInput(ns("report_name"), "Report Name", choices = c("PMG")),
        numericInput(ns("year"), "Year", value = 2025),
        sliderInput(ns("week"), "Week", min = 1, max = 52, value = 40),
        sliderInput(ns("selected_weeks"), "Selected Weeks", min = 1, max = 52, value = c(1, 40))
      ),
      
      mainPanel(
        navbarPage("PMI QC REPORT",
          tabPanel("Overview",
            fluidRow(
              column(3, valueBoxOutput(ns("kpi_sales"))),
              column(3, valueBoxOutput(ns("kpi_revenue"))),
              column(3, valueBoxOutput(ns("kpi_items"))),
              column(3, valueBoxOutput(ns("kpi_stores")))
            ),
            fluidRow(
              column(6,
                box(title = "Heatmap", width = NULL, plotlyOutput(ns("heatmap")))
              ),
              column(6,
                fluidRow(
                  column(6, box(title = "Sales Trend", width = NULL, plotlyOutput(ns("plot_sales")))),
                  column(6, box(title = "Revenue Trend", width = NULL, plotlyOutput(ns("plot_revenue"))))
                ),
                fluidRow(
                  column(6, box(title = "Total Sales", width = NULL, plotlyOutput(ns("plot_total_sales")))),
                  column(6, box(title = "Item Count", width = NULL, plotlyOutput(ns("plot_item_count"))))
                )
              )
            )
          ),
          
          tabPanel("Summary",
            h3("Summary Panel – Inhalt folgt")
          )
        )
      )
    )
  )
}
























# Run the app
shinyApp(ui, server)
