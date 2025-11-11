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

