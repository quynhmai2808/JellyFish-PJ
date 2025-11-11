map_UI <- function(id) {
  ns <- NS(id)
  
  dashboardPage(
    dashboardHeader(title = "PMI QC REPORT"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Inputs", tabName = "inputs", icon = icon("sliders-h")),
        selectInput(ns("retailer"), "Retailer", choices = c("EXXON")),
        selectInput(ns("report_name"), "Report Name", choices = c("PMG")),
        numericInput(ns("year"), "Year", value = 2025),
        selectInput(ns("week"), "Week", choices = NULL), #dynamic
        sliderInput(ns("selected_weeks"), "Selected Weeks", min = 1, max = 52, value = c(30, 31))
      )
    ),
    
    dashboardBody(
      tabBox(
        id = "tabs", width = 12,
        tabPanel("Overview",
                 fluidRow(
                   column(6, valueBoxOutput(ns("kpi_sales"))),
                   column(6, valueBoxOutput(ns("kpi_revenue"))),
                   column(6, valueBoxOutput(ns("kpi_items"))),
                   column(6, valueBoxOutput(ns("kpi_stores")))
                 ),
                 hr(),
                 h4("Weekly QC Trend"),
                 fluidRow(
                   column(4, box(title = "Heatmap", width = NULL, plotlyOutput(ns("heatmap")))),
                   column(9,
                          fluidRow(
                            column(5, box(title = "Sales Trend", width = NULL, plotlyOutput(ns("plot_sales")))),
                            column(5, box(title = "Revenue Trend", width = NULL, plotlyOutput(ns("plot_revenue"))))
                          ),
                          fluidRow(
                            column(5, box(title = "Total Sales", width = NULL, plotlyOutput(ns("plot_total_sales")))),
                            column(5, box(title = "Item Count", width = NULL, plotlyOutput(ns("plot_item_count"))))
                          )
                   )
                 )
        ),
        tabPanel("Summary",
                 h4("Data"),
                 dataTableOutput(ns("table_data"))
        )
      )
    )
  )
}
