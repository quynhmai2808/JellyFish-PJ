source('/qa/data1/_Project_Template/Code/source.R')

ui <-  map_UI("dashboard")

server <- function(input, output , session){
  
  # map_SV() calls moduleServer() inside, therfore need to call directly
  # (lazy load) all global functions in data.R.
  map_SV("dashboard")
}

shinyApp(ui = ui, server = server)
