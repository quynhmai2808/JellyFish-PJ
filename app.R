source('/qa/data1/_Project_Template/Code/source.R')

ui <-  map_UI("dashboard")

# ui <- map_UI

# server <- map_SV

server <- function(input, output , session){
  
  callModule(map_SV, "dashboard", data_by_group)
}

shinyApp(ui = ui, server = server)
