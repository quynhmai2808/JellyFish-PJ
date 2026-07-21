## Clear objects from workspace, set clock and define time stamp
rm(list=ls())
st <- Sys.time()
runid =  format(st, "%y%m%d")
gc()
options("width"=200)
getwd()

install.packages(c("shiny", "shinydashboard", "DT", "dplyr", "readr", "tibble", 
                   "ggplot2", "plotly"))
