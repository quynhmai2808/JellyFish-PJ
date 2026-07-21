## Clear objects from workspace, set clock and define time stamp
rm(list=ls())
st <- Sys.time()
runid =  format(st, "%y%m%d")
gc()
options("width"=200)
getwd()

required_pkgs <- c("shiny", "shinydashboard", "DT", "dplyr", "readr", "tibble",
                    "ggplot2", "plotly")

# FIX: chi cai package nao CHUA co san, tranh install lai moi lan source.R
# chay (moi lan mo app se rat cham neu luon install.packages toan bo).
missing_pkgs <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)

# FIX: truoc day file nay CHI install.packages() ma khong co library(),
# nen cac ham cua shiny/shinydashboard/ggplot2/plotly (goi khong tien to
# package::) trong dashboard.R/app.R se bao loi "could not find function".
invisible(lapply(required_pkgs, library, character.only = TRUE))
