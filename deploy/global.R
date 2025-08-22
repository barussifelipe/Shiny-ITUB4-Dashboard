library(DBI)
library(shiny)
library(shinydashboard)
library(dplyr)
library(shinyWidgets)
library(ggplot2)
library(tidyr)
library(DT)
library(sqldf)


select_year = function(df, year) {
  df %>% 
    select(id, matches(paste0("^[1-4]T(", paste(year, collapse = "|"), ")$")))
  
}
select_quarter = function(df_year, quarter) {
  df_year %>% 
    select(id, matches(paste0("^(", paste(quarter, collapse = "|"), ")T\\d{4}$")))
  
}