setwd("~/Documents/Programming/Shiny_Dashboard_ITUB4")

library(shiny)
library(shinydashboard)
library(dplyr)
library(shinyWidgets)

ativos = read.csv("data/ativos.csv")
dre = read.csv("data/DRE.csv")
passivo = read.csv("data/passivo.csv")
dfc = read.csv("data/dfc.csv")


#First, cleanup the ativos data,  I need to merge the data of the three first column, in order to have all blank 
#spaces filled. 
#Create a function for this 

merge_columns <- function(df) {
  df$X <- ifelse(df[, 1] != "", df[, 1], ifelse(df[, 2] != "", df[, 2], df[, 3]))
  df <- df[, -c(2, 3)]
  return(df)
}

ativos = merge_columns(ativos)
dre = merge_columns(dre)
passivo = merge_columns(passivo)

View(ativos)
View(dre)
View(passivo)


#Now that we have the data, we will build the dashboard 

shinyWidgetsGallery()

ui = dashboardPage(
  dashboardHeader(title = "Dashboard ITUB4"),
  dashboardSidebar(disable = TRUE), 
  dashboardBody(
    fluidRow(
      # First column (for year selection)
      column(width = 6,
             checkboxGroupButtons(
               inputId = "ano_picker",
               label = "Selecione o ano:",
               choices = c("2023", "2024"),
               justified = TRUE)
      ),
      
      # Second column (for quarter selection)
      column(width = 6,
             checkboxGroupButtons(
               inputId = "trimestre_picker",
               label = "Selecione o trimestre:",
               choices = c(1, 2, 3, 4),
               justified = TRUE)
      )
    ),
    h2("Painel Gerencial"), 
    fluidRow(
      valueBoxOutput("ativo"),
      valueBoxOutput("passivo"),
      valueBoxOutput("patrimonioliquido"),
      valueBoxOutput("p/vp"), 
      valueBoxOutput("receitaliquida") 
      
    )
  )
)




server = function(input, output) {
  # Server logic can be added here if needed
  
}
# Run the application
shinyApp(ui = ui, server = server)