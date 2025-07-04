setwd("~/Documents/Programming/Shiny_Dashboard_ITUB4")

library(shiny)
library(shinydashboard)
library(dplyr)
library(shinyWidgets)
library(ggplot2)
library(tidyr)

ativos = read.csv("data/ativo.csv")
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
dfc = merge_columns(dfc)

View(ativos)
View(dre)
View(passivo)
View(dfc)


#Now that we have the data, we will build the dashboard 

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
               choices = c("1", "2", "3", "4"),
               justified = TRUE)
      )
    ),
    h2("Painel Gerencial"), 
    fluidRow(
      valueBoxOutput("variacaocaixa", width = 3),
      valueBoxOutput("caixa", width = 3),
      valueBoxOutput("ativo", width = 3),
      valueBoxOutput("passivo", width = 3),
      valueBoxOutput("patrimonioliquido", width = 3),
      valueBoxOutput("receitabruta", width = 3),
      valueBoxOutput("perdasfinanceiras", width = 3),
      valueBoxOutput("receitaliquida", width = 3),
      valueBoxOutput("despesaoperacional", width = 3),
      valueBoxOutput("EBIT", width = 3), 
      valueBoxOutput("lucroliquido", width = 3),
      valueBoxOutput("margemebit", width = 3),
      valueBoxOutput("margemliquida", width = 3),
      valueBoxOutput("ROE", width = 3)
      ),
    fluidRow(
      box(plotOutput("lucroliquidoplot", height = 350, width = NULL)), 
      box(plotOutput("receitamargens", height = 350, width = NULL))
          )
    )
  )





server = function(input, output) {
  select_year = function(df, year) {
    df %>% 
      select(matches(paste0("^X[1-4]T(", paste(year, collapse = "|"), ")$")))
  }
  select_quarter = function(df_year, quarter) {
    df_year %>% 
      select(matches(paste0("^X(", paste(quarter, collapse = "|"), ")T\\d{4}$")))
  }
  
  convert_negative <- function(x) {
    ifelse(grepl("^\\s*\\(.*\\)\\s*$", x), 
           -as.numeric(gsub("[(),]", "", x)),
           as.numeric(x))
  }
  
  
  output$variacaocaixa <- renderValueBox({
    # Ensure inputs are available
    req(input$ano_picker, input$trimestre_picker)
    
    # Convert reactive inputs to static values
    selected_years <- as.character(input$ano_picker)
    selected_quarters <- as.character(input$trimestre_picker)
    
    # Filter data (non-reactive operations)
    dfc_year <- select_year(dfc, selected_years)  # dfc must be a dataframe, not a function
    dfc_quarter <- select_quarter(dfc_year, selected_quarters)
    
    # Calculate value
    caixa_values <- sapply(dfc_quarter[62, ], convert_negative)
    caixa_total <- sum(caixa_values, na.rm = TRUE)
    
    # Return valueBox with proper formatting
    valueBox(
      value = tags$p(style = "font-size: 60%;", caixa_total),
      subtitle = "Variação de Caixa (R$ Milhões)",
      icon = icon("money-bill-wave"),
      color = ifelse(caixa_total >= 0, "green", "red")
    )
  })
  
  output$caixa <- renderValueBox({
    req(input$ano_picker, input$trimestre_picker)
    
    selected_years <- as.character(input$ano_picker)
    selected_quarters <- as.character(input$trimestre_picker)
    
    dfc_year <- select_year(dfc, selected_years)
    dfc_quarter <- select_quarter(dfc_year, selected_quarters)
    
    caixa_values <- sapply(dfc_quarter[65, ], convert_negative)
    caixa_total <- sum(caixa_values, na.rm = TRUE)
    
    valueBox(
      value = tags$p(style = "font-size: 60%;", caixa_total),
      subtitle = "Caixa (R$ Milhões)",
      icon = icon("money-bill-wave"),
      color = ifelse(caixa_total >= 0, "green", "red")
    )
  })
  
  output$ativo <- renderValueBox({
    req(input$ano_picker, input$trimestre_picker)
    
    selected_years <- as.character(input$ano_picker)
    selected_quarters <- as.character(input$trimestre_picker)
    
    ativos_year <- select_year(ativos, selected_years)
    ativos_quarter <- select_quarter(ativos_year, selected_quarters)
    
    ativo_values <- ativos_quarter[26, ncol(ativos_quarter)]
    a
    
    valueBox(
      value = tags$p(style = "font-size: 60%;", ativo_values),
      subtitle = "Ativo Total (R$ Milhões)",
      icon = icon("warehouse"),
      color = "blue"
    )
  })
  
  output$passivo <- renderValueBox({
    req(input$ano_picker, input$trimestre_picker)
    
    selected_years <- as.character(input$ano_picker)
    selected_quarters <- as.character(input$trimestre_picker)
    
    passivo_year <- select_year(passivo, selected_years)
    passivo_quarter <- select_quarter(passivo_year, selected_quarters)
    
    passivo_values <- passivo_quarter[20, ncol(passivo_quarter)]
    
    
    valueBox(
      value = tags$p(style = "font-size: 60%;", passivo_values),
      subtitle = "Passivo Total (R$ Milhões)",
      icon = icon("warehouse"),
      color = "blue"
    )
  })
  
  output$patrimonioliquido <- renderValueBox({
    req(input$ano_picker, input$trimestre_picker)
    
    selected_years <- as.character(input$ano_picker)
    selected_quarters <- as.character(input$trimestre_picker)
    
    passivo_year <- select_year(passivo, selected_years)
    passivo_quarter <- select_quarter(passivo_year, selected_quarters)
    
    patrimonio_values <- sapply(passivo_quarter[28, ncol(passivo_quarter)], convert_negative)
    
    
    valueBox(
      value = tags$p(style = "font-size: 60%;", patrimonio_values),
      subtitle = "Patrimônio Líquido (R$ Milhões)",
      icon = icon("warehouse"),
      color = ifelse(patrimonio_values >= 0, "green", "red")
    )
  })
  
  output$receitabruta <- renderValueBox({
    req(input$ano_picker, input$trimestre_picker)
    
    selected_years <- as.character(input$ano_picker)
    selected_quarters <- as.character(input$trimestre_picker)
    
    dre_year <- select_year(dre, selected_years)
    dre_quarter <- select_quarter(dre_year, selected_quarters)
    
    receita_values <- sapply(dre_quarter[1, ], convert_negative)
    receita_total <- sum(receita_values, na.rm = TRUE)
    
    valueBox(
      value = tags$p(style = "font-size: 60%;", receita_total),
      subtitle = "Receita Bruta (R$ Milhões)",
      icon = icon("money-bill-wave"),
      color = ifelse(receita_total >= 0, "green", "red")
    )
  })
  
  output$perdasfinanceiras <- renderValueBox({
    req(input$ano_picker, input$trimestre_picker)
    
    selected_years <- as.character(input$ano_picker)
    selected_quarters <- as.character(input$trimestre_picker)
    
    dre_year <- select_year(dre, selected_years)
    dre_quarter <- select_quarter(dre_year, selected_quarters)
    
    perdas_values <- sapply(dre_quarter[12, ], convert_negative)
    perdas_total <- sum(perdas_values, na.rm = TRUE)
    
    valueBox(
      value = tags$p(style = "font-size: 60%;", perdas_total),
      subtitle = "Perdas Financeiras (R$ Milhões)",
      icon = icon("money-bill-wave"),
      color = ifelse(perdas_total >= 0, "green", "red")
    )
  })
  
  output$receitaliquida <- renderValueBox({
    req(input$ano_picker, input$trimestre_picker)
    
    selected_years <- as.character(input$ano_picker)
    selected_quarters <- as.character(input$trimestre_picker)
    
    dre_year <- select_year(dre, selected_years)
    dre_quarter <- select_quarter(dre_year, selected_quarters)
    
    receita_liquida_values <- sapply(dre_quarter[15, ], convert_negative)
    receita_liquida_total <- sum(receita_liquida_values, na.rm = TRUE)
    
    valueBox(
      value = tags$p(style = "font-size: 60%;", receita_liquida_total),
      subtitle = "Receita Líquida (R$ Milhões)",
      icon = icon("money-bill-wave"),
      color = ifelse(receita_liquida_total >= 0, "green", "red")
    )
  })
  
  output$despesaoperacional <- renderValueBox({
    req(input$ano_picker, input$trimestre_picker)
    
    selected_years <- as.character(input$ano_picker)
    selected_quarters <- as.character(input$trimestre_picker)
    
    dre_year <- select_year(dre, selected_years)
    dre_quarter <- select_quarter(dre_year, selected_quarters)
    
    despesa_values <- sapply(dre_quarter[16, ], convert_negative)
    despesa_total <- sum(despesa_values, na.rm = TRUE)
    
    valueBox(
      value = tags$p(style = "font-size: 60%;", despesa_total),
      subtitle = "Despesa Operacional (R$ Milhões)",
      icon = icon("money-bill-wave"),
      color = ifelse(despesa_total >= 0, "green", "red")
    )
  })
  
  output$EBIT <- renderValueBox({
    req(input$ano_picker, input$trimestre_picker)
    
    selected_years <- as.character(input$ano_picker)
    selected_quarters <- as.character(input$trimestre_picker)
    
    dre_year <- select_year(dre, selected_years)
    dre_quarter <- select_quarter(dre_year, selected_quarters)
    
    ebit_values <- sapply(dre_quarter[20, ], convert_negative)
    ebit_total <- sum(ebit_values, na.rm = TRUE)
    
    valueBox(
      value = tags$p(style = "font-size: 60%;", ebit_total),
      subtitle = "EBIT (R$ Milhões)",
      icon = icon("money-bill-wave"),
      color = ifelse(ebit_total >= 0, "green", "red")
    )
  })
  
  output$lucroliquido <- renderValueBox({
    req(input$ano_picker, input$trimestre_picker)
    
    selected_years <- as.character(input$ano_picker)
    selected_quarters <- as.character(input$trimestre_picker)
    
    dre_year <- select_year(dre, selected_years)
    dre_quarter <- select_quarter(dre_year, selected_quarters)
    
    lucro_values <- sapply(dre_quarter[23, ], convert_negative)
    lucro_total <- sum(lucro_values, na.rm = TRUE)
    
    valueBox(
      value = tags$p(style = "font-size: 60%;", lucro_total),
      subtitle = "Lucro Líquido (R$ Milhões)",
      icon = icon("money-bill-wave"),
      color = ifelse(lucro_total >= 0, "green", "red")
    )
  })
  
  output$margemebit <- renderValueBox({
    req(input$ano_picker, input$trimestre_picker)
    
    selected_years <- as.character(input$ano_picker)
    selected_quarters <- as.character(input$trimestre_picker)
    
    dre_year <- select_year(dre, selected_years)
    dre_quarter <- select_quarter(dre_year, selected_quarters)
    
    receita_values <- sapply(dre_quarter[15, ], convert_negative)
    ebit_values <- sapply(dre_quarter[20, ], convert_negative)
    
    margem_ebit <- sum(ebit_values, na.rm = TRUE) / sum(receita_values, na.rm = TRUE) * 100
    
    valueBox(
      value = tags$p(style = "font-size: 60%;", round(margem_ebit, 2)),
      subtitle = "Margem EBIT (%)",
      icon = icon("percent"),
      color = ifelse(margem_ebit >= 0, "green", "red")
    )
  })
  
  output$margemliquida <- renderValueBox({
    req(input$ano_picker, input$trimestre_picker)
    
    selected_years <- as.character(input$ano_picker)
    selected_quarters <- as.character(input$trimestre_picker)
    
    dre_year <- select_year(dre, selected_years)
    dre_quarter <- select_quarter(dre_year, selected_quarters)
    
    receita_values <- sapply(dre_quarter[15, ], convert_negative)
    lucro_values <- sapply(dre_quarter[23, ], convert_negative)
    
    margem_liquida <- sum(lucro_values, na.rm = TRUE) / sum(receita_values, na.rm = TRUE) * 100
    
    valueBox(
      value = tags$p(style = "font-size: 60%;", round(margem_liquida, 2)),
      subtitle = "Margem Líquida (%)",
      icon = icon("percent"),
      color = ifelse(margem_liquida >= 0, "green", "red")
    )
  })
  
  output$ROE <- renderValueBox({
    req(input$ano_picker, input$trimestre_picker)
    
    selected_years <- as.character(input$ano_picker)
    selected_quarters <- as.character(input$trimestre_picker)
    
    passivo_year <- select_year(passivo, selected_years)
    passivo_quarter <- select_quarter(passivo_year, selected_quarters)
    
    patrimonio_values <- sapply(passivo_quarter[28, ], convert_negative)
    patrimonio_total <- sum(patrimonio_values, na.rm = TRUE)
    
    dre_year <- select_year(dre, selected_years)
    dre_quarter <- select_quarter(dre_year, selected_quarters)
    
    lucro_values <- sapply(dre_quarter[23, ], convert_negative)
    lucro_total <- sum(lucro_values, na.rm = TRUE)
    
    roe <- (lucro_total / patrimonio_total) * 100
    
    valueBox(
      value = tags$p(style = "font-size: 60%;", round(roe, 2)),
      subtitle = "ROE (%)",
      icon = icon("percent"),
      color = ifelse(roe >= 0, "green", "red")
    )
  })
  
  labels = colnames(dre)[-1]
  lucro = sapply(dre[23, -1], convert_negative)
  
  
  output$lucroliquidoplot = renderPlot({
    ggplot(data = data.frame(lucro), aes(x = labels, y = lucro)) + 
      geom_col(aes(fill = ifelse(lucro >= 0, "Positive", "Negative"))) + 
      geom_text(aes(label = round(lucro, 2)), vjust = -0.5, color = "black", size = 3) +
      scale_fill_manual(values = c("Positive" = "green", "Negative" = "red")) +
      labs(title = "Lucro Líquido por Trimestre", x = "Trimestres", y = "Lucro Líquido (R$ Milhões)",
           fill = "Resultado") +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "top",
        plot.title = element_text(hjust = 0.5)
      )
    
      
  })
  output$receitamargens <- renderPlot({
    # Extract and convert values
    receita <- sapply(dre[1, -1], convert_negative)
    ebit <- sapply(dre[20, -1], convert_negative)
    lucro <- sapply(dre[23, -1], convert_negative)
    
    # Calculate margins
    ebit_margin <- round((ebit/receita)*100, 1)
    lucro_margin <- round((lucro/receita)*100, 1)
    
    # Create dataframe
    df <- data.frame(
      Trimestres = labels, 
      Receita = receita, 
      EBIT = ebit, 
      Lucro = lucro,
      EBIT_margin = ebit_margin,
      Lucro_margin = lucro_margin
    )
    
    # Reshape to long format
    df_long <- df %>%
      pivot_longer(
        cols = c(Receita, EBIT, Lucro),
        names_to = "Metrica",
        values_to = "Valor"
      ) %>%
      mutate(
        Margin = case_when(
          Metrica == "EBIT" ~ EBIT_margin,
          Metrica == "Lucro" ~ Lucro_margin,
          TRUE ~ NA_real_
        ),
        Label = ifelse(
          Metrica == "Receita", 
          format(Valor, big.mark=".", decimal.mark=","),
          paste0(Margin, "%)"
          )
        )
      )
    
    # Create plot
    ggplot(df_long, aes(x = Trimestres, y = Valor, fill = Metrica)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      geom_text(
        aes(label = Label),
        position = position_dodge(width = 0.8),
        vjust = 0.5,
        hjust = -0.1,
        size = 3,
        angle = 90,
        lineheight = 0.8
      ) +
      labs(
        title = "Receita Líquida, EBIT e Lucro Líquido por Trimestre",
        x = "Trimestres", 
        y = "Valores (R$ Milhões)", 
        fill = "Métricas"
      ) +
      scale_fill_manual(
        values = c("Receita" = "blue", "EBIT" = "orange", "Lucro" = "green"),
        labels = c("Receita Líquida", "EBIT", "Lucro Líquido")
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + # Add space for labels
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "top",
        plot.title = element_text(hjust = 0.5)
      )
  })
  
  
}
# Run the application
shinyApp(ui = ui, server = server)