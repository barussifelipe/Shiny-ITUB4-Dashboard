server = function(input, output) {

  data = get("client_data", envir = .GlobalEnv)
  
  
  output$variacaocaixa <- renderValueBox({
    # Ensure inputs are available
    req(input$ano_picker, input$trimestre_picker)
    
    # Convert reactive inputs to static values
    selected_years <- as.character(input$ano_picker)
    selected_quarters <- as.character(input$trimestre_picker)
    
    # Filter data (non-reactive operations)
    dfc_year <- select_year(data, selected_years)  # dfc must be a dataframe, not a function
    dfc_quarter <- select_quarter(dfc_year, selected_quarters)
    
    
    # Calculate value
    
    caixa_total <- 
      dfc_quarter[dfc_quarter$id == 'variacao_caixa', -1] %>%
      unlist() %>%
      as.numeric() %>%
      sum(na.rm = TRUE)
    
    # Return valueBox with proper formatting
    valueBox(
      value = tags$p(style = "font-size: 60%;", caixa_total),
      subtitle = "Variação de Caixa (R$ Milhões)",
      icon = icon("money-bill-wave", class = "fa-xs"),
      color = ifelse(caixa_total >= 0, "green", "red")
    )
  })
  
  output$caixa <- renderValueBox({
    req(input$ano_picker, input$trimestre_picker)
    
    selected_years <- as.character(input$ano_picker)
    selected_quarters <- as.character(input$trimestre_picker)
    
    dfc_year <- select_year(data, selected_years)
    dfc_quarter <- select_quarter(dfc_year, selected_quarters)
    
    caixa_values = dfc_quarter[dfc_quarter$id == 'caixa', ncol(dfc_quarter)]
    
    
    valueBox(
      value = tags$p(style = "font-size: 60%;", caixa_values),
      subtitle = "Caixa (R$ Milhões)",
      icon = icon("money-bill-wave", class = "fa-xs"),
      color = ifelse(caixa_values >= 0, "green", "red")
    )
  })
  
  output$ativo <- renderValueBox({
    req(input$ano_picker, input$trimestre_picker)
    
    selected_years <- as.character(input$ano_picker)
    selected_quarters <- as.character(input$trimestre_picker)
    
    ativos_year <- select_year(data, selected_years)
    ativos_quarter <- select_quarter(ativos_year, selected_quarters)
    
    ativo_values <- ativos_quarter[ativos_quarter$id == 'ativo', ncol(ativos_quarter)]
    
    
    valueBox(
      value = tags$p(style = "font-size: 60%;", ativo_values),
      subtitle = "Ativo Total (R$ Milhões)",
      icon = icon("warehouse", class = "fa-xs"),
      color = "blue"
    )
  })
  
  output$passivo <- renderValueBox({
    req(input$ano_picker, input$trimestre_picker)
    
    selected_years <- as.character(input$ano_picker)
    selected_quarters <- as.character(input$trimestre_picker)
    
    passivo_year <- select_year(data, selected_years)
    passivo_quarter <- select_quarter(passivo_year, selected_quarters)
    
    passivo_values <- passivo_quarter[passivo_quarter$id == "passivo", ncol(passivo_quarter)]
    
    
    valueBox(
      value = tags$p(style = "font-size: 60%;", passivo_values),
      subtitle = "Passivo Total (R$ Milhões)",
      icon = icon("warehouse", class = "fa-xs"),
      color = "blue"
    )
  })
  
  output$patrimonioliquido <- renderValueBox({
    req(input$ano_picker, input$trimestre_picker)
    
    selected_years <- as.character(input$ano_picker)
    selected_quarters <- as.character(input$trimestre_picker)
    
    passivo_year <- select_year(data, selected_years)
    passivo_quarter <- select_quarter(passivo_year, selected_quarters)
    
    patrimonio_values <- passivo_quarter[passivo_quarter$id == 'patrimonio_liquido', 
                                         ncol(passivo_quarter)]
    
    
    valueBox(
      value = tags$p(style = "font-size: 60%;", patrimonio_values),
      subtitle = "Patrimônio Líquido (R$ Milhões)",
      icon = icon("warehouse", class = "fa-xs"),
      color = ifelse(patrimonio_values >= 0, "green", "red")
    )
  })
  
  output$receitabruta <- renderValueBox({
    req(input$ano_picker, input$trimestre_picker)
    
    selected_years <- as.character(input$ano_picker)
    selected_quarters <- as.character(input$trimestre_picker)
    
    dre_year <- select_year(data, selected_years)
    dre_quarter <- select_quarter(dre_year, selected_quarters)
    
    receita_total = dre_quarter[dre_quarter$id == 'receita_bruta', -1] %>%
      unlist() %>%
      as.numeric() %>%
      sum(na.rm = TRUE)
    
    valueBox(
      value = tags$p(style = "font-size: 60%;", receita_total),
      subtitle = "Receita Bruta (R$ Milhões)",
      icon = icon("money-bill-wave", class = "fa-xs"),
      color = ifelse(receita_total >= 0, "green", "red")
    )
  })
  
  output$perdasfinanceiras <- renderValueBox({
    req(input$ano_picker, input$trimestre_picker)
    
    selected_years <- as.character(input$ano_picker)
    selected_quarters <- as.character(input$trimestre_picker)
    
    dre_year <- select_year(data, selected_years)
    dre_quarter <- select_quarter(dre_year, selected_quarters)
    
    perdas_total = dre_quarter[dre_quarter$id == 'perdas_financeiras', -1] %>%
      unlist() %>%
      as.numeric() %>%
      sum(na.rm = TRUE)
    
    valueBox(
      value = tags$p(style = "font-size: 60%;", perdas_total),
      subtitle = "Perdas Financeiras (R$ Milhões)",
      icon = icon("money-bill-wave", class = "fa-xs"),
      color = ifelse(perdas_total >= 0, "green", "red")
    )
  })
  
  output$receitaliquida <- renderValueBox({
    req(input$ano_picker, input$trimestre_picker)
    
    selected_years <- as.character(input$ano_picker)
    selected_quarters <- as.character(input$trimestre_picker)
    
    dre_year <- select_year(data, selected_years)
    dre_quarter <- select_quarter(dre_year, selected_quarters)
    
    receita_liquida_total = dre_quarter[dre_quarter$id == 'receita_liquida', -1] %>%
      unlist() %>%
      as.numeric() %>%
      sum(na.rm = TRUE)
    
    valueBox(
      value = tags$p(style = "font-size: 60%;", receita_liquida_total),
      subtitle = "Receita Líquida (R$ Milhões)",
      icon = icon("money-bill-wave", class = "fa-xs"),
      color = ifelse(receita_liquida_total >= 0, "green", "red")
    )
  })
  
  output$despesaoperacional <- renderValueBox({
    req(input$ano_picker, input$trimestre_picker)
    
    selected_years <- as.character(input$ano_picker)
    selected_quarters <- as.character(input$trimestre_picker)
    
    dre_year <- select_year(data, selected_years)
    dre_quarter <- select_quarter(dre_year, selected_quarters)
    
    
    despesa_total = dre_quarter[dre_quarter$id == 'despesa_operacional', -1] %>%
      unlist() %>%
      as.numeric() %>%
      sum(na.rm = TRUE)
    
    valueBox(
      value = tags$p(style = "font-size: 60%;", despesa_total),
      subtitle = "Despesa Operacional (R$ Milhões)",
      icon = icon("money-bill-wave", class = "fa-xs"),
      color = ifelse(despesa_total >= 0, "green", "red")
    )
  })
  
  output$EBIT <- renderValueBox({
    req(input$ano_picker, input$trimestre_picker)
    
    selected_years <- as.character(input$ano_picker)
    selected_quarters <- as.character(input$trimestre_picker)
    
    dre_year <- select_year(data, selected_years)
    dre_quarter <- select_quarter(dre_year, selected_quarters)
    
    ebit_total = dre_quarter[dre_quarter$id == 'EBIT', -1] %>%
      unlist() %>%
      as.numeric() %>%
      sum(na.rm = TRUE)
    
    valueBox(
      value = tags$p(style = "font-size: 60%;", ebit_total),
      subtitle = "EBIT (R$ Milhões)",
      icon = icon("money-bill-wave", class = "fa-xs"),
      color = ifelse(ebit_total >= 0, "green", "red")
    )
  })
  
  output$lucroliquido <- renderValueBox({
    req(input$ano_picker, input$trimestre_picker)
    
    selected_years <- as.character(input$ano_picker)
    selected_quarters <- as.character(input$trimestre_picker)
    
    dre_year <- select_year(data, selected_years)
    dre_quarter <- select_quarter(dre_year, selected_quarters)
    
    lucro_total = dre_quarter[dre_quarter$id == 'lucro_liquido', -1] %>%
      unlist() %>%
      as.numeric() %>%
      sum(na.rm = TRUE)
    
    valueBox(
      value = tags$p(style = "font-size: 60%;", lucro_total),
      subtitle = "Lucro Líquido (R$ Milhões)",
      icon = icon("money-bill-wave", class = "fa-xs"),
      color = ifelse(lucro_total >= 0, "green", "red")
    )
  })
  
  output$margemebit <- renderValueBox({
    req(input$ano_picker, input$trimestre_picker)
    
    selected_years <- as.character(input$ano_picker)
    selected_quarters <- as.character(input$trimestre_picker)
    
    dre_year <- select_year(data, selected_years)
    dre_quarter <- select_quarter(dre_year, selected_quarters)
    
    receita_values = dre_quarter[dre_quarter$id == "receita_liquida", -1] %>% 
      unlist() %>%
      as.numeric() %>% 
      sum(na.rm = TRUE)
    
    ebit_values = dre_quarter[dre_quarter$id == "EBIT", -1] %>% 
      unlist() %>%
      as.numeric() %>% 
      sum(na.rm = TRUE)
    
    
    margem_ebit = ebit_values / receita_values * 100
    
    valueBox(
      value = tags$p(style = "font-size: 60%;", round(margem_ebit, 2)),
      subtitle = "Margem EBIT (%)",
      icon = icon("percent", class = "fa-xs"),
      color = ifelse(margem_ebit >= 0, "green", "red")
    )
  })
  
  output$margemliquida <- renderValueBox({
    req(input$ano_picker, input$trimestre_picker)
    
    selected_years <- as.character(input$ano_picker)
    selected_quarters <- as.character(input$trimestre_picker)
    
    dre_year <- select_year(data, selected_years)
    dre_quarter <- select_quarter(dre_year, selected_quarters)
    
    receita_values = dre_quarter[dre_quarter$id == "receita_liquida", -1] %>% 
      unlist() %>%
      as.numeric() %>% 
      sum(na.rm = TRUE)
    
    lucro_values <- dre_quarter[dre_quarter$id == "lucro_liquido", -1] %>% 
      unlist() %>%
      as.numeric() %>% 
      sum(na.rm = TRUE)
    
    margem_liquida <- lucro_values / receita_values * 100
    
    valueBox(
      value = tags$p(style = "font-size: 60%;", round(margem_liquida, 2)),
      subtitle = "Margem Líquida (%)",
      icon = icon("percent", class = "fa-xs"),
      color = ifelse(margem_liquida >= 0, "green", "red")
    )
  })
  
  output$ROE <- renderValueBox({
    req(input$ano_picker, input$trimestre_picker)
    
    selected_years <- as.character(input$ano_picker)
    selected_quarters <- as.character(input$trimestre_picker)
    
    data_year <- select_year(data, selected_years)
    data_quarter <- select_quarter(data_year, selected_quarters)
    
    patrimonio_total = data_quarter[data_quarter$id == "patrimonio_liquido", ncol(data_quarter)]
    
    lucro_values <- data_quarter[data_quarter$id == "lucro_liquido", -1] %>% 
      unlist() %>%
      as.numeric() %>% 
      sum(na.rm = TRUE)
    
    roe <- (lucro_values / patrimonio_total) * 100
    
    valueBox(
      value = tags$p(style = "font-size: 60%;", round(roe, 2)),
      subtitle = "ROE (%)",
      icon = icon("percent", class = "fa-xs"),
      color = ifelse(roe >= 0, "green", "red")
    )
  })
  
  #I want to order by year when plotting 
  labels = factor(colnames(data)[-1], levels = unique(colnames(data)[-1]))
  
  lucro = data[data$id == "lucro_liquido", -1] %>% 
    unlist() %>% 
    as.numeric()
  
  receita <- data[data$id == "receita_liquida", -1] %>% 
    unlist() %>% 
    as.numeric()
  
  ebit <- data[data$id == "EBIT", -1] %>% 
    unlist() %>% 
    as.numeric()
  
  receita_bruta = data[data$id == "receita_bruta", -1] %>% 
    unlist() %>% 
    as.numeric()
  
  
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
  
  output$dre_table = DT::renderDataTable({
    datatable(data,
              options = list(
                pageLength = 5,
                scrollX = TRUE,
                autoWidth = TRUE
              ),
              rownames = FALSE) %>%
      #Color should be green for positive value, red otherwise. 
      formatStyle(columns = names(data[, -1]), fontSize = '90%', 
                  color = styleInterval(
                    cuts = 0,
                    values = c("red", "green")  # red for <= 0, green for > 0
                  ))
  })
  
  
  output$dre_plot = renderPlot({
    
    
    
    
    intervals = 1:length(labels)
    
    
    df_long <- data.frame(
      Trimestres = intervals,
      EBIT = ebit,
      Receita = receita_bruta,
      Lucro = lucro
    ) %>% 
      pivot_longer(
        cols = -Trimestres,
        names_to = "Metrica",
        values_to = "Valor"
      ) %>%
      # Ensure Metrica is a factor with correct levels
      mutate(Metrica = factor(Metrica, 
                              levels = c("Receita", "EBIT", "Lucro"),
                              labels = c("Receita Bruta", "EBIT", "Lucro Líquido")))
    
    ggplot(df_long, aes(x = Trimestres, y = Valor, fill = Metrica)) +
      geom_area(position = "stack", color = "black", alpha = 0.3, na.rm = TRUE) +
      labs(title = "DRE - Evolução Trimestral",
           x = "Trimestres",
           y = "Valores (R$ Milhões)",
           fill = "Métrica") +
      theme_bw()
    
    
    
  })
  
  
}