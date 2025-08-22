# UI ----------------------------------------------------------------------


ui = dashboardPage(
  dashboardHeader(title = "Dashboard Financeiro"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Resumo", tabName = "overview", icon = icon("dashboard")), 
      menuItem("DRE", tabName = "dre", icon = icon("file-invoice-dollar"))
    )
  ), 
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview", 
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
              )),
      tabItem(tabName = "dre", h2("Demonstração do Resultado do Exercício (DRE)"),
              fluidRow(
                column(width = 12,
                       box(DT::dataTableOutput("dre_table"), width = NULL)  # Placeholder for DRE table output
                )
              ),
              fluidRow(
                column(width = 12, 
                       box(plotOutput("dre_plot", height = 350), width = NULL)  # Placeholder for DRE plot output
                )
              )
      )
    )
  )
)



