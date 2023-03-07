#==================================================================
# Análise Gráfica UI
#==================================================================
analiseIndividualUI = function(){
  
  tabItem(tabName = "analiseIndividual",
          fluidRow(
            valueBoxOutput("relatorioAnalisados",   width = 3),
            valueBoxOutput("relatorioProdutividade",   width = 3),
            valueBoxOutput("relatorioCategoria",   width = 3),
            valueBoxOutput("relatorioProdutividadeExcedente",   width = 3)
          ),
          fluidRow(
            box(width = 12,
                plotOutput("relatorioClasses", width = '100%') %>% withSpinner()
            ),
            box(width = 6,
                plotOutput("histograma", width = '100%') %>% withSpinner()
            ),
            box(width = 6,
                plotOutput("distribuicaoClasses", width = '100%') %>% withSpinner()
            )
          )
  )
}

#==================================================================
# Análise gráfica menu item
#==================================================================
itemMenuAnaliseIndividual = function(){
  
  reactiveMenu(menuItem(text = "Análise individual",
           tabName = "analiseIndividual",
           icon = icon("line-chart"),
           selectInput(
             inputId = "fiscalInput",
             label = "Fiscal",
             choices = NULL,
             selected = ""
           ),
           selectInput(
             inputId = "periodoInput",
             label = "Período",
             choices = NULL,
             selected = ""
           )
  ), tabName = 'analiseIndividual')
  
}