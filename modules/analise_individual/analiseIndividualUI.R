#==================================================================
# Análise Gráfica UI
#==================================================================
analiseIndividualUI = function(){
  
  tabItem(tabName = "analiseIndividual",
          box(width = 12,
              plotOutput("relatorioClasses", width = '100%')
          ),
          box(width = 4,
              plotOutput("histograma", width = '100%')
          ),
          box(width = 4,
              plotOutput("distribuicaoClasses", width = '100%')
          ),
  )
}

#==================================================================
# Análise gráfica menu item
#==================================================================
itemMenuAnaliseIndividual = function(){
  
  menuItem(text = "Análise individual",
           tabName = "analiseIndividual",
           icon = icon("bar-chart"),
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
  )
  
}