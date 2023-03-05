#==================================================================
# Produtividade por período UI
#==================================================================
produtividadePeriodoUI = function(){
  
  tabItem(tabName = "produtividadePeriodo",
          fluidRow(
            valueBoxOutput("prodMedia",   width = 3),
            valueBoxOutput("prodMediana", width = 3),
            valueBoxOutput("prodMaxima",  width = 3),
            valueBoxOutput("prodMinima",  width = 3),
          )
  )
  
}

#==================================================================
# Análise gráfica menu item
#==================================================================
itemMenuProdutividadePeriodo = function(){
  
  menuItem(id = 'produtividadePeriodo',
           text = "Produtividade por período",
           tabName = "produtividadePeriodo",
           icon = icon("bar-chart"),
           dateRangeInput(inputId = "periodoProdInput",
                          label = "Selecione o periodo:",
                          start = NULL,
                          end = NULL,
                          format = "yyyy/mm"
            ),
            selected = T
    )
  
}