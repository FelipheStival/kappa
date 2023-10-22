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
          ),
          fluidRow(
              box(
                width = 12,
                column(
                  width = 12,
                  plotOutput('produtividadePeriodo', width = '100%', height = '80vh'),
                  dataTableOutput('produtividadeTable'),
                  downloadButton('btnDownload', 'Download', 'btn-primary btn-download')
                )
              )
          )
  )
  
}

#==================================================================
# Análise gráfica menu item
#==================================================================
itemMenuProdutividadePeriodo = function(){
  
  reactiveMenu(
           menuItem(
             id = 'produtividadePeriodo',
             text = "Produtividade por período",
             tabName = "produtividadePeriodo",
             icon = icon("bar-chart"),
             dateRangeInput(
               inputId = "periodoProdInput",
               label = "Selecione o periodo:",
               start = NULL,
               end = NULL,
               format = "yyyy/mm"
             ),
             selectInput(
               inputId = "fiscalInputProd",
               label = "Fiscal",
               choices = NULL,
               multiple = T
             )
    ), tabName = 'produtividadePeriodo')
  
}