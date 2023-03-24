#==================================================================
# Análise estátistica UI
#==================================================================
analiseEstatisticalUI = function(){
  
  tabItem(tabName = 'analiseEstatistica',
          fluidRow(
            valueBoxOutput("classe1", width = 6),
            valueBoxOutput("classe2", width = 6),
            valueBoxOutput("classe3", width = 6),
            valueBoxOutput("classe4", width = 6),
            valueBoxOutput("classe5", width = 6),
            valueBoxOutput("classe6", width = 6),
            valueBoxOutput("classe7", width = 6),
            valueBoxOutput("classe8", width = 6)
          )
  )
  
}

#==================================================================
# Análise Gráfica UI
#==================================================================
analiseIndividualUI = function(){
  
  tabItem(tabName = "analiseIndividual",
          fluidRow(
            div(
              class = 'infox-box-analise-individual',
              valueBoxOutput("relatorioAnalisados"),
              valueBoxOutput("relatorioProdutividade"),
              valueBoxOutput("relatorioCategoria"),
              valueBoxOutput("relatorioProdutividadeExcedente"),
              valueBoxOutput("relatorioProdutividadeCorrigida"),
              valueBoxOutput("relatorioProdutividadeCorrigidaCategoria")
            )
          ),
          fluidRow(
            box(width = 12,
                plotOutput("relatorioClasses", width = '100%') %>% withSpinner()
            ),
            box(width = 7,
                plotOutput("distribuicaoClasses", width = '100%') %>% withSpinner()
            ),
            box(width = 5,
                plotOutput("histograma", width = '100%') %>% withSpinner()
            )
          )
  )
  
}

#==================================================================
# Análise gráfica menu item
#==================================================================
itemMenuTrocarCategoria = function(){
  
  menuItem(text = "Configurar categorias",
           icon = icon("cog"),
           tags$div(
             class = 'container-categoria-input',
             selectInput(
               inputId = 'categoriaInput',
               label = 'Categoria',
               choices = c('A', 'B', 'C', 'D', 'E', 'F'),
               selected = 'A'
             ),
             HTML('<button type="button" class="btn btn-primary btn-add" id = "btn-add" data-toggle="tooltip" data-placement="bottom" title="Clique para adicionar uma nova categoria!">
                    <i class="fa fa-plus" aria-hidden="true"></i>
                  </button>'),
             HTML('<button type="button" class="btn btn-primary btn-remove" id = "btn-remove" data-toggle="tooltip" data-placement="bottom" title="Clique para remover a categoria selecionada!">
                     <i class="fa fa-trash-o" aria-hidden="true"></i>
                  </button>')
           ),
           numericInput(
             inputId = 'categoriaMin',
             label = 'Valor minímo',
             value = NULL
           ),
           numericInput(
             inputId = 'categoriaMax',
             label = 'Valor máximo',
             value = NULL
           )
  )
  
}


#==================================================================
# Análise gráfica menu item
#==================================================================
itemMenuAnaliseIndividual = function(){
  
  
  menuItem(text = "Análise individual",
           icon = icon("line-chart"),
           menuSubItem("Análise Gráfica", tabName = "analiseIndividual"),
           menuSubItem("Estátistica", tabName = "analiseEstatistica"),
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
           ),
           numericInput(
             inputId = 'fatorDeCorrecaoInput',
             label = 'Fator de Correção',
             value = 0,
             min = 0
           )
  )
  
}