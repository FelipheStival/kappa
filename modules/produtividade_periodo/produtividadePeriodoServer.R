#==================================================================
# Análise individual server
#
# @input objeto do tipo reactive com os inputs do usuario
# @output objeto do tipo reactive com os outputs do usuario
# @session dados relacionacdos a sessao
# @data objeto do tipo data.frame com dados das estacoes
#==================================================================
produtividadePeriodoServer = function(input, output, session, data) {
  
  # Atualizando input de periodo
  observe({
    
    periodo = unique(data$ano_mes)
    
    minPeriodo = min(periodo)
    maxPeriodo = max(periodo)
    
    updateDateRangeInput(
      session = session,
      inputId = 'periodoProdInput',
      start = minPeriodo,
      end = maxPeriodo
    )
    
  })
  
  # Atualizando info box da tela principal
  observe({
    
    # Filtrando dados de acordo com o periodo
    dadosTemp = data
    
    # Criando coluna temporaria para filtrando de dados
    dadosTemp$tempFiltro = dadosTemp$ano_mes
    dadosTemp$tempFiltro = paste(dadosTemp$tempFiltro, '01', sep = '/')
    dadosTemp$tempFiltro = ymd(dadosTemp$tempFiltro)
    
    # Filtrando dados
    dadosTemp = dadosTemp[
      dadosTemp$tempFiltro >= input$periodoProdInput[1] &
      dadosTemp$tempFiltro <= input$periodoProdInput[2],
    ]
    
    # Escrevendo infobox
    if(nrow(dadosTemp) > 1){
      
      output$prodMedia = renderInfoBox({
        
        infoBox(
          title = 'Produtividade Media',
          value = round( mean(dadosTemp$pontos), 2),
          icon = icon('list'),
          width = 1
        )
        
      })
      
      output$prodMediana = renderInfoBox({
        
        infoBox(
          title = 'Produtividade Mediana',
          value = round(median(dadosTemp$pontos), 2),
          icon = icon('list'),
          width = 1
        )
        
      })
      
      
      output$prodMaxima = renderInfoBox({
        
        infoBox(
          title = 'Produtividade Máxima',
          value = round(max(dadosTemp$pontos), 2),
          icon = icon('list'),
          width = 1
        )
        
      })
      
      output$prodMinima = renderInfoBox({
        
        infoBox(
          title = 'Produtividade Mínima',
          value = round(min(dadosTemp$pontos), 2),
          icon = icon('list'),
          width = 1
        )
        
      })
      
    }
  
    
  })
  
}