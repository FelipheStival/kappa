#==================================================================
# Análise individual server
#
# @input objeto do tipo reactive com os inputs do usuario
# @output objeto do tipo reactive com os outputs do usuario
# @session dados relacionacdos a sessao
# @data objeto do tipo data.frame com dados das estacoes
#==================================================================
produtividadePeriodoServer = function(input, output, session, data) {
  
  # Filtrando dados
  dadosGraficoPeriodo = reactive({
    
    dados = data
    dados$tempFiltro = criarColunaDataCompleta(dados)
    
    # Filtrando dados
    dados = dados[
      dados$tempFiltro >= input$periodoProdInput[1] &
      dados$tempFiltro <= input$periodoProdInput[2],
    ]
    
    # Transformando dados
    
    dados$prod = calcProdutividade(dados)
    
    dados = dados %>%
      group_by(ano_mes) %>%
      summarise(
        maximo = max(prod),
        minimo = min(prod)
      )
    
    dados = melt(dados)
    
    return(dados)
    
  })
  
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
    dadosTemp$tempFiltro = criarColunaDataCompleta(dadosTemp)
    
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
          icon = icon('info-circle'),
          color = 'aqua'
        )
        
      })
      
      output$prodMediana = renderInfoBox({
        
        infoBox(
          title = 'Produtividade Mediana',
          value = round(median(dadosTemp$pontos), 2),
          icon = icon('info-circle'),
          color = 'navy'
        )
        
      })
      
      
      output$prodMaxima = renderInfoBox({
        
        infoBox(
          title = 'Produtividade Máxima',
          value = round(max(dadosTemp$pontos), 2),
          icon = icon('info-circle'),
          color = 'blue'
        )
        
      })
      
      output$prodMinima = renderInfoBox({
        
        infoBox(
          title = 'Produtividade Mínima',
          value = round(min(dadosTemp$pontos), 2),
          icon = icon('info-circle'),
          color = 'teal'
        )
        
      })
      
    }
  
    
  })
  
  # Escrevendo gráfico de produtivadade por periódo
  output$produtividadePeriodo = renderPlot({
    
    if(nrow(dadosGraficoPeriodo()) > 1){
      graficoProdutividadePeriodo(dadosGraficoPeriodo())
    }
    
  })
  
  # Escrevendo tabela
  output$produtividadeTable = renderDataTable({
    
    if(nrow(dadosGraficoPeriodo()) > 1){
      
      dadosLegenda = dcast(dadosGraficoPeriodo(), variable ~ ano_mes )
      dadosLegenda$variable = as.character(dadosLegenda$variable)
      
      dadosLegenda$variable[1] = '<img src="/icon/maximo.png" height="20" width="80"></img>'
      dadosLegenda$variable[2] = '<img src="/icon/minimo.png" height="20" width="80"></img>'
      
      return(datatable(dadosLegenda, options = list(scrollX = TRUE, dom = 't'), escape = FALSE))
      
    }
    
  })
  
}