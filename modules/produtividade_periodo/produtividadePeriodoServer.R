#==================================================================
# Análise individual server
#
# @input objeto do tipo reactive com os inputs do usuario
# @output objeto do tipo reactive com os outputs do usuario
# @session dados relacionacdos a sessao
# @data objeto do tipo data.frame com dados das estacoes
#==================================================================
produtividadePeriodoServer = function(input, output, session, data) {
  
  # Criando dados por periodo
  dadosGraficoPeriodo = reactive({
    
    if(!is.null(input$periodoProdInput) && !is.null( input$fiscalInputProd)){
      
      dados = data
      dados$tempFiltro = criarColunaDataCompleta(dados)
      
      # Filtrando dados
      dados = dados[
        dados$tempFiltro >= input$periodoProdInput[1] &
        dados$tempFiltro <= input$periodoProdInput[2],
      ]
      
      # Calculando dados por fiscal/periodo
      dadosFiscal = dados %>%
        filter(sigla_fiscal == input$fiscalInputProd) %>%
        select(ano_mes, prod)
      
      dadosFiscal = melt(dadosFiscal)
      dadosFiscal$variable = input$fiscalInputProd
      
      
      # Calculando dados por periodo
      dadosPeriodo = dados %>%
        group_by(ano_mes) %>%
        summarise(
          maximo = max(prod),
          minimo = min(prod)
        )
      
      dadosPeriodo = melt(dadosPeriodo)
      
      # Juntando data.frames
      dados = rbind(dadosPeriodo, dadosFiscal)
      
      return(dados)
      
    }
    
    return(NULL)
    
  })
  
  # Atualizando input de Fiscal
  observe({
    
      # Filtrando dados
      fiscal = unique(data$sigla_fiscal)
      
      # Atualizando input
      updateSelectInput(session = session,
                        inputId = "fiscalInputProd",
                        choices = fiscal,
                        selected = fiscal[1]
      )
    
  })
  
  
  # Atualizando input de periodo
  observe({
    
    if(!is.null(input$fiscalInput)){
      
      if(input$fiscalInput != ""){
        
        # Filtrando dados
        periodos = unique(data[data$sigla_fiscal %in% input$fiscalInput, 'ano_mes'])
        
        minPeriodo = min(periodos)
        maxPeriodo = max(periodos)
        
        # Atualizando input
        updateDateRangeInput(
            session = session,
            inputId = 'periodoProdInput',
            start = minPeriodo,
            end = maxPeriodo
        )
        
      }
      
    }
    
  })
  
  
  # Atualizando info box da tela principal
  observe({
    
    if(!is.null(input$periodoProdInput)){
      
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
            value = round( mean(dadosTemp$prod), 2),
            icon = icon('info-circle'),
            color = 'aqua'
          )
          
        })
        
        output$prodMediana = renderInfoBox({
          
          infoBox(
            title = 'Produtividade Mediana',
            value = round(median(dadosTemp$prod), 2),
            icon = icon('info-circle'),
            color = 'navy'
          )
          
        })
        
        
        output$prodMaxima = renderInfoBox({
          
          infoBox(
            title = 'Produtividade Máxima',
            value = round(max(dadosTemp$prod), 2),
            icon = icon('info-circle'),
            color = 'blue'
          )
          
        })
        
        output$prodMinima = renderInfoBox({
          
          infoBox(
            title = 'Produtividade Mínima',
            value = round(min(dadosTemp$prod), 2),
            icon = icon('info-circle'),
            color = 'teal'
          )
          
        })
        
      }
      
    }
    
  
    
  })
  
  # Escrevendo gráfico de produtivadade por periódo
  output$produtividadePeriodo = renderPlot({
    
    if(!is.null(dadosGraficoPeriodo())){
      graficoProdutividadePeriodo(dadosGraficoPeriodo())
    }
    
  })
  
  # Escrevendo tabela
  output$produtividadeTable = renderDataTable({
    
    if(!is.null(dadosGraficoPeriodo())){
      
      dadosLegenda = dcast(dadosGraficoPeriodo(), variable ~ ano_mes )
      dadosLegenda$variable = as.character(dadosLegenda$variable)
      
      dadosLegenda$variable[1] = '<center><img src="icon//maximo.png" height="13" width="30">Máximo</img></center>'
      dadosLegenda$variable[2] = '<center><img src="icon//minimo.png" height="13" width="30">Mínimo</img></center>'
      dadosLegenda$variable[3] = sprintf('<center><img src="icon//input.png" height="13" width="32"><br>%s</img></center>', input$fiscalInputProd)
      
      names(dadosLegenda)[1] = 'Variável'
      
      return(datatable(dadosLegenda, options = list(scrollX = TRUE, dom = 't'), escape = FALSE))
      
    }
    
  })
  
}