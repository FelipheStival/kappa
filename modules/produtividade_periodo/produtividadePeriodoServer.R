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
        filter(sigla_fiscal %in% input$fiscalInputProd) %>%
        select(ano_mes, prod, sigla_fiscal)
      
      dadosFiscal = melt(dadosFiscal)
      dadosFiscal$variable = dadosFiscal$sigla_fiscal
      dadosFiscal$sigla_fiscal = NULL
      
      # Calculando dados por periodo
      dadosPeriodo = dados %>%
        group_by(ano_mes) %>%
        summarise(
          maximo = max(prod),
          minimo = min(prod)
        )
      
      dadosPeriodo = melt(dadosPeriodo)
      
      dadosPeriodo = dadosPeriodo[, c("ano_mes", "variable", "value")]
      dadosFiscal  = dadosFiscal[, c("ano_mes", "variable", "value")]
      
      # Juntando data.frames
      dados = rbind(dadosPeriodo, dadosFiscal)
      
      return(dados)
      
      
    }
    
    return(NULL)
    
  })
  
  # Criando dados formatados para downlaod e criação do gráfico
  dadosLegenda = reactive({
    
    dadosLegenda = dcast(dadosGraficoPeriodo(), variable ~ ano_mes ) %>% mutate_if(is.numeric, round, digits = 3)
    
    dadosLegenda$variable = as.character(dadosLegenda$variable)
    
    dadosLegenda$variable[1] = 'Máximo'
    dadosLegenda$variable[2] = 'Mínimo'
    
    names(dadosLegenda)[1] = 'Variável'
    
    return(dadosLegenda)
    
  })
  
  # Atualizando input de Fiscal
  observe({
      
      if(is.null(input$fiscalInputProd)) {
        
        # Filtrando dados
        fiscal = unique(data$sigla_fiscal)
        
        # Atualizando input
        updateSelectInput(session = session,
                          inputId = "fiscalInputProd",
                          choices = fiscal,
                          selected = fiscal[1]
        )
        
      }
    
  })
  
  
  # Atualizando input de periodo
  observe({
    
    if(!is.null(input$fiscalInputProd)){
      
      if(length(input$fiscalInputProd) > 0){
        
        # Filtrando dados
        periodos = unique(data[data$sigla_fiscal %in% input$fiscalInputProd, 'ano_mes'])
        
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
            value = round(mean(dadosTemp$prod), 2),
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
  
  # Escrevendo tabela
  output$produtividadeTable = renderDataTable({
    
    if(!is.null(dadosGraficoPeriodo())){
      
      return(datatable(dadosLegenda(), options = list(scrollX = TRUE, dom = 't'), escape = FALSE))
      
    }
    
  })
  
  # Escrevendo gráfico de produtivadade por periódo
  output$produtividadePeriodo = renderPlot({
    
    if(!is.null(dadosGraficoPeriodo())){
      graficoProdutividadePeriodo(dadosGraficoPeriodo())
    }
    
  })
  
  # Download dos dados
  output$btnDownload = downloadHandler(
    filename = function() {
      'dados.csv'
    },
    content = function(file) {
      write.csv(dadosLegenda(), file, row.names = FALSE)
    }
  )
  
}