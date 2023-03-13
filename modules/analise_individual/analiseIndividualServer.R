#==================================================================
# Análise individual server
#
# @input objeto do tipo reactive com os inputs do usuario
# @output objeto do tipo reactive com os outputs do usuario
# @session dados relacionacdos a sessao
# @data objeto do tipo data.frame com dados das estacoes
#==================================================================
analiseIndividualServer = function(input, output, session, data) {
  
  # Dados Gráficos
  dadosGraficos = reactive({
    
    # Filtrando dados de acordo com os inputs
    if(input$fiscalInput != "" && input$periodoInput != ""){
      
      dadosFiltrados = data[data$sigla_fiscal %in% input$fiscalInput &
                            data$ano_mes %in% input$periodoInput,]
      
      return(dadosFiltrados)
      
    }
    
    return(NULL)
    
  })
  
  # Dados periodo
  dadosPeriodo = reactive({
    
    if(input$periodoInput != ""){
      
      dadosFiltrados = data[data$ano_mes %in% input$periodoInput,]
      return(dadosFiltrados)
      
    }
    
    return(NULL)
    
  })
  
  
  # Atualizando input de Fiscal
  observe({
    
    # Filtrando dados
    fiscal = unique(data$sigla_fiscal)
    
    # Atualizando input
    updateSelectInput(session = session,
                      inputId = "fiscalInput",
                      choices = fiscal,
                      selected = fiscal[1]
    )
    
  })
  
  # Atualizando infoboxes
  observe({
    
    if(!is.null(dadosGraficos())){
      
      # Número de relátorios análisados
      output$relatorioAnalisados = renderInfoBox({
        
        infoBox(
          title = 'Relátorios Analisados',
          value = dadosGraficos()$num_RMO
        )
        
      })
      
      # Produtividade
      output$relatorioProdutividade = renderInfoBox({
        
        infoBox(
          title = 'Produtividade',
          value = round(dadosGraficos()$prod, 2)
        )
        
      })
      
      # Calcular categoria
      output$relatorioCategoria = renderInfoBox({
        
        categoria = calcularCategoria(dadosGraficos()$prod)
        
        infoBox(
          title = 'Categoria',
          value = categoria
        )
        
      })
      
      # Calcular produtividade excedente
      output$relatorioProdutividadeExcedente = renderInfoBox({
        
        prodExcedente = dadosGraficos()$prod %% 100
        
        infoBox(
          title = 'Produtividade excedente',
          value = round(prodExcedente, 2)
        )
        
      })
      
    }
    
  })
  
  # Atualizando input período
  observe({
    
    if(input$fiscalInput != ""){
      
      # Filtrando dados
      periodos = unique(data[data$sigla_fiscal %in% input$fiscalInput, 'ano_mes'])
      
      # Atualizando input
      updateSelectInput(session = session,
                        inputId = "periodoInput",
                        choices = periodos,
                        selected = periodos[1]
      )
      
    }
    
  })
  
  # Gráfico relátorio de classes indívidual
  output$relatorioClasses = renderPlot({
    
    if(!is.null(dadosGraficos())){
      graficoRelatorioClasses(dadosGraficos())
    }
    
  })
  
  # Gráfico histograma
  output$histograma = renderPlot({
    
    if(!is.null(dadosGraficos())){
      graficoHistograma(dadosPeriodo(), dadosGraficos())
    }
    
  })
  
  # Gráfico de distruicao
  output$distribuicaoClasses = renderPlot({
    
    if(!is.null(dadosGraficos())){
      graficoDistribuicao(dadosPeriodo(), dadosGraficos())
    }
    
  })
  
}