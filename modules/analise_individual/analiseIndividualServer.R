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
  
  # Atualizando input de mudança de categoria
  observe({
    
    categoria = input$categoriaInput
    
    # Obtendo valores
    valores = classificacaoCategoria[classificacaoCategoria$categoria == categoria,]
    
    # Atualizando inputs
    updateNumericInput(session = session,
                       inputId = "categoriaMin",
                       value = valores$minimo,
                       min = 0
    )
    
    updateNumericInput(session = session,
                       inputId = "categoriaMax",
                       value = valores$maximo,
                       min = 0
    )
    
    # Observando mudanças nos valores para a atualização 
    
  })
  
  observe({
    
    # Atualizando valores categoria
    if(!is.null(input$categoriaMax) && !is.na(input$categoriaMax) && !is.null(input$categoriaInput) && !is.na(input$categoriaMin)){
      
      classificacaoCategoria[classificacaoCategoria$categoria == input$categoriaInput, 'minimo'] = input$categoriaMin
      classificacaoCategoria[classificacaoCategoria$categoria == input$categoriaInput, 'maximo'] = input$categoriaMax
      
      classificacaoCategoria <<- classificacaoCategoria
      
    }
    
  })
  
  # Atualizando infoboxes
  observe({
    
    if(!is.null(dadosGraficos())){
      
      input$categoriaMax
      input$categoriaMin
      
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
      
      # Escrendo infobox Produtividade Corrigida
      output$relatorioProdutividadeCorrigida = renderInfoBox({
        
        valor = calcularProdutividadeRelativa(input$fatorDeCorrecaoInput, dadosGraficos()$prod)
        
        infoBox(
          title = 'Produtividade Corrigida',
          value = round(valor, 2)
        )
        
      })
      
    }
    
  })
  
  # Atulizando ferramentas de alteração
  observe({
    
    if(!is.null(dadosGraficos())){
      
      # Classe 1 config
      output$classe1 = renderInfoBox({
        
        infoBox(
          title = 'Classe 1',
          value = dadosGraficos()$classe_1,
          icon = icon('cog')
        )
        
      })
      
      # Classe 2 config
      output$classe2 = renderInfoBox({
        
        infoBox(
          title = 'Classe 2',
          value = dadosGraficos()$classe_2,
          icon = icon('cog')
        )
        
      })
      
      # Classe 3 config
      output$classe3 = renderInfoBox({
        
        infoBox(
          title = 'Classe 3',
          value = dadosGraficos()$classe_3,
          icon = icon('cog')
        )
        
      })
      
      # Classe 4 config
      output$classe4 = renderInfoBox({
        
        infoBox(
          title = 'Classe 4',
          value = dadosGraficos()$classe_4,
          icon = icon('cog')
        )
        
      })
      
      # Classe 5 config
      output$classe5 = renderInfoBox({
        
        infoBox(
          title = 'Classe 5',
          value = dadosGraficos()$classe_5,
          icon = icon('cog')
        )
        
      })
      
      # Classe 6 config
      output$classe6 = renderInfoBox({
        
        infoBox(
          title = 'Classe 6',
          value = dadosGraficos()$classe_6,
          icon = icon('cog')
        )
        
      })
      
      # Classe 7 config
      output$classe7 = renderInfoBox({
        
        infoBox(
          title = 'Classe 7',
          value = dadosGraficos()$classe_7,
          icon = icon('cog')
        )
        
      })
      
      # Classe 8 config
      output$classe8 = renderInfoBox({
        
        infoBox(
          title = 'Classe 8',
          value = dadosGraficos()$classe_8,
          icon = icon('cog')
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