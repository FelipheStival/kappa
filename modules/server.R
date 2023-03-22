#==================================================================
# Global Server
#
# @input objeto do tipo reactive com os inputs do usuario
# @output objeto do tipo reactive com os outputs do usuario
# @session dados relacionacdos a sessao
#==================================================================

server = shinyServer(function(input, output, session) {
  
  data = reactive({
    
    # Lendo arquivo com dados para a análise
    dados = read.csv('data//dados_analise.csv')
  
    # Removendo classe_7_8 dos dados
    dados$Classe_7_8 = NULL
    
    # Calculando produtivade para cada sigla/periodo
    dadosProd = dados %>%
      group_by(sigla_fiscal, ano_mes) %>% 
      summarise(prod = calcProdutividade(.data))
    
    # Juntando dados pela colunas sigla/periodo
    dados = inner_join(dados, dadosProd, by = c('sigla_fiscal', 'ano_mes'))
  
    
    return(dados)
    
  })
  
  # Javascript para alterar o title
  runjs(sprintf('$("title").html("%s");', APP_NAME))
  
  # Análise individual Server
  analiseIndividualServer(input, output, session, data())
  
  # Produtividade periodo Server
  produtividadePeriodoServer(input, output, session, data())
  
})
