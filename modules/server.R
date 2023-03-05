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
    
    return(dados)
    
  })
  
  # Análise individual Server
  analiseIndividualServer(input, output, session, data())
  
  # Produtividade periodo Server
  produtividadePeriodoServer(input, output, session, data())
  
})
