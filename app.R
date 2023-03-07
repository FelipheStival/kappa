#==============================================#
# Carregando pacotes a serem utilizados
app.LoadPackages = function()
{
  #=============================================#
  # Iniciando bibliotecas web
  require(shiny)
  require(shinydashboard)
  require(shinyjs)
  require(reshape2)
  require(ggplot2)
  require(ggthemes)
  require(lubridate)
  require(dplyr)
  require(Cairo)
  require(gridExtra)
  require(DT)
  require(shinycssloaders)
  
  #==============================================#
}

#==============================================#
# Carregando arquivos compilados
app.LoadModules = function() {
  
  # Carregando modulos secundarios
  modulos = list.files(pattern = ".R$",
                       recursive = T,
                       full.names = T)
  
  index = which(modulos %in% "./app.R")
  modulos = modulos[-index]
  
  log = sapply(modulos,source,encoding="utf-8")
  
}
#==============================================#

#==============================================#
# Carregando funções globais
app.loadGlobalFunctions = function(){
  
  # Função global para criar um menu reativo
  reactiveMenu <<- function(mi,tabName) {
    mi$children[[1]]$attribs['data-toggle']="tab"
    mi$children[[1]]$attribs['data-value'] = tabName
    if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
      mi$attribs$class=NULL
    }
    mi
  }
  
  # Função global para calcular a produtividade
  calcProdutividade <<- function(dados){
    
    prod = 0.3350059 * dados$notif_total +
      0.6462688 * dados$classe_1 +
      0.3879296 * dados$classe_2 +
      0.7690331 * dados$classe_3 +
      0.7067577 * dados$classe_4 +
      2.331816 * dados$classe_5 +
      2.03694 *  dados$classe_6 +
      2.541965 * dados$classe_7 +
      2.833974 * dados$classe_8
    
    return(prod)
    
  }
  
}
#==============================================#


app.LoadPackages()
app.loadGlobalFunctions()
app.LoadModules()

options(shiny.usecairo=T)

shinyApp(ui, server)
