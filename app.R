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
  require(viridis)
  require(shinymanager)
  
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
    
    prod = 0.5105935 * dados$notif_total+ 
      0.0435197 * dados$aux_viagens_totais+
      0.1892254 * dados$classe_1 +
      0.4755206  * dados$classe_2 +
      0.5006161 * dados$classe_3 +
      0.6122536 * dados$classe_4 +
      1.633267  * dados$classe_5 +
      1.99313 * dados$classe_6 +
      2.144695  * dados$classe_7+
      3.264827 * dados$classe_8
    
    return(prod)
    
  }
  
}
#==============================================#
options(shiny.usecairo=T)

app.LoadPackages()
app.loadGlobalFunctions()
app.LoadModules()

shinyApp(ui, server)

