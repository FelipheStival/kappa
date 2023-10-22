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
    
    prod = 0.98 * dados$notif_total+
      1.04 * dados$classe_1 +
      1.46  * dados$classe_2 +
      1.61 * dados$classe_3 +
      2.45 * dados$classe_4 +
      3.75* dados$classe_5 +
      5.91 * dados$classe_6
    
    
    return(prod)
    
  }
  
}
#==============================================#
options(shiny.usecairo=T)
options(OutDec= ",")

app.LoadPackages()
app.loadGlobalFunctions()
app.LoadModules()

shinyApp(ui, server)

