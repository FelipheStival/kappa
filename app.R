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


app.LoadPackages()
app.LoadModules()

shinyApp(ui, server)
