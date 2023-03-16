#======================================================
# Função para criar o gráfico de análise individual
# de classes
#
# @param dados data.frame que irá criar a coluna com o ano completo
# @return data.frame com coluna de ano completo criado
#======================================================

criarColunaDataCompleta = function(dados){
  
  # Criando coluna temporaria para filtrando de dados
  dados$tempFiltro = dados$ano_mes
  dados$tempFiltro = paste(dados$tempFiltro, '01', sep = '/')
  datasCompletas = ymd(dados$tempFiltro)
  
  return(datasCompletas)
  
}