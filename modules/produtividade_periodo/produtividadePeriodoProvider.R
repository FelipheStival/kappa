#==========================================================
# Função para criar uma data completa a partir do mes/ano
#==========================================================
criarColunaDataCompleta = function(dados){
  
  # Criando coluna temporaria para filtrando de dados
  dados$tempFiltro = dados$ano_mes
  dados$tempFiltro = paste(dados$tempFiltro, '01', sep = '/')
  datasCompletas = ymd(dados$tempFiltro)
  
  return(datasCompletas)
  
}