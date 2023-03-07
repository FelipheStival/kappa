#======================================================
# Função para calcular a categoria
#
# @param prod dados de produtividade
# @return String com a categoria de acordo com a produtividade
#======================================================

calcularCategoria = function(prod){
  
  categoria = case_when(
    prod >= 0 && prod <= 100 ~ 'F',
    prod >= 100 && prod <= 200 ~ 'E',
    prod >= 200 && prod <= 300 ~ 'D',
    prod >= 300 && prod <= 400 ~ 'C',
    prod >= 400 && prod <= 500 ~ 'B',
    prod >= 500 && prod <= 600 ~ 'A'
  )
  
  return(categoria)
  
}