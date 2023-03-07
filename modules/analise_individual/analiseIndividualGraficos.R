#======================================================
# Função para criar o gráfico de análise individual
# de classes
#
# @param dados dados utilizados para gerar o gráfico
# @return grafico objeto ggplot com gráfico
#======================================================
graficoRelatorioClasses = function(dados){
  
  # Criando data.frame formatado para o gráfico
  dados = dados[,c('classe_1', 'classe_2', 'classe_3', 'classe_4', 'classe_5', 'classe_6', 'classe_7', 'classe_8', 'Classe_7_8')]
  dados = melt(dados)
  
  colnames(dados) = c('name', 'count')
  
  # Criando gráfico
  grafico = ggplot(dados) +
    geom_col(aes(count, name,  fill = factor(name)), width = 0.6)  +
    theme_minimal()
  
  return(grafico)
  
}

#======================================================
# Função para criar o gráfico de histograma
#
# @param dados dados utilizados para gerar o gráfico
# @return grafico objeto ggplot com gráfico
#======================================================
graficoHistograma = function(dados){
  
  plot = ggplot(iris, aes(x = Sepal.Length)) +
    geom_histogram(bins = 30) +
    ggtitle("30 classes")
  
  return(plot)
  
}