#======================================================
# Função para criar o gráfico de análise individual
# de classes
#
# @param dados dados utilizados para gerar o gráfico
# @return grafico objeto ggplot com gráfico
#======================================================
graficoProdutividadePeriodo = function(dados){
  
  dadosGrafico = dados
  
  # Criando tabela legenda
  title = sprintf("Produtividade: %s - %s", min(dados$ano_mes), max(dadosGrafico$ano_mes))
  
  plot = ggplot(dadosGrafico, aes(x = ano_mes , y = value, color = variable, group = variable)) + 
    geom_line(size = 2) +
    geom_point(size = 5, shape = 18) +
    theme_minimal() +
    ggtitle(title) +
    theme(
      plot.title = element_text(color="black", size=20)
    ) + 
    theme(legend.position="none") +
    ylab("") +
    xlab("")
  
  
  return(plot)
  
}