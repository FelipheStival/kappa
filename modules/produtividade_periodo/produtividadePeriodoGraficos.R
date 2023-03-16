#======================================================
# Função para criar o gráfico de análise individual
# de classes
#
# @param dados dados utilizados para gerar o gráfico
# @return grafico objeto ggplot com gráfico
#======================================================
graficoProdutividadePeriodo = function(dados, fiscalSelecionado){
  
  dadosGrafico = dados
  
  # Criando tabela legenda
  title = sprintf("Produtividade: %s - %s", min(dados$ano_mes), max(dadosGrafico$ano_mes))
  
  plot = ggplot(dadosGrafico, aes(x = ano_mes , y = value, color = variable, group = variable)) + 
    geom_line(size = 1) +
    geom_point(size = 5, shape = 18) +
    theme_minimal() +
    ggtitle(title) +
    theme(
      plot.title = element_text(color="black", size=20)
    ) + 
    scale_color_viridis(discrete = TRUE, option = 'D') +
    ylab("") +
    xlab("") +
    labs(fill="Variável")
  
  
  return(plot)
  
}