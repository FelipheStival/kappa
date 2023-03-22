#======================================================
# Função para criar o gráfico de análise individual
# de classes.
#======================================================
graficoRelatorioClasses = function(dados){
  
  # Criando data.frame formatado para o gráfico
  dados = dados[,c('classe_1', 'classe_2', 'classe_3', 'classe_4', 'classe_5', 'classe_6', 'classe_7', 'classe_8')]
  dados = melt(dados)
  
  colnames(dados) = c('name', 'count')
  
  dados$perc = paste(round(dados$count/sum(dados$count)*100, 2), '%', sep = '')
  
  grafico = ggplot(dados, aes(count, name,  fill = factor(name))) +
    geom_col(aes(fill = factor(name)), width = 0.6)  +
    geom_text(aes(label = perc),  colour="black", hjust = -0.1) +
    scale_fill_viridis(discrete = TRUE, option = 'G') +
    theme_minimal() +
    xlab('Quantidade(%)') +
    ylab('Distribuição dos relatórios por classes') +
    labs(fill="Classes")
  
  return(grafico)
  
}

#======================================================
# Função para criar o gráfico de histograma
#======================================================
graficoHistograma = function(dadosPeriodo, dadosSigla){
  
  grafico = ggplot(dadosPeriodo, aes(x = prod)) + 
    geom_histogram(aes(y = ..density..),
                   colour = 1, fill = "white") +
    geom_density(fill="red", alpha=.1) +
    geom_vline(xintercept = dadosSigla$prod, linetype = 2, color = "red", lwd = 1.2) +
    xlab('Produtividade') +
    ylab('Frequência') +
    theme_minimal()
  
  return(grafico)
  
}

#======================================================
# Função para criar o gráfico para distribuição 
# de classes
#======================================================
graficoDistribuicao = function(dadosPeriodo, dadosSigla){
  
  # Calculando dados do periodo
  dadosPlotPeriodo = dadosPeriodo[,c('classe_1', 'classe_2', 'classe_3', 'classe_4', 'classe_5', 'classe_6', 'classe_7', 'classe_8')]
  dadosPlotPeriodo = melt(dadosPlotPeriodo)
  
  dadosPlotPeriodo = dadosPlotPeriodo %>%
    group_by(variable) %>%
    summarise(value = sum(value)) %>%
    as.data.frame
  
  dadosPlotPeriodo$prec = paste(round(dadosPlotPeriodo$value/sum(dadosPlotPeriodo$value)*100, 2), '%', sep = '')
  
  # Calculando dados da sigla
  dadosPlotSigla = dadosSigla[,c('classe_1', 'classe_2', 'classe_3', 'classe_4', 'classe_5', 'classe_6', 'classe_7', 'classe_8')]
  dadosPlotSigla = melt(dadosPlotSigla)
  
  dadosPlotSigla$prec = paste(round(dadosPlotSigla$value/sum(dadosPlotSigla$value)*100, 2), '%', sep = '')
  
  # Criando subgrupo para gerar o gráfico
  dadosPlotPeriodo$subgrupo = 'Período'
  dadosPlotSigla$subgrupo = dadosSigla$sigla_fiscal
  
  # Renomenado colunas
  colnames(dadosPlotSigla) = c('name', 'count', 'perc', 'subgrupo')
  colnames(dadosPlotPeriodo) = c('name', 'count', 'perc', 'subgrupo')
  
  dadosPlot = rbind(dadosPlotPeriodo, dadosPlotSigla)
  
  grafico = ggplot(dadosPlot, aes(count, name,  fill = subgrupo)) +
    geom_bar(stat="identity", width=0.7, position=position_dodge(width=0.8)) +
    geom_text(aes(label = perc),  colour="black", hjust = -0.1, position = position_dodge(0.9)) +
    theme_minimal() +
    scale_fill_viridis(discrete = TRUE, option = 'H') +
    xlab('Quantidade(%)') +
    ylab('Distribuição dos relatórios por classes') +
    labs(fill="Fiscal")
  
  return(grafico)
  
}

