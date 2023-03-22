#======================================================
# Função para calcular a categoria, caso a categoria
# não seja encontrada irá escrever uma mensagem de erro
#======================================================
calcularCategoria = function(prod){
  
  tempFiltro = classificacaoCategoria[classificacaoCategoria$minimo <= prod & 
                                      classificacaoCategoria$maximo >= prod,]
  
  categoria = tempFiltro$categoria
  
  if(length(categoria) != 1){
    
    htmlError = '<span class = "error" data-toggle="tooltip" title="Verifique os ranges configurados para as categorias!">
                    Categoria não encontrada
                </span>
                <script>
                    $(".error").tooltip("show");
                </script>'
    
    return(HTML(htmlError))
    
  }
  
  
  return(tempFiltro$categoria)
  
}

#======================================================
# Função para calcular a produtividade relátiva
#======================================================
calcularProdutividadeRelativa = function(fatorDeCorrecao, produtividade){
  
  if(!is.na(fatorDeCorrecao)){
    return((100/(100-fatorDeCorrecao)) * produtividade)
  }
  
  return(produtividade)
  
}