ui = dashboardPage(
  
  #========================header=========================
  dashboardHeader(
    title = tags$img(src='logos//crea.png', width = 50),
    tags$li(class = "dropdown header-logo",
            tags$img(src = "logos//kappa.png", height = "50px", width = "auto", style = "padding-top:5px;margin-right:10px")
    )
  ),
  
  #=======================================================
  
  
  #=======================SiderBar========================
  
  dashboardSidebar(
    includeCSS('www//style//style.css'),
    useShinyjs(),
    sidebarMenu(
      itemMenuAnaliseIndividual(),
      itemMenuProdutividadePeriodo(),
      itemMenuTrocarCategoria()
    )),
  
  #========================================================
  
  
  #=======================body=============================
  
  dashboardBody(
    tabItems(
      analiseIndividualUI(),
      produtividadePeriodoUI(),
      analiseEstatisticalUI()
    )
  )
  
  #========================================================
)

# Caso a global NEED_LOGIN esteja ativada, a tela de login será requirida
if(LOGIN){
  ui = secure_app(ui, enable_admin = TRUE, theme = 'flatly', language = 'pt-BR')
}

