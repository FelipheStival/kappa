

ui = dashboardPage(
  
  #========================header=========================
  
  dashboardHeader( title =  APP_NAME),
  
  #=======================================================
  
  
  #=======================SiderBar========================
  
  dashboardSidebar(
    includeCSS('www//style//style.css'),
    sidebarMenu(
      itemMenuAnaliseIndividual(),
      itemMenuProdutividadePeriodo()
    )),
  
  #========================================================
  
  
  #=======================body=============================
  
  dashboardBody(
    tabItems(
      analiseIndividualUI(),
      produtividadePeriodoUI()
    )
  )
  
  #========================================================
)
