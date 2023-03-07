

ui = dashboardPage(
  
  #========================header=========================
  
  dashboardHeader( title =  APP_NAME),
  
  #=======================================================
  
  
  #=======================SiderBar========================
  
  dashboardSidebar(
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
