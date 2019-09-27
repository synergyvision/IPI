credentials <- list("test" = "202cb962ac59075b964b07152d234b70")


shinyServer(function(input, output) {
  
  # shinyURL.server()
  # 
  # USER <- reactiveValues(Logged = FALSE)
  # 
  # observeEvent(input$.login, {
  #   if (isTRUE(credentials[[input$.username]]==input$.password)){
  #     USER$Logged <- TRUE
  #   } else {
  #     show("message")
  #     output$message = renderText("Invalid user name or password")
  #     delay(2000, hide("message", anim = TRUE, animType = "fade"))
  #   }
  # })
  
  # output$app = renderUI(
  #   if (!isTRUE(USER$Logged)) {
  #     fluidRow(column(width=4, offset = 4,
  #                     wellPanel(id = "login",
  #                               textInput(".username", "Username:"),
  #                               passwordInput(".password", "Password:"),
  #                               div(actionButton(".login", "Log in"), style="text-align: center;")
  #                     ),
  #                     textOutput("message")
  #     ))
  #   } else {
  #     
  #     dashboardPage(
  #       
  #       #//////////////#
  #       #/// HEADER ///#
  #       #//////////////#
  #       
  #       dashboardHeader(title = NULL, titleWidth = 188,#188,
  #                       dropdownMenu(type = "messages",
  #                                    messageItem(
  #                                      from = "Alerta",
  #                                      message = "Niveles de Riesgo Atípicos",
  #                                      icon = icon("exclamation-triangle"),
  #                                      time = "2018-05-12"
  #                                    )
  #                                    #,#final messageitem
  #                                    
  #                                    # messageItem(
  #                                    #            from = "Señal",
  #                                    #            message = "Volatilidad Anormal",
  #                                    #            icon = icon("life-ring"),
  #                                    #            time = "2018-05-12"
  #                                    #             )#final messageitem
  #                       )#final dropdownmenu
  #       ),#final dashboardheader
  #       #Sidebar
  #       dashboardSidebar( 
  #         
  #         #tags$style(HTML(".main-sidebar{width: 250px;}")),
  #         
  #         #sidebarSearchForm(label = "Ingrese un Número", "searchText", "searchButton"),
  #         
  #         sidebarMenu(id = "tabs",
  #                     
  #                     menuItem("Consolidado", icon = icon("home"),tabName = "consolidado"
  #                              
  #                     ),#fin menuitem 
  #                     
  #                     #menuItem("Comparativo", icon = icon("circle-o"), tabName = "comparativo"),
  #                     menuItem("Gestión Técnica", icon = icon("wrench"), 
  #                              
  #                              #menuSubItem("Datos", tabName = "datos", icon = icon("circle-o")),
  #                              
  #                              menuSubItem("Resumen Centro de Atención", tabName = "gt_ca",icon = icon(" ")),
  #                              
  #                              menuSubItem("Resumen Línea de Negocio", tabName = "gt_ln",icon = icon(" ")),
  #                              
  #                              menuSubItem("Detalle de Línea de Negocio", tabName = "gt_dln",icon = icon(" ")),
  #                              
  #                              menuSubItem("Siniestralidad Ppal Cuentas", tabName = "gt_siniestro",icon = icon(" "))
  #                              
  #                              
  #                     ),#fin menuitem 
  #                     menuItem("Gestión Comercial", icon = icon("briefcase"), 
  #                              
  #                              menuSubItem("Línea de Negocio", tabName = "gc_ln", icon = icon(" ")),
  #                              
  #                              menuSubItem("Centro de atención", tabName = "gc_ca", icon = icon(" ")),
  #                              
  #                              menuItem("Productores", tabName = "gc_prod", icon = icon(" "),
  #                                       menuSubItem("Detalle Productor", tabName = "prod_dp", icon = icon(" ")),
  #                                       menuSubItem("Fecha Intermediario", tabName = "prod_fi", icon = icon(" "))
  #                              ),
  #                              
  #                              menuSubItem("Detalles de Pólizas", tabName = "gc_dp", icon = icon(" ")),
  #                              
  #                              menuSubItem("Detalles de Siniestros", tabName = "gc_ds", icon = icon(" ")),
  #                              
  #                              menuSubItem("Detalle de Consolidado", tabName = "gc_dc", icon = icon(" ")),
  #                              
  #                              menuItem("Incentivos", tabName = "gc_inc", icon = icon("trophy"),
  #                                       menuSubItem("Concursos Mensuales", tabName = "inc_cm", icon = icon(" ")),
  #                                       menuSubItem("Concursos Generales", tabName = "inc_cg", icon = icon(" "))
  #                              )
  #                              
  #                              
  #                     ),#fin menuitem 
  #                     
  #                     menuItem("Gestión Financiera", icon = icon("wallet"),tabName = "gf",badgeLabel = "Nuevo", badgeColor = "green"),
  #                     
  #                     menuItem("Indicadores", icon = icon("chart-line"), 
  #                              menuSubItem("Indicadores Macro", tabName = "ind_macro", icon = icon(" ")),
  #                              menuSubItem("Indicadores Micro", tabName = "ind_micro", icon = icon(" "))
  #                     ),
  #                     menuItem("BBDD", icon = icon("database"), 
  #                              menuSubItem("Cartera", tabName = "bbdd_c", icon = icon(" ")),
  #                              menuSubItem("Siniestros", tabName = "bbdd_s", icon = icon(" "))
  #                     ),
  #                     menuItem("Proyección", tabName = "proy",icon = icon("forward"),badgeLabel = "Nuevo", badgeColor = "green"
  #                              
  #                     ),
  #                     menuItem("Simulación", tabName = "sim",icon = icon("bullseye"),badgeLabel = "Nuevo", badgeColor = "green"
  #                              
  #                     ),
  #                     menuItem("Carga de Información", icon = icon("upload"),tabName = "ci"),
  #                     menuItem("Consultas en Línea", icon = icon("file"),tabName = "cl"
  #                              
  #                     ),
  #                     menuItem("En Certificación", icon = icon("file"),tabName = "ec"
  #                              
  #                     ),
  #                     
  #                     menuItem("Acerca", icon = icon("exclamation-circle"), tabName = "acerca"))
  #         
  #       ), #final dashboardsidebar
  #       
  #       #////////////#
  #       #/// BODY ///#
  #       #////////////#
  #       
  #       dashboardBody(VisionHeader(), #tags$style(HTML(".main-sidebar{width: 250px;}")),
  #                     tags$style(HTML("
  #                                     .box.box-solid.box-primary>.box-header {
  #                                     color:#fff;
  #                                     background:#024A86;
  #                                     
  #                                     }
  #                                     
  #                                     .box.box-solid.box-primary{
  #                                     border-bottom-color:#00FF00;
  #                                     border-left-color:#00FF00;
  #                                     border-right-color:#00FF00;
  #                                     border-top-color:#00FF00;
  #                                     }")),
  #                 
  #                     tabItems(
  #                       
  #                       #///////////////////#
  #                       #/// CONSOLIDADO ///#
  #                       #///////////////////#
  #                       
  #                       tabItem(tabName = "consolidado",
  #                               h2(" Información Consolidado"),
  #                               
  #                               
  #                               box(width=12,title="Consolidado",status="primary",solidHeader=TRUE ,
  #                                   column(width = 6,
  #                                          #box( width = 6, background = "navy",
  #                                          dateInput(inputId="fecha1", label="Desde:", language= "es",
  #                                                    width = "100%")#final dateimput 
  #                                          #),#final box
  #                                   ),#final column
  #                                   #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box
  #                                   
  #                                   column(width = 6,
  #                                          #box( width = 6, background = "navy",
  #                                          dateInput(inputId="fecha2", label="Hasta:", language= "es",
  #                                                    width = "100%")#final dateimput 
  #                                          #)#final box
  #                                   ),#final column
  #                                   #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box
  #                                   
  #                                   column(width = 6,
  #                                          #box( width = 6, background = "navy",
  #                                          selectInput("centro_atencion", "Centro de Atención:",
  #                                                      choices = c("UNIVERSAL DE SEGUROS, C.A","CENTRO 1","CENTRO 2","CENTRO 3"))
  #                                          #)#final box
  #                                   ),
  #                                   column(width = 6,
  #                                          #box( width = 6, background = "navy",
  #                                          selectInput("cuentas_esp", "Cuentas Especiales:",
  #                                                      choices = c("Todas","Cuenta 1","Cuenta 2","Cuenta 3"))
  #                                          #)#final box
  #                                   ),
  #                                   column(width = 6,
  #                                          #box( width = 6, background = "navy",
  #                                          actionButton("consultar", "Consultar",
  #                                                       style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
  #                                   )
  #                                   
  #                                   
  #                                   #)#final fluidrow
  #                                   
  #                                   
  #                                   
  #                               ), # final box
  #                               
  #                               verbatimTextOutput('fecha_ini'),
  #                               verbatimTextOutput('fecha_fin'),
  #                               verbatimTextOutput('sucursal'),
  #                               verbatimTextOutput('cuenta'),
  #                               
  #                               #TABLA 1
  #                               fluidRow(
  #                                 box(style="overflow-x:scroll",width = 12,title="Inventario por Centros de Atención",status="primary",solidHeader=TRUE,
  #                                     dataTableOutput("tabla1_con")),
  #                                 
  #                                 
  #                                 
  #                                 #TABLA 2
  #                                 box(style="overflow-x:scroll",width = 12,title="Prima Cobrada por Centros de Atención",status="primary",solidHeader=TRUE,
  #                                     dataTableOutput("tabla2_con"))
  #                                 
  #                               )
  #                               
  #                               
  #                       ),
  #                       
  #                       #///////////////////////#
  #                       #/// GESTIÓN TÉCNICA ///#
  #                       #///////////////////////#
  #                       
  #                       #||||||||||||||||||||||||||||||||||#
  #                       #||| RESUMEN CENTRO DE ATENCIÓN |||#
  #                       #||||||||||||||||||||||||||||||||||#
  #                       
  #                       tabItem(tabName = "gt_ca",
  #                               h2(" Información Resumen Centro de Atención")
  #                               
  #                       ),
  #                       
  #                       #||||||||||||||||||||||||||||||||#
  #                       #||| RESUMEN LÍNEA DE NEGOCIO |||#
  #                       #||||||||||||||||||||||||||||||||#
  #                       
  #                       tabItem(tabName = "gt_ln",
  #                               h2(" Información Resumen Línea de Negocio")
  #                               
  #                       ),
  #                       
  #                       #||||||||||||||||||||||||||||||||#
  #                       #||| DETALLE LÍNEA DE NEGOCIO |||#
  #                       #||||||||||||||||||||||||||||||||#
  #                       
  #                       tabItem(tabName = "gt_dln",
  #                               h2(" Información Detalle de Línea de Negocio")
  #                               
  #                       ),
  #                       
  #                       #||||||||||||||||||||||||||||||||||||||||||#
  #                       #||| SINIESTRALIDAD PRINCIPALES CUENTAS |||#
  #                       #||||||||||||||||||||||||||||||||||||||||||#
  #                       
  #                       tabItem(tabName = "gt_siniestro",
  #                               h2("Información Siniestralidad Principales Cuentas")
  #                               
  #                       ),
  #                       
  #                       #/////////////////////////#
  #                       #/// GESTIÓN COMERCIAL ///#
  #                       #/////////////////////////#
  #                       
  #                       #|||||||||||||||||||||||||#
  #                       #|||  LÍNEA DE NEGOCIO |||#
  #                       #|||||||||||||||||||||||||#
  #                       
  #                       tabItem(tabName = "gc_ln",
  #                               h2("Información Línea de Negocio")
  #                               
  #                       ),
  #                       
  #                       #||||||||||||||||||||||||||#
  #                       #||| CENTRO DE ATENCIÓN |||#
  #                       #||||||||||||||||||||||||||#
  #                       
  #                       tabItem(tabName = "gc_ca",
  #                               h2("Información Centro de Atención")
  #                               
  #                       ),
  #                       
  #                       #|||||||||||||||||||||||||#
  #                       #||| DETALLE PRODUCTOR |||#
  #                       #|||||||||||||||||||||||||#
  #                       
  #                       tabItem(tabName = "prod_dp",
  #                               h2("Información detalle productor")
  #                               
  #                       ),
  #                       
  #                       #|||||||||||||||||||||||||||#
  #                       #||| FECHA INTERMEDIARIO |||#
  #                       #|||||||||||||||||||||||||||#
  #                       
  #                       tabItem(tabName = "prod_fi",
  #                               h2("Información fecha intermediario")
  #                               
  #                       ),
  #                       
  #                       #||||||||||||||||||||||#
  #                       #||| DETALLE PÓLIZA |||#
  #                       #||||||||||||||||||||||#
  #                       
  #                       tabItem(tabName = "gc_dp",
  #                               h2("Información detalle póliza")
  #                               
  #                       ),
  #                       
  #                       #||||||||||||||||||||||||||#
  #                       #||| DETALLE SINIESTROS |||#
  #                       #||||||||||||||||||||||||||#
  #                       
  #                       tabItem(tabName = "gc_ds",
  #                               h2("Información detalle siniestros")
  #                               
  #                       ),
  #                       
  #                       #|||||||||||||||||||||||||||#
  #                       #||| DETALLE CONSOLIDADO |||#
  #                       #|||||||||||||||||||||||||||#
  #                       
  #                       tabItem(tabName = "gc_dc",
  #                               h2("Información detalle consolidado")
  #                               
  #                       ),
  #                       
  #                       #|||||||||||||||||||||||||||#
  #                       #||| CONCURSOS MENSUALES |||#
  #                       #|||||||||||||||||||||||||||#
  #                       
  #                       tabItem(tabName = "inc_cm",
  #                               h2("Información concursos mensuales")
  #                               
  #                       ),
  #                       
  #                       #|||||||||||||||||||||||||||#
  #                       #||| CONCURSOS GENERALES |||#
  #                       #|||||||||||||||||||||||||||#
  #                       
  #                       tabItem(tabName = "inc_cg",
  #                               h2("Información concursos generales")
  #                               
  #                       ),
  #                       
  #                       #//////////////////////////#
  #                       #/// GESTIÓN FINANCIERA ///#
  #                       #//////////////////////////#
  #                       
  #                       tabItem(tabName = "gf",
  #                               h2("Información Gestión Financiera")
  #                               
  #                       ),
  #                       
  #                       #////////////////////#
  #                       #///  INDICADORES ///#
  #                       #////////////////////#
  #                       
  #                       tabItem(tabName = "ind_macro",
  #                               h2("Información indicadores macro")
  #                               
  #                       ),
  #                       
  #                       tabItem(tabName = "ind_micro",
  #                               h2("Información indicadores micro")
  #                               
  #                       ),
  #                       
  #                       
  #                       #/////////////#
  #                       #///  BBDD ///#
  #                       #/////////////#
  #                       
  #                       #||||||||||||||||||||#
  #                       #||| BBDD CARTERA |||#
  #                       #||||||||||||||||||||#
  #                       
  #                       tabItem(tabName = "bbdd_c",
  #                               h2("Información BBDD Cartera")
  #                               
  #                       ),
  #                       
  #                       #|||||||||||||||||||||||#
  #                       #||| BBDD SINIESTROS |||#
  #                       #|||||||||||||||||||||||#
  #                       
  #                       tabItem(tabName = "bbdd_s",
  #                               h2("Información BBDD Siniestros")
  #                               
  #                       ),
  #                       
  #                       #////////////////////////////#
  #                       #/// CARGA DE INFORMACIÓN ///#
  #                       #////////////////////////////#
  #                       
  #                       tabItem(tabName = "ci",
  #                               h2("Información carga información")
  #                               
  #                       ),
  #                       
  #                       #///////////////////#
  #                       #///  PROYECCIÓN ///#
  #                       #///////////////////#
  #                       
  #                       tabItem(tabName = "proy",
  #                               h2("Información Proyección")
  #                               
  #                       ),
  #                       
  #                       #///////////////////#
  #                       #///  SIMULACIÓN ///#
  #                       #///////////////////#
  #                       
  #                       tabItem(tabName = "sim",
  #                               h2("Información Simulación")
  #                               
  #                       ),
  #                       
  #                       #//////////////////////////#
  #                       #/// CONSULTAR EN LÍNEA ///#
  #                       #//////////////////////////#
  #                       
  #                       tabItem(tabName = "cl",
  #                               h2("Información consultas en línea")
  #                               
  #                       ),
  #                       
  #                       #////////////////////////#
  #                       #/// EN CERTIFICACIÓN ///#
  #                       #////////////////////////#
  #                       
  #                       tabItem(tabName = "ec",
  #                               h2("Información en certificación")
  #                               
  #                       ),
  #                       
  #                       #//////////////#
  #                       #/// ACERCA ///#
  #                       #//////////////#
  #                       
  #                       tabItem(tabName = "acerca",
  #                               box( width = 9, status="warning",
  #                                    h3(ACERTITLE_TEXT),
  #                                    tags$hr(),
  #                                    h4(ACERVER_TEXT),
  #                                    h4(ACERRIF_TEXT),
  #                                    h4(ACERRS_TEXT),
  #                                    h4(ACERRS_TEXT2),
  #                                    tags$hr(),
  #                                    tags$img(src="img/visionrisk.png", width=300, align = "left"),
  #                                    br(),
  #                                    h5(ACERSUBSV_TEXT),
  #                                    br(),
  #                                    tagList(shiny::icon("map-marker"), ACERDIR_TEXT),br(),
  #                                    tagList(shiny::icon("phone"), ACERTLF_TEXT),br(),
  #                                    tagList(shiny::icon("envelope-o"), ACERCORR_TEXT)
  #                               )#final box
  #                       )#final tabitem
  #                     )#final tabitems
  #                     )#final dashboardbody
  #       )#final dashboardpage
  #    #OJO
  #      shinyURL.ui()
  #   }
  #   
  # )
  # 

  #///////////////////#
  #/// CONSOLIDADO ///#
  #///////////////////#
  
  observeEvent(input$consultar, {
  output$fecha_ini<-renderPrint({
    #agrego dependencia
    input$consultar
    #
    isolate({
    paste(substr(input$fecha1,9,10),substr(input$fecha1,6,7),substr(input$fecha1,1,4),sep = "/")
    })
    
     #paste(substr(input$fecha1,9,10),substr(input$fecha1,6,7),substr(input$fecha1,1,4),sep = "/")
    
      })
  }) #final observeevent
  
  
  observeEvent(input$consultar, {
  output$fecha_fin<-renderPrint({
    # #agrego dependencia 
    # input$consultar
    # #
    # isolate({ 
    #   paste(substr(input$fecha2,9,10),substr(input$fecha2,6,7),substr(input$fecha2,1,4),sep = "/")
    # }) 
       paste(substr(input$fecha2,9,10),substr(input$fecha2,6,7),substr(input$fecha2,1,4),sep = "/")
    
    
      })
  })#final observeevent
  
  
  observeEvent(input$consultar, {
  output$sucursal<-renderPrint({
    # #agrego dependencia 
    # input$consultar
    # #
    # isolate({ 
    #   input$centro_atencion
    # })
    input$centro_atencion
    
      })
  })#final observeevent
  
  
  observeEvent(input$consultar, {
  output$cuenta<-renderPrint({
    # #agrego dependencia 
    # input$consultar
    # #
    # isolate({ 
    # input$cuentas_esp
    # })
    input$cuentas_esp
    
      })
  })#final observeevent
  
  #tabla 1
  
  observeEvent(input$consultar, {
  output$t1 <-  renderUI(
    
     box(style="overflow-x:scroll",width = 12,title="Inventario por Centros de Atención",status="primary",solidHeader=TRUE,
         dataTableOutput("tabla1_con"))
  )
  })#final observeevent
  
  #tabla 1  consolidado
  output$tabla1_con <- renderDataTable(
    {
    #agrego dependencia 
    input$consultar
    #
    isolate({ 
    
    a <- as.data.frame(matrix(0,nrow = 10,ncol = 37))
    names(a) <- c("Centro De Atención","Pólizas Nuevas Auto","Pólizas Renovadas Auto",
                  "Pólizas Auto","% Ppto Auto","Cartera Activa Auto","Interm Activos Auto",
                  "Pólizas Nuevas Fianza","Pólizas Renovadas Fianza",
                  "Pólizas Fianza","% Ppto Fianza","Cartera Activa Fianza","Interm Activos Fianza",
                  "Pólizas Nuevas Patrimoniales","Pólizas Renovadas Patrimoniales",
                  "Pólizas Patrimoniales","% Ppto Patrimoniales","Cartera Activa Patrimoniales","Interm Activos Patrimoniales",
                  "Pólizas Nuevas Personas","Pólizas Renovadas Personas",
                  "Pólizas Personas","% Ppto Personas","Cartera Activa Personas","Interm Activos Personas",
                  "Pólizas Nuevas Salud","Pólizas Renovadas Salud",
                  "Pólizas Salud","% Ppto Salud","Cartera Activa Salud","Interm Activos Salud",
                  "Pólizas Nuevas General","Pólizas Renovadas General",
                  "Pólizas General","% Ppto General","Cartera Activa General","Interm Activos General"
                  )
    #return(datatable(a, options = list(paging = FALSE)))
    return(a)  
    
    })#final isolate
    
    },rownames = FALSE,options = list(
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#04B404', 'color': '#fff'});",
          "}")
        ))
  
  #tabla 2
  
  observeEvent(input$consultar, {
    output$t2 <-  renderUI(
       box(style="overflow-x:scroll",width = 12,title="Prima Cobrada por Centros de Atención",status="primary",solidHeader=TRUE,
           dataTableOutput("tabla2_con"))
    )
  })#final observeevent
  

  #tabla 2  consolidado
  output$tabla2_con <- renderDataTable({
    #agrego dependencia
    input$consultar
    
    #
    isolate({ 
      b <- read.csv(paste0(getwd(),"/Datos/data_consolidado_tabla2.txt"), sep="")
      
  
    # names(b) <- c("Centro De Atención","Prima Cobrada Auto","% Ppto Auto","% Sin. Auto",
    #               "% Sin Rolling 12 Auto","% Persistencia Auto","% Persistencia Rolling 12 Auto",
    #               "Prima Cobrada Fianza","% Ppto Fianza","% Sin. Fianza",
    #               "% Sin Rolling 12 Fianza","% Persistencia Fianza","% Persistencia Rolling 12 Fianza",
    #               "Prima Cobrada Patrimoniales","% Ppto Patrimoniales","% Sin. Patrimoniales",
    #               "% Sin Rolling 12 Patrimoniales","% Persistencia Patrimoniales","% Persistencia Rolling 12 Patrimoniales",
    #               "Prima Cobrada Personas","% Ppto Personas","% Sin. Personas",
    #               "% Sin Rolling 12 Personas","% Persistencia Personas","% Persistencia Rolling 12 Personas",
    #               "Prima Cobrada Salud","% Ppto Salud","% Sin. Salud",
    #               "% Sin Rolling 12 Salud","% Persistencia Salud","% Persistencia Rolling 12 Salud",
    #               "Prima Cobrada General","% Ppto General","% Sin. General",
    #               "% Sin Rolling 12 General","% Persistencia General","% Persistencia Rolling 12 General"
    #               )
    # 
    
    
      #condicional para filtrar por fecha
      b[,38] <- as.character(b[,38])
      lim1 <- which(b[,38]==input$fecha1)
      lim2 <- which(b[,38]==input$fecha2)
      b <- b[lim1:lim2,]
      
      
      #condicional para filtrar por centro de atencion
      b <- b[b[,1]==input$centro_atencion,]
      
      #condicional para filtrar por cuentas especiales
      if(input$cuentas_esp=="Todas"){
        b <- b[1:nrow(b),]
      }else{
        b <- b[b[,39]==input$cuentas_esp,]
      }
      
      return(b[,-c(38,39)])
      
    
    }) #final isolate
    
  },rownames = FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
      "}")
  ))
 

  #tabla 3
  
  observeEvent(input$consultar, {
    output$t3 <-  renderUI(

      box(style="overflow-x:scroll",width = 12,title="Inventario por Centros de Atención",status="primary",solidHeader=TRUE,
          dataTableOutput("tabla3_con"))
      
    )
  })#final observeevent
  
  
  #tabla 2  consolidado
  output$tabla3_con <- renderDataTable({
    #agrego dependencia
    input$consultar
    
    #
    isolate({ 
    #leo data de la carpeta datos
    #b <- read.csv(paste0(getwd(),"/Datos/data_consolidado.txt"), sep="")
    b <- read.csv(paste0(getwd(),"/Datos/data_consolidado_nueva.txt"), sep="")
      
    #veo filtros a aplicar
    # if(is.null(input$centro_atencion) &is.null(input$cuentas_esp)){
    #  head(iris)
    # }else{
    #   return(b)
    #   }
    
    #a <- seq.Date(as.Date(input$fecha1),as.Date(input$fecha2),by="days")
    
    #b <- b[which(b[,1]==a),]
    
    #condicional para filtrar por fecha
    # b[,1] <- as.character(b[,1])
    # lim1 <- which(b[,1]==input$fecha1)
    # lim2 <- which(b[,1]==input$fecha2)
     b[,38] <- as.character(b[,38])
     lim1 <- which(b[,38]==input$fecha1)
     lim2 <- which(b[,38]==input$fecha2)
    b <- b[lim1:lim2,]
    
    
    #condicional para filtrar por centro de atencion
    #b <- b[b[,2]==input$centro_atencion,]
    b <- b[b[,1]==input$centro_atencion,]
    
    #condicional para filtrar por cuentas especiales
    if(input$cuentas_esp=="Todas"){
      b <- b[1:nrow(b),]
    }else{
      #b <- b[b[,3]==input$cuentas_esp,]
      b <- b[b[,39]==input$cuentas_esp,]
    }
    
    return(b[,-c(38,39)])
    
    }) #final isolate

  },rownames = FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
      "}")
  ))
  
  #Nuevos botones Consolidado
  output$consolidado_opc <- renderUI({ 
    
    #selectInput("obs_tif", "Seleccionar títulos", obs_elim_tif(),multiple = TRUE)
    box(width=12,title="Consolidado",status="primary",solidHeader=TRUE ,
        column(width = 6,
               #box( width = 6, background = "navy",
               dateInput(inputId="fecha1", label="Desde:", language= "es",
                         width = "100%")#final dateimput
               #),#final box
        ),#final column
        #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box
        
        column(width = 6,
               #box( width = 6, background = "navy",
               dateInput(inputId="fecha2", label="Hasta:", language= "es",
                         width = "100%")#final dateimput
               #)#final box
        ),#final column
        #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box
        
        column(width = 6,
               #box( width = 6, background = "navy",
               selectInput("centro_atencion", "Centro de Atención:",
                           choices = centro_aten_cons())
               #)#final box
        ),
        column(width = 6,
               #box( width = 6, background = "navy",
               selectInput("cuentas_esp", "Cuentas Especiales:",
                           choices = c("Todas",cuentas_esp_cons()))
               #)#final box
        ),
        column(width = 6,
               #box( width = 6, background = "navy",
               actionButton("consultar", "Consultar",
                            style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
        )
        
        
        #)#final fluidrow
        
        
        
    ) # final box
  })
  
  #funcion que me extrae las opciones/nieveles de los centro de atencion
  centro_aten_cons <- reactive({
    #b <- read.csv(paste0(getwd(),"/Datos/data_consolidado.txt"), sep="")
    #b[,2] <- as.factor(b[,2])
    #return(levels(b[,2]))
    b <- read.csv(paste0(getwd(),"/Datos/data_consolidado_nueva.txt"), sep="")
    b[,1] <- as.factor(b[,1])
    return(levels(b[,1]))
  })
  
  #funcion que me extrae las opciones/nieveles de los centro de atencion
  cuentas_esp_cons <- reactive({
    # b <- read.csv(paste0(getwd(),"/Datos/data_consolidado.txt"), sep="")
    # b[,3] <- as.factor(b[,3])
    # return(levels(b[,3]))
     b <- read.csv(paste0(getwd(),"/Datos/data_consolidado_nueva.txt"), sep="")
     b[,39] <- as.factor(b[,39])
     return(levels(b[,39]))
  })
  
  #mensaje de fechas disponibles
  observeEvent(input$consultar, {
    output$fechas_disp_cons<-renderPrint({
      #agrego dependencia
      input$consultar
      #
      isolate({
        # b <- read.csv(paste0(getwd(),"/Datos/data_consolidado.txt"), sep="")
        # b[,1] <- as.Date(b[,1])
        # fechas <- range(b[,1])
        # f1 <- paste(substr(fechas[1],9,10),substr(fechas[1],6,7),substr(fechas[1],1,4),sep = "/")
        # f2 <- paste(substr(fechas[2],9,10),substr(fechas[2],6,7),substr(fechas[2],1,4),sep = "/")
        #                                           
        b <- read.csv(paste0(getwd(),"/Datos/data_consolidado_nueva.txt"), sep="")
        b[,38] <- as.Date(b[,38])
        fechas <- range(b[,38])
        f1 <- paste(substr(fechas[1],9,10),substr(fechas[1],6,7),substr(fechas[1],1,4),sep = "/")
        f2 <- paste(substr(fechas[2],9,10),substr(fechas[2],6,7),substr(fechas[2],1,4),sep = "/")
        
        print(paste0("Las fechas disponibles se encuentran entre el ",f1," y el ",f2))
        
      })
      

    })
  }) #final observeevent
  
  
#GESTON COMERCIAL
  
  

#LÍNEA DE NEGOCIO
#tabla 1 LÍNEA DE NEGOCIO

observeEvent(input$consultar_ca, {
  output$t1_ca <-  renderUI(
    
    box(style="overflow-x:scroll",width = 12,title="Productores por Centros de Atención",status="primary",solidHeader=TRUE,
        dataTableOutput("tabla1_ca"))
  )
})#final observeevent

#tabla 1  
output$tabla1_ca <- renderDataTable(
  {
    #agrego dependencia 
    input$consultar_ca
    #
    isolate({ 
      
      a <- as.data.frame(matrix(0,nrow = 10,ncol = 21))
      names(a) <- c("Centro De Atención","Código Productor","Productor",
                    "Cobrado Auto","% Auto","% Sin Auto",
                    "Cobrado Fianza","% Fianza","% Sin Fianza",
                    "Cobrado Patrimoniales","% Patrimoniales","% Sin Patrimoniales",
                    "Cobrado Personas","% Personas","% Sin Personas",
                    "Cobrado Salud","% Salud","% Sin Salud",
                    "Cobrado General","% General","% Sin General"
                    )
      #return(datatable(a, options = list(paging = FALSE)))
      return(a)  
      
    })#final isolate
    
  },rownames = FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#04B404', 'color': '#fff'});",
      "}")
  ))

#tabla 2 LÍNEA DE NEGOCIO

observeEvent(input$consultar_ca, {
  output$t2_ca <-  renderUI(
    
    box(style="overflow-x:scroll",width = 12,title="Inventario de Pólizas",status="primary",solidHeader=TRUE,
        dataTableOutput("tabla2_ca"))
  )
})#final observeevent

#tabla 2  
output$tabla2_ca <- renderDataTable(
  {
    #agrego dependencia 
    input$consultar_ca
    #
    isolate({ 
      
      a <- as.data.frame(matrix(0,nrow = 10,ncol = 20))
      names(a) <- c("Código Productor","Nombre Productor",
                    "Pólizas Auto","% Ppto Auto","Cartera Activa Auto",
                    "Pólizas Fianza","% Ppto Fianza","Cartera Activa Fianza",
                    "Pólizas Patrimoniales","% Ppto Patrimoniales","Cartera Activa Patrimoniales",
                    "Pólizas Personas","% Ppto Personas","Cartera Activa Personas",
                    "Pólizas Salud","% Ppto Salud","Cartera Activa Salud",
                    "Pólizas General","% Ppto General","Cartera Activa General"
                    )
      #return(datatable(a, options = list(paging = FALSE)))
      return(a)  
      
    })#final isolate
    
  },rownames = FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#04B404', 'color': '#fff'});",
      "}")
  ))

#PRODUCTORES

#tabla 1 PRODUCTORES

observeEvent(input$consultar_prod, {
  output$t1_prod <-  renderUI(
    
    box(style="overflow-x:scroll",width = 12,title="Productores Centros de Atención",status="primary",solidHeader=TRUE,
        dataTableOutput("tabla1_prod"))
  )
})#final observeevent

#tabla 1  
output$tabla1_prod <- renderDataTable(
  {
    #agrego dependencia 
    input$consultar_prod
    #
    isolate({ 
      
      a <- as.data.frame(matrix(0,nrow = 6,ncol = 8))
      names(a) <- c("Línea Negocio","Prima Cobrada","Prima Devengada",
                    "Siniestros Pagados","Siniestros Pendientes",
                    "Siniestros Incurridos","Cantidad Siniestros",
                    "Siniestralidad")
      a[,1] <- c("Auto","Fianza","Patrimoniales","Personas","Salud","Totales:")
      
      #return(datatable(a, options = list(paging = FALSE)))
      return(a)  
      
    })#final isolate
    
  },rownames = FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#04B404', 'color': '#fff'});",
      "}")
  ))

#tabla 2 productores

observeEvent(input$consultar_prod, {
  output$t2_prod <-  renderUI(
    
    box(style="overflow-x:scroll",width = 12,title="Gráficos",status="primary",solidHeader=TRUE,
        dataTableOutput("tabla2_prod"))
  )
})#final observeevent

#tabla 2  
output$tabla2_prod <- renderDataTable(
  {
    #agrego dependencia 
    input$consultar_prod
    #
    isolate({ 
      
    
      
    })#final isolate
    
  },rownames = FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#04B404', 'color': '#fff'});",
      "}")
  ))

#tabla 3 productores

observeEvent(input$consultar_prod, {
  output$t3_prod <-  renderUI(
    
    box(style="overflow-x:scroll",width = 12,title="Inventario de Pólizas",status="primary",solidHeader=TRUE,
        dataTableOutput("tabla3_prod"))
  )
})#final observeevent

#tabla 2  
output$tabla3_prod <- renderDataTable(
  {
    #agrego dependencia 
    input$consultar_prod
    #
    isolate({ 
      a <- as.data.frame(matrix(0,nrow = 10,ncol = 20))
      names(a) <- c("Código Productor","Nombre Productor",
                    "Pólizas Auto","% Ppto Auto","Cartera Activa Auto",
                    "Pólizas Fianza","% Ppto Fianza","Cartera Activa Fianza",
                    "Pólizas Patrimoniales","% Ppto Patrimoniales","Cartera Activa Patrimoniales",
                    "Pólizas Personas","% Ppto Personas","Cartera Activa Personas",
                    "Pólizas Salud","% Ppto Salud","Cartera Activa Salud",
                    "Pólizas General","% Ppto General","Cartera Activa General"
      )
      return(a) 
      
      
    })#final isolate
    
  },rownames = FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#04B404', 'color': '#fff'});",
      "}")
  ))


#tabla 1 PRODUCTORES - FICHA INTERMEDIARIO
observeEvent(input$consultar_prod_fi1, {
  output$t1_prod_fi <-  renderUI(
    
    box(style="overflow-x:scroll",width = 12,title="Datos de Identificación",status="primary",solidHeader=TRUE,
        dataTableOutput("tabla1_prod_fi"))
  )
})#final observeevent

#tabla 1  
output$tabla1_prod_fi <- renderDataTable(
  {
    #agrego dependencia 
    input$consultar_prod_fi1
    #
    isolate({ 
      
      a <- as.data.frame(matrix(0,nrow = 6,ncol = 5))
      names(a) <- c("Nombre","Tipo","Sucursal",
                    "Código","Fecha")

      #return(datatable(a, options = list(paging = FALSE)))
      return(a)  
      
    })#final isolate
    
  },rownames = FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#04B404', 'color': '#fff'});",
      "}")
  ))

#tabla 2 PRODUCTORES - FICHA INTERMEDIARIO
observeEvent(input$consultar_prod_fi1, {
  output$t2_prod_fi <-  renderUI(
    
    box(style="overflow-x:scroll",width = 12,title="Datos Cartera",status="primary",solidHeader=TRUE,
        dataTableOutput("tabla2_prod_fi"))
  )
})#final observeevent

#tabla 2
output$tabla2_prod_fi <- renderDataTable(
  {
    #agrego dependencia 
    input$consultar_prod_fi1
    #
    isolate({ 
      
      a <- as.data.frame(matrix(0,nrow = 6,ncol = 12))
      names(a) <- c("Línea Negocio","Prima Cobrada","Prima Devengada",
                    "Siniestros Pagados","Comisiones Bonos",
                    "Siniestros Incurridos","Cantidad Siniestros",
                    "% Siniestralidad","% Persistencia","Rentabilidad",
                    "% Cumplimiento Presupuesto Inventario",
                    "% Cumplimiento Presupuesto Primas Cobradas")

      #return(datatable(a, options = list(paging = FALSE)))
      return(a)  
      
    })#final isolate
    
  },rownames = FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#04B404', 'color': '#fff'});",
      "}")
  ))

#tabla 3 PRODUCTORES - FICHA INTERMEDIARIO
observeEvent(input$consultar_prod_fi1, {
  output$t3_prod_fi <-  renderUI(
    
    box(style="overflow-x:scroll",width = 12,title="Información Detallada de las Principales Cuentas",status="primary",solidHeader=TRUE,
        dataTableOutput("tabla3_prod_fi"))
  )
})#final observeevent

#tabla 3
output$tabla3_prod_fi <- renderDataTable(
  {
    #agrego dependencia 
    input$consultar_prod_fi1
    #
    isolate({ 
      
      a <- as.data.frame(matrix(0,nrow = 6,ncol = 11))
      names(a) <- c("Cliente","Línea Negocio","Póliza","Prima Cobrada",
                    "Fecha Suscipción","Vigencia Desde","Vigencia Hasta",
                    "Prima Devengada","Siniestros Pagados","Siniestros Incurridos",
                    "% Siniestralidad")

      #return(datatable(a, options = list(paging = FALSE)))
      return(a)  
      
    })#final isolate
    
  },rownames = FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#04B404', 'color': '#fff'});",
      "}")
  ))

#tabla 4 PRODUCTORES - FICHA INTERMEDIARIO
observeEvent(input$consultar_prod_fi1, {
  output$t4_prod_fi <-  renderUI(
    
    box(style="overflow-x:scroll",width = 12,title="Información Detallada de los Principales Siniestros",status="primary",solidHeader=TRUE,
        dataTableOutput("tabla4_prod_fi"))
  )
})#final observeevent

#tabla 4
output$tabla4_prod_fi <- renderDataTable(
  {
    #agrego dependencia 
    input$consultar_prod_fi1
    #
    isolate({ 
      
      a <- as.data.frame(matrix(0,nrow = 6,ncol = 8))
      names(a) <- c("Cliente","Póliza","Línea Negocio","Siniestro",
                    "Fecha Declaración","Fecha Ocurrencia",
                    "Total Pagado","Reserva")

      #return(datatable(a, options = list(paging = FALSE)))
      return(a)  
      
    })#final isolate
    
  },rownames = FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#04B404', 'color': '#fff'});",
      "}")
  ))

#tabla 1 DETALLE PÓLIZAS 
observeEvent(input$gc_dp_consultar, {
  output$t1_gc_dp <-  renderUI(
    
    box(style="overflow-x:scroll",width = 12,title="Detalle de Pólizas",status="primary",solidHeader=TRUE,
        dataTableOutput("tabla1_gc_dp"))
  )
})#final observeevent

#tabla 4
output$tabla1_gc_dp <- renderDataTable(
  {
    #agrego dependencia 
    input$gc_dp_consultar
    #
    isolate({ 
      
      a <- as.data.frame(matrix(0,nrow = 6,ncol = 10))
      names(a) <- c("Sucursal","Ramo","Línea Negocio","Nombre del Cliente",
                    "Número Póliza","Prima Cobrada","Prima Devengada",
                    "Comisión","Fecha Inicio Vigencia",
                    "Fecha Fin Vigencia")
      
      #return(datatable(a, options = list(paging = FALSE)))
      return(a)  
      
    })#final isolate
    
  },rownames = FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#04B404', 'color': '#fff'});",
      "}")
  ))

#tabla 1 DETALLE SINIESTROS 
observeEvent(input$gc_ds_consultar, {
  output$t1_gc_ds <-  renderUI(
    
    box(style="overflow-x:scroll",width = 12,title="Detalle de Siniestros",status="primary",solidHeader=TRUE,
        dataTableOutput("tabla1_gc_ds"))
  )
})#final observeevent

#tabla 4
output$tabla1_gc_ds <- renderDataTable(
  {
    #agrego dependencia 
    input$gc_ds_consultar
    #
    isolate({ 
      
      a <- as.data.frame(matrix(0,nrow = 6,ncol = 10))
      names(a) <- c("Sucursal","Ramo","Línea Negocio","Nombre del Cliente",
                    "Centro de Atención Receptor","Número Póliza",
                    "Número Siniestro","Siniestros Pagados",
                    "Siniestros Pendientes","Siniestros Incurridos")
      
      #return(datatable(a, options = list(paging = FALSE)))
      return(a)  
      
    })#final isolate
    
  },rownames = FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#04B404', 'color': '#fff'});",
      "}")
  ))


#tabla 1 DETALLE CONSOLIDADO 
observeEvent(input$gc_dc_consultar, {
  output$t1_gc_dc <-  renderUI(
    
    box(style="overflow-x:scroll",width = 12,title="Detalle Consolidado",status="primary",solidHeader=TRUE,
        dataTableOutput("tabla1_gc_dc"))
  )
})#final observeevent

#tabla 4
output$tabla1_gc_dc <- renderDataTable(
  {
    #agrego dependencia 
    input$gc_dc_consultar
    #
    isolate({ 
      
      a <- as.data.frame(matrix(0,nrow = 6,ncol = 15))
      names(a) <- c("Sucursal","Ramo","Línea Negocio","Nombre del Cliente",
                    "Número Póliza","Prima Cobrada","Prima Devengada",
                    "Siniestros Pagados","Siniestros Pendientes",
                    "Siniestros Incurridos","Recuperaciones",
                    "Comisiones e Incentivos Pagados","Gastos Directos del Ramo",
                    "Resultado Técnico Antes de Gasto","Siniestralidad"
                    )
      
      #return(datatable(a, options = list(paging = FALSE)))
      return(a)  
      
    })#final isolate
    
  },rownames = FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#04B404', 'color': '#fff'});",
      "}")
  ))


#GESTION TÉCNICA
#CENTRO DE ATENCIÓN

output$gt_rsa_opc <- renderUI({ 
  
  box(width=12,title="Gestión Técnica Centro de Atención",status="primary",solidHeader=TRUE ,
      column(width = 6,
             #box( width = 6, background = "navy",
             dateInput(inputId="gt_ca_1", label="Desde", language= "es",
                       width = "100%")#final dateimput
             
             #),#final box
      ),#final column
      #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box
      
      column(width = 6,
             #box( width = 6, background = "navy",
             dateInput(inputId="gt_ca_2", label="Hasta", language= "es",
                       width = "100%")#final dateimput
             #)#final box
      ),#final column
      #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box
      
      column(width = 6,
             #box( width = 6, background = "navy",
             selectInput("gt_ca_3", "Centro de Atención",
                         choices = gt_rca_b1())
             #)#final box
      ),
      column(width = 6,
             #box( width = 6, background = "navy",
             selectInput("gt_ca_4", "Cuentas Especiales",
                         choices = gt_rca_b2())
             #)#final box
      ),
      
      column(width = 6,
             #box( width = 6, background = "navy",
             actionButton("gt_ca_boton", "Consultar",
                          style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
      )
      
      
      #)#final fluidrow
      
      
      
  ) # final box
  
  
}) #final renderUI

#funcion que me extrae las opciones/nieveles de los centro de atencion
gt_rca_b1 <- reactive({
  #b <- read.csv(paste0(getwd(),"/Datos/data_consolidado.txt"), sep="")
  #b[,2] <- as.factor(b[,2])
  #return(levels(b[,2]))
  b <- read.csv(paste0(getwd(),"/Datos/data_gt_rca1.txt"), sep="")
  b[,1] <- as.factor(b[,1])
  return(levels(b[,1]))
})

#funcion que me extrae las opciones/nieveles de las cuentas especiales
gt_rca_b2 <- reactive({
  # b <- read.csv(paste0(getwd(),"/Datos/data_consolidado.txt"), sep="")
  # b[,3] <- as.factor(b[,3])
  # return(levels(b[,3]))
  b <- read.csv(paste0(getwd(),"/Datos/data_gt_rca1.txt"), sep="")
  b[,14] <- as.factor(b[,14])
  return(levels(b[,14]))
})

#mensaje de fechas disponibles
observeEvent(input$gt_ca_boton, {
  output$fechas_disp_gt_rca<-renderPrint({
    #agrego dependencia
    input$gt_ca_boton
    #
    isolate({
      # b <- read.csv(paste0(getwd(),"/Datos/data_consolidado.txt"), sep="")
      # b[,1] <- as.Date(b[,1])
      # fechas <- range(b[,1])
      # f1 <- paste(substr(fechas[1],9,10),substr(fechas[1],6,7),substr(fechas[1],1,4),sep = "/")
      # f2 <- paste(substr(fechas[2],9,10),substr(fechas[2],6,7),substr(fechas[2],1,4),sep = "/")
      #                                           
      b <- read.csv(paste0(getwd(),"/Datos/data_gt_rca1.txt"), sep="")
      b[,13] <- as.Date(b[,13])
      fechas <- range(b[,13])
      f1 <- paste(substr(fechas[1],9,10),substr(fechas[1],6,7),substr(fechas[1],1,4),sep = "/")
      f2 <- paste(substr(fechas[2],9,10),substr(fechas[2],6,7),substr(fechas[2],1,4),sep = "/")
      
      print(paste0("Las fechas disponibles se encuentran entre el ",f1," y el ",f2))
      
    })
    
    
  })
}) #final observeevent



#tabla 1
  output$t1_gt_ca <-  renderUI(
    
    box(style="overflow-x:scroll",width = 12,status="primary",solidHeader=TRUE,
        dataTableOutput("tabla1_gt_ca"))
  )

#tabla 1
  
output$tabla1_gt_ca <- renderDataTable(
  {
    #agrego dependencia 
    #input$gc_dc_consultar
    #
    #isolate({ 
      
      a <- as.data.frame(matrix(0,nrow = 6,ncol = 12))
      names(a) <- c("Centro Atención","Pólizas Nuevas","Pólizas Renovadas",
                    "Inventario Real","Inventario Presupuesto","Inventario Cumplimiento (%)",
                    "Participación Primas Real (%)","Participación Primas Presupuesto (%)",
                    "Primas Cobradas Netas de Devolución Real","Primas Cobradas Netas de Devolución Presupuesto",
                    "Primas Cobradas Netas de Devolución Cumplimiento (%)",
                    "Reservas de Primas al Inicio Real"
      )
      
      #return(datatable(a, options = list(paging = FALSE)))
      return(a)  
      
   # })#final isolate
    
  },rownames = FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#04B404', 'color': '#fff'});",
      "}")
  ))

#tabla 2
output$t2_gt_ca <-  renderUI(
  
  box(style="overflow-x:scroll",width = 12,status="primary",solidHeader=TRUE,
      dataTableOutput("tabla2_gt_ca"))
)

#tabla 2
output$tabla2_gt_ca <- renderDataTable(
  {
    #agrego dependencia 
    #input$gc_dc_consultar
    #
    #isolate({ 
    
    a <- as.data.frame(matrix(0,nrow = 6,ncol = 13))
    names(a) <- c("Centro Atención","(*) Siniestralidad Incurrida Real (%)","Siniestralidad Incurrida Presupuesto (%)",
                  "(*) Siniestralidad Incurrida sin IBNR Real (%)","Siniestralidad Incurrida sin IBNR Presupuesto (%)",
                  "(*) Siniestralidad Incurrida Rolling 12 Real (%)","Siniestralidad Incurrida Rolling 12 Presupuesto (%)",
                  "Persistencia (%)","Persistencia Rolling 12 (%)",
                  "Comisiones sobre Devengada Real (%)","Comisiones sobre Devengada Presupuesto (%)",
                  "Comisiones sobre Cobrado Real (%)","Comisiones sobre Cobrado Presupuesto (%)"
    )
    
    #return(datatable(a, options = list(paging = FALSE)))
    return(a)  
    
    # })#final isolate
    
  },rownames = FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#04B404', 'color': '#fff'});",
      "}")
  ))

#LÍNEA DE NEGOCIO

#tabla 1
output$t1_gt_ln <-  renderUI(
  
  box(style="overflow-x:scroll",width = 12,status="primary",solidHeader=TRUE,
      dataTableOutput("tabla1_gt_ln"))
)

#tabla 1
output$tabla1_gt_ln <- renderDataTable(
  {
    #agrego dependencia 
    #input$gc_dc_consultar
    #
    #isolate({ 
    
    a <- as.data.frame(matrix(0,nrow = 6,ncol = 12))
    names(a) <- c("Centro Atención","Pólizas Nuevas","Pólizas Renovadas",
                  "Inventario Real","Inventario Presupuesto","Inventario Cumplimiento (%)",
                  "Participación Primas Real (%)","Participación Primas Presupuesto (%)",
                  "Primas Cobradas Netas de Devolución Real","Primas Cobradas Netas de Devolución Presupuesto",
                  "Primas Cobradas Netas de Devolución Cumplimiento (%)",
                  "Reservas de Primas al Inicio Real"
    )
    
    #return(datatable(a, options = list(paging = FALSE)))
    a[,1] <- c("Auto","Fianzas","Patrimoniales","Personas","Salud","Totales")
    return(a)  
    
    # })#final isolate
    
  },rownames = FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#04B404', 'color': '#fff'});",
      "}")
  ))

#tabla 2
output$t2_gt_ln <-  renderUI(
  
  box(style="overflow-x:scroll",width = 12,status="primary",solidHeader=TRUE,
      dataTableOutput("tabla2_gt_ln"))
)

#tabla 2
output$tabla2_gt_ln <- renderDataTable(
  {
    #agrego dependencia 
    #input$gc_dc_consultar
    #
    #isolate({ 
    
    a <- as.data.frame(matrix(0,nrow = 6,ncol = 13))
    names(a) <- c("Centro Atención","(*) Siniestralidad Incurrida Real (%)","Siniestralidad Incurrida Presupuesto (%)",
                  "(*) Siniestralidad Incurrida sin IBNR Real (%)","Siniestralidad Incurrida sin IBNR Presupuesto (%)",
                  "(*) Siniestralidad Incurrida Rolling 12 Real (%)","Siniestralidad Incurrida Rolling 12 Presupuesto (%)",
                  "Persistencia (%)","Persistencia Rolling 12 (%)",
                  "Comisiones sobre Devengada Real (%)","Comisiones sobre Devengada Presupuesto (%)",
                  "Comisiones sobre Cobrado Real (%)","Comisiones sobre Cobrado Presupuesto (%)"
    )
    
    #return(datatable(a, options = list(paging = FALSE)))
    a[,1] <- c("Auto","Fianzas","Patrimoniales","Personas","Salud","Totales")
    return(a)  
    
    # })#final isolate
    
  },rownames = FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#04B404', 'color': '#fff'});",
      "}")
  ))

#DETALLE POR LÍNEA DE NEGOCIO

#tabla 1
output$t1_gt_dln <-  renderUI(
  
  box(style="overflow-x:scroll",width = 12,status="primary",solidHeader=TRUE,
      dataTableOutput("tabla1_gt_dln"))
)

#tabla 1
output$tabla1_gt_dln <- renderDataTable(
  {
    #agrego dependencia 
    #input$gc_dc_consultar
    #
    #isolate({ 
    
    a <- as.data.frame(matrix(0,nrow = 4,ncol = 12))
    names(a) <- c("Ramo","Pólizas Nuevas","Pólizas Renovadas","Cantidad de Asegurados",
                  "Inventario Real","Inventario Presupuesto","Inventario Cumplimiento (%)",
                  "Participación Primas Real (%)","Participación Primas Presupuesto (%)",
                  "Primas Cobradas Netas de Devolución Nueva","Primas Cobradas Netas de Devolución Renovada",
                  "Primas Cobradas Netas de Devolución Real"
    )
    
    #return(datatable(a, options = list(paging = FALSE)))
    a[,1] <- c("AUTOMOVIL","AUTOMIVIL CASCO FLOTA","RESPONSABILIDAD CIVIL DE VEHICULOS","TOTAL")
    return(a)  
    
    # })#final isolate
    
  },rownames = FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#04B404', 'color': '#fff'});",
      "}")
  ))

#tabla 2
output$t2_gt_dln <-  renderUI(
  
  box(style="overflow-x:scroll",width = 12,status="primary",solidHeader=TRUE,
      dataTableOutput("tabla2_gt_dln"))
)

#tabla 2
output$tabla2_gt_dln <- renderDataTable(
  {
    #agrego dependencia 
    #input$gc_dc_consultar
    #
    #isolate({ 
    
    a <- as.data.frame(matrix(0,nrow = 4,ncol = 13))
    names(a) <- c("Ramo","(*) Siniestralidad Incurrida Real (%)","Siniestralidad Incurrida Presupuesto (%)",
                  "(*) Siniestralidad Incurrida sin IBNR Real (%)","Siniestralidad Incurrida sin IBNR Presupuesto (%)",
                  "(*) Siniestralidad Incurrida Rolling 12 Real (%)","Siniestralidad Incurrida Rolling 12 Presupuesto (%)",
                  "Persistencia (%)","Persistencia Rolling 12 (%)",
                  "Comisiones sobre Devengada Real (%)","Comisiones sobre Devengada Presupuesto (%)",
                  "Comisiones sobre Cobrado Real (%)","Comisiones sobre Cobrado Presupuesto (%)"
    )
    
    #return(datatable(a, options = list(paging = FALSE)))
    a[,1] <- c("AUTOMOVIL","AUTOMIVIL CASCO FLOTA","RESPONSABILIDAD CIVIL DE VEHICULOS","TOTAL")
    return(a)  
    
    # })#final isolate
    
  },rownames = FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#04B404', 'color': '#fff'});",
      "}")
  ))



}) #final Shinyserver