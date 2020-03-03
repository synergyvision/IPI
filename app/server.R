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
      b <- read.csv(paste0(getwd(),"/Datos/data_consolidado_tabla2.txt"), sep="",encoding = "UTF-8")
      
  
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
    b <- read.csv(paste0(getwd(),"/Datos/data_consolidado_nueva.txt"), sep="",encoding = "UTF-8")
      
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
    b <- read.csv(paste0(getwd(),"/Datos/data_consolidado_nueva.txt"), sep="",encoding = "UTF-8")
    b[,1] <- as.factor(b[,1])
    return(levels(b[,1]))
  })
  
  #funcion que me extrae las opciones/nieveles de los centro de atencion
  cuentas_esp_cons <- reactive({
    # b <- read.csv(paste0(getwd(),"/Datos/data_consolidado.txt"), sep="")
    # b[,3] <- as.factor(b[,3])
    # return(levels(b[,3]))
     b <- read.csv(paste0(getwd(),"/Datos/data_consolidado_nueva.txt"), sep="",encoding = "UTF-8")
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
        b <- read.csv(paste0(getwd(),"/Datos/data_consolidado_nueva.txt"), sep="",encoding = "UTF-8")
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
  
  #OPCIONES
  output$gc_ln_opc <- renderUI({ 
    
    box(width=12,title="Información Línea de Negocio",status="primary",solidHeader=TRUE ,
        column(width = 6,
               #box( width = 6, background = "navy",
               dateInput(inputId="fecha1_ca", label="Desde:", language= "es",
                         width = "100%")#final dateimput
               #),#final box
        ),#final column
        #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box
        
        column(width = 6,
               #box( width = 6, background = "navy",
               dateInput(inputId="fecha2_ca", label="Hasta:", language= "es",
                         width = "100%")#final dateimput
               #)#final box
        ),#final column
        #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box
        
        column(width = 6,
               #box( width = 6, background = "navy",
               selectInput("centro_atencion_ca", "Centro de Atención:",
                           choices = c("Centro 1","Centro 2","Centro 3","Centro 4","Centro 5"))
               #)#final box
        ),
        column(width = 6,
               #box( width = 6, background = "navy",
               selectInput("cuentas_esp_ca", "Cuentas Especiales:",
                           choices = c("Cuenta 1","Cuenta 2","Cuenta 3","Cuenta 4","Cuenta 5"))
               #)#final box
        ),
        column(width = 6,
               #box( width = 6, background = "navy",
               actionButton("consultar_ca", "Consultar",
                            style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
        )
    ) #final box
    
  })
  
  
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
      
      a <- read.csv(paste0(getwd(),"/Datos/data_gc_ln1.txt"), sep="",encoding = "UTF-8")

      #filtro por fecha
      a$Fecha <- as.character(a$Fecha)
      # #print(head(a[,4]))
      #print(str(a))
      lim1 <- which(a$Fecha==input$fecha1_ca)
      #  print(lim1)
      lim2 <- which(a$Fecha==input$fecha2_ca)
      # # print(lim2)
      a <- a[lim1:lim2,]
      
      #filtro por ATENCION
      a <- a[a[,1]==input$centro_atencion_ca,]
      
      #filtro por CUENTA ESPECIAL
      a <- a[a[,23]==input$cuentas_esp_ca,]
      
  
      return(a[,-c(22,23)])  
      
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
      
      a <- read.csv(paste0(getwd(),"/Datos/data_gc_ln2.txt"), sep="",encoding = "UTF-8")
      
      #filtro por fecha
      a$Fecha <- as.character(a$Fecha)
      # #print(head(a[,4]))
      #print(str(a))
      lim1 <- which(a$Fecha==input$fecha1_ca)
      #  print(lim1)
      lim2 <- which(a$Fecha==input$fecha2_ca)
      # # print(lim2)
      a <- a[lim1:lim2,]
      
      #filtro por ATENCION
      a <- a[a[,23]==input$centro_atencion_ca,]
      
      #filtro por CUENTA ESPECIAL
      a <- a[a[,22]==input$cuentas_esp_ca,]
      
      
      return(a[,-c(21,22,23)])  
      
    })#final isolate
    
  },rownames = FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#04B404', 'color': '#fff'});",
      "}")
  ))


#CENTRO DE ATENCION
#OPCIONES
output$gc_ca_opc <- renderUI({ 
  
  box(width=12,title="Centro de Atención",status="primary",solidHeader=TRUE ,
      column(width = 6,
             #box( width = 6, background = "navy",
             dateInput(inputId="gc_ca1", label="Desde:", language= "es",
                       width = "100%")#final dateimput
             #),#final box
      ),#final column
      #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box
      
      column(width = 6,
             #box( width = 6, background = "navy",
             dateInput(inputId="gc_ca2", label="Hasta:", language= "es",
                       width = "100%")#final dateimput
             #)#final box
      ),#final column
      #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box
      
      column(width = 6,
             #box( width = 6, background = "navy",
             selectInput("gc_ca3", "Centro de Atención:",
                         choices = c("Centro 1","Centro 2","Centro 3","Centro 4","Centro 5"))
             #)#final box
      ),
      column(width = 6,
             #box( width = 6, background = "navy",
             selectInput("gc_ca4", "Cuentas Especiales:",
                         choices = c("Cuenta 1","Cuenta 2","Cuenta 3","Cuenta 4","Cuenta 5"))
             #)#final box
      ),
      column(width = 6,
             #box( width = 6, background = "navy",
             actionButton("gc_ca_boton", "Consultar",
                          style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
      )
      
      
      #)#final fluidrow
      
      
      
  ) # final box
  
  
})


#tabla 1 CENTRO DE ATENCION

observeEvent(input$gc_ca_boton, {
  output$t1_gc_ca <-  renderUI(
    
    box(style="overflow-x:scroll",width = 12,title="Productores por Centros de Atención",status="primary",solidHeader=TRUE,
        dataTableOutput("tabla1_gc_ca"))
  )
})#final observeevent

#tabla 1  
output$tabla1_gc_ca <- renderDataTable(
  {
    #agrego dependencia 
    input$gc_ca_boton
    #
    isolate({ 
      
      a <- read.csv(paste0(getwd(),"/Datos/data_gc_ca1.txt"), sep="",encoding = "UTF-8")
      
       #filtro por fecha
       a$Fecha <- as.character(a$Fecha)
      # # #print(head(a[,4]))
      # #print(str(a))
       lim1 <- which(a$Fecha==input$gc_ca1)
       #  print(lim1)
       lim2 <- which(a$Fecha==input$gc_ca2)
      # # # print(lim2)
       a <- a[lim1:lim2,]
      # 
       #filtro por ATENCION
       a <- a[a[,1]==input$gc_ca3,]
       
       #filtro por CUENTA ESPECIAL
       a <- a[a[,23]==input$gc_ca4,]
      # 
      
      return(a[,-c(22,23)])  
      
    })#final isolate
    
  },rownames = FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#04B404', 'color': '#fff'});",
      "}")
  ))


#tabla 2 CENTRO DE ATENCION

observeEvent(input$gc_ca_boton, {
  output$t2_gc_ca <-  renderUI(
    
    box(style="overflow-x:scroll",width = 12,title="Productores por Centros de Atención",status="primary",solidHeader=TRUE,
        dataTableOutput("tabla2_gc_ca"))
  )
})#final observeevent

#tabla 1  
output$tabla2_gc_ca <- renderDataTable(
  {
    #agrego dependencia 
    input$gc_ca_boton
    #
    isolate({ 
      
      a <- read.csv(paste0(getwd(),"/Datos/data_gc_ca2.txt"), sep="",encoding = "UTF-8")
      
      #filtro por fecha
      a$Fecha <- as.character(a$Fecha)
      # # #print(head(a[,4]))
      # #print(str(a))
      lim1 <- which(a$Fecha==input$gc_ca1)
      #  print(lim1)
      lim2 <- which(a$Fecha==input$gc_ca2)
      # # # print(lim2)
      a <- a[lim1:lim2,]
      # 
      #filtro por ATENCION
      a <- a[a[,22]==input$gc_ca3,]
      
      #filtro por CUENTA ESPECIAL
      a <- a[a[,23]==input$gc_ca4,]
      # 
      
      return(a[,-c(21,22,23)])  

    })#final isolate
    
  },rownames = FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#04B404', 'color': '#fff'});",
      "}")
  ))




#PRODUCTORES
#OPCIONES BOTONES
output$gc_prod_dp_opc <- renderUI({ 
  box(width=12,title="Productores",status="primary",solidHeader=TRUE ,
      column(width = 6,
             #box( width = 6, background = "navy",
             dateInput(inputId="fecha1_prod", label="Desde:", language= "es",
                       width = "100%")#final dateimput
             #),#final box
      ),#final column
      #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box
      
      column(width = 6,
             #box( width = 6, background = "navy",
             dateInput(inputId="fecha2_prod", label="Hasta:", language= "es",
                       width = "100%")#final dateimput
             #)#final box
      ),#final column
      #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box
      
      column(width = 6,
             #box( width = 6, background = "navy",
             selectInput("centro_atencion_prod", "Centro de Atención:",
                         choices =c("Centro 1","Centro 2","Centro 3","Centro 4","Centro 5"))
             #)#final box
      ),column(width = 6,
               #box( width = 6, background = "navy",
               selectInput("productores_prod", "Productores:",
                           choices = c("Productor 1","Productor 2","Productor 3","Productor 4","Productor 5"))
               #)#final box
      ),
      column(width = 12,
             #box( width = 6, background = "navy",
             selectInput("cuentas_esp_prod", "Cuentas Especiales:",
                         choices = c("Cuenta 1","Cuenta 2","Cuenta 3","Cuenta 4","Cuenta 5"))
             #)#final box
      ),
      column(width = 6,
             #box( width = 6, background = "navy",
             actionButton("consultar_prod", "Consultar",
                          style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
      )
  ) #final box
  
})


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
      
      a <- read.csv(paste0(getwd(),"/Datos/data_gc_prod_dp1.txt"), sep="",encoding = "UTF-8")
      
      ##filtro por fecha
      a$Fecha <- as.character(a$Fecha)
      # # #print(head(a[,4]))
      # #print(str(a))
      lim1 <- which(a$Fecha==input$fecha1_prod)
      #  print(lim1)
      lim2 <- which(a$Fecha==input$fecha2_prod)
      # # # print(lim2)
      a <- a[lim1:lim2,]
      # 
      #filtro por ATENCION
      a <- a[a[,12]==input$centro_atencion_prod,]
      
      #filtro por CUENTA ESPECIAL
      a <- a[a[,10]==input$cuentas_esp_prod,]
      
      #filtro por productores 
      a <- a[a[,11]==input$productores_prod,]
      
      return(a[,-c(9,10,11,12)])  
      
      
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
      a <- read.csv(paste0(getwd(),"/Datos/data_gc_prod_dp3.txt"), sep="",encoding = "UTF-8")
      
      ##filtro por fecha
      a$Fecha <- as.character(a$Fecha)
      # # #print(head(a[,4]))
      # #print(str(a))
      lim1 <- which(a$Fecha==input$fecha1_prod)
      #  print(lim1)
      lim2 <- which(a$Fecha==input$fecha2_prod)
      # # # print(lim2)
      a <- a[lim1:lim2,]
      # 
      #filtro por ATENCION
      a <- a[a[,24]==input$centro_atencion_prod,]
      
      #filtro por CUENTA ESPECIAL
      a <- a[a[,22]==input$cuentas_esp_prod,]
      
      #filtro por productores 
      a <- a[a[,23]==input$productores_prod,]
      
      return(a[,-c(21,22,23,24)]) 
      
      
    })#final isolate
    
  },rownames = FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#04B404', 'color': '#fff'});",
      "}")
  ))

#PRODUCTORES 
#FICHA INTERMEDIARIO
#OPCIONES
output$gc_prod_fi_opc <- renderUI({ 
  box(width=12,title="Productores",status="primary",solidHeader=TRUE ,
      column(width = 6,
             #box( width = 6, background = "navy",
             selectInput("prod_fi_a", "Asesor: ",
                        # choices = c("20050-CARLOS ENRIQUE SILVERA","20051-JOSÉ CANALES","20052-EDUARDO CARDONA"))
                        choices = asesor())
                        
      ),#final column
      #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box
      
      column(width = 6,
             #box( width = 6, background = "navy",
             dateInput(inputId="prod_fi_fh", label="Fecha hasta:", language= "es",
                       width = "100%")#final dateimput
             #)#final box
      ),
      column(width = 4,
             #box( width = 6, background = "navy",
             actionButton("consultar_prod_fi1", "Ver Resumen",
                          style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
      ),
      column(width = 4,
             #box( width = 6, background = "navy",
             actionButton("consultar_prod_fi2", "Ver PDF",
                          style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
      )
      ,
      column(width = 4,
             #box( width = 6, background = "navy",
             actionButton("consultar_prod_fi3", "Cancelar",
                          style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
      )
  ) #final box
  
})

#FUNCION QUE ME LEE LOS ASESORES DISPONIBLES

asesor <- function(){
  a <- read.csv(paste0(getwd(),"/Datos/data_gc_prod_fi1.txt"), sep="",encoding = "UTF-8")
  
  return(levels(as.factor(as.character(a[,1]))))
}

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
      
      a <- read.csv(paste0(getwd(),"/Datos/data_gc_prod_fi1.txt"), sep="",encoding = "UTF-8")
      
      #filtro por fecha
      a$Fecha <- as.character(a$Fecha)

      lim2 <- which(a$Fecha==input$prod_fi_fh)
      # # # print(lim2)
      a <- a[1:lim2,]
      
      #filtro por asesor
      a <- a[a[,1]==input$prod_fi_a,]
      
      
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
      
      a <- read.csv(paste0(getwd(),"/Datos/data_gc_prod_fi2.txt"), sep="",encoding = "UTF-8")
      
      #filtro por fecha
      a$Fecha <- as.character(a$Fecha)
      
      lim2 <- which(a$Fecha==input$prod_fi_fh)
      # # # print(lim2)
      a <- a[1:lim2,]
      
      #filtro por asesor
      a <- a[a[,15]==input$prod_fi_a,]
      
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
      
      a <- read.csv(paste0(getwd(),"/Datos/data_gc_prod_fi3.txt"), sep="",encoding = "UTF-8")
      
      #filtro por fecha
      a$Fecha <- as.character(a$Fecha)
      
      lim2 <- which(a$Fecha==input$prod_fi_fh)
      # # # print(lim2)
      a <- a[1:lim2,]
      
      #filtro por asesor
      a <- a[a[,13]==input$prod_fi_a,]
      
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
      
      a <- read.csv(paste0(getwd(),"/Datos/data_gc_prod_fi4.txt"), sep="",encoding = "UTF-8")
      
      #filtro por fecha
      a$Fecha <- as.character(a$Fecha)
      
      lim2 <- which(a$Fecha==input$prod_fi_fh)
      # # # print(lim2)
      a <- a[1:lim2,]
      
      #filtro por asesor
      a <- a[a[,10]==input$prod_fi_a,]
      return(a)  
      
    })#final isolate
    
  },rownames = FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#04B404', 'color': '#fff'});",
      "}")
  ))


#DETALLE PÓLIZAS 
#BOTONES
output$gc_dp_opc <- renderUI({ 
  box(width=12,title="Detalle de Pólizas",status="primary",solidHeader=TRUE ,
      column(width = 6,
             #box( width = 6, background = "navy",
             dateInput(inputId="gc_dp_d", label="Desde:", language= "es",
                       width = "100%")#final dateimput
             #),#final box
      ),#final column
      #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box
      
      column(width = 6,
             #box( width = 6, background = "navy",
             dateInput(inputId="gc_dp_h", label="Hasta:", language= "es",
                       width = "100%")#final dateimput
             #)#final box
      ),#final column
      #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box
      
      column(width = 6,
             #box( width = 6, background = "navy",
             selectInput("gc_dp_ca", "Centro de Atención:",
                         choices = c("Centro 1","Centro 2","Centro 3","Centro 4","Centro 5"))
             #)#final box
      ),
      column(width = 6,
             #box( width = 6, background = "navy",
             selectInput("gc_dp_p", "Productores:",
                         choices = c("Productor 1","Productor 2","Productor 3","Productor 4","Productor 5"))
             #)#final box
      ),
      column(width = 12,
             #box( width = 6, background = "navy",
             selectInput("gc_dp_ce", "Cuentas Especiales:",
                         choices = c("Cuenta 1","Cuenta 2","Cuenta 3","Cuenta 4","Cuenta 5"))
             #)#final box
      ),
      column(width = 6,
             #box( width = 6, background = "navy",
             actionButton("gc_dp_consultar", "Consultar",
                          style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
      )
      
      
      #)#final fluidrow
      
      
      
  ) # final box
  
})



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
      
      a <- read.csv(paste0(getwd(),"/Datos/data_gc_dp1.txt"), sep="",encoding = "UTF-8")
      
      ##filtro por fecha
      a$Fecha <- as.character(a$Fecha)
      # # #print(head(a[,4]))
      # #print(str(a))
      lim1 <- which(a$Fecha==input$gc_dp_d)
      #  print(lim1)
      lim2 <- which(a$Fecha==input$gc_dp_h)
      # # # print(lim2)
      a <- a[lim1:lim2,]
      # 
      #filtro por ATENCION
      a <- a[a[,1]==input$gc_dp_ca,]
      
      #filtro por CUENTA ESPECIAL
      a <- a[a[,13]==input$gc_dp_ce,]
      
      #filtro por productores 
      a <- a[a[,12]==input$gc_dp_p,]
      
      return(a[,-c(11,12,13)]) 
      
    })#final isolate
    
  },rownames = FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#04B404', 'color': '#fff'});",
      "}")
  ))


#DETALLE SINIESTROS
#OPCIONES 
output$gc_dsin_opc <- renderUI({ 
  
  box(width=12,title="Detalle de Siniestros",status="primary",solidHeader=TRUE ,
      column(width = 6,
             #box( width = 6, background = "navy",
             dateInput(inputId="gc_ds_d", label="Desde:", language= "es",
                       width = "100%")#final dateimput
             #),#final box
      ),#final column
      #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box
      
      column(width = 6,
             #box( width = 6, background = "navy",
             dateInput(inputId="gc_ds_h", label="Hasta:", language= "es",
                       width = "100%")#final dateimput
             #)#final box
      ),#final column
      #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box
      
      column(width = 6,
             #box( width = 6, background = "navy",
             selectInput("gc_ds_ca", "Centro de Atención:",
                         choices = c("Centro 1","Centro 2","Centro 3","Centro 4","Centro 5"))
             #)#final box
      ),
      column(width = 6,
             #box( width = 6, background = "navy",
             selectInput("gc_ds_p", "Productores:",
                         choices = c("Productor 1","Productor 2","Productor 3","Productor 4","Productor 5"))
             #)#final box
      ),
      column(width = 12,
             #box( width = 6, background = "navy",
             selectInput("gc_ds_ce", "Cuentas Especiales:",
                         choices = c("Cuenta 1","Cuenta 2","Cuenta 3","Cuenta 4","Cuenta 5"))
             #)#final box
      ),
      column(width = 6,
             #box( width = 6, background = "navy",
             actionButton("gc_ds_consultar", "Consultar",
                          style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
      )
      
      
      #)#final fluidrow
      
      
      
  ) # final box
  
})


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
      a <- read.csv(paste0(getwd(),"/Datos/data_gc_dsin1.txt"), sep="",encoding = "UTF-8")
      
      ##filtro por fecha
      a$Fecha <- as.character(a$Fecha)
      # # #print(head(a[,4]))
      # #print(str(a))
      lim1 <- which(a$Fecha==input$gc_ds_d)
      #  print(lim1)
      lim2 <- which(a$Fecha==input$gc_ds_h)
      # # # print(lim2)
      a <- a[lim1:lim2,]
      # 
      #filtro por ATENCION
      a <- a[a[,1]==input$gc_ds_ca,]
      
      #filtro por CUENTA ESPECIAL
      a <- a[a[,13]==input$gc_ds_ce,]
      
      #filtro por productores 
      a <- a[a[,12]==input$gc_ds_p,]
      
      return(a[,-c(11,12,13)]) 

      
    })#final isolate
    
  },rownames = FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#04B404', 'color': '#fff'});",
      "}")
  ))

#DETALLE CONSOLIDADO
#OPCIONES 
output$gc_dcon_opc <- renderUI({ 
  
  box(width=12,title="Detalle Consolidado",status="primary",solidHeader=TRUE ,
      column(width = 6,
             #box( width = 6, background = "navy",
             dateInput(inputId="gc_dc_d", label="Desde:", language= "es",
                       width = "100%")#final dateimput
             #),#final box
      ),#final column
      #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box
      
      column(width = 6,
             #box( width = 6, background = "navy",
             dateInput(inputId="gc_dc_h", label="Hasta:", language= "es",
                       width = "100%")#final dateimput
             #)#final box
      ),#final column
      #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box
      
      column(width = 6,
             #box( width = 6, background = "navy",
             selectInput("gc_dc_ca", "Centro de Atención:",
                         choices = c("Centro 1","Centro 2","Centro 3","Centro 4","Centro 5"))
             #)#final box
      ),
      column(width = 6,
             #box( width = 6, background = "navy",
             selectInput("gc_dc_p", "Productores:",
                         choices = c("Productor 1","Productor 2","Productor 3","Productor 4","Productor 5"))
             #)#final box
      ),
      column(width = 12,
             #box( width = 6, background = "navy",
             selectInput("gc_dc_ce", "Cuentas Especiales:",
                         choices = c("Cuenta 1","Cuenta 2","Cuenta 3","Cuenta 4","Cuenta 5"))
             #)#final box
      ),
      column(width = 6,
             #box( width = 6, background = "navy",
             actionButton("gc_dc_consultar", "Consultar",
                          style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
      )
      
      
      #)#final fluidrow
      
      
      
  ) # final box
  
})

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
      a <- read.csv(paste0(getwd(),"/Datos/data_gc_dcon1.txt"), sep="",encoding = "UTF-8")
      
      ##filtro por fecha
      a$Fecha <- as.character(a$Fecha)
      # # #print(head(a[,4]))
      # #print(str(a))
      lim1 <- which(a$Fecha==input$gc_dc_d)
      #  print(lim1)
      lim2 <- which(a$Fecha==input$gc_dc_h)
      # # # print(lim2)
      a <- a[lim1:lim2,]
      # 
      #filtro por ATENCION
      a <- a[a[,1]==input$gc_dc_ca,]
      
      #filtro por CUENTA ESPECIAL
      a <- a[a[,18]==input$gc_dc_ce,]
      
      #filtro por productores 
      a <- a[a[,17]==input$gc_dc_p,]
      
      return(a[,-c(16,17,18)]) 

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
                         choices = c("Todas",gt_rca_b2()))
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
  b <- read.csv(paste0(getwd(),"/Datos/data_gt_rca1.txt"), sep="",encoding = "UTF-8")
  b[,1] <- as.factor(b[,1])
  return(levels(b[,1]))
})

#funcion que me extrae las opciones/nieveles de las cuentas especiales
gt_rca_b2 <- reactive({
  # b <- read.csv(paste0(getwd(),"/Datos/data_consolidado.txt"), sep="")
  # b[,3] <- as.factor(b[,3])
  # return(levels(b[,3]))
  b <- read.csv(paste0(getwd(),"/Datos/data_gt_rca1.txt"), sep="",encoding = "UTF-8")
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
      b <- read.csv(paste0(getwd(),"/Datos/data_gt_rca1.txt"), sep="",encoding = "UTF-8")
      b[,13] <- as.Date(b[,13])
      fechas <- range(b[,13])
      f1 <- paste(substr(fechas[1],9,10),substr(fechas[1],6,7),substr(fechas[1],1,4),sep = "/")
      f2 <- paste(substr(fechas[2],9,10),substr(fechas[2],6,7),substr(fechas[2],1,4),sep = "/")
      
      print(paste0("Las fechas disponibles se encuentran entre el ",f1," y el ",f2))
      
    })
    
    
  })
}) #final observeevent



#tabla 1
observeEvent(input$gt_ca_boton, {
  output$t1_gt_ca <-  renderUI(
    
    box(style="overflow-x:scroll",title = "Gestión Técnica Centro de Atención",width = 12,status="primary",solidHeader=TRUE,
        dataTableOutput("tabla1_gt_ca")
        )
  )
})#final observeEvent
  
#tabla 1
  
output$tabla1_gt_ca <- renderDataTable(
  {
    #agrego dependencia 
    input$gt_ca_boton
    
    isolate({ 
      
      # a <- as.data.frame(matrix(0,nrow = 6,ncol = 12))
      # names(a) <- c("Centro Atención","Pólizas Nuevas","Pólizas Renovadas",
      #               "Inventario Real","Inventario Presupuesto","Inventario Cumplimiento (%)",
      #               "Participación Primas Real (%)","Participación Primas Presupuesto (%)",
      #               "Primas Cobradas Netas de Devolución Real","Primas Cobradas Netas de Devolución Presupuesto",
      #               "Primas Cobradas Netas de Devolución Cumplimiento (%)",
      #               "Reservas de Primas al Inicio Real"
      # )
      
      #return(datatable(a, options = list(paging = FALSE)))
    b <- read.csv(paste0(getwd(),"/Datos/data_gt_rca1.txt"), sep="",encoding = "UTF-8")
    
    #veo filtros a aplicar
  
     b[,13] <- as.character(b[,13])
     lim1 <- which(b[,13]==input$gt_ca_1)
     lim2 <- which(b[,13]==input$gt_ca_2)
     b <- b[lim1:lim2,]
     
     
    #condicional para filtrar por centro de atencion
    # #b <- b[b[,2]==input$centro_atencion,]
     b <- b[b[,1]==input$gt_ca_3,]
     
    #condicional para filtrar por cuentas especiales
     if(input$gt_ca_4=="Todas"){
       b <- b[1:nrow(b),]
     }else{
       #b <- b[b[,3]==input$cuentas_esp,]
       b <- b[b[,14]==input$gt_ca_4,]
     }
     
    # return(b[,-c(38,39)])
      return(b[,-c(13,14)])  
      
    })#final isolate
    
  },rownames = FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#04B404', 'color': '#fff'});",
      "}")
  ))

#tabla 2
observeEvent(input$gt_ca_boton, {
output$t2_gt_ca <-  renderUI(
  
  box(style="overflow-x:scroll",title = "Gestión Técnica Centro de Atención",width = 12,status="primary",solidHeader=TRUE,
      dataTableOutput("tabla2_gt_ca"))
)
})#final observeEvent


#tabla 2
output$tabla2_gt_ca <- renderDataTable(
  {
    #agrego dependencia 
    input$gt_ca_boton
    #
    isolate({ 
    
    # a <- as.data.frame(matrix(0,nrow = 6,ncol = 13))
    # names(a) <- c("Centro Atención","(*) Siniestralidad Incurrida Real (%)","Siniestralidad Incurrida Presupuesto (%)",
    #               "(*) Siniestralidad Incurrida sin IBNR Real (%)","Siniestralidad Incurrida sin IBNR Presupuesto (%)",
    #               "(*) Siniestralidad Incurrida Rolling 12 Real (%)","Siniestralidad Incurrida Rolling 12 Presupuesto (%)",
    #               "Persistencia (%)","Persistencia Rolling 12 (%)",
    #               "Comisiones sobre Devengada Real (%)","Comisiones sobre Devengada Presupuesto (%)",
    #               "Comisiones sobre Cobrado Real (%)","Comisiones sobre Cobrado Presupuesto (%)"
    # )
    
    #return(datatable(a, options = list(paging = FALSE)))
    b <- read.csv(paste0(getwd(),"/Datos/data_gt_rca2.txt"), sep="",encoding = "UTF-8")
    
    #veo filtros a aplicar
    
     b[,14] <- as.character(b[,14])
     lim1 <- which(b[,14]==input$gt_ca_1)
     lim2 <- which(b[,14]==input$gt_ca_2)
     b <- b[lim1:lim2,]

    #condicional para filtrar por centro de atencion
     #b <- b[b[,2]==input$centro_atencion,]
    b <- b[b[,1]==input$gt_ca_3,]
     
    #condicional para filtrar por cuentas especiales
    if(input$gt_ca_4=="Todas"){
       b <- b[1:nrow(b),]
    }else{
       #b <- b[b[,3]==input$cuentas_esp,]
       b <- b[b[,15]==input$gt_ca_4,]
     }
    # 
    # return(b[,-c(38,39)])
    return(b[,-c(14,15)])  
    
     })#final isolate
    
  },rownames = FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#04B404', 'color': '#fff'});",
      "}")
  ))

#LÍNEA DE NEGOCIO

output$gt_rln_opc <- renderUI({ 
  fluidRow(

    box(width=12,title="Resumen Línea de Negocio",status="primary",solidHeader=TRUE ,
        column(width = 6,
               #box( width = 6, background = "navy",
               dateInput(inputId="gt_rln1", label="Desde:", language= "es",
                         width = "100%")#final dateimput
               #),#final box
        ),#final column
        #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box

        column(width = 6,
               #box( width = 6, background = "navy",
               dateInput(inputId="gt_rln2", label="Hasta:", language= "es",
                         width = "100%")#final dateimput
               #)#final box
        ),#final column
        #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box

        column(width = 6,
               #box( width = 6, background = "navy",
               selectInput("gt_rln3", "Línea de Negocio",
                           choices = c("Auto","Fianzas","Patrimoniales","Personas","Salud"))
               #)#final box
        ),
        column(width = 6,
               #box( width = 6, background = "navy",
               selectInput("gt_rln4", "Cuentas Especiales:",
                           choices = c("Todas","Cuenta 1","Cuenta 2","Cuenta 3"))
               #)#final box
        ),
        column(width = 6,
               #box( width = 6, background = "navy",
               actionButton("gt_rln_boton", "Consultar",
                            style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
        )


        )#final box



    ) # final fluidrow

  
  
  
})

#tabla 1
observeEvent(input$gt_rln_boton, {
output$t1_gt_ln <-  renderUI(
  
  box(style="overflow-x:scroll",width = 12,status="primary",solidHeader=TRUE,
      dataTableOutput("tabla1_gt_ln"))
)

})#final observeenvent

#tabla 1
output$tabla1_gt_ln <- renderDataTable(
  {
    #agrego dependencia 
    input$gt_rln_boton
    #
    isolate({ 
    a <- read.csv(paste0(getwd(),"/Datos/data_gt_rln1.txt"), sep="",encoding = "UTF-8")
    
    # a <- as.data.frame(matrix(0,nrow = 6,ncol = 12))
    # names(a) <- c("Centro Atención","Pólizas Nuevas","Pólizas Renovadas",
    #               "Inventario Real","Inventario Presupuesto","Inventario Cumplimiento (%)",
    #               "Participación Primas Real (%)","Participación Primas Presupuesto (%)",
    #               "Primas Cobradas Netas de Devolución Real","Primas Cobradas Netas de Devolución Presupuesto",
    #               "Primas Cobradas Netas de Devolución Cumplimiento (%)",
    #               "Reservas de Primas al Inicio Real"
    # )
    # 
    # #return(datatable(a, options = list(paging = FALSE)))
    # a[,1] <- c("Auto","Fianzas","Patrimoniales","Personas","Salud","Totales")
    #veo filtros a aplicar
    
    a[,13] <- as.character(a[,13])
    lim1 <- which(a[,13]==input$gt_rln1)
    lim2 <- which(a[,13]==input$gt_rln2)
    a <- a[lim1:lim2,]
    
    #condicional para filtrar por linea de negocio
    #b <- b[b[,2]==input$centro_atencion,]
    a <- a[a[,1]==input$gt_rln3,]
    
    #condicional para filtrar por cuentas especiales
    if(input$gt_rln4=="Todas"){
      a <- a[1:nrow(a),]
    }else{
      #b <- b[b[,3]==input$cuentas_esp,]
      a <- a[a[,14]==input$gt_rln4,]
    }
    # 
    # return(b[,-c(38,39)])
    return(a[,-c(13,14)])  
    #return(a)  
    
     })#final isolate
    
  },rownames = FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#04B404', 'color': '#fff'});",
      "}")
  ))

#tabla 2
observeEvent(input$gt_rln_boton, {
output$t2_gt_ln <-  renderUI(
  
  box(style="overflow-x:scroll",width = 12,status="primary",solidHeader=TRUE,
      dataTableOutput("tabla2_gt_ln"))
)
}) #final observeenvent

#tabla 2
output$tabla2_gt_ln <- renderDataTable(
  {
    #agrego dependencia 
    input$gt_rln_boton
    #
    isolate({ 
    a <- read.csv(paste0(getwd(),"/Datos/data_gt_rln2.txt"), sep="",encoding = "UTF-8")
    
    # a <- as.data.frame(matrix(0,nrow = 6,ncol = 13))
    # names(a) <- c("Centro Atención","(*) Siniestralidad Incurrida Real (%)","Siniestralidad Incurrida Presupuesto (%)",
    #               "(*) Siniestralidad Incurrida sin IBNR Real (%)","Siniestralidad Incurrida sin IBNR Presupuesto (%)",
    #               "(*) Siniestralidad Incurrida Rolling 12 Real (%)","Siniestralidad Incurrida Rolling 12 Presupuesto (%)",
    #               "Persistencia (%)","Persistencia Rolling 12 (%)",
    #               "Comisiones sobre Devengada Real (%)","Comisiones sobre Devengada Presupuesto (%)",
    #               "Comisiones sobre Cobrado Real (%)","Comisiones sobre Cobrado Presupuesto (%)"
    # )
    # 
    # #return(datatable(a, options = list(paging = FALSE)))
    # a[,1] <- c("Auto","Fianzas","Patrimoniales","Personas","Salud","Totales")
    a[,14] <- as.character(a[,14])
    lim1 <- which(a[,14]==input$gt_rln1)
    lim2 <- which(a[,14]==input$gt_rln2)
    a <- a[lim1:lim2,]
    
    #condicional para filtrar por linea de negocio
    #b <- b[b[,2]==input$centro_atencion,]
    a <- a[a[,1]==input$gt_rln3,]
    
    #condicional para filtrar por cuentas especiales
    if(input$gt_rln4=="Todas"){
      a <- a[1:nrow(a),]
    }else{
      #b <- b[b[,3]==input$cuentas_esp,]
      a <- a[a[,15]==input$gt_rln4,]
    }
    # 
    # return(b[,-c(38,39)])
    return(a[,-c(14,15)])  
    #return(a)  
    
    })#final isolate
    
  },rownames = FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#04B404', 'color': '#fff'});",
      "}")
  ))

#DETALLE POR LÍNEA DE NEGOCIO

output$gt_dln_opc <- renderUI({ 
  
  box(width=12,title="Información Detalle de Línea de Negocio",status="primary",solidHeader=TRUE ,
      column(width = 6,
             #box( width = 6, background = "navy",
             dateInput(inputId="gt_dln_1", label="Desde", language= "es",
                       width = "100%")#final dateimput

             #),#final box
      ),#final column
      #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box

      column(width = 6,
             #box( width = 6, background = "navy",
             dateInput(inputId="gt_dln_2", label="Hasta", language= "es",
                       width = "100%")#final dateimput
             #)#final box
      ),#final column
      #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box

      column(width = 6,
             #box( width = 6, background = "navy",
             selectInput("gt_dln_3", "Centro de Atención",
                         choices = c("Centro 1","Centro 2","Centro 3","Centro 4","Centro 5"))
             #)#final box
      ),
      column(width = 6,
             #box( width = 6, background = "navy",
             selectInput("gt_dln_4", "Categoría",
                         choices = c("AUTOMOVIL","AUTOMOVIL CASCO FLOTA","RESPONSABILIDAD CIVIL DE VEHICULOS"))
             #)#final box
      ),
      column(width = 12,
             #box( width = 6, background = "navy",
             selectInput("gt_dln_5", "Cuentas Especiales",
                         choices = c("Cuenta 1","Cuenta 2","Cuenta 3","Cuenta 4"))
             #)#final box
      ),

      column(width = 6,
             #box( width = 6, background = "navy",
             actionButton("gt_dln_boton", "Consultar",
                          style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
      )


      #)#final fluidrow



  ) # final box

   
  
  
  
  
})





#tabla 1
observeEvent(input$gt_dln_boton, {
output$t1_gt_dln <-  renderUI(
  
  box(style="overflow-x:scroll",width = 12,status="primary",solidHeader=TRUE,
      dataTableOutput("tabla1_gt_dln"))
)
}) #final ObserveEvent

#tabla 1
output$tabla1_gt_dln <- renderDataTable(
  {
    #agrego dependencia 
    input$gt_dln_boton
    #
    isolate({ 
      a <- read.csv(paste0(getwd(),"/Datos/data_gt_dln1.txt"), sep="",encoding = "UTF-8")
      
    # a <- as.data.frame(matrix(0,nrow = 4,ncol = 12))
    # names(a) <- c("Ramo","Pólizas Nuevas","Pólizas Renovadas","Cantidad de Asegurados",
    #               "Inventario Real","Inventario Presupuesto","Inventario Cumplimiento (%)",
    #               "Participación Primas Real (%)","Participación Primas Presupuesto (%)",
    #               "Primas Cobradas Netas de Devolución Nueva","Primas Cobradas Netas de Devolución Renovada",
    #               "Primas Cobradas Netas de Devolución Real"
    # )
    # 
    # #return(datatable(a, options = list(paging = FALSE)))
    # a[,1] <- c("AUTOMOVIL","AUTOMIVIL CASCO FLOTA","RESPONSABILIDAD CIVIL DE VEHICULOS","TOTAL")
      a[,13] <- as.character(a[,13])
      lim1 <- which(a[,13]==input$gt_dln_1)
      lim2 <- which(a[,13]==input$gt_dln_2)
      a <- a[lim1:lim2,]
      
      #filtro centro de atencion
      a <- a[a[,15]==input$gt_dln_3,]
      
      #condicional para filtrar por linea de negocio
      
      a <- a[a[,1]==input$gt_dln_4,]
      
      #condicional para filtrar por cuentas especiales
      if(input$gt_dln_5=="Todas"){
        a <- a[1:nrow(a),]
      }else{
        #b <- b[b[,3]==input$cuentas_esp,]
        a <- a[a[,14]==input$gt_dln_5,]
      }
      # 
      # return(b[,-c(38,39)])
      return(a[,-c(13,14,15)])  
   
    
    })#final isolate
    
  },rownames = FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#04B404', 'color': '#fff'});",
      "}")
  ))

#tabla 2
observeEvent(input$gt_dln_boton, {
output$t2_gt_dln <-  renderUI(
  
  box(style="overflow-x:scroll",width = 12,status="primary",solidHeader=TRUE,
      dataTableOutput("tabla2_gt_dln"))
)
}) #final ObserveEvent


#tabla 2
output$tabla2_gt_dln <- renderDataTable(
  {
    #agrego dependencia 
    input$gt_dln_boton
    #
    isolate({ 
      a <- read.csv(paste0(getwd(),"/Datos/data_gt_dln2.txt"), sep="",encoding = "UTF-8")
      
    # a <- as.data.frame(matrix(0,nrow = 4,ncol = 13))
    # names(a) <- c("Ramo","(*) Siniestralidad Incurrida Real (%)","Siniestralidad Incurrida Presupuesto (%)",
    #               "(*) Siniestralidad Incurrida sin IBNR Real (%)","Siniestralidad Incurrida sin IBNR Presupuesto (%)",
    #               "(*) Siniestralidad Incurrida Rolling 12 Real (%)","Siniestralidad Incurrida Rolling 12 Presupuesto (%)",
    #               "Persistencia (%)","Persistencia Rolling 12 (%)",
    #               "Comisiones sobre Devengada Real (%)","Comisiones sobre Devengada Presupuesto (%)",
    #               "Comisiones sobre Cobrado Real (%)","Comisiones sobre Cobrado Presupuesto (%)"
    # )
    # 
    # #return(datatable(a, options = list(paging = FALSE)))
    # a[,1] <- c("AUTOMOVIL","AUTOMIVIL CASCO FLOTA","RESPONSABILIDAD CIVIL DE VEHICULOS","TOTAL")
      a[,14] <- as.character(a[,14])
      lim1 <- which(a[,14]==input$gt_dln_1)
      lim2 <- which(a[,14]==input$gt_dln_2)
      a <- a[lim1:lim2,]
      
      #filtro centro de atencion
      a <- a[a[,16]==input$gt_dln_3,]
      
      #condicional para filtrar por linea de negocio
      
      a <- a[a[,1]==input$gt_dln_4,]
      
      #condicional para filtrar por cuentas especiales
      if(input$gt_dln_5=="Todas"){
        a <- a[1:nrow(a),]
      }else{
        #b <- b[b[,3]==input$cuentas_esp,]
        a <- a[a[,15]==input$gt_dln_5,]
      }
      # 
      # return(b[,-c(38,39)])
      return(a[,-c(14,15,16)])  
      
      #return(a)  
    
     })#final isolate
    
  },rownames = FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#04B404', 'color': '#fff'});",
      "}")
  ))


#GESTION TECNICA
#SINIESTRALIDAD PRINCIPALES CUENTAS

output$gt_sin_opc <- renderUI({ 
  
  box(width=12,title="Siniestralidad Principales Cuentas",status="primary",solidHeader=TRUE ,
      column(width = 12,
             #box( width = 6, background = "navy",
             selectInput("gt_sin_1", "Asesor",
                         choices = c("Asesor 1","Asesor 2","Asesor 3","Asesor 4","Asesor 5"))

             #),#final box
      ),#final column
      #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box

      column(width = 6,
             #box( width = 6, background = "navy",
             selectInput("gt_sin_2", "Ramo",
                         choices = c("Auto","Fianzas","Patrimoniales","Personas","Salud"))
             #)#final box
      ),#final column
      #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box

      column(width = 6,
             #box( width = 6, background = "navy",
             selectInput("gt_sin_3", "Número de Póliza",
                         choices = n_poliza())
             #)#final box
      ),
      column(width = 6,
             #box( width = 6, background = "navy",
             dateInput(inputId="gt_sin_4", label="Fecha Hasta", language= "es",
                       width = "100%")#final dateimput
             #)#final box
      ),
      column(width = 6,
             #box( width = 6, background = "navy",
             selectInput("gt_sin_5", "Contrato",
                         choices = con())
             #)#final box
      ),

      column(width = 6,
             #box( width = 6, background = "navy",
             actionButton("gt_sin_boton1", "Consultar",
                          style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
      ),

      column(width = 6,
             #box( width = 6, background = "navy",
             actionButton("gt_sin_boton2", "Cancelar",
                          style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
      )



      #)#final fluidrow



  ) #final box

})

#creo funcion para extraer nombre de polizas
n_poliza <- reactive({
  
  a <- read.csv(paste0(getwd(),"/Datos/data_gt_sinpc.txt"), sep="",encoding = "UTF-8")

  return(levels(as.factor(as.character(a$Número.de.Póliza))))
  
})

#creo funcion para extraer nombre de contratos
con <- reactive({
  
  a <- read.csv(paste0(getwd(),"/Datos/data_gt_sinpc.txt"), sep="",encoding = "UTF-8")
  
  return(levels(as.factor(as.character(a$Contrato))))
  
})



#tabla 
observeEvent(input$gt_sin_boton1, {
  output$t1_gt_sin <-  renderUI(
    
    box(style="overflow-x:scroll",width = 12,status="primary",solidHeader=TRUE,
        dataTableOutput("tabla2_gt_sin"))
  )
}) #final ObserveEvent


#tabla 
output$tabla2_gt_sin <- renderDataTable(
  {
    #agrego dependencia 
    input$gt_sin_boton1
    #
    isolate({ 
      a <- read.csv(paste0(getwd(),"/Datos/data_gt_sinpc.txt"), sep="",encoding = "UTF-8")
      
      #filtro por fecha
      #print(input$gt_sin_4)
      a$Fecha <- as.character(a$Fecha)
      # #print(head(a[,4]))
      #print(str(a))
      lim1 <- which(a$Fecha=="2019-01-01")
      #  print(lim1)
      lim2 <- which(a$Fecha==input$gt_sin_4)
      # # print(lim2)
      a <- a[lim1:lim2,]
      
      #filtro por asesor
      a <- a[a[,1]==input$gt_sin_1,]
       
      #filtro por linea de ramo
      a <- a[a[,2]==input$gt_sin_2,]
      
      #filtro por numero de poliza
      a <- a[a[,3]==input$gt_sin_3,]
      
      #filtro por contrato
      a <- a[a[,5]==input$gt_sin_5,]
      
     
       
      
      # #condicional para filtrar por cuentas especiales
      # if(input$gt_dln_5=="Todas"){
      #   a <- a[1:nrow(a),]
      # }else{
      #   #b <- b[b[,3]==input$cuentas_esp,]
      #   a <- a[a[,15]==input$gt_dln_5,]
      # }
      # # 
      # # return(b[,-c(38,39)])
      # return(a[,-c(14,15,16)])  
      
      return(a)  
      
    })#final isolate
    
  },rownames = FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#04B404', 'color': '#fff'});",
      "}")
  ))

##MAPA

states_filtered <- reactive({
  states %>%
    filter(
      period %in% input$period_filter,
      claim_type %in% input$type_filter) %>%
    group_by(state) %>%
    summarise(value = sum(avg_state))
})

init_states <- data_frame("abb" = character(0), "name" = character(0))
sel_states_val <- reactiveVal(value = init_states)

observeEvent(input$sel_state, {
  current_selection <- sel_states_val()
  updated_selection <- list(as_data_frame(input$sel_state[c("abb", "name")]), current_selection)
  sel_states_val(dplyr::bind_rows(updated_selection))
})

observeEvent(input$unsel_state, {
  updated_selection <- sel_states_val() %>%
    filter(abb != input$unsel_state$abb)
  sel_states_val(updated_selection)
})

# clear selected states after filter applied
observeEvent(states_filtered(), {
  sel_states_val(init_states)
})

output$states_map <- renderHighchart({
  hold_states <- states_filtered()
  
  state_select = JS("function(event) {
       Shiny.onInputChange('sel_state', { abb: event.target['hc-a2'], name: event.target.name, nonce: Math.random() });
    }")
  
  state_unselect = JS("function(event) {
      // Queue is defined in www/sender-queue.js
      queue.send('unsel_state', { abb: event.target['hc-a2'], nonce: Math.random()})
    }")
  
  highchart(type = "map") %>%
    hc_exporting(
      enabled = TRUE,
      buttons = tychobratools::hc_btn_options()
    ) %>%
    hc_add_series(
      mapData = mapdata, 
      data = list_parse(hold_states), 
      joinBy = c("hc-a2", "state"),
      allAreas = FALSE,
      dataLabels = list(enabled = TRUE, format = '{point.value:,.0f}'),
      name = "Spending by Claim",
      tooltip = list(
        valueDecimals = 0, 
        valuePrefix = "$"
      )
    ) %>% 
    hc_plotOptions(
      series = list(
        allowPointSelect = TRUE,
        states = list(
          select = list(
            color = "#32cd32"
          )
        ),
        point = list(
          events = list(
            unselect = state_unselect,
            select = state_select
          )
        )
      )        
    ) %>%
    hc_colorAxis(auxpar = NULL) %>%
    hc_title(text = "Madicare Spending by Claim") %>%
    hc_subtitle(text = "2015 Q4")
})

hospitals_filtered <- reactive({
  
  out <- hospitals %>%
    filter(
      period %in% input$period_filter,
      claim_type %in% input$type_filter) 
  
  
  # filter by selected state if a state is clicked on
  if (nrow(sel_states_val()) > 0) {
    out <- out %>%
      filter(state %in% sel_states_val()$abb)
  }
  
  out
})

hospitals_grouped <- reactive({
  hospitals_filtered() %>%
    select(-state) %>%
    group_by(hospital, provider_id) %>%
    summarise(
      value = sum(avg_hospital)
    ) %>%
    arrange(desc(value)) 
})

output$hospitals_tbl_title <- renderText({
  if (nrow(sel_states_val()) == 0) {
    out <- "Nationwide Hospitals"
  } else {
    state_names <- paste(sel_states_val()$name, collapse = ", ")
    out <- paste0(state_names, " Hospitals")
  }
  out
})

output$hospitals_tbl <- DT::renderDataTable({
  
  DT::datatable(
    hospitals_grouped(),
    selection = list(
      mode = 'single',
      selected = 1
    ),
    colnames = c(
      "Hospital",
      "Provider ID",
      "Total Average Spending (All categories)"
    ),
    rownames = FALSE,
    options = list(
      dom = 'tp'
    )
  ) %>%
    formatCurrency(
      column = 3,
      currency = "",
      digits = 0
    )
})

output$sel_state_name <- renderText({
  if (nrow(sel_states_val()) == 0) {
    "Nation"
  } else {
    paste0(sel_states_val()$name, collapse = ", ")
  }
})

state_provider_counts <- reactive({
  hospitals_filtered() %>%
    summarise(
      n_hospitals = n_distinct(hospital),
      n_providers = n_distinct(provider_id)
    ) 
})

state_provider_stats <- reactive({
  hospitals_grouped() %>%
    ungroup() %>%
    summarise(
      median_value = median(value),
      max_value = max(value),
      min_value = min(value)
    )
})

nation_provider_stats <- reactive({
  states_filtered() %>%
    ungroup() %>%
    summarise(
      median_value = median(value),
      max_value = max(value),
      min_value = min(value)
    )
})

output$state_locations <- DT::renderDataTable({
  counts <- state_provider_counts() %>%
    gather(key = "key", value = "value")
  
  out <- counts %>%
    mutate(key = c("# Hospitals", "# Providers"))
  
  col_headers <- htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(colspan = 2, 'Locations')
      )
    )
  ))
  
  datatable(
    out,
    rownames = FALSE,
    container = col_headers,
    options = list(
      ordering = FALSE,
      dom = 't'
    ),
    selection = "none"
  ) %>%
    formatCurrency(
      column = 2,
      currency = "",
      digits = 0
    )
})


output$state_meta_tbl <- DT::renderDataTable({
  
  state <- state_provider_stats() %>%
    gather(key = "key", value = "value")
  
  out <- state %>%
    mutate(key = c("Median Provider", 
                   "Most Expensive Provider",
                   "Cheapest Provider"
    ))
  col_headers <- htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(colspan = 2, 'Provider Statistics')
      )
    )
  ))
  
  datatable(
    out,
    rownames = FALSE,
    container = col_headers,
    options = list(
      ordering = FALSE,
      dom = 't'
    ),
    selection = "none"
  ) %>%
    formatCurrency(
      column = 2,
      currency = "",
      digits = 0
    )
})

output$single_hospital_tbl_title <- renderText({
  if (is.null(input$hospitals_tbl_rows_selected)) {
    out <- "Click Table to View Individual Hospital Details"
  } else {
    sel_provider <- hospitals_grouped()[input$hospitals_tbl_rows_selected, ]$provider_id
    out <- hospitals_filtered() %>%
      filter(provider_id == sel_provider) %>%
      pull(hospital)
  }
  out[1]
})

output$single_hospital_tbl <- renderDataTable({
  
  req(input$hospitals_tbl_rows_selected)
  sel_provider <- hospitals_grouped()[input$hospitals_tbl_rows_selected, ]$provider_id
  
  out <- hospitals_filtered() %>%
    filter(provider_id == sel_provider) %>%
    select(period, claim_type, avg_hospital, avg_nation) %>%
    arrange(desc(avg_hospital))
  
  
  col_headers <- htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, 'Hospital Visit Period'),
        th(rowspan = 2, 'Claim Type'),
        th(colspan = 2, 'Average Claim')
      ),
      tr(
        th('Per Hospital'),
        th('National')
      )
    )
  ))
  
  datatable(
    out,
    container = col_headers,
    rownames = FALSE,
    options = list(
      dom = "tp",
      ordering = FALSE
    ),
    selection = "none"
  ) %>%
    formatCurrency(
      columns = 3:4,
      digits = 0,
      currency = ""
    )
})






}) #final Shinyserver