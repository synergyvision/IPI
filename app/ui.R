shinyUI(
  
  # fluidPage(
  # # Add Javascript
  # tags$head(
  #   tags$link(rel="stylesheet", type="text/css",href="style.css"),
  #   tags$script(type="text/javascript", src = "md5.js"),
  #   tags$script(type="text/javascript", src = "passwdInputBinding.js")
  # ),
  # useShinyjs(),
  # 
  # titlePanel("Favor ingresar credenciales"),
  # 
  # uiOutput("app")
  # 
  # )

  dashboardPage(

    #//////////////#
    #/// HEADER ///#
    #//////////////#

    dashboardHeader(title = NULL, titleWidth = 188,#188,
              dropdownMenu(type = "messages",
                           messageItem(
                                      from = "Alerta",
                                      message = "Niveles de Riesgo Atípicos",
                                      icon = icon("exclamation-triangle"),
                                      time = "2018-05-12"
                                      )
                           #,#final messageitem

                          # messageItem(
                          #            from = "Señal",
                          #            message = "Volatilidad Anormal",
                          #            icon = icon("life-ring"),
                          #            time = "2018-05-12"
                          #             )#final messageitem
                          )#final dropdownmenu
                  ),#final dashboardheader
    #Sidebar
    dashboardSidebar(

      #tags$style(HTML(".main-sidebar{width: 250px;}")),

                #sidebarSearchForm(label = "Ingrese un Número", "searchText", "searchButton"),

                sidebarMenu(id = "tabs",

                menuItem("Consolidado", icon = icon("home"),tabName = "consolidado"

                          ),#fin menuitem

                            #menuItem("Comparativo", icon = icon("circle-o"), tabName = "comparativo"),
                menuItem("Gestión Técnica", icon = icon("wrench"),

                            #menuSubItem("Datos", tabName = "datos", icon = icon("circle-o")),

                            menuSubItem("Resumen Centro de Atención", tabName = "gt_ca",icon = icon(" ")),

                            menuSubItem("Resumen Línea de Negocio", tabName = "gt_ln",icon = icon(" ")),

                            menuSubItem("Detalle de Línea de Negocio", tabName = "gt_dln",icon = icon(" ")),

                            menuSubItem("Siniestralidad Ppal Cuentas", tabName = "gt_siniestro",icon = icon(" "))


                          ),#fin menuitem
                menuItem("Gestión Comercial", icon = icon("briefcase"),

                         menuSubItem("Línea de Negocio", tabName = "gc_ln", icon = icon(" ")),

                         menuSubItem("Centro de atención", tabName = "gc_ca", icon = icon(" ")),

                         menuItem("Productores", tabName = "gc_prod", icon = icon(" "),
                                     menuSubItem("Detalle Productor", tabName = "prod_dp", icon = icon(" ")),
                                     menuSubItem("Ficha Intermediario", tabName = "prod_fi", icon = icon(" "))
                                     ),

                         menuSubItem("Detalles de Pólizas", tabName = "gc_dp", icon = icon(" ")),

                         menuSubItem("Detalles de Siniestros", tabName = "gc_ds", icon = icon(" ")),

                         menuSubItem("Detalle de Consolidado", tabName = "gc_dc", icon = icon(" ")),

                         menuItem("Incentivos", tabName = "gc_inc", icon = icon("trophy"),
                                  menuSubItem("Concursos Mensuales", tabName = "inc_cm", icon = icon(" ")),
                                  menuSubItem("Concursos Generales", tabName = "inc_cg", icon = icon(" "))
                                  )


                ),#fin menuitem

                menuItem("Gestión Financiera", icon = icon("wallet"),tabName = "gf",badgeLabel = "Nuevo", badgeColor = "green"),

                menuItem("Indicadores", icon = icon("chart-line"),
                                   menuItem("Indicadores Macro", tabName = "ind_macro", icon = icon(" "),
                                            menuSubItem("Detalle Centro de Atención", tabName = "ind_macro_dca", icon = icon(" ")),
                                            menuSubItem("Resumen Línea de Negocio", tabName = "ind_macro_ln", icon = icon(" ")),
                                            menuSubItem("Detalle Línea de Negocio", tabName = "ind_macro_dln", icon = icon(" "))
                                            ),
                                   menuItem("Indicadores Micro", tabName = "ind_micro", icon = icon(" "),
                                            menuItem("Auto", tabName = "ind_micro_a", icon = icon(" "),
                                                     menuSubItem("Cartera", tabName = "ind_micro_a_c", icon = icon(" "))
                                                        ),
                                               
                                            menuItem("Salud", tabName = "ind_micro_s", icon = icon(" "),
                                                     menuSubItem("Cartera", tabName = "ind_micro_s_c", icon = icon(" ")),
                                                     menuSubItem("Siniestro", tabName = "ind_micro_s_s", icon = icon(" "))
                                                     )
                                               )
                          ),
                 menuItem("BBDD", icon = icon("database"),
                         menuSubItem("Cartera", tabName = "bbdd_c", icon = icon(" ")),
                         menuSubItem("Siniestros", tabName = "bbdd_s", icon = icon(" ")),
                         menuSubItem("Detalle por Intermediario", tabName = "bbdd_di", icon = icon(" ")),
                         menuSubItem("Cartera por Coberturas", tabName = "bbdd_cc", icon = icon(" ")),
                         menuSubItem("Pólizas SEUS", tabName = "bbdd_ps", icon = icon(" "))
                  ),
                menuItem("Proyección", tabName = "proy",icon = icon("forward"),badgeLabel = "Nuevo", badgeColor = "green"

                ),
                menuItem("Simulación", tabName = "sim",icon = icon("bullseye"),badgeLabel = "Nuevo", badgeColor = "green"

                ),
                menuItem("Carga de Información", icon = icon("upload"),tabName = "ci"),
                menuItem("Consultas en Línea", icon = icon("file"),tabName = "cl",
                         menuSubItem("Cobrado Diario", tabName = "cl_cd", icon = icon(" ")),
                         menuSubItem("Recibos Pendientes", tabName = "cl_rp", icon = icon(" "))
                ),
                menuItem("En Certificación", icon = icon("file"),tabName = "ec",
                         menuSubItem("Resultado Técnico por Cliente", tabName = "ec_rtc", icon = icon(" "))
                ),

                            menuItem("Acerca", icon = icon("exclamation-circle"), tabName = "acerca"))

                ), #final dashboardsidebar

    #////////////#
    #/// BODY ///#
    #////////////#

    dashboardBody(VisionHeader(), #tags$style(HTML(".main-sidebar{width: 250px;}")),
                  tags$style(HTML("
    .box.box-solid.box-primary>.box-header {
    color:#fff;
    background:#024A86;

                      }

    .box.box-solid.box-primary{
    border-bottom-color:#00FF00;
    border-left-color:#00FF00;
    border-right-color:#00FF00;
    border-top-color:#00FF00;
    }")),

            tabItems(

              #///////////////////#
              #/// CONSOLIDADO ///#
              #///////////////////#

              tabItem(tabName = "consolidado",
                      h2(" Información Consolidado"),


                      # box(width=12,title="Consolidado",status="primary",solidHeader=TRUE ,
                      #     column(width = 6,
                      #            #box( width = 6, background = "navy",
                      #                                    dateInput(inputId="fecha1", label="Desde:", language= "es",
                      #                                              width = "100%")#final dateimput
                      #     #),#final box
                      #     ),#final column
                      #     #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box
                      # 
                      #     column(width = 6,
                      #            #box( width = 6, background = "navy",
                      #                           dateInput(inputId="fecha2", label="Hasta:", language= "es",
                      #                                     width = "100%")#final dateimput
                      #     #)#final box
                      #     ),#final column
                      #     #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box
                      # 
                      #     column(width = 6,
                      #            #box( width = 6, background = "navy",
                      #            selectInput("centro_atencion", "Centro de Atención:",
                      #                        choices = c("UNIVERSAL DE SEGUROS, C.A","CENTRO 1","CENTRO 2","CENTRO 3"))
                      #            #)#final box
                      #     ),
                      #     column(width = 6,
                      #            #box( width = 6, background = "navy",
                      #            selectInput("cuentas_esp", "Cuentas Especiales:",
                      #                        choices = c("Todas","Cuenta 1","Cuenta 2","Cuenta 3"))
                      #            #)#final box
                      #     ),
                      #     column(width = 6,
                      #            #box( width = 6, background = "navy",
                      #            actionButton("consultar", "Consultar",
                      #                         style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
                      #     )
                      # 
                      # 
                      #     #)#final fluidrow
                      # 
                      # 
                      # 
                      # ), # final box

                      #Prueba nuevos imputs
                      htmlOutput("consolidado_opc"),
                      
                      #mensaje de disponibilidad de fechas
                      verbatimTextOutput('fechas_disp_cons'),
                      
                      #verbatimTextOutput('fecha_ini'),
                      #verbatimTextOutput('fecha_fin'),
                      #verbatimTextOutput('sucursal'),
                      #verbatimTextOutput('cuenta'),

                      #TABLA 1
                      fluidRow(
                     # uiOutput("t1"),

 

                      #TABLA 2
                      # box(style="overflow-x:scroll",width = 12,title="Prima Cobrada por Centros de Atención",status="primary",solidHeader=TRUE,
                      #     dataTableOutput("tabla2_con"))
                      
                      
                      #TABLA 3 - DATA DE PRUEBA
                      uiOutput("t3"),
                      
                      
                      uiOutput("t2")
                      
                      )
                      
                      


              ),

              #///////////////////////#
              #/// GESTIÓN TÉCNICA ///#
              #///////////////////////#

              #||||||||||||||||||||||||||||||||||#
              #||| RESUMEN CENTRO DE ATENCIÓN |||#
              #||||||||||||||||||||||||||||||||||#

              tabItem(tabName = "gt_ca",
                      h2(" Información Resumen Centro de Atención"),
                      
                      # box(width=12,title="Gestión Técnica Centro de Atención",status="primary",solidHeader=TRUE ,
                      #     column(width = 6,
                      #            #box( width = 6, background = "navy",
                      #            dateInput(inputId="gt_ca_1", label="Desde", language= "es",
                      #                      width = "100%")#final dateimput
                      #            
                      #            #),#final box
                      #     ),#final column
                      #     #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box
                      #     
                      #     column(width = 6,
                      #            #box( width = 6, background = "navy",
                      #            dateInput(inputId="gt_ca_2", label="Hasta", language= "es",
                      #                      width = "100%")#final dateimput
                      #            #)#final box
                      #     ),#final column
                      #     #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box
                      #     
                      #     column(width = 6,
                      #            #box( width = 6, background = "navy",
                      #            selectInput("gt_ca_3", "Área de Negocio",
                      #                        choices = c("Auto","Fianza","Patrinomiales","Personas",
                      #                                    "Salud"))
                      #            #)#final box
                      #     ),
                      #     column(width = 6,
                      #            #box( width = 6, background = "navy",
                      #            selectInput("gt_ca_4", "Cuentas Especiales",
                      #                        choices = c("Cuenta 1","Cuenta 2","Cuenta 3","Cuenta 4"))
                      #            #)#final box
                      #     ),
                      #     
                      #     column(width = 6,
                      #            #box( width = 6, background = "navy",
                      #            actionButton("gt_ca_boton", "Consultar",
                      #                         style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
                      #     )
                      #     
                      #     
                      #     #)#final fluidrow
                      #     
                      #     
                      #     
                      # ), # final box
                      
                      #nuevos imputs
                      htmlOutput("gt_rsa_opc"),
                      
                      #mensaje de disponibilidad de fechas
                      verbatimTextOutput('fechas_disp_gt_rca'),
                      
                      fluidRow(
                     
                      box(width=12,title="Gestión Técnica Centro de Atención",status="primary",solidHeader=TRUE ,
                          #TABLA 1
                       
                        uiOutput("t1_gt_ca"),
                        
                        #TABLA 2
                        uiOutput("t2_gt_ca")
                         
                        
                      ) #final box
                      
                      )#final fluidRow

              ),

              #||||||||||||||||||||||||||||||||#
              #||| RESUMEN LÍNEA DE NEGOCIO |||#
              #||||||||||||||||||||||||||||||||#

              tabItem(tabName = "gt_ln",
                      h2(" Información Resumen Línea de Negocio"),
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
                                   selectInput("gt_rln3", "Centro de Atención:",
                                               choices = c("UNIVERSAL DE SEGUROS, C.A","CENTRO 1","CENTRO 2","CENTRO 3"))
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
                            
                            
                            #)#final fluidrow
                            
                            
                            
                        ), # final box
                        
                        box(width=12,title="Resumen Gestión Técnica Línea de Negocio",status="primary",solidHeader=TRUE ,
                            #TABLA 1
                            
                            uiOutput("t1_gt_ln"),
                            
                            #TABLA 2
                            uiOutput("t2_gt_ln")
                            
                            
                        ) #final box
                        
                      )#final fluidRow

              ),

              #||||||||||||||||||||||||||||||||#
              #||| DETALLE LÍNEA DE NEGOCIO |||#
              #||||||||||||||||||||||||||||||||#

              tabItem(tabName = "gt_dln",
                      h2("Información Detalle de Línea de Negocio"),
                      
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
                                             choices = c("Universal de Seguros","Centro 2","Centro 3","Centro 4"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("gt_dln_4", "Área de Negocio",
                                             choices = c("Auto","Fianza","Patrinomiales","Personas",
                                                         "Salud"))
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
                          
                          
                          
                      ), # final box
                      
                      
                      
                      fluidRow(
                        
                        box(width=12,title="Detalle por Línea de Negocio",status="primary",solidHeader=TRUE ,
                            #TABLA 1
                            
                            uiOutput("t1_gt_dln"),
                            
                            #TABLA 2
                            uiOutput("t2_gt_dln")
                            
                            
                        ) #final box
                        
                      )#final fluidRow

              ),

              #||||||||||||||||||||||||||||||||||||||||||#
              #||| SINIESTRALIDAD PRINCIPALES CUENTAS |||#
              #||||||||||||||||||||||||||||||||||||||||||#

              tabItem(tabName = "gt_siniestro",
                      h2("Información Siniestralidad Principales Cuentas"),
                      
                      
                      box(width=12,title="Siniestralidad Principales Cuentas",status="primary",solidHeader=TRUE ,
                          column(width = 12,
                                 #box( width = 6, background = "navy",
                                 selectInput("gt_sin_1", "Asesor",
                                             choices = c("Asesor 1","Asesor 2","Asesor 3","Asesor 4"))
                                 
                                 #),#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("gt_sin_2", "Ramo",
                                             choices = c("Ramo 1","Ramo 2","Ramo 3","Ramo 4"))
                                 #)#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("gt_sin_3", "Número de Póliza",
                                             choices = c("Póliza 1","Póliza 2","Póliza 3","Póliza 4"))
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
                                             choices = c("Contrato 1","Contrato 2","Contrato 3","Contrato 4"))
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

              ),

              #/////////////////////////#
              #/// GESTIÓN COMERCIAL ///#
              #/////////////////////////#

              #|||||||||||||||||||||||||#
              #|||  LÍNEA DE NEGOCIO |||#
              #|||||||||||||||||||||||||#

              tabItem(tabName = "gc_ln",
                      h2("Información Línea de Negocio"),
                      
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
                                             choices = c("UNIVERSAL DE SEGUROS, C.A","CENTRO 1","CENTRO 2","CENTRO 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("cuentas_esp_ca", "Cuentas Especiales:",
                                             choices = c("Todas","Cuenta 1","Cuenta 2","Cuenta 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 actionButton("consultar_ca", "Consultar",
                                              style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
                          )
                      ) #final box
                          ,
                          #TABLA 1
                          fluidRow(
                            uiOutput("t1_ca")
                            ),
                      
                      #TABLA 2
                      fluidRow(
                        uiOutput("t2_ca")
                      )
                            
                          
                          
                          #)#final fluidrow
                          
                          
                          
                      

              ),

              #||||||||||||||||||||||||||#
              #||| CENTRO DE ATENCIÓN |||#
              #||||||||||||||||||||||||||#

              tabItem(tabName = "gc_ca",
                      h2("Información Centro de Atención"),
                      
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
                                             choices = c("UNIVERSAL DE SEGUROS, C.A","CENTRO 1","CENTRO 2","CENTRO 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("gc_ca4", "Cuentas Especiales:",
                                             choices = c("Todas","Cuenta 1","Cuenta 2","Cuenta 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 actionButton("gc_ca_boton", "Consultar",
                                              style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
                          )
                          
                          
                          #)#final fluidrow
                          
                          
                          
                      ) # final box
                      
       

              ),

              #|||||||||||||||||||||||||#
              #||| DETALLE PRODUCTOR |||#
              #|||||||||||||||||||||||||#

              tabItem(tabName = "prod_dp",
                      h2("Información detalle productor"),
                      
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
                                             choices = c("UNIVERSAL DE SEGUROS, C.A","CENTRO 1","CENTRO 2","CENTRO 3"))
                                 #)#final box
                          ),column(width = 6,
                                   #box( width = 6, background = "navy",
                                   selectInput("productores_prod", "Productores:",
                                               choices = c("Productor 1","Productor 2","Productor 3","Productor 4"))
                                   #)#final box
                          ),
                          column(width = 12,
                                 #box( width = 6, background = "navy",
                                 selectInput("cuentas_esp_prod", "Cuentas Especiales:",
                                             choices = c("Todas","Cuenta 1","Cuenta 2","Cuenta 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 actionButton("consultar_prod", "Consultar",
                                              style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
                          )
                      ) #final box
                      ,
                      #TABLA 1
                      fluidRow(
                        uiOutput("t1_prod")
                      ),
                      
                      #TABLA 2
                      fluidRow(
                        uiOutput("t2_prod")
                      ),
                      
                      #TABLA 3
                      fluidRow(
                        uiOutput("t3_prod")
                      )
                      
                      
                      

              ),

              #|||||||||||||||||||||||||||#
              #||| FiCHA INTERMEDIARIO |||#
              #|||||||||||||||||||||||||||#

              tabItem(tabName = "prod_fi",
                      h2("Información ficha intermediario"),
                      
                      box(width=12,title="Productores",status="primary",solidHeader=TRUE ,
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("prod_fi_a", "Asesor: ",
                                             choices = c("20050-CARLOS ENRIQUE SILVERA","20051-JOSÉ CANALES","20052-EDUARDO CARDONA"))
                                 
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
                          ),
                          column(width = 4,
                                 #box( width = 6, background = "navy",
                                 actionButton("consultar_prod_fi3", "Cancelar",
                                              style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
                          )
                      ), #final box
                      
                      #TABLA 1
                      fluidRow(
                        uiOutput("t1_prod_fi")
                      ),
                      
                      #TABLA 2
                      fluidRow(
                        uiOutput("t2_prod_fi")
                      ),
                      
                      #TABLA 3
                      fluidRow(
                        uiOutput("t3_prod_fi")
                      ),
                      
                      #TABLA 4
                      fluidRow(
                        uiOutput("t4_prod_fi")
                      )
                      
                      
                      
              ),

              #||||||||||||||||||||||#
              #||| DETALLE PÓLIZA |||#
              #||||||||||||||||||||||#

              tabItem(tabName = "gc_dp",
                      h2("Información detalle póliza"),
                      
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
                                             choices = c("UNIVERSAL DE SEGUROS, C.A","CENTRO 1","CENTRO 2","CENTRO 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                   #box( width = 6, background = "navy",
                                   selectInput("gc_dp_p", "Productores:",
                                               choices = c("Productor 1","Productor 2","Productor 3","Productor 4"))
                                   #)#final box
                          ),
                          column(width = 12,
                                 #box( width = 6, background = "navy",
                                 selectInput("gc_dp_ce", "Cuentas Especiales:",
                                             choices = c("Todas","Cuenta 1","Cuenta 2","Cuenta 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 actionButton("gc_dp_consultar", "Consultar",
                                              style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
                          )
                          
                          
                          #)#final fluidrow
                          
                          
                          
                      ), # final box
                      
                      #TABLA 1
                      fluidRow(
                        uiOutput("t1_gc_dp")
                      )
                      
                      

              ),

              #||||||||||||||||||||||||||#
              #||| DETALLE SINIESTROS |||#
              #||||||||||||||||||||||||||#

              tabItem(tabName = "gc_ds",
                      h2("Información detalle siniestros"),
                      
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
                                             choices = c("UNIVERSAL DE SEGUROS, C.A","CENTRO 1","CENTRO 2","CENTRO 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("gc_ds_p", "Productores:",
                                             choices = c("Productor 1","Productor 2","Productor 3","Productor 4"))
                                 #)#final box
                          ),
                          column(width = 12,
                                 #box( width = 6, background = "navy",
                                 selectInput("gc_ds_ce", "Cuentas Especiales:",
                                             choices = c("Todas","Cuenta 1","Cuenta 2","Cuenta 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 actionButton("gc_ds_consultar", "Consultar",
                                              style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
                          )
                          
                          
                          #)#final fluidrow
                          
                          
                          
                      ), # final box
                      
                      #TABLA 1
                      fluidRow(
                        uiOutput("t1_gc_ds")
                      )

              ),

              #|||||||||||||||||||||||||||#
              #||| DETALLE CONSOLIDADO |||#
              #|||||||||||||||||||||||||||#

              tabItem(tabName = "gc_dc",
                      h2("Información detalle consolidado"),
                      
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
                                             choices = c("UNIVERSAL DE SEGUROS, C.A","CENTRO 1","CENTRO 2","CENTRO 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("gc_dc_p", "Productores:",
                                             choices = c("Productor 1","Productor 2","Productor 3","Productor 4"))
                                 #)#final box
                          ),
                          column(width = 12,
                                 #box( width = 6, background = "navy",
                                 selectInput("gc_dc_ce", "Cuentas Especiales:",
                                             choices = c("Todas","Cuenta 1","Cuenta 2","Cuenta 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 actionButton("gc_dc_consultar", "Consultar",
                                              style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
                          )
                          
                          
                          #)#final fluidrow
                          
                          
                          
                      ), # final box
                      
                      #TABLA 1
                      fluidRow(
                        uiOutput("t1_gc_dc")
                      )
                      

              ),

              #|||||||||||||||||||||||||||#
              #||| CONCURSOS MENSUALES |||#
              #|||||||||||||||||||||||||||#

              tabItem(tabName = "inc_cm",
                      h2("Información concursos mensuales"),
                      
                      box(width=12,title="Concursos Mensuales",status="primary",solidHeader=TRUE ,
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("gc_cm_tc", "Tipo de Concurso:",
                                             choices = c("Consurso 1","Consurso 2","Consurso 3","Consurso 4"))
                                 
                                 #),#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("gc_cm_s", "Sucursal:",
                                             choices = c("UNIVERSAL DE SEGUROS, C.A","CENTRO 1","CENTRO 2","CENTRO 3"))
                                 
                                 #)#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box
                          
                          column(width = 12,
                                 #box( width = 6, background = "navy",
                                 selectInput("gc_cm_a", "Asesor:",
                                             choices = c("Asesor 1","Asesor 2","Asesor 3","Asesor 4"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 actionButton("gc_cm_consultar", "Consultar",
                                              style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
                          )
                          
                          
                          #)#final fluidrow
                          
                          
                          
                      ) # final box

              ),

              #|||||||||||||||||||||||||||#
              #||| CONCURSOS GENERALES |||#
              #|||||||||||||||||||||||||||#

              tabItem(tabName = "inc_cg",
                      h2("Información concursos generales"),
                      
                      box(width=12,title="Período Concurso: Desde - Hasta",status="primary",solidHeader=TRUE ,
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("gc_inc_cg_1", "Tipo Concursos",
                                             choices = c("Concurso 1","Concurso 2","Concurso 3","Concurso 4"))
                                 
                                 #),#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("gc_inc_cg_2", "Concurso",
                                             choices = c("Concurso 1","Concurso 2","Concurso 3","Concurso 4"))
                                 #)#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("gc_inc_cg_3", "Sucursal",
                                             choices = c("Sucursal 1","Sucursal 2","Sucursal 3","Sucursal 4"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("gc_inc_cg_4", "Asesor",
                                             choices = c("Asesor 1","Asesor 2","Asesor 3","Asesor 4"))
                                 #)#final box
                          ),
                          column(width = 12,
                                 #box( width = 6, background = "navy",
                                 selectInput("gc_inc_cg_5", "Observaciones",
                                             choices = c("Observación 1","Observación 2","Observación 3","Observación 4"))
                                 #)#final box
                          ),
                          
                      
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 actionButton("gc_inc_cg_boton1", "Consultar",
                                              style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 actionButton("gc_inc_cg_boton2", "Cancelar",
                                              style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
                          )
                          
                          
                          #)#final fluidrow
                          
                          
                          
                      ) # final box

              ),

              #//////////////////////////#
              #/// GESTIÓN FINANCIERA ///#
              #//////////////////////////#

              tabItem(tabName = "gf",
                      h2("Información Gestión Financiera"),
                      
                      box(width=12,title="Gestión Financiera",status="primary",solidHeader=TRUE ,
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="gf1", label="Desde:", language= "es",
                                           width = "100%")#final dateimput
                                 #),#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="gf2", label="Hasta:", language= "es",
                                           width = "100%")#final dateimput
                                 #)#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("gf3", "Centro de Atención:",
                                             choices = c("UNIVERSAL DE SEGUROS, C.A","CENTRO 1","CENTRO 2","CENTRO 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("gf4", "Cuentas Especiales:",
                                             choices = c("Todas","Cuenta 1","Cuenta 2","Cuenta 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 actionButton("gf1_boton", "Consultar",
                                              style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
                          )
                          
                          
                          #)#final fluidrow
                          
                          
                          
                      ) # final box

              ),

              #////////////////////#
              #///  INDICADORES ///#
              #////////////////////#
              
              #//////////////////////////#
              #///  INDICADORES MACRO ///#
              #//////////////////////////#

              tabItem(tabName = "ind_macro",
                      h2("Información indicadores macro")

              ),
              
              #|||||||||||||||||||||||||||||||||||#
              #|||  DETALLE CENTRO DE ATENCION |||#
              #|||||||||||||||||||||||||||||||||||#
              
              tabItem(tabName = "ind_macro_dca",
                      h2("Detalle Centro de Atención"),
                      
                      box(width=12,title="Detalle Centro de Atención",status="primary",solidHeader=TRUE ,
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="ind_macro_dca1", label="Desde:", language= "es",
                                           width = "100%")#final dateimput
                                 #),#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="ind_macro_dca2", label="Hasta:", language= "es",
                                           width = "100%")#final dateimput
                                 #)#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("ind_macro_dca3", "Centro de Atención:",
                                             choices = c("UNIVERSAL DE SEGUROS, C.A","CENTRO 1","CENTRO 2","CENTRO 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("ind_macro_dca4", "Cuentas Especiales:",
                                             choices = c("Todas","Cuenta 1","Cuenta 2","Cuenta 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 actionButton("ind_macro_dca_boton", "Consultar",
                                              style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
                          )
                          
                          
                          #)#final fluidrow
                          
                          
                          
                      ) # final box
                      
              ),
              
              #|||||||||||||||||||||||||||||||||#
              #|||  RESUMEN LÍNEA DE NEGOCIO |||#
              #|||||||||||||||||||||||||||||||||#
              
              tabItem(tabName = "ind_macro_ln",
                      h2("Resumen Línea de Negocio"),
                      
                      box(width=12,title="Resumen Línea de Negocio",status="primary",solidHeader=TRUE ,
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="ind_macro_rln1", label="Desde:", language= "es",
                                           width = "100%")#final dateimput
                                 #),#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="ind_macro_rln2", label="Hasta:", language= "es",
                                           width = "100%")#final dateimput
                                 #)#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("ind_macro_rln3", "Centro de Atención:",
                                             choices = c("UNIVERSAL DE SEGUROS, C.A","CENTRO 1","CENTRO 2","CENTRO 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("ind_macro_rln4", "Cuentas Especiales:",
                                             choices = c("Todas","Cuenta 1","Cuenta 2","Cuenta 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 actionButton("ind_macro_rln_boton", "Consultar",
                                              style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
                          )
                          
                          
                          #)#final fluidrow
                          
                          
                          
                      ) # final box
                      
              ),
              
              #|||||||||||||||||||||||||||||||||#
              #|||  DETALLE LÍNEA DE NEGOCIO |||#
              #|||||||||||||||||||||||||||||||||#
              
              tabItem(tabName = "ind_macro_dln",
                      h2("Detalle Línea de Negocio"),
                      
                      box(width=12,title="Detalle Línea de Negocio",status="primary",solidHeader=TRUE ,
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="ind_macro_dln1", label="Desde:", language= "es",
                                           width = "100%")#final dateimput
                                 #),#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="ind_macro_dln2", label="Hasta:", language= "es",
                                           width = "100%")#final dateimput
                                 #)#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("ind_macro_dln3", "Centro de Atención:",
                                             choices = c("UNIVERSAL DE SEGUROS, C.A","CENTRO 1","CENTRO 2","CENTRO 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("ind_macro_dln4", "Cuentas Especiales:",
                                             choices = c("Todas","Cuenta 1","Cuenta 2","Cuenta 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 actionButton("ind_macro_dln_boton", "Consultar",
                                              style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
                          )
                          
                          
                          #)#final fluidrow
                          
                          
                          
                      ) # final box
                      
              ),

              #//////////////////////////#
              #///  INDICADORES MICRO ///#
              #//////////////////////////#
              
              tabItem(tabName = "ind_micro",
                      h2("Información indicadores micro")

              ),
              
              #/////////////#
              #///  AUTO ///#
              #/////////////#
              
              #||||||||||||||||#
              #|||  CARTERA |||#
              #||||||||||||||||#
              
              tabItem(tabName = "ind_micro_a_c",
                      h2("Información Auto-Cartera"),
                      
                      box(width=12,title="Indicadores",status="primary",solidHeader=TRUE ,
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="ind_mi_a_c1", label="Desde:", language= "es",
                                           width = "100%")#final dateimput
                                 
                                 #),#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="ind_mi_a_c2", label="Hasta:", language= "es",
                                           width = "100%")#final dateimput
                                 #)#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("ind_mi_a_c3", "Centro de Atención:",
                                             choices = c("Centro 1","Centro 2","Centro 3","Centro 4"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("ind_mi_a_c4", "Área de Negocio:",
                                             choices = c("Auto","Fianza","Patrinomiales","Personas",
                                                         "Salud"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("ind_mi_a_c5", "Tipo de Póliza:",
                                             choices = c("Póliza 1","Póliza 2","Póliza 3","Póliza 4"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("ind_mi_a_c6", "Pólizas Nuevas:",
                                             choices = c("Póliza 1","Póliza 2","Póliza 3","Póliza 4"))
                                 #)#final box
                          ),
                          column(width = 12,
                                 #box( width = 6, background = "navy",
                                 selectInput("ind_mi_a_c7", "Plan:",
                                             choices = c("Plan 1","Plan 2","Plan 3","Plan 4"))
                                 #)#final box
                          ),
                        
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 actionButton("ind_mi_a_c_boton", "Consultar",
                                              style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
                          )
                          
                          
                          #)#final fluidrow
                          
                          
                          
                      ) # final box
                      
                      
                      
                      
                      
                      
              ),
              
              #//////////////#
              #///  SALUD ///#
              #//////////////#

              #||||||||||||||||#
              #|||  CARTERA |||#
              #||||||||||||||||#
              
              tabItem(tabName = "ind_micro_s_c",
                      h2("Información Salud-Cartera"),
                      
                      box(width=12,title="Información Salud-Cartera",status="primary",solidHeader=TRUE ,
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="ind_micro_s_c1", label="Desde:", language= "es",
                                           width = "100%")#final dateimput
                                 #),#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="ind_micro_s_c2", label="Hasta:", language= "es",
                                           width = "100%")#final dateimput
                                 #)#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("ind_micro_s_c3", "Centro de Atención:",
                                             choices = c("UNIVERSAL DE SEGUROS, C.A","CENTRO 1","CENTRO 2","CENTRO 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("ind_micro_s_c4", "Cuentas Especiales:",
                                             choices = c("Todas","Cuenta 1","Cuenta 2","Cuenta 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 actionButton("ind_micro_s_c_boton", "Consultar",
                                              style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
                          )
                          
                          
                          #)#final fluidrow
                          
                          
                          
                      ) # final box
                      
              ),
              
              #||||||||||||||||||#
              #|||  SINIESTRO |||#
              #||||||||||||||||||#
              
              tabItem(tabName = "ind_micro_s_s",
                      h2("Información Salud-Siniestro"),
                      
                      
                      box(width=12,title="Indicadores Micro Siniestros",status="primary",solidHeader=TRUE ,
                          column(width = 4,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="ind_m_sin_1", label="Desde", language= "es",
                                           width = "100%")#final dateimput
                                 #),#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box
                          
                          column(width = 4,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="ind_m_sin_2", label="Hasta", language= "es",
                                           width = "100%")#final dateimput
                                 #)#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box
                          
                          column(width = 4,
                                 #box( width = 6, background = "navy",
                                 selectInput("ind_m_sin_3", "Tipo de Estudio",
                                             choices = c("Estudio 1","Estudio 2","Estudio 3","Estudio 4"))
                                 #)#final box
                          ),
                          column(width = 4,
                                 #box( width = 6, background = "navy",
                                 selectInput("ind_m_sin_4", "Entidad",
                                             choices = c("Entidad 1","Entidad 2","Entidad 3","Entidad 4"))
                                 #)#final box
                          ),
                          column(width = 4,
                                 #box( width = 6, background = "navy",
                                 selectInput("ind_m_sin_5", "Ramo",
                                             choices = c("Ramo 1","Ramo 2","Ramo 3","Ramo 4"))
                                 #)#final box
                          ),
                          column(width = 4,
                                 #box( width = 6, background = "navy",
                                 selectInput("ind_m_sin_6", "Póliza",
                                             choices = c("Póliza 1","Póliza 2","Póliza 3","Póliza 4"))
                                 #)#final box
                          ),
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 actionButton("ind_m_sin_boton", "Consultar",
                                              style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
                          )
                          
                          
                          
                          #)#final fluidrow
                          
                          
                          
                      ) #final box
                      
              ),

              #/////////////#
              #///  BBDD ///#
              #/////////////#

              #||||||||||||||||||||#
              #||| BBDD CARTERA |||#
              #||||||||||||||||||||#

              tabItem(tabName = "bbdd_c",
                      h2("Información BBDD Cartera"),
                      
                      box(width=12,title="Información BBDD Cartera",status="primary",solidHeader=TRUE ,
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="bbdd_c1", label="Desde:", language= "es",
                                           width = "100%")#final dateimput
                                 #),#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="bbdd_c2", label="Hasta:", language= "es",
                                           width = "100%")#final dateimput
                                 #)#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("bbdd_c3", "Centro de Atención:",
                                             choices = c("UNIVERSAL DE SEGUROS, C.A","CENTRO 1","CENTRO 2","CENTRO 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("bbdd_c4", "Cuentas Especiales:",
                                             choices = c("Todas","Cuenta 1","Cuenta 2","Cuenta 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 actionButton("bbdd_c_boton", "Consultar",
                                              style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
                          )
                          
                          
                          #)#final fluidrow
                          
                          
                          
                      ) # final box

              ),

              #|||||||||||||||||||||||#
              #||| BBDD SINIESTROS |||#
              #|||||||||||||||||||||||#

              tabItem(tabName = "bbdd_s",
                      h2("Información BBDD Siniestros"),
                      
                      box(width=12,title="Información BBDD Siniestros",status="primary",solidHeader=TRUE ,
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="bbdd_s1", label="Desde:", language= "es",
                                           width = "100%")#final dateimput
                                 #),#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="bbdd_s2", label="Hasta:", language= "es",
                                           width = "100%")#final dateimput
                                 #)#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("bbdd_s3", "Centro de Atención:",
                                             choices = c("UNIVERSAL DE SEGUROS, C.A","CENTRO 1","CENTRO 2","CENTRO 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("bbdd_s4", "Cuentas Especiales:",
                                             choices = c("Todas","Cuenta 1","Cuenta 2","Cuenta 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 actionButton("bbdd_s_boton", "Consultar",
                                              style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
                          )
                          
                          
                          #)#final fluidrow
                          
                          
                          
                      ) # final box

              ),

              #|||||||||||||||||||||||||||||||||||||||#
              #||| BBDD DETALLE POR INTERMEDIARIOS |||#
              #|||||||||||||||||||||||||||||||||||||||#
              
              tabItem(tabName = "bbdd_di",
                      h2("Información BBDD Detalles por Intermediarios"),
                      
                      box(width=12,title="Información BBDD Detalles por Intermediarios",status="primary",solidHeader=TRUE ,
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="bbdd_di1", label="Desde:", language= "es",
                                           width = "100%")#final dateimput
                                 #),#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="bbdd_di2", label="Hasta:", language= "es",
                                           width = "100%")#final dateimput
                                 #)#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("bbdd_di3", "Centro de Atención:",
                                             choices = c("UNIVERSAL DE SEGUROS, C.A","CENTRO 1","CENTRO 2","CENTRO 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("bbdd_di4", "Cuentas Especiales:",
                                             choices = c("Todas","Cuenta 1","Cuenta 2","Cuenta 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 actionButton("bbdd_di_boton", "Consultar",
                                              style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
                          )
                          
                          
                          #)#final fluidrow
                          
                          
                          
                      ) # final box
                      
              ),
              
              #|||||||||||||||||||||||||||||||||||#
              #||| BBDD CARTERA POR COBERTURAS |||#
              #|||||||||||||||||||||||||||||||||||#
              
              tabItem(tabName = "bbdd_cc",
                      h2("Información BBDD Cartera por Coberturas"),
                      
                      box(width=12,title="Información BBDD Cartera por Coberturas",status="primary",solidHeader=TRUE ,
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="bbdd_cc1", label="Desde:", language= "es",
                                           width = "100%")#final dateimput
                                 #),#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="bbdd_cc2", label="Hasta:", language= "es",
                                           width = "100%")#final dateimput
                                 #)#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("bbdd_cc3", "Centro de Atención:",
                                             choices = c("UNIVERSAL DE SEGUROS, C.A","CENTRO 1","CENTRO 2","CENTRO 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("bbdd_cc4", "Cuentas Especiales:",
                                             choices = c("Todas","Cuenta 1","Cuenta 2","Cuenta 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 actionButton("bbdd_cc_boton", "Consultar",
                                              style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
                          )
                          
                          
                          #)#final fluidrow
                          
                          
                          
                      ) # final box
                      
              ),
              
              #|||||||||||||||||||||||||#
              #||| BBDD PÓLIZAS SEUS |||#
              #|||||||||||||||||||||||||#
              
              tabItem(tabName = "bbdd_ps",
                      h2("Información BBDD Pólizas SEUS"),
                      
                      box(width=12,title="Información BBDD Pólizas SEUS",status="primary",solidHeader=TRUE ,
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="bbdd_ps1", label="Desde:", language= "es",
                                           width = "100%")#final dateimput
                                 #),#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="bbdd_ps2", label="Hasta:", language= "es",
                                           width = "100%")#final dateimput
                                 #)#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("bbdd_ps3", "Centro de Atención:",
                                             choices = c("UNIVERSAL DE SEGUROS, C.A","CENTRO 1","CENTRO 2","CENTRO 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("bbdd_ps4", "Cuentas Especiales:",
                                             choices = c("Todas","Cuenta 1","Cuenta 2","Cuenta 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 actionButton("bbdd_ps_boton", "Consultar",
                                              style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
                          )
                          
                          
                          #)#final fluidrow
                          
                          
                          
                      ) # final box
                      
              ),
              
              #////////////////////////////#
              #/// CARGA DE INFORMACIÓN ///#
              #////////////////////////////#

              tabItem(tabName = "ci",
                      h2("Información carga información"),
                      
                      box(width=12,title="Información carga información",status="primary",solidHeader=TRUE ,
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="ci1", label="Desde:", language= "es",
                                           width = "100%")#final dateimput
                                 #),#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="ci2", label="Hasta:", language= "es",
                                           width = "100%")#final dateimput
                                 #)#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("ci3", "Centro de Atención:",
                                             choices = c("UNIVERSAL DE SEGUROS, C.A","CENTRO 1","CENTRO 2","CENTRO 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("ci4", "Cuentas Especiales:",
                                             choices = c("Todas","Cuenta 1","Cuenta 2","Cuenta 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 actionButton("ci_boton", "Consultar",
                                              style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
                          )
                          
                          
                          #)#final fluidrow
                          
                          
                          
                      ) # final box

              ),

              #///////////////////#
              #///  PROYECCIÓN ///#
              #///////////////////#

              tabItem(tabName = "proy",
                      h2("Información Proyección"),
                      
                      box(width=12,title="Información Proyección",status="primary",solidHeader=TRUE ,
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="proy1", label="Desde:", language= "es",
                                           width = "100%")#final dateimput
                                 #),#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="proy2", label="Hasta:", language= "es",
                                           width = "100%")#final dateimput
                                 #)#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("proy3", "Centro de Atención:",
                                             choices = c("UNIVERSAL DE SEGUROS, C.A","CENTRO 1","CENTRO 2","CENTRO 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("proy4", "Cuentas Especiales:",
                                             choices = c("Todas","Cuenta 1","Cuenta 2","Cuenta 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 actionButton("proy_boton", "Consultar",
                                              style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
                          )
                          
                          
                          #)#final fluidrow
                          
                          
                          
                      ) # final box

              ),

              #///////////////////#
              #///  SIMULACIÓN ///#
              #///////////////////#

              tabItem(tabName = "sim",
                      h2("Información Simulación"),
                      
                      box(width=12,title="Información Simulación",status="primary",solidHeader=TRUE ,
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="sim1", label="Desde:", language= "es",
                                           width = "100%")#final dateimput
                                 #),#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="sim2", label="Hasta:", language= "es",
                                           width = "100%")#final dateimput
                                 #)#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("sim3", "Centro de Atención:",
                                             choices = c("UNIVERSAL DE SEGUROS, C.A","CENTRO 1","CENTRO 2","CENTRO 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("sim4", "Cuentas Especiales:",
                                             choices = c("Todas","Cuenta 1","Cuenta 2","Cuenta 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 actionButton("sim_boton", "Consultar",
                                              style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
                          )
                          
                          
                          #)#final fluidrow
                          
                          
                          
                      ) # final box

              ),

              #//////////////////////////#
              #/// CONSULTAR EN LÍNEA ///#
              #//////////////////////////#

              tabItem(tabName = "cl",
                      h2("Información consultas en línea")

              ),
              
              #||||||||||||||||||||||#
              #||| COBRADO DIARIO |||#
              #||||||||||||||||||||||#
              
              tabItem(tabName = "cl_cd",
                      h2("Información Cobrado Diario"),
                      
                      box(width=12,title="Información Cobrado Diario",status="primary",solidHeader=TRUE ,
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="cl_cd1", label="Desde:", language= "es",
                                           width = "100%")#final dateimput
                                 #),#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="cl_cd2", label="Hasta:", language= "es",
                                           width = "100%")#final dateimput
                                 #)#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("cl_cd3", "Centro de Atención:",
                                             choices = c("UNIVERSAL DE SEGUROS, C.A","CENTRO 1","CENTRO 2","CENTRO 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("cl_cd4", "Cuentas Especiales:",
                                             choices = c("Todas","Cuenta 1","Cuenta 2","Cuenta 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 actionButton("cl_cd_boton", "Consultar",
                                              style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
                          )
                          
                          
                          #)#final fluidrow
                          
                          
                          
                      ) # final box
                      
              ),
              
              #||||||||||||||||||||||||||#
              #||| RECIBOS PENDIENTES |||#
              #||||||||||||||||||||||||||#
              
              tabItem(tabName = "cl_rp",
                      h2("Información Recibos Pendientes"),
                      
                      box(width=12,title="Información Recibos Pendientes",status="primary",solidHeader=TRUE ,
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="cl_rp1", label="Desde:", language= "es",
                                           width = "100%")#final dateimput
                                 #),#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="cl_rp2", label="Hasta:", language= "es",
                                           width = "100%")#final dateimput
                                 #)#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("cl_rp3", "Centro de Atención:",
                                             choices = c("UNIVERSAL DE SEGUROS, C.A","CENTRO 1","CENTRO 2","CENTRO 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("cl_rp4", "Cuentas Especiales:",
                                             choices = c("Todas","Cuenta 1","Cuenta 2","Cuenta 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 actionButton("cl_rp_boton", "Consultar",
                                              style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
                          )
                          
                          
                          #)#final fluidrow
                          
                          
                          
                      ) # final box
                      
              ),
              
              #////////////////////////#
              #/// EN CERTIFICACIÓN ///#
              #////////////////////////#

              tabItem(tabName = "ec",
                      h2("Información en certificación")
              ),
              
              #|||||||||||||||||||||||||||||||||||||#
              #||| RESULTADO TÉCNICO POR CLIENTE |||#
              #|||||||||||||||||||||||||||||||||||||#

              tabItem(tabName = "ec_rtc",
                      h2("Información Resultado Técnico por Cliente"),
                      box(width=12,title="Información Resultado Técnico por Cliente",status="primary",solidHeader=TRUE ,
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="ec_rtc1", label="Desde:", language= "es",
                                           width = "100%")#final dateimput
                                 #),#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')), #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 dateInput(inputId="ec_rtc2", label="Hasta:", language= "es",
                                           width = "100%")#final dateimput
                                 #)#final box
                          ),#final column
                          #box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box
                          
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("ec_rtc3", "Centro de Atención:",
                                             choices = c("UNIVERSAL DE SEGUROS, C.A","CENTRO 1","CENTRO 2","CENTRO 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 selectInput("ec_rtc4", "Cuentas Especiales:",
                                             choices = c("Todas","Cuenta 1","Cuenta 2","Cuenta 3"))
                                 #)#final box
                          ),
                          column(width = 6,
                                 #box( width = 6, background = "navy",
                                 actionButton("ec_rtc_boton", "Consultar",
                                              style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
                          )
                          
                          
                          #)#final fluidrow
                          
                          
                          
                      ) # final box
                      
              ),
              
              #//////////////#
              #/// ACERCA ///#
              #//////////////#

                       tabItem(tabName = "acerca",
                              box( width = 9, status="warning",
                                   h3(ACERTITLE_TEXT),
                                   tags$hr(),
                                   h4(ACERVER_TEXT),
                                   h4(ACERRIF_TEXT),
                                   h4(ACERRS_TEXT),
                                   h4(ACERRS_TEXT2),
                                   tags$hr(),
                                   tags$img(src="img/visionrisk.png", width=300, align = "left"),
                                   br(),
                                   h5(ACERSUBSV_TEXT),
                                   br(),
                                   tagList(shiny::icon("map-marker"), ACERDIR_TEXT),br(),
                                   tagList(shiny::icon("phone"), ACERTLF_TEXT),br(),
                                   tagList(shiny::icon("envelope-o"), ACERCORR_TEXT)
                                )#final box
                      )#final tabitem
                    )#final tabitems
                  )#final dashboardbody
                )#final dashboardpage
        

  )#final shinyui

