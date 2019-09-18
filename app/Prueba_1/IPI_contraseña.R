rm(list = ls())

# Make sure a package is at least some version (only installs from CRAN)

###########################
#######   GLOBAL   ########
###########################

#cargo librerias a usar
library(shiny)
library(readr)
#library(rriskDistributions)
#library(fitdistrplus)
library(shinydashboard)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)
#library(jrvFinance)
#library(plotly)
#library(rbokeh)
#library(reshape2)
#library(xlsx)
#library(nloptr)
#library(alabama)
library(DT)
library(xtable)
library(webshot)
library(readxl)
library(xml2)
#library(rvest)
#library(VaRES)
#library(lmomco)

#libreria contraseña
library(shinyjs)
library(shinyURL)
library(sodium)

options(OutDec = ",")


# Encabezado Vision
VisionHeader <- function(){tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),
  tags$img(src="www/vision1.png" , id = "VisionLogo", width = 130 ),
  # singleton(includeScript("app/www/js/d3.js")),
  # singleton(includeScript("app/www/js/underscore.js")),
  # singleton(includeScript("app/www/js/jquery-ui.js")),
   singleton(includeCSS("app/www/css/app.css"))
)}

ACERTITLE_TEXT<-"Acerca de VisionRisk™"
ACERSUBSV_TEXT<-"Tecnología para Especulación, Inversión, Economía, Finanzas y Riesgo"
ACERVER_TEXT<-"Versión: 0.0.0"
ACERRIF_TEXT<-"Rif: "
ACERRS_TEXT<-"Copyright © 2014-2018 Synergy Vision"
ACERRS_TEXT2 <- "All Rights Reserved"
ACERDIR_TEXT<-"Centro Gerencial Mohedano, La Castellana"
ACERTLF_TEXT<-"0212-2630808 / 0414-2769752"
ACERCORR_TEXT<-"contacto@synergy.vision"






#TEXTOS
############################################# DATA ###############################################

UPLOADDATA_TEXT<-"Cargar el archivo con los datos"
SELECTFILE_TEXT<-'Seleccione el archivo'
FILESELEC_TEXT<-'Aun no seleccionas el archivo...'
BUTTSELEC_TEXT<-'Buscar'
WITHHEADER_TEXT<-"Con encabezado"
SEPARATOR_TEXT<-"Separador"
COMILLAS_TEXT<-"Comillas"
ENCABEZADO_TEXT<-"Encabezado de los datos"

UPLOADFILETYPE_CONF<-c('text/csv',
                       'text/comma-separated-values',
                       'text/tab-separated-values',
                       'text/plain',
                       '.csv',
                       '.tsv')

UPLOADFILESEP_CONF<-c('Coma'=',',
                      'Punto y coma'=';',
                      'Tab'='\t')

UPLOADFILESEP_CONF_1<-c('Coma'=',',
                        'Punto y coma'=';',
                        'Tab'='\t','Espacio'=' ')

UPLOADCOMILLAS_CONF<-c('Ninguna'='',
                       'Comilla doble'='"',
                       'Comilla simple'="'")

############################################# ANÁLISIS ##############################################

PRUEBDISTTITLE_TEXT<-"Resultados de Pruebas de Distribución"
DISTWITHPARAMTITLE_TEXT<-"Función de Distribución con los Parámetros correspondientes"
GRAFTITLE_TEXT<-"Gráficos"
AHORROTABTITLE_TEXT<-"Análisis Cuentas de Ahorro"
CORRITABTITLE_TEXT<-"Análisis Cuentas Corrientes"
CORRIRTABTITLE_TEXT<-"Análisis Cuentas Corrientes Remuneradas"
SELECFUNCTION_TEXT<-"Seleccione una Distribución"
CORRTABTITLE_TEXT<-"Análisis Cuentas Corrientes"
CORRRTABTITLE_TEXT<-"Análisis Cuentas Corrientes Remuneradas"

#SELECCIÓN DE DISTRIBUCIÓN
#NORMAL
NLABEL1<-"Media"
NLABEL2<-"Desviación Típica"
#EXPONENCIAL
ELABEL1<-"Lambda"
#CAUCHY
CLABEL1<-"Mu"
CLABEL2<-"Theta"
#LOGISTICA
LLABEL1<-"S"
LLABEL2<-"L"
#BETA
BLABEL1<-"S1"
BLABEL2<-"S2"
#CHICUADRADO
CCLABEL<-"Grados de Libertad"
#UNIFORME
ULABEL1<-"Valor mínimo"
ULABEL2<-"Valor máximo"
#GAMMA
GLABEL1<-"M"
GLABEL2<-"Lambda"
#LOGNORMAL
LNLABEL1<-"Media"
LNLABEL2<-"Desviación Típica"
#WEIBULL
WLABEL1<-"S1"
WLABEL2<-"S2"
#FISHER
FLABEL1<-"Grados de Libertad 1"
FLABEL2<-"Grados de Libertad 2"
#T-STUDENT
TLABEL1<-"Grados de Libertad"
#GOMPERTZ
GOLABEL1<-"S1"
GOLABEL2<-"S2"


#Selección análisis cuentas de ahorro
DISTANALAH_CONF<-c("Normal"="Normal", "Exponential"="Exponential",
                   "Cauchy"="Cauchy", "Logistic"="Logistic",
                   "Beta"="Beta", "Chi-square"="Chi-square",
                   "Uniform"="Uniform","Gamma"="Gamma",
                   "Lognormal"="Lognormal", "Weibull"="Weibull",
                   "F"="F", "Student"="Student", "Gompertz"="Gompertz")

############################################# VAR ###############################################
#TÍTULO DE LA CAJA
BOXSELECVARTITLE_TEXT<-"Seleccione Porcentaje del VaR"

#SUBTITULOS DEL ITEM
VARAHTITLE_TEXT<-"Valor en Riesgo (VaR) para las Cuentas de Ahorro"
VARCOTITLE_TEXT<-"Valor en Riesgo (VaR) para las Cuentas Corrientes"
VARCRTITLE_TEXT<-"Valor en Riesgo (VaR) para las Cuentas Corrientes Remuneradas"

#DISTRIBUCIÓN
VARINNOR_TEXT<-"Formulación del VaR para la Distribución Normal $$VaR_p(X) = \\mu + \\sigma \\Phi^{-1}(p)$$"
VARINEXP_TEXT<-"Formulación del VaR para la Distribución Exponencial $$VaR_p(X) = -\\frac{1}{\\lambda}log(1-p)$$"
VARINCAU_TEXT<-"Formulación del VaR para la Distribución Cauchy $$VaR_p(X) = \\mu + \\sigma tan(\\pi(p-\\frac{1}{2}))$$"
VARINLOG_TEXT<-"Formulación del VaR para la Distribución Logistica $$VaR_p(X) = \\mu + \\sigma log[p(1-p)]$$"
VARINBET_TEXT<- "Formulación del VaR para la Distribución Beta"
VARINCHC_TEXT<- "Formulación del VaR para la Distribución Chi Cuadrado"
VARINUNF_TEXT<- "Formulación del VaR para la Distribución Uniforme $$VaR_p(X) = a + p(b-a)$$"
VARINGAM_TEXT<- "Formulación del VaR para la Distribución Gamma $$VaR_p(X) = \\frac{1}{b} Q^{-1}(a, 1-p)$$"
VARINLGN_TEXT<- "Formulación del VaR para la Distribución Lognormal $$VaR_p(X) = e^{[\\mu + \\sigma \\Phi^{-1}(p)]}$$"
VARINWEI_TEXT<- "Formulación del VaR para la Distribución Weibull $$VaR_p(X) = \\sigma[-log(1-p)]^{\\frac{1}{\alpha}}$$"
VARINF_TEXT<- "Formulación del VaR para la Distribución F $$VaR_p(X) = \\mu + F^{-1}(p)\\sigma$$"
VARINTST_TEXT<- "Formulación del VaR para la Distribución T student $$VaR_p(X) = \\mu + T^{-1}(p)\\sigma$$"
VARINGOM_TEXT<- "Formulación del VaR para la Distribución Gompertz"

VARTINNOR_TEXT<- "Formulación del TVaR para la Distribución Normal $$TVaR_p(X) = \\mu + \\frac{σ}{p} \\int_{0}^{p} \\Phi^{-1}(v) dv$$"
VARTINEXP_TEXT<- "Formulación del TVaR para la Distribución Exponencial $$VaR_p(X) = -\\frac{1}{\\lambda}log(1-p)$$"
VARTINCAU_TEXT<- "Formulación del TVaR para la Distribución Cauchy $$TVaR_p(X) = \\mu + \\frac{\\sigma}{p} \\int_{0}^{p} tan(\\pi(v-\\frac{1}{2}))dv$$"
VARTINLOG_TEXT<- "Formulación del TVaR para la Distribución Lognormal $$TVaR_p(X) = \\frac{e^{\\mu}}{p} \\int_{0}^{p} e^{\\sigma \\Phi^{-1}(v)}dv$$"
VARTINBET_TEXT<- "Formulación del TVaR para la Distribución Beta"
VARTINCHC_TEXT<- "Formulación del TVaR para la Distribución Chi Cuadrado"
VARTINUNF_TEXT<- "Formulación del TVaR para la Distribución Uniforme $$TVaR_p(X) = a + \\frac{p}{2} (b-a)$$"
VARTINGAM_TEXT<- "Formulación del TVaR para la Distribución Gamma $$TVaR_p(X) = \\frac{1}{bp} \\int_{0}^{p}Q^{-1}(a, 1-v)dv$$"
VARTINLGN_TEXT<- "Formulación del TVaR para la Distribución Lognormal $$TVaR_p(X) = \\frac{e^{\\mu}}{p} \\int_{0}^{p} e^{\\sigma \\Phi^{-1}(v)}dv$$"
VARTINWEI_TEXT<- "Formulación del TVaR para la Distribución Weibull $$TVaR_p(X) = \\frac{\\sigma}{p}\\gamma[1+\\frac{1}{\\alpha}, -log(1-p)]$$"
VARTINF_TEXT<- "Formulación del TVaR para la Distribución F $$TVaR_p(X) = \\mu + \\int_{0}^{p}F^{-1}(v)\\sigma dv$$"
VARTINTST_TEXT<- "Formulación del TVaR para la Distribución T student $$TVaR_p(X) = \\mu + \\int_{0}^{p}T^{-1}(v)\\sigma dv$$"
VARTINGOM_TEXT<- "Formulación del TVaR para la Distribución Gompertz"


##############################
#######  FIN GLOBAL   ########
##############################



# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("Autenticación", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Usuario", label = tagList(icon("user"), "Usuario")),
                   passwordInput("passwd", placeholder="Contraseña", label = tagList(icon("unlock-alt"), "Contraseña")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "Ingresar", style = "color: white; background-color:#3c8dbc;
                                  padding: 10px 15px; width: 150px; cursor: pointer;
                                  font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("¡Contraseña errónea!",
                                  style = "color: red; font-weight: 600; 
                                  padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     br(),
                     br(),
                     tags$code("Usuario: myuser  Contraseña: mypass"),
                     br(),
                     tags$code("Usuario: myuser1  Contraseña: mypass1")
                     ))
                     )

credentials = data.frame(
  username_id = c("myuser", "myuser1"),
  passod   = sapply(c("mypass", "mypass1"),password_store),
  permission  = c("basic", "advanced"), 
  stringsAsFactors = F
)

header <- dashboardHeader(title = NULL, uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel"),collapsed = TRUE) 
body <- dashboardBody(VisionHeader(),shinyjs::useShinyjs(),tags$style(HTML("
    .box.box-solid.box-primary>.box-header {
                                                            color:#fff;
                                                            background:#024A86;
                                                            
                                                            }
                                                            
                                                            .box.box-solid.box-primary{
                                                            border-bottom-color:#00FF00;
                                                            border-left-color:#00FF00;
                                                            border-right-color:#00FF00;
                                                            border-top-color:#00FF00;
                                                            }")) ,uiOutput("body"))
ui<-dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
  
  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
              shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
  #modulo para mostrar y quitar el siderbarpanel
  # observe({
  #   if(USER$login == TRUE) {
  #     shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
  #   } else {
  #     #shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  #   }
  # })
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
            font-weight: bold; margin:5px; padding: 10px;")
  })
  
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
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
    }
  })
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      tabItems(
        
        #///////////////////#
        #/// CONSOLIDADO ///#
        #///////////////////#
        
        tabItem(tabName = "consolidado",
                h2(" Información Consolidado"),
                
                
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
                                       choices = c("UNIVERSAL DE SEGUROS, C.A","CENTRO 1","CENTRO 2","CENTRO 3"))
                           #)#final box
                    ),
                    column(width = 6,
                           #box( width = 6, background = "navy",
                           selectInput("cuentas_esp", "Cuentas Especiales:",
                                       choices = c("Todas","Cuenta 1","Cuenta 2","Cuenta 3"))
                           #)#final box
                    ),
                    column(width = 6,
                           #box( width = 6, background = "navy",
                           actionButton("consultar", "Consultar",
                                        style="color: #fff; background-color: #04B404; border-color: #04B404") #)#final box
                    )
                    
                    
                    #)#final fluidrow
                    
                    
                    
                ), # final box
                
                verbatimTextOutput('fecha_ini'),
                verbatimTextOutput('fecha_fin'),
                verbatimTextOutput('sucursal'),
                verbatimTextOutput('cuenta'),
                
                #TABLA 1
                fluidRow(
                  uiOutput("t1"),
                  
                  
                  
                  #TABLA 2
                  # box(style="overflow-x:scroll",width = 12,title="Prima Cobrada por Centros de Atención",status="primary",solidHeader=TRUE,
                  #     dataTableOutput("tabla2_con"))
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
                h2(" Información Detalle de Línea de Negocio"),
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
                h2("Información Siniestralidad Principales Cuentas")
                
        ),
        
        #/////////////////////////#
        #/// GESTIÓN COMERCIAL ///#
        #/////////////////////////#
        
        #|||||||||||||||||||||||||#
        #|||  LÍNEA DE NEGOCIO |||#
        #|||||||||||||||||||||||||#
        
        tabItem(tabName = "gc_ln",
                h2("Información Línea de Negocio"),
                
                box(width=12,title="Centro de Atención",status="primary",solidHeader=TRUE ,
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
                h2("Información Centro de Atención")
                
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
                h2("Información concursos generales")
                
        ),
        
        #//////////////////////////#
        #/// GESTIÓN FINANCIERA ///#
        #//////////////////////////#
        
        tabItem(tabName = "gf",
                h2("Información Gestión Financiera")
                
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
                h2("Detalle Centro de Atención")
                
        ),
        
        #|||||||||||||||||||||||||||||||||#
        #|||  RESUMEN LÍNEA DE NEGOCIO |||#
        #|||||||||||||||||||||||||||||||||#
        
        tabItem(tabName = "ind_macro_ln",
                h2("Resumen Línea de Negocio")
                
        ),
        
        #|||||||||||||||||||||||||||||||||#
        #|||  DETALLE LÍNEA DE NEGOCIO |||#
        #|||||||||||||||||||||||||||||||||#
        
        tabItem(tabName = "ind_macro_dln",
                h2("Detalle Línea de Negocio")
                
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
                h2("Información Auto-Cartera")
                
        ),
        
        #//////////////#
        #///  SALUD ///#
        #//////////////#
        
        #||||||||||||||||#
        #|||  CARTERA |||#
        #||||||||||||||||#
        
        tabItem(tabName = "ind_micro_s_c",
                h2("Información Salud-Cartera")
                
        ),
        
        #||||||||||||||||||#
        #|||  SINIESTRO |||#
        #||||||||||||||||||#
        
        tabItem(tabName = "ind_micro_s_s",
                h2("Información Salud-Siniestro")
                
        ),
        
        #/////////////#
        #///  BBDD ///#
        #/////////////#
        
        #||||||||||||||||||||#
        #||| BBDD CARTERA |||#
        #||||||||||||||||||||#
        
        tabItem(tabName = "bbdd_c",
                h2("Información BBDD Cartera")
                
        ),
        
        #|||||||||||||||||||||||#
        #||| BBDD SINIESTROS |||#
        #|||||||||||||||||||||||#
        
        tabItem(tabName = "bbdd_s",
                h2("Información BBDD Siniestros")
                
        ),
        
        #|||||||||||||||||||||||||||||||||||||||#
        #||| BBDD DETALLE POR INTERMEDIARIOS |||#
        #|||||||||||||||||||||||||||||||||||||||#
        
        tabItem(tabName = "bbdd_di",
                h2("Información BBDD Detalles por Intermediarios")
                
        ),
        
        #|||||||||||||||||||||||||||||||||||#
        #||| BBDD CARTERA POR COBERTURAS |||#
        #|||||||||||||||||||||||||||||||||||#
        
        tabItem(tabName = "bbdd_cc",
                h2("Información BBDD Cartera por Coberturas")
                
        ),
        
        #|||||||||||||||||||||||||#
        #||| BBDD PÓLIZAS SEUS |||#
        #|||||||||||||||||||||||||#
        
        tabItem(tabName = "bbdd_ps",
                h2("Información BBDD Pólizas SEUS")
                
        ),
        
        #////////////////////////////#
        #/// CARGA DE INFORMACIÓN ///#
        #////////////////////////////#
        
        tabItem(tabName = "ci",
                h2("Información carga información")
                
        ),
        
        #///////////////////#
        #///  PROYECCIÓN ///#
        #///////////////////#
        
        tabItem(tabName = "proy",
                h2("Información Proyección")
                
        ),
        
        #///////////////////#
        #///  SIMULACIÓN ///#
        #///////////////////#
        
        tabItem(tabName = "sim",
                h2("Información Simulación")
                
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
                h2("Información Cobrado Diario")
                
        ),
        
        #||||||||||||||||||||||||||#
        #||| RECIBOS PENDIENTES |||#
        #||||||||||||||||||||||||||#
        
        tabItem(tabName = "cl_rp",
                h2("Información Recibos Pendientes")
                
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
                h2("Información Resultado Técnico por Cliente")
                
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
    }
    else {
      loginpage
    }
  })
  
  # output$results <-  DT::renderDataTable({
  #   datatable(iris, options = list(autoWidth = TRUE,
  #                                  searching = FALSE))
  # })
  
  
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
    b <- data.frame(matrix(0,nrow = 10,ncol = 37))
    names(b) <- c("Centro De Atención","Prima Cobrada Auto","% Ppto Auto","% Sin. Auto",
                  "% Sin Rolling 12 Auto","% Persistencia Auto","% Persistencia Rolling 12 Auto",
                  "Prima Cobrada Fianza","% Ppto Fianza","% Sin. Fianza",
                  "% Sin Rolling 12 Fianza","% Persistencia Fianza","% Persistencia Rolling 12 Fianza",
                  "Prima Cobrada Patrimoniales","% Ppto Patrimoniales","% Sin. Patrimoniales",
                  "% Sin Rolling 12 Patrimoniales","% Persistencia Patrimoniales","% Persistencia Rolling 12 Patrimoniales",
                  "Prima Cobrada Personas","% Ppto Personas","% Sin. Personas",
                  "% Sin Rolling 12 Personas","% Persistencia Personas","% Persistencia Rolling 12 Personas",
                  "Prima Cobrada Salud","% Ppto Salud","% Sin. Salud",
                  "% Sin Rolling 12 Salud","% Persistencia Salud","% Persistencia Rolling 12 Salud",
                  "Prima Cobrada General","% Ppto General","% Sin. General",
                  "% Sin Rolling 12 General","% Persistencia General","% Persistencia Rolling 12 General"
    )
    return(b)
  },rownames = FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
      "}")
  ))
  
  
  
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
  
  } #final server

runApp(list(ui = ui, server = server))
