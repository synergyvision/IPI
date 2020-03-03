rm(list = ls())

# Make sure a package is at least some version (only installs from CRAN)



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
library(highcharter)
library(shinyWidgets)
library(tidyr)
library(shinythemes)
library(tychobratools)

options(OutDec = ",")


# Encabezado Vision
VisionHeader <- function(){tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),
  tags$img(src="img/vision1.png" , id = "VisionLogo", width = 130 ),
  singleton(includeScript("www/js/d3.js")),
  singleton(includeScript("www/js/underscore.js")),
  singleton(includeScript("www/js/jquery-ui.js")),
  singleton(includeCSS("www/css/app.css"))
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


#################################### LOG IN ###############################################

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


#MAPA
hospitals <- read.csv("~/IPI/app/ejemplo_mapa/prueba_mapa_VZLA/data/hospitals_ven.txt", sep="")
states <- read.csv("~/IPI/app/ejemplo_mapa/prueba_mapa_VZLA/data/states_ven.txt", sep="")
# } else {
# see "data-prep.R" file.  It will create the `dat` data frame
# and load it in the global environment
#source("data-prep.R")
#}

# choice options for inputs
period_choices <- unique(states$period)
type_choices <- unique(states$claim_type)

nation_n_hospitals <- unique(hospitals$hospital) %>%
  length()

nation_n_providers <- unique(hospitals$provider_id) %>%
  length()

# download data to draw map
#mapdata <- download_map_data("countries/us/us-all")
mapdata <- download_map_data("countries/ve/ve-all")


# highcharter options 
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)
rm(hcoptslang)
