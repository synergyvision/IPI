rm(list = ls())

# Make sure a package is at least some version (only installs from CRAN)



#cargo librerias a usar
library(shiny)
library(readr)
library(rriskDistributions)
library(fitdistrplus)
library(shinydashboard)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)
library(jrvFinance)
library(plotly)
library(rbokeh)
#library(reshape2)
#library(xlsx)
library(nloptr)
library(alabama)
library(DT)
library(xtable)
library(webshot)
library(readxl)
library(xml2)
library(rvest)
library(VaRES)
library(lmomco)

#libreria contraseña
library(shinyjs)
library(shinyURL)

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


tit <- c("TIF042019", "TIF082019", "TIF112019", "TIF102020", "TIF112020", "TIF022021",
         "TIF032022", "TIF042023", "TIF012024", "TIF062025", "TIF012026", "TIF112027",
         "TIF032028", "TIF052028", "TIF082028", "TIF122028", "TIF022029", "TIF032029",
         "TIF112029", "TIF122029", "TIF022030", "TIF032030", "TIF102030", "TIF022031",
         "TIF032031", "TIF022032", "TIF032032", "TIF032033", "TIF052034", "TIF092034",
         "TIF092035", "TIF052036", "TIF012037")


#VEBONOS iniciales
# tit1=c("VEBONO072018","VEBONO022019","VEBONO032019","VEBONO042019","VEBONO102019","VEBONO012020",
#        "VEBONO062020","VEBONO092020","VEBONO112020","VEBONO012021","VEBONO052021",
#        "VEBONO122021","VEBONO022022","VEBONO012023","VEBONO022024","VEBONO042024",
#        "VEBONO012025","VEBONO022025","VEBONO062026","VEBONO032027","VEBONO042028",
#        "VEBONO102028","VEBONO052029","VEBONO102029","VEBONO072030","VEBONO032031",
#        "VEBONO062032","VEBONO072033","VEBONO022034")
#VEBONOS ACTUALIZADOS FEBRERO 2019
tit1 <- c("VEBONO032019", "VEBONO042019", "VEBONO102019", "VEBONO012020", "VEBONO062020",
          "VEBONO092020", "VEBONO112020", "VEBONO012021", "VEBONO052021", "VEBONO122021",
          "VEBONO022022", "VEBONO012023", "VEBONO122023", "VEBONO022024", "VEBONO042024",
          "VEBONO012025", "VEBONO022025", "VEBONO082025", "VEBONO062026", "VEBONO032027",
          "VEBONO042028", "VEBONO052028", "VEBONO062028", "VEBONO102028", "VEBONO112028",
          "VEBONO012029", "VEBONO042029", "VEBONO052029", "VEBONO092029", "VEBONO102029",
          "VEBONO072030", "VEBONO102030", "VEBONO032031", "VEBONO062032", "VEBONO072033",
          "VEBONO022034", "VEBONO032034a", "VEBONO032034b", "VEBONO032035", "VEBONO122036",
          #          "VEBONO022034", "VEBONO032034", "VEBONO032035", "VEBONO122036",
          "VEBONO082037")

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
