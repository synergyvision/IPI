#creo data de prueba para las diferentes secciones
#Sección Consolidado
library(lubridate)

fe <- seq.Date(as.Date("2019-01-01"),as.Date("2019-12-31"),by = "days")
centro <- c("Centro 1","Centro 2","Centro 3","Centro 4","Centro 5","Centro 6","Centro 7",
            "Centro 8","Centro 9","Centro 10","Centro 11","Centro 12","Centro 13","Centro 14",
            "Centro 15","Centro 16","Centro 17","Centro 18","Centro 19","Centro 20")

cuentas <- c("Cuenta 1","Cuenta 2","Cuenta 3","Cuenta 4","Cuenta 5","Cuenta 6","Cuenta 7",
             "Cuenta 8","Cuenta 9","Cuenta 10")

ind <- round(runif(n = length(fe),min = 1,max = 20))

ind1 <- round(runif(n = length(fe),min = 1,max = 10))

data1 <- data.frame(Fecha=fe,Centro_atencion=centro[ind],Cuentas_esp=cuentas[ind1])
write.table(x = data1,file = paste0(getwd(),"/app/Datos/","data_consolidado.txt"))





