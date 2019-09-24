#creo data de prueba para las diferentes secciones
#Sección Consolidado
library(lubridate)

fe <- seq.Date(as.Date("2019-01-01"),as.Date("2019-12-31"),by = "days")
centro <- c("Centro 1","Centro 2","Centro 3","Centro 4","Centro 5","Centro 6","Centro 7",
            "Centro 8","Centro 9","Centro 10","Centro 11","Centro 12","Centro 13","Centro 14",
            "Centro 15","Centro 16","Centro 17","Centro 18","Centro 19","Centro 20")

cuentas <- c("Cuenta 1","Cuenta 2","Cuenta 3","Cuenta 4","Cuenta 5")

ind <- round(runif(n = length(fe),min = 1,max = 20))

ind1 <- round(runif(n = length(fe),min = 1,max = 5))

data1 <- data.frame(Fecha=fe,Centro_atencion=centro[ind],Cuentas_esp=cuentas[ind1])
write.table(x = data1,file = paste0(getwd(),"/app/Datos/","data_consolidado.txt"))


#uso del paquete FakeR
library(fakeR)

#ejemplo
# single column of an unordered, string factor
state_df <- data.frame(division=state.division)
# character variable
state_df$division <- as.character(state_df$division)
# numeric variable
state_df$area <- state.area
# factor variable
state_df$region <- state.region
state_sim <- simulate_dataset(state_df)

#tabla 1
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

#relleno primeras filas
a$`Centro De Atención` <- c("Centro 1","Centro 2","Centro 3","Centro 4","Centro 5")

#auto
a[,2] <- c("poliza nueva auto 1","poliza nueva auto 2","poliza nueva auto 3","poliza nueva 4","poliza nueva auto 5")
a[,3] <- c("poliza renovada auto 1","poliza renovada auto 2","poliza renovada auto 3","poliza renovada auto 4","poliza renovada auto 5")
a[,4] <- c("poliza auto 1","poliza auto 2","poliza auto 3","poliza auto 4","poliza auto 5")
a[,6] <- c("c act auto 1","c act auto 2","c act auto 3","c act auto 4","c act auto 5")
a[,7] <- c("int act auto 1","int act auto 2","int act auto 3","int act auto 4","int act auto 5")

#Fianza
a[,8] <- c("poliza nueva fianza 1","poliza nueva fianza 2","poliza nueva fianza 3","poliza nueva 4","poliza nueva fianza 5")
a[,9] <- c("poliza renovada fianza 1","poliza renovada fianza 2","poliza renovada fianza 3","poliza renovada fianza 4","poliza renovada fianza 5")
a[,10] <- c("poliza fianza 1","poliza fianza 2","poliza fianza 3","poliza fianza 4","poliza fianza 5")
a[,12] <- c("c act fianza 1","c act fianza 2","c act fianza 3","c act fianza 4","c act fianza 5")
a[,13] <- c("int act fianza 1","int act fianza 2","int act fianza 3","int act fianza 4","int act fianza 5")


#Patrimoniales
a[,14] <- c("poliza nueva patrimoniales 1","poliza nueva patrimoniales 2","poliza nueva patrimoniales 3","poliza nueva 4","poliza nueva patrimoniales 5")
a[,15] <- c("poliza renovada patrimoniales 1","poliza renovada patrimoniales 2","poliza renovada patrimoniales 3","poliza renovada patrimoniales 4","poliza renovada patrimoniales 5")
a[,16] <- c("poliza patrimoniales 1","poliza patrimoniales 2","poliza patrimoniales 3","poliza patrimoniales 4","poliza patrimoniales 5")
a[,18] <- c("c act patrimoniales 1","c act patrimoniales 2","c act patrimoniales 3","c act patrimoniales 4","c act patrimoniales 5")
a[,19] <- c("int act patrimoniales 1","int act patrimoniales 2","int act patrimoniales 3","int act patrimoniales 4","int act patrimoniales 5")


#Personas
a[,8] <- c("poliza nueva fianza 1","poliza nueva fianza 2","poliza nueva fianza 3","poliza nueva 4","poliza nueva fianza 5")
a[,9] <- c("poliza renovada fianza 1","poliza renovada fianza 2","poliza renovada fianza 3","poliza renovada fianza 4","poliza renovada fianza 5")
a[,10] <- c("poliza fianza 1","poliza fianza 2","poliza fianza 3","poliza fianza 4","poliza fianza 5")
a[,12] <- c("c act fianza 1","c act fianza 2","c act fianza 3","c act fianza 4","c act fianza 5")
a[,13] <- c("int act fianza 1","int act fianza 2","int act fianza 3","int act fianza 4","int act fianza 5")


#Salud
a[,8] <- c("poliza nueva fianza 1","poliza nueva fianza 2","poliza nueva fianza 3","poliza nueva 4","poliza nueva fianza 5")
a[,9] <- c("poliza renovada fianza 1","poliza renovada fianza 2","poliza renovada fianza 3","poliza renovada fianza 4","poliza renovada fianza 5")
a[,10] <- c("poliza fianza 1","poliza fianza 2","poliza fianza 3","poliza fianza 4","poliza fianza 5")
a[,12] <- c("c act fianza 1","c act fianza 2","c act fianza 3","c act fianza 4","c act fianza 5")
a[,13] <- c("int act fianza 1","int act fianza 2","int act fianza 3","int act fianza 4","int act fianza 5")


#General
a[,8] <- c("poliza nueva fianza 1","poliza nueva fianza 2","poliza nueva fianza 3","poliza nueva 4","poliza nueva fianza 5")
a[,9] <- c("poliza renovada fianza 1","poliza renovada fianza 2","poliza renovada fianza 3","poliza renovada fianza 4","poliza renovada fianza 5")
a[,10] <- c("poliza fianza 1","poliza fianza 2","poliza fianza 3","poliza fianza 4","poliza fianza 5")
a[,12] <- c("c act fianza 1","c act fianza 2","c act fianza 3","c act fianza 4","c act fianza 5")
a[,13] <- c("int act fianza 1","int act fianza 2","int act fianza 3","int act fianza 4","int act fianza 5")



a_sim <- simulate_dataset(a[,1:2])
a_sim1 <- simulate_dataset(a[,1:2])

#
library(charlatan)

ch_generate('job', 'phone_number', n = 30)

#NO ES VIABLE GENERAR DATA DE ESTE TIPO MEDIANTE ESTE PAQUETE


