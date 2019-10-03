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
a <- as.data.frame(matrix(0,nrow = 365,ncol = 37))
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


#creo fecha
fecha <- seq.Date(as.Date("2019-01-01"),as.Date("2019-12-31"),by = "days")

#relleno primeras filas
ind <- ch_integer(365,1,20)
centro <- c("Centro 1","Centro 2","Centro 3","Centro 4","Centro 5","Centro 6","Centro 7",
            "Centro 8","Centro 9","Centro 10","Centro 11","Centro 12","Centro 13","Centro 14",
            "Centro 15","Centro 16","Centro 17","Centro 18","Centro 19","Centro 20")

#
a$fecha <- fecha
a$`Centro De Atención` <- centro[ind]

#auto

a[,2] <- ch_integer(365,1,300)
a[,3] <- ch_integer(365,1,150)
a[,4] <- ch_integer(365,1,250)
a[,5] <- ch_unif(n = 365, min = 0, max = 200)
a[,6] <- ch_integer(365,1,200)
a[,7] <- ch_integer(365,1,100)

#Fianza
a[,8] <- ch_integer(365,1,300)
a[,9] <- ch_integer(365,1,150)
a[,10] <- ch_integer(365,1,250)
a[,11] <- ch_unif(n = 365, min = 0, max = 200)
a[,12] <- ch_integer(365,1,200)
a[,13] <- ch_integer(365,1,100)

#Patrimoniales
a[,14] <- ch_integer(365,1,300)
a[,15] <- ch_integer(365,1,150)
a[,16] <- ch_integer(365,1,250)
a[,17] <- ch_unif(n = 365, min = 0, max = 200)
a[,18] <- ch_integer(365,1,200)
a[,19] <- ch_integer(365,1,100)

#Personas
a[,20] <- ch_integer(365,1,300)
a[,21] <- ch_integer(365,1,150)
a[,22] <- ch_integer(365,1,250)
a[,23] <- ch_unif(n = 365, min = 0, max = 200)
a[,24] <- ch_integer(365,1,200)
a[,25] <- ch_integer(365,1,100)

#Salud
a[,26] <- ch_integer(365,1,300)
a[,27] <- ch_integer(365,1,150)
a[,28] <- ch_integer(365,1,250)
a[,29] <- ch_unif(n = 365, min = 0, max = 200)
a[,30] <- ch_integer(365,1,200)
a[,31] <- ch_integer(365,1,100)

#General
a[,32] <- ch_integer(365,1,300)
a[,33] <- ch_integer(365,1,150)
a[,34] <- ch_integer(365,1,250)
a[,35] <- ch_unif(n = 365, min = 0, max = 200)
a[,36] <- ch_integer(365,1,200)
a[,37] <- ch_integer(365,1,100)

#agrego cuentas especiales
ind1 <- ch_integer(365,1,5)
cuentas <- c("Cuenta 1","Cuenta 2","Cuenta 3","Cuenta 4","Cuenta 5")

a$cuentas_esp <- cuentas[ind1]

#
write.table(x = a,file = paste0(getwd(),"/app/Datos/","data_consolidado_nueva.txt"))


# a_sim <- simulate_dataset(a[,1:2])
# a_sim1 <- simulate_dataset(a[,1:2])

#
library(charlatan)

ch_generate('job', 'phone_number', n = 30)

#NO ES VIABLE GENERAR DATA DE ESTE TIPO MEDIANTE ESTE PAQUETE

#tabla 2
b <- as.data.frame(matrix(0,nrow = 365,ncol = 37))
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


#creo fecha
fecha <- seq.Date(as.Date("2019-01-01"),as.Date("2019-12-31"),by = "days")

#relleno primeras filas
ind <- ch_integer(365,1,20)
centro <- c("Centro 1","Centro 2","Centro 3","Centro 4","Centro 5","Centro 6","Centro 7",
            "Centro 8","Centro 9","Centro 10","Centro 11","Centro 12","Centro 13","Centro 14",
            "Centro 15","Centro 16","Centro 17","Centro 18","Centro 19","Centro 20")

#
b$fecha <- fecha
b$`Centro De Atención` <- centro[ind]

#auto

b[,2] <- ch_integer(365,1,50000000)
b[,3] <- ch_unif(n = 365, min = 0, max = 800)
b[,4] <- ch_unif(n = 365, min = 0, max = 200)
b[,5] <- ch_unif(n = 365, min = 0, max = 200)
b[,6] <- ch_unif(n = 365, min = 0, max = 100)
b[,7] <- ch_unif(n = 365, min = 0, max = 100)

#Fianza
b[,8] <- ch_integer(365,1,50000000)
b[,9] <- ch_unif(n = 365, min = 0, max = 800)
b[,10] <- ch_unif(n = 365, min = 0, max = 200)
b[,11] <- ch_unif(n = 365, min = 0, max = 200)
b[,12] <- ch_unif(n = 365, min = 0, max = 100)
b[,13] <- ch_unif(n = 365, min = 0, max = 100)

#Pbtrimonibles
b[,14] <- ch_integer(365,1,50000000)
b[,15] <- ch_unif(n = 365, min = 0, max = 800)
b[,16] <- ch_unif(n = 365, min = 0, max = 200)
b[,17] <- ch_unif(n = 365, min = 0, max = 200)
b[,18] <- ch_unif(n = 365, min = 0, max = 100)
b[,19] <- ch_unif(n = 365, min = 0, max = 100)

#Personbs
b[,20] <- ch_integer(365,1,50000000)
b[,21] <- ch_unif(n = 365, min = 0, max = 800)
b[,22] <- ch_unif(n = 365, min = 0, max = 200)
b[,23] <- ch_unif(n = 365, min = 0, max = 200)
b[,24] <- ch_unif(n = 365, min = 0, max = 100)
b[,25] <- ch_unif(n = 365, min = 0, max = 100)

#Sblud
b[,26] <- ch_integer(365,1,50000000)
b[,27] <- ch_unif(n = 365, min = 0, max = 800)
b[,28] <- ch_unif(n = 365, min = 0, max = 200)
b[,29] <- ch_unif(n = 365, min = 0, max = 200)
b[,30] <- ch_unif(n = 365, min = 0, max = 100)
b[,31] <- ch_unif(n = 365, min = 0, max = 100)

#General
b[,32] <- ch_integer(365,1,50000000)
b[,33] <- ch_unif(n = 365, min = 0, max = 800)
b[,34] <- ch_unif(n = 365, min = 0, max = 200)
b[,35] <- ch_unif(n = 365, min = 0, max = 200)
b[,36] <- ch_unif(n = 365, min = 0, max = 100)
b[,37] <- ch_unif(n = 365, min = 0, max = 100)

#agrego cuentas especiales
ind1 <- ch_integer(365,1,5)
cuentas <- c("Cuenta 1","Cuenta 2","Cuenta 3","Cuenta 4","Cuenta 5")

b$cuentas_esp <- cuentas[ind1]

#
write.table(x = b,file = paste0(getwd(),"/app/Datos/","data_consolidado_tabla2.txt"))

#SECCION GESTION TECNICA
#RESUMEN CENTRO DE ATENCION
#Tabla 1

a <- as.data.frame(matrix(0,nrow =365,ncol = 12))
names(a) <- c("Centro Atención","Pólizas Nuevas","Pólizas Renovadas",
              "Inventario Real","Inventario Presupuesto","Inventario Cumplimiento (%)",
              "Participación Primas Real (%)","Participación Primas Presupuesto (%)",
              "Primas Cobradas Netas de Devolución Real","Primas Cobradas Netas de Devolución Presupuesto",
              "Primas Cobradas Netas de Devolución Cumplimiento (%)",
              "Reservas de Primas al Inicio Real")


#creo fecha
fecha <- seq.Date(as.Date("2019-01-01"),as.Date("2019-12-31"),by = "days")

#relleno primeras filas
ind <- ch_integer(365,1,20)
centro <- c("Centro 1","Centro 2","Centro 3","Centro 4","Centro 5","Centro 6","Centro 7",
            "Centro 8","Centro 9","Centro 10","Centro 11","Centro 12","Centro 13","Centro 14",
            "Centro 15","Centro 16","Centro 17","Centro 18","Centro 19","Centro 20")

#
a$fecha <- fecha
a$`Centro Atención` <- centro[ind]

#
a[,2] <- ch_integer(365,1,2000)
a[,3] <- ch_integer(365,1,700)
a[,4] <- ch_integer(365,1,800)
a[,5] <- ch_integer(365,1,2000)
a[,6] <- ch_unif(n = 365, min = 0, max = 150)
a[,7] <- ch_unif(n = 365, min = 0, max = 100)
a[,8] <- ch_unif(n = 365, min = 0, max = 100)
a[,9] <- ch_integer(365,1,100000000)
a[,10] <- ch_integer(365,1,100000000)
a[,11] <- ch_unif(n = 365, min = 0, max = 600)
a[,12] <-  ch_integer(365,1,100000000)

#agrego cuentas especiales
ind1 <- ch_integer(365,1,5)
cuentas <- c("Cuenta 1","Cuenta 2","Cuenta 3","Cuenta 4","Cuenta 5")

a$cuentas_esp <- cuentas[ind1]

#
write.table(x = a,file = paste0(getwd(),"/app/Datos/","data_gt_rca1.txt"))


#tabla 2
a <- as.data.frame(matrix(0,nrow = 365,ncol = 13))
names(a) <- c("Centro Atención","(*) Siniestralidad Incurrida Real (%)","Siniestralidad Incurrida Presupuesto (%)",
              "(*) Siniestralidad Incurrida sin IBNR Real (%)","Siniestralidad Incurrida sin IBNR Presupuesto (%)",
              "(*) Siniestralidad Incurrida Rolling 12 Real (%)","Siniestralidad Incurrida Rolling 12 Presupuesto (%)",
              "Persistencia (%)","Persistencia Rolling 12 (%)",
              "Comisiones sobre Devengada Real (%)","Comisiones sobre Devengada Presupuesto (%)",
              "Comisiones sobre Cobrado Real (%)","Comisiones sobre Cobrado Presupuesto (%)")

#creo fecha
fecha <- seq.Date(as.Date("2019-01-01"),as.Date("2019-12-31"),by = "days")

#relleno primeras filas
ind <- ch_integer(365,1,20)
centro <- c("Centro 1","Centro 2","Centro 3","Centro 4","Centro 5","Centro 6","Centro 7",
            "Centro 8","Centro 9","Centro 10","Centro 11","Centro 12","Centro 13","Centro 14",
            "Centro 15","Centro 16","Centro 17","Centro 18","Centro 19","Centro 20")

#
a$fecha <- fecha
a$`Centro Atención` <- centro[ind]

#
a[,2] <- ch_unif(n = 365, min = 0, max = 150)
a[,3] <- ch_unif(n = 365, min = 0, max = 100)
a[,4] <- ch_unif(n = 365, min = 0, max = 200)
a[,5] <- ch_unif(n = 365, min = 0, max = 100)
a[,6] <- ch_unif(n = 365, min = 0, max = 150)
a[,7] <- ch_unif(n = 365, min = 0, max = 150)
a[,8] <- ch_unif(n = 365, min = 0, max = 50)
a[,9] <- ch_unif(n = 365, min = 0, max = 50)
a[,10] <- ch_unif(n = 365, min = 0, max = 50)
a[,11] <- ch_unif(n = 365, min = 0, max = 50)
a[,12] <-  ch_unif(n = 365, min = 0, max = 100)
a[,13] <-  ch_unif(n = 365, min = 0, max = 100)

#agrego cuentas especiales
ind1 <- ch_integer(365,1,5)
cuentas <- c("Cuenta 1","Cuenta 2","Cuenta 3","Cuenta 4","Cuenta 5")

a$cuentas_esp <- cuentas[ind1]

#
write.table(x = a,file = paste0(getwd(),"/app/Datos/","data_gt_rca2.txt"))

#SECCION GESTION TECNICA
#RESUMEN LINEA DE NEGOCIO
#TABLA 1

a <- as.data.frame(matrix(0,nrow = 365,ncol = 12))
names(a) <- c("Línea Negocio","Pólizas Nuevas","Pólizas Renovadas",
              "Inventario Real","Inventario Presupuesto","Inventario Cumplimiento (%)",
              "Participación Primas Real (%)","Participación Primas Presupuesto (%)",
              "Primas Cobradas Netas de Devolución Real","Primas Cobradas Netas de Devolución Presupuesto",
              "Primas Cobradas Netas de Devolución Cumplimiento (%)",
              "Reservas de Primas al Inicio Real"
)

#
ind <- ch_integer(365,1,5)
lineas <- c("Auto","Fianzas","Patrimoniales","Personas","Salud")

a[,1] <- lineas[ind]


#RELLENO COLUMNAS
a[,2] <- ch_integer(n = 365, min = 0, max = 400)
a[,3] <- ch_integer(n = 365, min = 1, max = 200)
a[,4] <- ch_integer(n = 365, min = 0, max = 400)
a[,5] <- ch_integer(n = 365, min = 0, max = 600)
a[,6] <- ch_unif(n = 365, min = 0, max = 200)
a[,7] <- ch_unif(n = 365, min = 0, max = 100)
a[,8] <- ch_unif(n = 365, min = 0, max = 100)
a[,9] <- ch_integer(n = 365, min = 0, max = 50000000000)
a[,10] <- ch_integer(n = 365, min = 0, max = 50000000000)
a[,11] <- ch_unif(n = 365, min = 0, max = 1000)
a[,12] <-  ch_unif(n = 365, min = 0, max = 50000000000)


#creo fecha
fecha <- seq.Date(as.Date("2019-01-01"),as.Date("2019-12-31"),by = "days")

a$fecha <- fecha

#agrego cuentas especiales
ind1 <- ch_integer(365,1,5)
cuentas <- c("Cuenta 1","Cuenta 2","Cuenta 3","Cuenta 4","Cuenta 5")

a$cuentas_esp <- cuentas[ind1]

write.table(x = a,file = paste0(getwd(),"/app/Datos/","data_gt_rln1.txt"))


#TABLA 2

a <- as.data.frame(matrix(0,nrow = 365,ncol = 13))
names(a) <- c("Centro Atención","(*) Siniestralidad Incurrida Real (%)","Siniestralidad Incurrida Presupuesto (%)",
              "(*) Siniestralidad Incurrida sin IBNR Real (%)","Siniestralidad Incurrida sin IBNR Presupuesto (%)",
              "(*) Siniestralidad Incurrida Rolling 12 Real (%)","Siniestralidad Incurrida Rolling 12 Presupuesto (%)",
              "Persistencia (%)","Persistencia Rolling 12 (%)",
              "Comisiones sobre Devengada Real (%)","Comisiones sobre Devengada Presupuesto (%)",
              "Comisiones sobre Cobrado Real (%)","Comisiones sobre Cobrado Presupuesto (%)"
)

#
ind <- ch_integer(365,1,5)
lineas <- c("Auto","Fianzas","Patrimoniales","Personas","Salud")

a[,1] <- lineas[ind]

#RELLENO COLUMNAS
a[,2] <- ch_unif(n = 365, min = 0, max = 400)
a[,3] <- ch_unif(n = 365, min = 1, max = 200)
a[,4] <- ch_unif(n = 365, min = 0, max = 400)
a[,5] <- ch_unif(n = 365, min = 0, max = 600)
a[,6] <- ch_unif(n = 365, min = 0, max = 200)
a[,7] <- ch_unif(n = 365, min = 0, max = 100)
a[,8] <- ch_unif(n = 365, min = 0, max = 100)
a[,9] <- ch_unif(n = 365, min = 0, max = 200)
a[,10] <- ch_unif(n = 365, min = 0, max = 500)
a[,11] <- ch_unif(n = 365, min = 0, max = 1000)
a[,12] <-  ch_unif(n = 365, min = 0, max = 200)
a[,13] <-  ch_unif(n = 365, min = 0, max = 300)

#creo fecha
fecha <- seq.Date(as.Date("2019-01-01"),as.Date("2019-12-31"),by = "days")

a$fecha <- fecha

#agrego cuentas especiales
ind1 <- ch_integer(365,1,5)
cuentas <- c("Cuenta 1","Cuenta 2","Cuenta 3","Cuenta 4","Cuenta 5")

a$cuentas_esp <- cuentas[ind1]

write.table(x = a,file = paste0(getwd(),"/app/Datos/","data_gt_rln2.txt"))

#SECCION GESTION TECNICA
#DETALLE LINEA DE NEGOCIO
#TABLA 1

a <- as.data.frame(matrix(0,nrow = 365,ncol = 12))
names(a) <- c("Ramo","Pólizas Nuevas","Pólizas Renovadas","Cantidad de Asegurados",
              "Inventario Real","Inventario Presupuesto","Inventario Cumplimiento (%)",
              "Participación Primas Real (%)","Participación Primas Presupuesto (%)",
              "Primas Cobradas Netas de Devolución Nueva","Primas Cobradas Netas de Devolución Renovada",
              "Primas Cobradas Netas de Devolución Real"
)

#return(datatable(a, options = list(paging = FALSE)))
casos <- c("AUTOMOVIL","AUTOMOVIL CASCO FLOTA","RESPONSABILIDAD CIVIL DE VEHICULOS")
ind <- ch_integer(365,1,3)

a[,1] <- casos[ind]

#RELLENO COLUMNAS
a[,2] <- ch_integer(n = 365, min = 0, max = 200)
a[,3] <- ch_integer(n = 365, min = 1, max = 100)
a[,4] <- ch_integer(n = 365, min = 0, max = 4000)
a[,5] <- ch_integer(n = 365, min = 0, max = 300)
a[,6] <- ch_integer(n = 365, min = 0, max = 200)
a[,7] <- ch_unif(n = 365, min = 0, max = 200)
a[,8] <- ch_unif(n = 365, min = 0, max = 100)
a[,9] <- ch_unif(n = 365, min = 0, max = 100)
a[,10] <- ch_unif(n = 365, min = 0, max = 50000000000)
a[,11] <- ch_unif(n = 365, min = 0, max = 50000000000)
a[,12] <-  ch_unif(n = 365, min = 0, max = 50000000000)


#creo fecha
fecha <- seq.Date(as.Date("2019-01-01"),as.Date("2019-12-31"),by = "days")

a$fecha <- fecha

#agrego cuentas especiales
ind1 <- ch_integer(365,1,5)
cuentas <- c("Cuenta 1","Cuenta 2","Cuenta 3","Cuenta 4","Cuenta 5")

a$cuentas_esp <- cuentas[ind1]

#agrego centro de atencion
ind2 <- ch_integer(365,1,5)
centro <- c("Centro 1","Centro 2","Centro 3","Centro 4","Centro 5")
a$centro <- centro[ind2]


write.table(x = a,file = paste0(getwd(),"/app/Datos/","data_gt_dln1.txt"))


#TABLA 2
a <- as.data.frame(matrix(0,nrow = 365,ncol = 13))
names(a) <- c("Ramo","(*) Siniestralidad Incurrida Real (%)","Siniestralidad Incurrida Presupuesto (%)",
              "(*) Siniestralidad Incurrida sin IBNR Real (%)","Siniestralidad Incurrida sin IBNR Presupuesto (%)",
              "(*) Siniestralidad Incurrida Rolling 12 Real (%)","Siniestralidad Incurrida Rolling 12 Presupuesto (%)",
              "Persistencia (%)","Persistencia Rolling 12 (%)",
              "Comisiones sobre Devengada Real (%)","Comisiones sobre Devengada Presupuesto (%)",
              "Comisiones sobre Cobrado Real (%)","Comisiones sobre Cobrado Presupuesto (%)"
)

#return(datatable(a, options = list(paging = FALSE)))
casos <- c("AUTOMOVIL","AUTOMOVIL CASCO FLOTA","RESPONSABILIDAD CIVIL DE VEHICULOS")
ind <- ch_integer(365,1,3)

a[,1] <- casos[ind]

#RELLENO COLUMNAS
a[,2] <- ch_unif(n = 365, min = 0, max = 200)
a[,3] <- ch_unif(n = 365, min = 1, max = 100)
a[,4] <- ch_unif(n = 365, min = 0, max = 400)
a[,5] <- ch_unif(n = 365, min = 0, max = 300)
a[,6] <- ch_unif(n = 365, min = 0, max = 200)
a[,7] <- ch_unif(n = 365, min = 0, max = 200)
a[,8] <- ch_unif(n = 365, min = 0, max = 100)
a[,9] <- ch_unif(n = 365, min = 0, max = 100)
a[,10] <- ch_unif(n = 365, min = 0, max = 200)
a[,11] <- ch_unif(n = 365, min = 0, max = 300)
a[,12] <-  ch_unif(n = 365, min = 0, max = 400)
a[,13] <-  ch_unif(n = 365, min = 0, max = 200)

#creo fecha
fecha <- seq.Date(as.Date("2019-01-01"),as.Date("2019-12-31"),by = "days")

a$fecha <- fecha

#agrego cuentas especiales
ind1 <- ch_integer(365,1,5)
cuentas <- c("Cuenta 1","Cuenta 2","Cuenta 3","Cuenta 4","Cuenta 5")

a$cuentas_esp <- cuentas[ind1]

#agrego centro de atencion
ind2 <- ch_integer(365,1,5)
centro <- c("Centro 1","Centro 2","Centro 3","Centro 4","Centro 5")

a$centro <- centro[ind2]


write.table(x = a,file = paste0(getwd(),"/app/Datos/","data_gt_dln2.txt"))


#SECCION GESTION TECNICA
#SINIESTRALIDAD PPAL CUENTAS
#TABLA 1

a <- as.data.frame(matrix(0,nrow = 365,ncol = 5))
names(a) <- c("Asesor","Ramo","Número de Póliza","Fecha","Contrato")

#creo fecha
fecha <- seq.Date(as.Date("2019-01-01"),as.Date("2019-12-31"),by = "days")

a$Fecha <- fecha

#
ind <- ch_integer(365,1,5)
lineas <- c("Auto","Fianzas","Patrimoniales","Personas","Salud")

a$Ramo <- lineas[ind]

#
ind1 <- ch_integer(365,1,5)
asesor <- c("Asesor 1","Asesor 2","Asesor 3","Asesor 4","Asesor 5")

a$Asesor <- asesor[ind1]

#RELLENO COLUMNAS
a[,3] <- ch_integer(n = 365, min = 1, max = 10)
a[,5] <- ch_integer(n = 365, min = 1, max = 10)


write.table(x = a,file = paste0(getwd(),"/app/Datos/","data_gt_sinpc.txt"))



