shinyServer(function(input, output) {

  #///////////////////#
  #/// CONSOLIDADO ///#
  #///////////////////#
  
  output$fecha_ini<-renderPrint({
    #agrego dependencia 
    input$consultar
    #
    isolate({ 
    paste(substr(input$fecha1,9,10),substr(input$fecha1,6,7),substr(input$fecha1,1,4),sep = "/")
    })
      })
  
  output$fecha_fin<-renderPrint({
    #agrego dependencia 
    input$consultar
    #
    isolate({ 
      paste(substr(input$fecha2,9,10),substr(input$fecha2,6,7),substr(input$fecha2,1,4),sep = "/")
    }) 
      })
  
  output$sucursal<-renderPrint({
    #agrego dependencia 
    input$consultar
    #
    isolate({ 
      input$centro_atencion
    })
      })
  
  
  output$cuenta<-renderPrint({
    #agrego dependencia 
    input$consultar
    #
    isolate({ 
    input$cuentas_esp
    })
      })
  
  
  #tabla 1  consolidado
  
  output$tabla1_con <- renderDataTable({
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
    return(a)
      },rownames = FALSE,options = list(
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#04B404', 'color': '#fff'});",
          "}")
        ))

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
 
})
