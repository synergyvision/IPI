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
  

 
})
