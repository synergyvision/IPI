library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)


#my_username <- "test"
#my_password <- "test"

#credenciales como en otro script
credentials = data.frame(
  username_id = c("test", "test1"),
  passod   = sapply(c("test", "test1"),password_store),
  permission  = c("basic", "advanced"), 
  stringsAsFactors = F
)

# ui1 <- function(){
#   tagList(
#     div(id = "login",
#         # wellPanel(
#         #   tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
#         #   textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
#         #   passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
#         #   br(),
#         #   div(
#         #     style = "text-align: center;",
#         #     actionButton("Login", "Log in", style = "color: white; background-color:#3c8dbc;
#         #                  padding: 10px 15px; width: 150px; cursor: pointer;
#         #                  font-size: 18px; font-weight: 600;"),
#         #     shinyjs::hidden(
#         #       div(id = "nomatch1",
#         #           tags$p("Oops! Incorrect username or password!",
#         #                  style = "color: red; font-weight: 600; 
#         #                  padding-top: 5px;font-size:16px;", 
#         #                  class = "text-center"))),
#         #     br(),
#         #     br(),
#         #     tags$code("Username: myuser  Password: mypass"),
#         #     br(),
#         #     tags$code("Username: myuser1  Password: mypass1")
#         #     ))
#         wellPanel(textInput("userName", "Username"),
#                   passwordInput("passwd", "Password"),
#                   br(),
#                   # div(
#                   #   style = "text-align: center;",
#                   #   actionButton("Login", "Log in", style = "color: white; background-color:#3c8dbc;
#                   #                 padding: 10px 15px; width: 150px; cursor: pointer;
#                   #                 font-size: 18px; font-weight: 600;"),
#                   #   shinyjs::hidden(
#                   #     div(id = "nomatch",
#                   #         tags$p("Oops! Incorrect username or password!",
#                   #                style = "color: red; font-weight: 600;
#                   #                 padding-top: 5px;font-size:16px;",
#                   #                class = "text-center"))),
#                   #   br(),
#                   #   br(),
#                   #   tags$code("Username: myuser  Password: mypass"),
#                   #   br(),
#                   #   tags$code("Username: myuser1  Password: mypass1")
#                   # )
# 
#                    actionButton("Login", "Log in")
#                   )#final wellPanel
#         #shinyjs::hidden(
#         #   div(id = "nomatch",
#         #       tags$p("Oops! Incorrect username or password!",
#         #              style = "color: red; font-weight: 600; 
#         #                           padding-top: 5px;font-size:16px;", 
#         #              class = "text-center")))
#         
#         
#         ),
#     tags$style(type="text/css", "#login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
#   )}

ui1 <- function(){
  tagList(div(id = "login", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
           wellPanel(
             shinyjs::useShinyjs(),
             tags$h2("Autenticación", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
             textInput("userName", placeholder="Usuario", label = tagList(icon("user"), "Usuario")),
             passwordInput("passwd", placeholder="Contraseña", label = tagList(icon("unlock-alt"), "Contraseña")),
             br(),
             div(
               style = "text-align: center;",
               actionButton("Login", "Ingresar", style = "color: white; background-color:#3c8dbc;
                                  padding: 10px 15px; width: 150px; cursor: pointer;
                                  font-size: 18px; font-weight: 600;"),
               shinyjs::hidden(
                 div(id = "nomatch1",
                     tags$p("¡Contraseña errónea!",
                            style = "color: red; font-weight: 600; 
                                  padding-top: 5px;font-size:16px;", 
                            class = "text-center"))),
               br(),
               br(),
               tags$code("Usuario: test  Contraseña: test")
             ))
))}

ui2 <- function(){tagList(tabPanel("Test"))}

ui = (htmlOutput("page"))

server = (function(input, output,session) {
  
  Login = FALSE
  USER <- reactiveValues(Login = Login)
  
  # observe({ 
  #   if (USER$Logged == FALSE) {
  #     if (!is.null(input$Login)) {
  #       if (input$Login > 0) {
  #         Username <- isolate(input$userName)
  #         Password <- isolate(input$passwd)
  #         Id.username <- which(my_username == Username)
  #         Id.password <- which(my_password == Password)
  #         if (length(Id.username) > 0 & length(Id.password) > 0) {
  #           if (Id.username == Id.password) {
  #             USER$Logged <- TRUE
  #           } else {
  #             shinyjs::toggle(id = "nomatch1", anim = TRUE, time = 1, animType = "fade")
  #             shinyjs::delay(3000, shinyjs::toggle(id = "nomatch1", anim = TRUE, time = 1, animType = "fade"))
  #           }
  #         }else {
  #           shinyjs::toggle(id = "nomatch1", anim = TRUE, time = 1, animType = "fade")
  #           shinyjs::delay(3000, shinyjs::toggle(id = "nomatch1", anim = TRUE, time = 1, animType = "fade"))
  #         }
  #         
  #       } 
  #     }
  #   }    
  # })
  observe({ 
    if (USER$Login == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          #Id.username <- which(my_username == Username)
          #Id.password <- which(my_password == Password)
          # if (length(Id.username) > 0 & length(Id.password) > 0) {
          #   if (Id.username == Id.password) {
          #     USER$Logged <- TRUE
          #   } else {
          #     shinyjs::toggle(id = "nomatch1", anim = TRUE, time = 1, animType = "fade")
          #     shinyjs::delay(3000, shinyjs::toggle(id = "nomatch1", anim = TRUE, time = 1, animType = "fade"))
          #   }
          # }else {
          #   shinyjs::toggle(id = "nomatch1", anim = TRUE, time = 1, animType = "fade")
          #   shinyjs::delay(3000, shinyjs::toggle(id = "nomatch1", anim = TRUE, time = 1, animType = "fade"))
          # }
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$Login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch1", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch1", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch1", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch1", anim = TRUE, time = 1, animType = "fade"))
          }
          
          
          
        } 
      }
    }    
  })
  
  
  observe({
    if (USER$Login == FALSE) {
      
      output$page <- renderUI({
        div(class="outer",do.call(bootstrapPage,c("",ui1())))
      })
    }
    if (USER$Login == TRUE) 
    {
      output$page <- renderUI({
        div(class="outer",do.call(navbarPage,c(inverse=TRUE,title = "Contratulations you got in!",ui2())))
      })
      #print(ui)
    }
  })
})

runApp(list(ui = ui, server = server))