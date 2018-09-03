library(shiny)
library(visNetwork)
library(sqldf)
library(readr)

ui <- fluidPage(
  titlePanel("Calidad de vida de una madre soltera"),
  fluidPage( tabsetPanel(
    tabPanel("Estructura familiar", 
             sidebarLayout(sidebarPanel = verticalLayout(textInput("families", "Familia:", "0000"),
                                                         uiOutput("campSelector"),
                                                         textOutput("result")), 
                           mainPanel = visNetworkOutput("network"))),
    tabPanel("Regresión lineal", "contents"),
    tabPanel("Formulario", "contents")))
)

server <- function(input, output) {
  #Load DB
  DataBase <- read_csv("Caracteristicas y composicion del hogar.csv")
  
  # Select input
  queryFamilies=sqldf("SELECT LLAVEHOG FROM DataBase group by LLAVEHOG")
  IdsFamilies <- c("Nadie")
  for (row in 1:nrow(queryFamilies)) {
    IdsFamilies <- c(IdsFamilies, queryFamilies[row, 'LLAVEHOG'])
  }
  output$campSelector <- renderUI({
    selectInput("familiesSelector", "Familias", IdsFamilies) 
  })
  
  #Evento de cuando cambia el campo de texto
  #observeEvent(input$familiesSelector, { # Si se va a usar el selector quitar este comentario
  # familyQuery$data <- input$familiesSelector # Si se va a usar el selector quitar este comentario
  observeEvent(input$families, { # Si se va a usar el texto quitar este comentario
    familyQuery$data <- input$families # Si se va a usar el text quitar este comentario
    output$result <- renderText({""})
  })
  familyQuery <- reactiveValues(data = NULL)
  
  output$network <- renderVisNetwork({
    query=paste("SELECT * FROM DataBase WHERE LLAVEHOG = '",familyQuery$data,sep="")
    query=paste(query,"\' ORDER BY ORDEN",sep="")
    #Do query
    queryFamilies = sqldf(query)
    if(nrow(queryFamilies) == 0){
      output$result <- renderText({ 
        paste("No se encontraron resultados con el número de familia ", familyQuery$data) 
      })
      return()
    }else{
      Sex = c()
      edgesTo = c()
      edgesFrom = c()
      label = c()
      for (row in 1:nrow(queryFamilies)) {
        label = c(label, row)
        sex <- queryFamilies[row, 'P6020']
        relationship <- queryFamilies[row, 'P6051']
        father <- queryFamilies[row, 'P6081']
        mother <- queryFamilies[row, 'P6083']
        spouse <- queryFamilies[row, 'P6071']
        #Color node
        if(sex == 1){
          Sex = c(Sex, 'skyblue')
        }
        if(sex == 2){
          Sex = c(Sex, 'pink')
        }
        
        if(relationship == 1){
          next
        }
        
        # Relationship with home lead
        if(relationship == 2 || relationship == 3 || relationship == 5 || relationship == 7 || relationship == 9){
          edgesTo <- c(edgesTo, 1)
          edgesFrom <- c(edgesFrom, row)
          if(relationship == 2){
            edgesFrom <- c(edgesFrom, 1)
            edgesTo <- c(edgesTo, row)
          }
        }
        
        if(father == 1 && queryFamilies[row, 'P6081S1'] != 1){
          edgesFrom <- c(edgesFrom, row)
          edgesTo <- c(edgesTo, queryFamilies[row, 'P6081S1'])
        }
        
        if(mother == 1 && queryFamilies[row, 'P6083S1'] != 1){
          edgesFrom <- c(edgesFrom, row)
          edgesTo <- c(edgesTo, queryFamilies[row, 'P6083S1'])
        }
        
        if(spouse == 1 && queryFamilies[row, 'P6071S1'] != 1 && queryFamilies[row, 'P6071S1'] > row){
          edgesTo <- c(edgesTo, row)
          edgesFrom <- c(edgesFrom, queryFamilies[row, 'P6071S1'])
          edgesTo <- c(edgesTo, queryFamilies[row, 'P6071S1'])
          edgesFrom <- c(edgesFrom, row)
        }
      }
      
      # minimal example
      nodes <- data.frame(id = 1:nrow(queryFamilies), 
                          color = Sex,
                          label = label)
      edges <- data.frame(from = edgesFrom, to = edgesTo)
      
      visNetwork(nodes, edges, main = "Estructura familiar", height = "100%", width = "100%") %>%
        visEdges(arrows =list(to = list(enabled = TRUE, scaleFactor = 1.4)),
                 color = list(color = "black")) %>%
        visPhysics(solver = "forceAtlas2Based", 
                   forceAtlas2Based = list(gravitationalConstant = -50)) %>%
        visNodes(size = 35, font = '28px arial black') 
    }
  })
}

shinyApp(ui = ui, server = server)

