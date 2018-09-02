library(shiny)
library(tidyverse)
library(DT)
library(readr)
library(sqldf)
library(igraph)

ui <- fluidPage(
   
  fluidPage( tabsetPanel(
    tabPanel("Estructura familiar", 
             sidebarLayout(sidebarPanel = verticalLayout(textInput("families", "Familia:", "0000"),
                                                         uiOutput("campSelector"),
                                                         textOutput("result")), 
                           mainPanel = plotOutput("plot", width="100%", height = "600px"))),
    tabPanel("Regresión lineal", "contents"),
    tabPanel("Formulario", "contents")))
)

server <- function(input, output) {
   # Se carga la BD
   DataBase <- read_csv("CompisicionFinal.csv")
   
   # Select input
   queryFamilies=sqldf("SELECT LLAVEHOG FROM DataBase group by LLAVEHOG")
   IdsFamilies <- c("0000")
   for (row in 1:nrow(queryFamilies)) {
     IdsFamilies <- c(IdsFamilies, queryFamilies[row, 'LLAVEHOG'])
   }
   output$campSelector <- renderUI({
     selectInput("familiesSelector", "Familias", IdsFamilies) 
   })
   
   #Evento de cuando cambia el campo de texto
   observeEvent(input$families, {
     familyQuery$data <- input$families
     # familyQuery$data <- input$familiesSelector Si se va a usar el selector quitar este comentario
     output$result <- renderText({""})
   })
   familyQuery <- reactiveValues(data = NULL)
   
   # Grafica de la estructura de la familia
   output$plot <- renderPlot({
     if(is.null(familyQuery$data) || familyQuery$data == '0000'){
       return()
     }else{
       query=paste("SELECT * FROM DataBase WHERE LLAVEHOG = '",familyQuery$data,sep="")
       query=paste(query,"'",sep="")
       Family <- sqldf(query)
       totalRows = nrow(Family)
       if(totalRows == 0){
         output$result <- renderText({ 
           paste("No se encontraron resultados con el número de familia ", familyQuery$data) 
           })
       }else{
         edges = c()
         for (row in 1:nrow(Family)){
           parentesco <- Family[row, 'P6051']
           if(parentesco == 1){
             next
           }
           
           Padre <- Family[row, 'P6081']
           Madre <- Family[row, 'P6083']
           Sexo <- Family[row, 'P6020']
           Conyuge <- Family[row, 'P6071']
           
           if(parentesco == 2 || parentesco == 3 || parentesco == 5 || parentesco == 7 || parentesco == 9){
             edges <- c(edges, row)
             edges <- c(edges, 1)
             if(parentesco == 2){
               edges <- c(edges, 1)
               edges <- c(edges, row)
             }
           }
           
           if(Padre == 1 && Family[row, 'P6081S1'] != 1){
             edges <- c(edges, row)
             edges <- c(edges, Family[row, 'P6081S1'])
           }
           
           if(Madre == 1 && Family[row, 'P6083S1'] != 1){
             edges <- c(edges, row)
             edges <- c(edges, Family[row, 'P6083S1'])
           }
           
           if(Conyuge == 1 && Family[row, 'P6071S1'] != 1 && Family[row, 'P6071S1'] > row){
             edges <- c(edges, row)
             edges <- c(edges, Family[row, 'P6071S1'])
             edges <- c(edges, Family[row, 'P6071S1'])
             edges <- c(edges, row)
           }
         }
         g1 <- graph( edges=edges, n= nrow(Family))
         plot(g1, edge.arrow.size =.05)
       }
     }
   })
}

shinyApp(ui = ui, server = server)

