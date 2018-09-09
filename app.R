library(shiny)
library(visNetwork)
library(sqldf)
library(readr)
library(DT)
library(dplyr)


ui <- fluidPage(
  titlePanel("Calidad de vida de una madre soltera"),
  tabsetPanel(
  tabPanel("Estructura del hogar", 
           sidebarLayout(sidebarPanel(verticalLayout(dataTableOutput("table")),                                              renderTable('table')), 
                           mainPanel(visNetworkOutput("network")))),
    tabPanel("Regresión lineal", "contents"),
    tabPanel("Formulario", "contents"))
)

server <- function(input, output) {
  #Load DB
  DataBase <- read_csv("BD/EstructuraFamiliar.csv")
  distinct_(DataBase, "LLAVEHOG", "ORDEN")
  
  familyQuery <- reactiveValues(data = NULL)
  
  queryFamilies=sqldf("SELECT LLAVEHOG FROM DataBase group by LLAVEHOG")
  output$table <- renderDataTable(queryFamilies, 
                                  selection = 'single')
  
  observeEvent(input$table_cell_clicked, {
    familyQuery$data <- input$table_cell_clicked
  })
  
  
  output$network <- renderVisNetwork({
    query=paste("SELECT DISTINCT * FROM DataBase WHERE LLAVEHOG = '",familyQuery$data,sep="")
    query=paste(query,"\' ORDER BY ORDEN",sep="")
    #Do query
    queryFamily = sqldf(query)
    if(nrow(queryFamily) == 0){
      output$result <- renderText({ 
        paste("No se encontraron resultados con el número de familia ", familyQuery$data) 
      })
      return()
    }else{
      Sex = c()
      edgesTo = c()
      edgesFrom = c()
      label = c()
      labelEdges = c() 
      for (row in 1:nrow(queryFamily)) {
        label = c(label, row)
        sex <- queryFamily[row, 'sexo']
        relationship <- queryFamily[row, 'parentesco']
        father <- queryFamily[row, 'padre']
        mother <- queryFamily[row, 'madre']
        spouse <- queryFamily[row, 'conyugue']
        
        #Color node
        if(sex == 1){
          Sex = c(Sex, 'skyblue')
        }
        if(sex == 2){
          Sex = c(Sex, 'pink')
        }
        
        # Relationship with home lead
        if(relationship == 7){
          labelEdges <- c(labelEdges, '')
          edgesTo <- c(edgesTo, 1)
          edgesFrom <- c(edgesFrom, row)
        }
        
        if(father == 1){
          labelEdges <- c(labelEdges, '')
          edgesFrom <- c(edgesFrom, row)
          edgesTo <- c(edgesTo, queryFamily[row, 'padreNo'])
        }
        
        if(mother == 1){
          labelEdges <- c(labelEdges, 'Madre')
          edgesFrom <- c(edgesFrom, row)
          edgesTo <- c(edgesTo, queryFamily[row, 'madreNo'])
        }
        
        if(spouse == 1 ){
          labelEdges <- c(labelEdges, '')
          edgesTo <- c(edgesTo, row)
          edgesFrom <- c(edgesFrom, queryFamily[row, 'conyugueNo'])
        }
        
      }
      
      nodes <- data.frame(id = 1:nrow(queryFamily), 
                          color = Sex,
                          label = label)
      edges <- data.frame(from = edgesFrom, to = edgesTo, label = labelEdges)
      
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

