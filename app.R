library(shiny)
library(visNetwork)
library(sqldf)
library(readr)
library(DT)

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
  DataBase <- read_csv("Caracteristicas y composicion del hogar.csv")
  
  familyQuery <- reactiveValues(data = NULL)
  
  queryFamilies=sqldf("SELECT LLAVEHOG FROM DataBase group by LLAVEHOG")
  output$table <- renderDataTable(queryFamilies, 
                                  selection = 'single')
  
  observeEvent(input$table_cell_clicked, {
    familyQuery$data <- input$table_cell_clicked
  })
  
  
  output$network <- renderVisNetwork({
    query=paste("SELECT * FROM DataBase WHERE LLAVEHOG = '",familyQuery$data,sep="")
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
      for (row in 1:nrow(queryFamily)) {
        label = c(label, row)
        sex <- queryFamily[row, 'P6020']
        relationship <- queryFamily[row, 'P6051']
        father <- queryFamily[row, 'P6081']
        mother <- queryFamily[row, 'P6083']
        spouse <- queryFamily[row, 'P6071']
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
        
        if(father == 1 && queryFamily[row, 'P6081S1'] != 1){
          edgesFrom <- c(edgesFrom, row)
          edgesTo <- c(edgesTo, queryFamily[row, 'P6081S1'])
        }
        
        if(mother == 1 && queryFamily[row, 'P6083S1'] != 1){
          edgesFrom <- c(edgesFrom, row)
          edgesTo <- c(edgesTo, queryFamily[row, 'P6083S1'])
        }
        
        if(spouse == 1 && queryFamily[row, 'P6071S1'] != 1 && queryFamily[row, 'P6071S1'] > row){
          edgesTo <- c(edgesTo, row)
          edgesFrom <- c(edgesFrom, queryFamily[row, 'P6071S1'])
          edgesTo <- c(edgesTo, queryFamily[row, 'P6071S1'])
          edgesFrom <- c(edgesFrom, row)
        }
      }
      
      nodes <- data.frame(id = 1:nrow(queryFamily), 
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

