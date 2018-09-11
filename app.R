library(shiny)
library(visNetwork)
library(sqldf)
library(readr)
library(DT)
library(dplyr)


ui <- fluidPage(align="center",
  titlePanel("Calidad de vida de una madre soltera"),
  tabsetPanel(
  tabPanel("Estructura del hogar", 
           sidebarLayout(sidebarPanel(verticalLayout(dataTableOutput("table")),                                              renderTable('table')), 
                           mainPanel(verticalLayout(visNetworkOutput("network"),
                                                    dataTableOutput("familia")),
                                     dataTableOutput("table2"))
                                      )),
  
  tabPanel("Formulario", fluidRow(
              selectInput("estrato", "Estrato",
                          list(`Estrato` = c(" ","1", "2", "3","4","5","6","8"))
              ),
              selectInput("tipoCon", "Tipo contrato",
                          list(`Contrato` = c("", "Formal", "Informal"))
              ),
              column(12,align="center",actionButton("regres", "Enviar")),
              column(12,align="center",textOutput("satisfaccion"))
    ))
)
)

server <- function(input, output) {
  #Load DB
  DataBase <- read_csv("BD/EstructuraFamiliar.csv")
  distinct_(DataBase, "LLAVEHOG", "ORDEN")
  
  familyQuery <- reactiveValues(data = NULL)
  
  queryFamilies=sqldf("SELECT LLAVEHOG FROM DataBase group by LLAVEHOG")
  output$table <- renderDataTable(queryFamilies, 
                                  selection = 'single')
  
  ########### Tabla de los valores de la madre #######
  #Load BDTodasColumnas
  
  DataBase2 <- read_csv("BD/BDTodasColumnas.csv")
  
  distinct_(DataBase2, "LLAVEHOG", "ORDEN")
  
  query=paste("SELECT TipoContrato, Estrato FROM DataBase2 WHERE LLAVEHOG = '",familyQuery$data,sep="")
  
  
  
  
  
  ############ Formulario ###########
  observeEvent(input$regres, {
    contrato = 0
    estrato2 = 0
    estrato5 = 0
    estrato6 = 0
    if(input$tipoCon == "Formal"){
      contrato = 2
    }else if(input$tipoCon == "Informal"){
      contrato = 1
    }
    if(input$estrato == "2"){
      estrato2 = 1
    }else if(input$estrato == "5"){
      estrato5 = 1
    }else if(input$estrato == "6"){
      estrato6 = 1
    }
    satisfaccion <- 8.38 + 0.3020*estrato2 + 1.5474*estrato5 + 1.9936*estrato6-0.4477*contrato-0.4941*contrato
    resultado = paste("El nivel de satisfaccion de la mujer es:", satisfaccion, sep=" ")
    output$satisfaccion <- renderText({
      resultado
    })
  })
  
  observeEvent(input$table_cell_clicked, {
    familyQuery$data <- input$table_cell_clicked
  })
  
  ######### RED  ######
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

