#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
source("ortogonal.R")
library(shiny)
library(DT)

ui <- fluidPage(
   
  navbarPage("Orthogonal",
             navbarMenu("База даних", tabPanel(actionButton("save", label <- "Зберегти до бази даних")),tabPanel(actionButton("load", label <- "Отримати з бази даних")))),
             
   sidebarLayout(
      sidebarPanel(
        numericInput("n", label <- h3("Кількість факторів"), value <- 5, min <- 3, max <- 10),
        numericInput("m", label <- h3("Кількість рівнів"), value <- 5, min <- 3, max <- 10),
        #numericInput("m", label <- h3("Кількість відгуків"), value <- 2, min <- 1, max <- 2),
        textInput("y1", label <- h3("y1")),
        textInput("y2", label <- h3("y2")),
        actionButton("go", label <- "Почати")
        #downloadButton("downloadData", label <- "Зберегти план")
      ),
      
      mainPanel(
        DT::dataTableOutput("mytable"),
        h3("D-efficiency"),
        textOutput("value"),
        h3("Відтворюваність дослідів"),
        textOutput("valuemin"),
        textOutput("valuemin1"),
        h3("Коэфіцієнти рівняння"),
        textOutput("valuecoef"),
        textOutput("valuecoef1"),
        h3("Адекватність моделі"),
        textOutput("valueadek"),
        h3("Математична модель"),
        textOutput("valuemat"),
        textOutput("valuemat1")
        
      )
   )
)

server <- function(input, output) {
    table1=0
  observeEvent(input$go, {
    DK=calculateDesign(input$n,input$m,input$y1,input$y2)
    print(DK)
        output$mytable <- DT::renderDataTable({
        n <- input$n
        m <- input$m
        y <- input$y1
        y1 <- input$y2
        
        
        output$value <-renderText({as.character(DK[2])})
      data.frame(DK[1])
        },
      extensions = c('Buttons','Scroller'), 
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'print'),
        deferRender = TRUE,
        scrollY = 200,
        scroller = TRUE))
      
      if(input$y1!=""&&input$y2!=""){
        output$valuemin<-renderText({as.character(paste("Критерий G=",DK[5], "що менше табличного значення",DK[11], ". Відтворюваність дослідів підтверджено."))})
        
        output$valueadek<-renderText({as.character(paste("Критерій F = ",DK[12], ", що менше ніж табличне значення ",DK[13], ". Адекватність моделі підтверджена."))})
        
        output$valuemat<-renderText({as.character(DK[4])})
        output$valuecoef<-renderText({as.character(paste(DK[3]," незначимі:", DK[6]))})
      }
     table1<-DK[1]
  })
  observeEvent(input$save, {
    showNotification("Збережено")
  })
  data <- reactive(table1)
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Plan-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data, file)
    }
  )
  
  
}

# Run the application 
shinyApp(ui <- ui, server <- server)

