library(shiny)
library(dplyr)
library(DT)
library(png)
library(googlesheets)

data1<-read.csv("ListadoInvest.csv", header=T)
data2<-read.csv("ListadoSabor.csv", header=T)


ui <- fluidPage(
  titlePanel("Pedido"),
  fluidRow(
    column(4,
           selectInput("inv","Investigador:",c("",data1))),
    column(3,
           selectInput("sab1","Sabor Opcion 1:",c("",data2))),
    column(1,
           numericInput("num1", "Cant.:", value = 0)),
    column(2,
           selectInput("sab2","Sabor Opcion 2:",c("",data2))),
    column(2,
           selectInput("sab3","Sabor Opcion 3:",c("",data2)))
  ),
  fluidRow(
    column(6,imageOutput("dona")),
    column(6,actionButton("submit", label = "Enviar"))
  ),
  DT::dataTableOutput("table"),
  fluidRow(
    column(12,actionButton("submit2", label = "Crear tabla resumen"))
  ),
  DT::dataTableOutput("resumen"),
  fluidRow(
    column(6,downloadButton("downloadData", "Descargar"))
  ),
  conditionalPanel("input.num1 == 2019", column(6,actionButton("clear", label = "Borrar todo")))
  ###esta funcion habilita el borrado solo para un administrador, que escriba 2019 en el numero en 'Cant.'
)

# Define server logic ----
server <- function(input, output) {
  if(file.exists("salida.csv")){
    all_data<-read.csv2("salida.csv")
  }else{
    all_data<-data.frame(Investigador=character(),
                         Op1=character(),
                         Cant1=numeric(),
                         Op2=character(),
                         Cant2=numeric(),
                         Op3=character(),
                         Cant3=numeric())
  }

  output$table <- DT::renderDataTable(df())
  df <- eventReactive(input$submit,{
    data<-data.frame(Hora=as.character(as.POSIXct(Sys.time(), tz="America/Bogota")),
                     Investigador=input$inv,
                     Op1=input$sab1,
                     Cant1=input$num1,
                     Op2=input$sab2,
                     Op3=input$sab3)
     
    all_data<<-rbind(all_data,data)
    write.csv2(all_data,"salida.csv",row.names = F)
    
    all_data
  })

  foto<-eventReactive(input$sab1,{
    if (is.null(input$sab1)){
      return(NULL) 
    }else{
    }
  })
  
  output$dona<- renderImage({
    outfile <- tempfile(fileext='.png')
    
    # Generate the PNG
    png(outfile, width=400, height=400)
    plot(0, type='n', xlim=0:1, ylim=0:1,axes = FALSE,xlab = "", ylab = "")
    if (is.null(input$sab1) | input$sab1 == ""){
      qq<-readPNG("./fotos/Todas.png", TRUE)
      rasterImage(qq, 0, 0, 1, 1)
    }else{
      qq<-readPNG(paste0("./fotos/",input$sab1,".png"), TRUE)
      rasterImage(qq, 0, 0, 1, 1)
    }
   
    dev.off()
    
    # Return a list containing the filename
    list(src = outfile,
         contentType = 'image/png',
         width = 400,
         height = 400,
         alt = "jaja")
  }, deleteFile = TRUE)
  
  output$resumen <- DT::renderDataTable(res())
  res<-eventReactive(input$submit2,{
    tabla_res<-all_data %>% group_by(Op1) %>% summarise(n=sum(Cant1))
    tabla_res
  })
  observeEvent(input$clear,{
    output$table <- renderText({
    })
    system(paste("rm -f", "salida.csv"))
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("pedidos.csv", sep = "")
    },
    content = function(file) {
      write.csv(all_data, file, row.names = FALSE)
    }
  )
  
}

# Run the app ----
shinyApp(ui = ui, server = server)