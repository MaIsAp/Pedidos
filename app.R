library(shiny)
library(dplyr)
library(DT)
library(png)
library(googlesheets)

data1<-read.csv("ListadoInvest.csv", header=T , fileEncoding="utf-8")
data2<-read.csv("ListadoSabor.csv", header=T , fileEncoding="utf-8")
readRDS("auth.rds")
Ped<-gs_title("Pedidos")

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
  )
)

# Define server logic ----
server <- function(input, output) {
    output$table <- DT::renderDataTable(df())
  df <- eventReactive(input$submit,{
    data<-data.frame(Hora=as.character(as.POSIXct(Sys.time(), tz="America/Bogota")),
                     Investigador=input$inv,
                     Op1=input$sab1,
                     Cant1=input$num1,
                     Op2=input$sab2,
                     Op3=input$sab3)
     
    if(nrow(Ped %>% gs_read(ws="Hoja 1"))==0){
      Ped <- Ped %>% 
        gs_edit_cells(ws = "Hoja 1", input = data[1,], trim = TRUE)
    }else{
      Ped <- Ped %>% 
        gs_add_row(ws = "Hoja 1", input = data)
    }
    
    Ped_df<<-Ped %>% gs_read(ws="Hoja 1")
    Ped_df
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
    tabla_res<-Ped_df %>% group_by(Op1) %>% summarise(n=sum(Cant1))
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
      write.csv(Ped_df, file, row.names = FALSE,fileEncoding = "utf-8")
    }
  )
  
}

# Run the app ----
shinyApp(ui = ui, server = server)