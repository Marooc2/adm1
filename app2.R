#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#install.packages(c("shiny","ggplot2","dplyr","XML","dplyr","jsonlite"))

library(shiny)
library(ggplot2)
library(XML)
library(dplyr)
library(jsonlite)

# Define UI for application that draws a histogram
{ui <- fluidPage(
  tabsetPanel(
    tabPanel("Lectura de Datos",
             titlePanel("Carga datos: "),
             sidebarLayout(
               sidebarPanel(
                 h1("Se cargo correctamente")
               ),
               mainPanel(
                 dataTableOutput("tablainput") 
                 )
               )
             ),
    
    tabPanel("Consultas",
             titlePanel("Consultas: "),
             sidebarLayout(
               sidebarPanel(
                 radioButtons("consulta","Seleccione consulta: ",
                              choices = c("ninguna",
                                          "Consulta 1"="c1"))
               ),
               mainPanel(
                 dataTableOutput("consultas")
                 )
               )
             ),
    
    tabPanel("Muestreo",
             titlePanel("Muestreo: "),
             sidebarLayout(
               sidebarPanel(
                 
               ),
               mainPanel(
                 dataTableOutput("muestreo")
                 )
               )
             ),
    
    tabPanel("Modelo",
             h1("Modelo regresional"),
               plotOutput("modelo")
             ),
    
    tabPanel("Visualizaciones",
             tabsetPanel(
               tabPanel("Visualizacion 1",
                        sidebarLayout(
                          sidebarPanel(
                            titlePanel("Panel de control"),
                            h5("Controles"),
                            sliderInput("var1","Primeros 20 alumnos",
                                        min = 0, max = 20,step=1, value=c(1,5))
                          ),
                          mainPanel(
                            h1("Salida de datos"),
                            plotOutput("grafico1")
                          )
                        )
               ),
               
               tabPanel("Visualizacion 2",
                        sidebarLayout(
                          sidebarPanel(
                            titlePanel("Panel de control"),
                            sliderInput("var2","Edades:", 
                                        min = 15, max = 25,step=1, value=c(15,17))
                          ),
                          mainPanel(
                            h1("Salida de datos"),
                            plotOutput("grafico2")
                          )
                        )
               ),
               
               tabPanel("Visualizacion 3",
                        sidebarLayout(
                          sidebarPanel(
                            titlePanel("Panel de control"),
                            sliderInput("var3", "Estudiante : ",min = 0, max = 50,step=1,value=c(1,5))),
                          mainPanel(
                            h1("Salida de datos"),
                            plotOutput("grafico3")
                            )
                          )
                        )
               )
             ),
    tabPanel("Knn",
             sidebarLayout(
               sidebarPanel(
                 titlePanel("Hola")
               ),
               mainPanel(
                 plotOutput("kmean")
               )
             )
             )
    )
  )
}

# Define server logic required to draw a histogram
server <- function(input, output, session) 
  {
  
  # Lectura
  
  output$tablainput <- renderDataTable({
    
    df <- read.csv("Consultas/dataset.csv",header = T,sep = ",")
    
  })
  
  #Consultas
  
  output$consultas <- renderDataTable({
    
    if(input$consulta =="ninguna")
      return(df)
    
    if(input$consulta == "c1"){
      dfc1<-subset(df, age > 18, select = c(sex, age, address)) %>%
        filter(sex == "F", address == "U")
      return(dfc1)
    }
    
    if(input$consulta == "c2"){
      EMM10<-subset(df, G1.x < 10, select = c(sex,G1.x))%>%
        filter(sex == "M")
      return(EMM10)
    }
    
    if(input$consulta == "c3"){
      CEPM<- df %>% select(G1.x) %>% count(G1.x)
      return(CEPM)
    }
    
    if(input$consulta =="c4"){
      EFNA<- subset(df, G2.y > 10, select = c(sex,G1.x,school))%>%
        filter(sex == "M",school == "MS")
      return(EFNA)
    }
    
    if(input$consulta =="c5"){
      MH<-subset(df, age < 18, select = c(sex, age, address, famsize)) %>%
        filter(sex == "M", address == "R", famsize == "GT3")
      return(MH)
    }
    
     if(input$consulta =="c6"){
       MH2<-subset(df, select = c(school, age, Pstatus, guardian.x)) %>%
         filter(school == "GP", Pstatus == "A")
      return(MH2)
     }
    
    if(input$consulta =="c7"){
      PN<-subset(df, select = c(school, sex, age, Medu, Fedu)) %>%
        filter(school == "GP", Fedu == "1", Medu == '1')
      return(PN)
    }
    
    if(input$consulta =="c8"){
      MFJ<-subset(df, select = c(school, sex, age, Mjob, Fjob)) %>%
        filter(school == "MS", Mjob == "at_home", Fjob == "services")
      return(MFJ)
    }
    
    if(input$consulta =="c9"){
      MFJ2<-subset(df, age < 19, select = c(school, sex, age, Pstatus, guardian.x, guardian.y)) %>%
        filter(school == "MS", Pstatus == "T", guardian.x == "mother", guardian.y == "mother")
      return(MFJ2)
    }
    
    if(input$consulta =="c10"){
      SPC <- subset(df, select = c(school, sex, age, paid.x, famsup.x)) %>%
        filter(famsup.x == "yes", paid.x == "yes")
      return(SPC)
    }
    
    if(input$consulta =="c11"){
      EMM10<-subset(df, G1.x < 10, select = c(sex,G1.x))%>%filter(sex == "M")
      return(EMM10)
    }
    
    if(input$consulta =="c12"){
      CEPM<- df %>% select(G1.x) %>% count(G1.x)
      return(CEPM)
      
    }
    
    if(input$consulta =="c13"){
      EFNA<- subset(df, G2.y > 10, select = c(sex,G1.x,school))%>%filter(sex == "M",school == "MS")
      return(EFNA)
    }
    
    
  })
  
  #regresion
  
  output$modelo <- renderPlot({
    
    regresion <- lm(G1.x ~ G2.x , data = df)
    plot(df$G1.x, df$G2.x, xlab = "Notas primer periodo mate", ylab = "Notas segundo periodo mate")
    abline(regresion)
  })
  
  #Graficos
  
  output$grafico1 <- renderPlot({
    
    df = df[,c("X","traveltime.x")]
    dat <- reactive({
      test <- df[df$X %in% seq(from=min(input$var1),to=max(input$var1),by=1),]
    })
    ggplot(dat(),aes(x=X,y=traveltime.x,fill=traveltime.x))+geom_bar(stat="identity")+
      xlab("Alumno") + ylab("Tiempo en que demora en llegar al colegio")
  },height = 400,width = 600)
  
  output$grafico2 <- renderPlot({
    
    df = df[,c("age","G3.y")]
    dat <- reactive({
      test <- df[df$age %in% seq(from=min(input$var2),to=max(input$var2),by=1),]
    })
    ggplot(dat(),aes(x=age,y=G3.y))+geom_point(colour='blue')+ xlab("Edades") +
      ylab("Nota en el curso de portugues tercer periodo")
  },height = 400,width = 600)
   
  output$grafico3 <- renderPlot({
     
     df = df[,c("X","G3.y")]
     dat <- reactive({
       test <- df[df$X %in% seq(from=min(input$var3),to=max(input$var3),by=1),]
     })
     ggplot(dat(),aes(x=X,y=G3.y))+geom_point(colour='red')+ xlab("Alumno") +
       ylab("Nota en el curso de portugues tercer periodo")
   },height = 400,width = 600)
  
  #muestreo
  
  #knn
  
}

shinyApp(ui = ui, server = server)


