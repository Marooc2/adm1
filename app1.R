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
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Recoleccion de Datos",
             titlePanel("Carga un dataset"),
             sidebarLayout(
               sidebarPanel(
                 radioButtons('tipoarchivo', 'Elija la extension:',
                             c('.csv'="csv",'.tsv'="tsv",'.xml' ="xml",'.json'="json"),"csv"),
                 tags$hr(),
                 fileInput('file', 'Elige el archivo',
                           accept = c(
                             'text/csv',
                             'text/comma-separated-values',
                             'text/tab-separated-values',
                             'text/plain',
                             '.csv',
                             '.tsv',
                             '.xml',
                             '.json'
                           )
                 ),
                 tags$hr(),
                 checkboxInput('header', 'Header', TRUE),
                 radioButtons('sep', 'Separador',
                              c(Coma=',',
                                Tab='\t'),
                              ','),
                 tags$hr(),
                 radioButtons("disp", "Mostrar",
                              choices = c(Head = "head",
                                          All = "all"),
                              selected = "head")
                 
               ),
               mainPanel(
                 dataTableOutput('tablainput')
               )
             )
    ),
    
    tabPanel("Preparacion de los datos",
             
             sidebarLayout(
               sidebarPanel(
                 h4("Datos"),
                 
                 radioButtons('q', 'Quitar NA',
                              c(Si='a',
                                No=FALSE),
                              selected = 'a'),
                 radioButtons('n', 'Normalizar',
                              c(Si='c',
                                No=FALSE),selected = 'c')
               ),
               mainPanel(h4("Salida de datos"),
                         dataTableOutput("fi"))
               
             )
    ),
    
    tabPanel("Exploracion y consultas",
             titlePanel("Consultas: "),
             sidebarLayout(
               sidebarPanel(
                 selectInput('consulta',
                             "Selecciona consulta",
                             c("Ninguna"="ninguna"))
               ),
               mainPanel(
                 dataTableOutput("exploracion")
               )
             )
    ),
    
    tabPanel("modelo",
             h1("Modelo regresional"),
             fluidRow(
               column(2,
                      selectInput('modx',"Selecciona columna",c('G1.x','G2.x','G3.x'))
               ),
               column(2,
                      selectInput('mody',"Selecciona columna",c('G1.y','G2.y','G3.y'))
               ),
               plotOutput('modelo')
             )
    ),
    
    tabPanel("Visualizaciones",
             tabsetPanel(
               tabPanel("Visualizacion 1",
                        sidebarLayout(
                          sidebarPanel(
                            titlePanel("Panel de control"),
                            h5("Controles"),
                            sliderInput("idSlider1","Primeros 20 alumnos", min = 0, max = 20,step=1, value=c(1,5))
                          ),
                          mainPanel(
                            h1("Salida de datos"),
                            plotOutput("idSalida1")
                          )
                        )),
               tabPanel("Visualizacion 2",
                        sidebarLayout(
                          sidebarPanel(
                            titlePanel("Panel de control"),
                            h5("Controles"),
                            sliderInput("ages","Edades:", min = 15, max = 25,step=1, value=c(15,17))
                          ),
                          mainPanel(
                            h1("Salida de datos"),
                            plotOutput("idSalida2")
                          )
                        )),
               tabPanel("Visualizacion 3",
                        sidebarLayout(
                          
                          sidebarPanel(
                            titlePanel("Panel de control"),
                            h5("Controles"),
                            sliderInput("equis", "Estudiante : ",min = 0, max = 50,step=1,value=c(1,5))),
                          mainPanel(h1("Salida de datos"),
                                    plotOutput("idSalida4")))
               )  
               
             )
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output, session) 
  {
  
  output$tablainput <- renderDataTable({
      
    inFile <- input$file
    
    if (is.null(inFile))
      return(NULL)
    
    if(input$tipoarchivo == "csv"){
    df <- read.csv(inFile$datapath,header = input$header,
                     sep = input$sep)
       if(input$disp == "head")
         return(head(df))
        else
         return(df)
    }
    
    if(input$tipoarchivo =="xml"){
      df <- xmlToDataFrame(inFile$datapath)
      return(df)
      }

    if(input$tipoarchivo =="json"){
      df <- fromJSON(readLines(inFile$datapath))
      return(df)
    }
    
  })
  
  output$fi <- renderDataTable({
    
    inFile <- input$file
    
    if(input$q=='a'){
      
      NAs = function(df)
      {
        for(i in 1:NCOL(df))
        {
          df=filter(df, !is.na(df[,i]))
        }
        
        return(df)
      }
    }
    
    
    if(input$n=='c'){
      
      Normalizar = function(file)
      {
        Normalizar = function(df)
        {
          return(data.frame(scale(df, center = TRUE)))
        }
        Normalizar(dt)
      }
    }
    
    read.csv(inFile$datapath)
  })
  
  output$preparacion <- renderDataTable({
    
    inFile <- input$file
    
    if (is.null(inFile))
      return(NULL)
    
    df <- read.csv(inFile$datapath)
    
  })
  
  output$exploracion <- renderDataTable({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    
    df <- read.csv(inFile$datapath)
    
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
  
  #REGRESION
  
  output$modelo <- renderPlot({
  
    inFile <- input$file
    
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath)
    
    regresion <- lm(G1.x ~ G2.x , data = df)
    plot(df$G1.x, df$G2.x, xlab = "Notas primer periodo mate", ylab = "Notas segundo periodo mate")
    abline(regresion)
  })
  
  #Graficos
  
  output$idSalida1 <- renderPlot({
    
    inFile <- input$file
    
    if (is.null(inFile))
      return(NULL)
    
    df <- read.csv(inFile$datapath)
    
    df = df[,c("X","traveltime.x")]
    datt <- reactive({
      testt <- df[df$X %in% seq(from=min(input$idSlider1),to=max(input$idSlider1),by=1),]
    })
    ggplot(datt(),aes(x=X,y=traveltime.x,fill=traveltime.x))+ 
      xlab("Alumno") + 
      ylab("Tiempo en que demora en llegar al colegio")+geom_bar(stat="identity")
  },height = 400,width = 600)
  
  output$idSalida2 <- renderPlot({
    
    inFile <- input$file
    
    if (is.null(inFile))
      return(NULL)
    
    df <- read.csv(inFile$datapath)
    
    df = df[,c("age","G3.y")]
    dat <- reactive({
      test <- df[df$age %in% seq(from=min(input$ages),to=max(input$ages),by=1),]
    })
    ggplot(dat(),aes(x=age,y=G3.y))+geom_point(colour='blue')+ xlab("Edades") + ylab("Nota en el curso de portugues tercer periodo")
  },height = 400,width = 600)
   
  output$idSalida4 <- renderPlot({
     
     inFile <- input$file
     
     if (is.null(inFile))
       return(NULL)
     
     df <- read.csv(inFile$datapath)
     
     df = df[,c("X","G3.y")]
     dat <- reactive({
       test <- df[df$X %in% seq(from=min(input$equis),to=max(input$equis),by=1),]
     })
     ggplot(dat(),aes(x=X,y=G3.y))+geom_point(colour='red')+ xlab("Alumno") + ylab("Nota en el curso de portugues tercer periodo")
   },height = 400,width = 600)
  
  
  
  
  {
    x=c(1,3,13,12,16,14,18,13,12,16,13)
    y=c(16,12,13,14,15,12,16,11,13,18,17)
    dt = data.frame(x,y)
    
    
    #Imputacion
    Imputacion = function(df)
    {
      for(i in 1:NCOL(df))
      {
        df=filter(df, !is.na(df[,i]))
      }
      
      return(df)
    }
    #Normalizacion
    Normalizar = function(df)
    {
      return(data.frame(scale(df, center = FALSE)))
    }
    Normalizar(dt)
    }
}

shinyApp(ui = ui, server = server)


