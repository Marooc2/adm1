#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#install.packages(c("shiny","ggplot2","dplyr"))

library(shiny)
library(ggplot2)
library(dplyr)
gsub()
{# Define UI for application that draws a histogram
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
  
  ids=sample(1:nrow(dt),nrow(dt)*0.8)
  dtentre = dt[ids,]
  nrow(dtentre)
  dtvali=dt[-ids,]
  ggplot(dtvali,aes(x=dtvali[,1],y=dtvali[,2],color=dtvali[,3])) + geom_point()
  
  #knn
  
  knn <- function(dt,x,y){
    p<-0
    dm<-(abs(x-dt[,1])+abs(y-dt[,2]))
    dt<-cbind(dt,dm)
    minimo<-min(dm)
    for (i in 1:nrow(dt)) {
      if(minimo==dt[i,4])
        p<-i
    }
    return(dt[p,3])
  }
  
}

shinyApp(ui = ui, server = server)

x<- sample(30:60,60,replace = T)
y<- sample(60:100,60,replace = T)
dt <- data.frame(x,y)

categoria <- function(dt,n){
  c<-c()
  for(i in 1:n){
    if(x[i]>29 & x[i]<40)
      c<-c(c,'A')
    else if(x[i]>39 & x[i]<50)
      c<-c(c,'B')
    else c<-c(c,'C')
  }
  dt<-cbind(dt,c)
  return(dt)
}
dt<-categoria(dt,dim(dt)[1])
ggplot(dt,aes(x=dt[,1],y=dt[,2],color=dt[,3])) + geom_point()


knn(dtentre,7,77)
knn(dtentre,40,77)
knn(dtentre,80,77)


x<-c(14,7,13,12,16,14,18,13,12,16,13)

y<-c(16,12,13,14,15,12,16,11,13,18,17)
dt=data.frame(x,y)
#dt es el dataframe de entrenamiento
#vx son los datos de validacion
#vy son los datos a predecir

vx=c(10,12,14)
regresion <-function(dt, vx){
  lista<-list()
  class(lista)
  x=dt$x
  y=dt$y
  sumX=sum(x)
  sumY=sum(y)
  prodXY=sum(x*y)
  promX=mean(x)
  dt$x2=x^2
  dt$y2=y^2
  x2=sum(x^2)
  y2=sum(y^2)
  
  promY=mean(y)
  #calculo de covarianza
  cov=(prodXY/length(x))-(promX*promY)
  #calculo de las desviacion estandar X, Y
  dx=sqrt((x2/length(x))-promX^2)
  dy=sqrt((y2/length(y))-promY^2)
  #coeficiente de correlacion de Pearson
  r=cov/(dx*dy)
  #generando recta regresional
  vy=promY+(cov/dx^2)*(vx-promX)
  lista[[1]]<-dt# dt entrenamiento
  lista[[2]]<-vx# datos de validacion
  lista[[3]]<-vy# datos predecidos
  lista[[4]]<-r
  
  #print(vy)
  return (lista)
}
View(regresion(dt, vx)[[1]])
regresion(dt, vx)[[2]]
regresion(dt, vx)[[3]]
regresion(dt, vx)[[4]]

}