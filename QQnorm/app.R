ui<-fluidPage(
    titlePanel("Gráficas de Probabilidad Normal", windowTitle = "Gráficas de Probabilidad Normal"),
    fluidRow(
      column(2,
        selectInput("dist","Elija la función de densidad: ", choices =
                  list(Continuas = list("Normal", "Beta","Chi-cuadrada"))
                  )
      ),
      column(3,
             conditionalPanel(
               condition = "input.dist == 'Normal'",
               sliderInput("media", "Media: ", value = 0, min = -20, max = 20),
               sliderInput("desv", "Desviación estándar: ", value = 1,min=0.1,max=10)
             ),
             conditionalPanel(
               condition = "input.dist == 'Chi-cuadrada'",
               sliderInput("df", "Grados de libertad: ", value = 2, min = 2, step = 1,max =30)
             ),
             conditionalPanel(
               condition = "input.dist == 'Beta'",
               sliderInput("alfa","Alfa: ", value = 1,min=1, max=20),
               sliderInput("beta", "Beta: ", value = 1,min=1, max=20)
             ),
      ),
      column(3,
             sliderInput("m", 
                         "Tamaño de muestra: ", 
                         value = 10,
                         min = 5, 
                         max = 200),
             checkboxInput(inputId="linea",
                           label="Dibujar la qqline", 
                           value = F),
      )
    ),
    fluidRow(
      column(4,
             plotOutput("plot"),
             plotOutput("plotqq")
      ),
      column(4,
             plotOutput("plot2")
      )
    )
)
server<-function(input, output, session){
    dist<-reactive(input$dist)
    media<-reactive(input$media)
    sd<-reactive(input$desv)
    lambda<-reactive(input$lambda)
    n<-reactive(input$n)
    p<-reactive(input$p)
    alfa<-reactive(input$alfa)
    beta<-reactive(input$beta)
    df<-reactive(input$df)
    m<-reactive(input$m)
    linea <- reactive(input$linea)
    output$plot<-renderPlot({
      inf<-switch(dist(),"Normal"=media()-3*sd(),"Beta"=0,"Chi-cuadrada"=0)
      sup<-switch(dist(),"Normal"=media()+3*sd(),"Beta"=1,"Chi-cuadrada"=df()*2)
      puntos<-switch(dist(),"Normal"=seq(from=inf,to=sup,length.out=1000),"Beta"=seq(from=inf,to=sup,length.out=1000),
                     "Chi-cuadrada"=seq(from=inf,to=sup,length.out = 1000))
      Density<-switch(dist(),"Normal"=dnorm(puntos,media(),sd()),
                      "Beta"=dbeta(puntos,alfa(),beta()),
                      "Chi-cuadrada"=dchisq(puntos,df()))
      media<-switch(dist(),"Normal"=media(),"Beta"=alfa()/(alfa()+beta()),"Chi-cuadrada" = df())
      yl<<-c(0,4*max(Density)/3)
      plot(puntos,Density,type="l", col = "cyan3",lwd=2.5, xlab="Values",main=dist(),ylim=yl)
      abline(v = media, col="cyan2",lwd = 2, lty = 2)
      })
    output$plot2<-renderPlot({
      inf<-switch(dist(),"Normal"=media()-3*sd(),"Beta"=0,"Chi-cuadrada"=0)
      sup<-switch(dist(),"Normal"=media()+3*sd(),"Beta"=1,"Chi-cuadrada"=df()*2)
      puntos<-switch(dist(),"Normal"=seq(from=inf,to=sup,length.out=1000),"Beta"=seq(from=inf,to=sup,length.out=1000),
                     "Chi-cuadrada"=seq(from=inf,to=sup,length.out = 1000))
      Acumulada<-switch(dist(),"Normal"=pnorm(puntos,media(),sd()),
                        "Beta"=pbeta(puntos,alfa(),beta()),
                        "Chi-cuadrada"=pchisq(puntos,df()))
      media<-switch(dist(),"Normal"=media(),"Beta"=alfa()/(alfa()+beta()),"Chi-cuadrada" = df())      
      plot(puntos,Acumulada,type="l", col = "cyan3",lwd=2.5, xlab="Values",main=dist(),ylim=c(0,1))
      abline(v = media, col="cyan2",lwd = 2, lty = 2)
    })
    output$plotqq<-renderPlot({
      set.seed(12894141)
      Muestra<-switch(dist(),"Normal"=rnorm(m(),media(),sd()),
                        "Beta"=rbeta(m(),alfa(),beta()),
                        "Chi-cuadrada"=rchisq(m(),df()))
      qqnorm(Muestra)
      if (linea()) qqline(Muestra)
    })
}

shinyApp(ui, server)
