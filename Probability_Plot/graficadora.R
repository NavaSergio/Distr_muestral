ui<-fluidPage(
    titlePanel(":)", windowTitle = "Gráficas de Probabilidad Normal"),
    fluidRow(
      column(2,
        selectInput("dist","Choose the probability distribution: ", choices =
                  list(Continuas = list("Normal", "Beta","Chi-cuadrado"))
                  )
      ),
      column(2,
             conditionalPanel(
               condition = "input.dist == 'Normal'",
               sliderInput("media", 
                           "Mean: ", 
                           value = 0,
                           min = -20, 
                           max = 20),
               sliderInput("desv", "Standard deviation: ", value = 1,min=0.1,max=10)
             ),
             conditionalPanel(
               condition = "input.dist == 'Chi-cuadrado'",
               sliderInput("df", "Degrees of freedom: ", value = 2, min = 2, step = 1,max =30)
             ),
             conditionalPanel(
               condition = "input.dist == 'Beta'",
               sliderInput("alfa","Alpha: ", value = 1,min=1, max=20),
               sliderInput("beta", "Beta: ", value = 1,min=1, max=20)
             ),
      )
    ),
    fluidRow(
      column(6,
             plotOutput("plot"),
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
    output$plot<-renderPlot({
      
      inf<-switch(dist(),"Normal"=media()-3*sd(),"Beta"=0,"Chi-cuadrado"=0)
      sup<-switch(dist(),"Normal"=media()+3*sd(),"Beta"=1,"Chi-cuadrado"=df()*2)
      puntos<-switch(dist(),"Normal"=seq(from=inf,to=sup,length.out=1000),"Beta"=seq(from=inf,to=sup,length.out=1000),
                     "Chi-cuadrado"=seq(from=inf,to=sup,length.out = 1000))
      Density<-switch(dist(),"Normal"=dnorm(puntos,media(),sd()),
                      "Beta"=dbeta(puntos,alfa(),beta()),
                      "Chi-cuadrado"=dchisq(puntos,df()))
      Acumulada<-switch(dist(),"Normal"=pnorm(puntos,media(),sd()),
                      "Beta"=pbeta(puntos,alfa(),beta()),
                      "Chi-cuadrado"=pchisq(puntos,df()))
      
      media<-switch(dist(),"Normal"=media(),"Beta"=alfa()/(alfa()+beta()),"Chi-cuadrado" = df())
      yl<<-c(0,4*max(Density)/3)
      plot(puntos,Density,type="l", col = "cyan3",lwd=2.5, xlab="Values",main=dist(),ylim=yl)
      abline(v = media, col="cyan2",lwd = 2, lty = 2)
      })
    output$plot2<-renderPlot({
      inf<-switch(dist(),"Normal"=media()-3*sd(),"Beta"=0,"Chi-cuadrado"=0)
      sup<-switch(dist(),"Normal"=media()+3*sd(),"Beta"=1,"Chi-cuadrado"=df()*2)
      puntos<-switch(dist(),"Normal"=seq(from=inf,to=sup,length.out=1000),"Beta"=seq(from=inf,to=sup,length.out=1000),
                     "Chi-cuadrado"=seq(from=inf,to=sup,length.out = 1000))
      Density<-switch(dist(),"Normal"=dnorm(puntos,media(),sd()),
                      "Beta"=dbeta(puntos,alfa(),beta()),
                      "Chi-cuadrado"=dchisq(puntos,df()))
      Acumulada<-switch(dist(),"Normal"=pnorm(puntos,media(),sd()),
                        "Beta"=pbeta(puntos,alfa(),beta()),
                        "Chi-cuadrado"=pchisq(puntos,df()))
      
      media<-switch(dist(),"Normal"=media(),"Beta"=alfa()/(alfa()+beta()),"Chi-cuadrado" = df())      
      plot(puntos,Acumulada,type="l", col = "cyan3",lwd=2.5, xlab="Values",main=dist(),ylim=c(0,1))
      abline(v = media, col="cyan2",lwd = 2, lty = 2)
    })
}

shinyApp(ui, server)
