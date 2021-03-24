#Installiere noetige Pakete
library(shiny)
library(quantmod)
library(compositions)
library(ggplot2)

shinyServer(function(input, output, session) {
  #input und Ziel Ordner festlegen
  setwd("C:/Users/Himel/OneDrive/Studium/Bachelor (Wirtschaftsmathematik - B Sc.)/7. Semester/Seminararbeit/Shiny/Chartser_GOOG")
  
  invisible(Sys.setlocale("LC_MESSAGES", "C"))
  invisible(Sys.setlocale("LC_TIME", "C"))
  
  
  #Aktuelle Zeit_Funktion
  output$Aktuelle_Zeit <- renderText({
    invalidateLater(100, session)
    paste("-------------------","Aktuelle Uhrzeit: ", Sys.time(),"--------------------")
  })
  
  #Funktion fuer Konfidenzintervall_Plot
  f <- function( x,n, n.draw, conf.level){
    
    mu <- mean(x)
    SD <- sqrt(var(x))
    set.seed(1000)
    Matriz = matrix(rnorm(n.draw * n, mu, SD), n)
    Konfidenz = function(x) t.test(x, conf.level = conf.level )$conf.int
    KKonfidenz_Intervall = apply(Matriz, 2, Konfidenz)
    plot(range(KKonfidenz_Intervall), c(0, 1 + n.draw), type = "n", xlab = "Intervallaenge",
         ylab = "Stichprobenumfang", main = "Konfidenzintervalle", xlim = c(-0.04,0.04))
    for (i in 1:n.draw) {
      
      if(KKonfidenz_Intervall[1, i]>mu ||  KKonfidenz_Intervall[2, i]<mu) {lines(KKonfidenz_Intervall[, i], rep(i, 2), lwd = 2, col = "red")}
      else{lines(KKonfidenz_Intervall[, i], rep(i, 2), lwd = 2, col = "blue")}
    }
    abline(v = mu, lwd = 2, lty = 2, col = "black")
    
  }
   
  #Funktion fuer Veranschaulichung von Ueberschreitungswahrscheinlichkeit:
  f1<- function(x,alpha, mu0){
    
    #stichprobenumfang:
    n <- length(x);n
    s0 <- sqrt(var(x)) #sigma bekannt
    
    #Gauss.test(): Funktion fuer Gauss test
    t <-Gauss.test(x, y = NULL, mean = mu0,
                   sd = s0, alternativ = "two.sided")
    
    p.value <-as.numeric(t$p.value)
    par(mfrow = c(1,1), bg = "white")
    curve(dnorm(x), from = -5, to = 5, ylab="f(x)", xlab=" ", 
          lwd=2, axes = F, col = "blue")
    
    legend("topright", inset=.08, title="Wahrscheinlichkeit",
           c("Aplha", "P_Value"), fill=c("blue","darkred"), ncol = 1,bg = 'lightblue')
    
    text(-3, 0.3,expression(f(x) == frac(1, sqrt(2*pi))* 
                              ~~ exp(- ~~ frac(1, 2)~x^2)), cex = 1.2)
    mtext(expression(paste("Hypothesentest: ",H[0],
                           ": ", mu == mu[0],~", ",alpha," (blau), ",
                           "P-Value  ",  "(rot)")), 
          cex = 1.2, line = 1, col = "darkblue")
    
    axis(2)
    lines(c(-6,140), c(0,0), col = "black", lwd= 2)
    xval <- seq(-5,5, 0.01)
    xarea <- c( xval,5)
    yarea <- c(dnorm(xval), 0)
    polygon(xarea, yarea, col = "darkgreen", angle=135, lwd = 2)
    lines(c(0,0), c(0,dnorm(0)), lwd = 2)
    
    #Abhelnungsbereich scharffieren:
    mu <- mu0
    sigma <- s0
    xval <- seq(qnorm(1 - (alpha/2)),5, 0.01)
    xarea <- c(qnorm(1 - (alpha/2)), xval)
    yarea <- c(0, dnorm(xval))
    polygon(xarea, yarea, col = "darkblue", angle=135)
    text(2.5, 0.07,expression(paste(frac(1,2),~ alpha)),
         col = "darkblue", cex = 1.2 )
    
    
    xval <- seq(-5, qnorm(alpha/2), 0.01)
    xarea <- c( xval, qnorm(alpha/2))
    yarea <- c(dnorm(xval), 0)
    polygon(xarea, yarea, col = "darkblue", angle=135)
    text(-2.5, 0.07,expression(paste(frac(1,2),~ alpha)),
         col = "darkblue" ,cex = 1.2)
    text(mu,-0.01, expression( mu ==0), cex = 1)
      
    #Bereich der uberschreitungswahrscheinlichkeit Scharffieren:
    xval <- seq(qnorm(p.value/2, lower.tail = F), 5, 0.01)
    xarea <- c(qnorm(p.value/2, lower.tail = F), xval)
    yarea <- c(0, dnorm(xval))
    polygon(xarea, yarea, col = "darkred",density=10,
            angle=-135, lwd = 2)
    polygon(xarea, yarea, col = "darkred",density=10,
            angle=135, lwd = 2)
    text(2, 0.17, expression(paste(frac(1,2),~ "p-Val")),
         col = "darkred", cex = 1.2)
    xval <- seq(-5,qnorm(p.value/2, lower.tail = T), 0.01)
    xarea <- c( xval,qnorm(p.value/2, lower.tail = T))
    yarea <- c(dnorm(xval), 0)
    polygon(xarea, yarea, col = "darkred", angle=135,
            density = 10, lwd = 2)
    polygon(xarea, yarea, col = "darkred", angle=-135,
            density = 10, lwd = 2)
    text(-1.95, 0.17, expression(paste(frac(1,2),~ "p-Val")),
         col = "darkred", cex = 1.2)
    
  }
  
  #Liste aller Schaltflaeche
  output$Liste <- renderUI({
    switch(input$tab,
           
           "Gauss_VS_t" = Gauss_VS_t,
           "Aktienkurs.csv" = Aktienkurs.csv,
           "Aktienkurs1" = Aktienkurs1,
           "KURS_Rendite_plot" = KURS_Rendite_plot,
           "Konfidenzintervall" = Konfidenzintervall,
           "t-Test(1-Sample)" = Hypotest,
           "p_Value" = p_Val,
           "Aktienkurs2" = Aktienkurs2,
           "RenditeVergleich Plot" = RenditeVergleich_Plot,
           "t-Test(2-Sample)" = Hypotest1,
           "Hilfe" = Hilfe
           
    )
  })
  
  #Benutzeroberflaeche fuer Gauss vs. t-Verteilung konfigurieren
  Gauss_VS_t <- div(
        numericInput("unter", div("Untere Grenze fuer X-Werte", style = "color:darkorange"), -4),
        numericInput("ober", div("Obere Grenze fuer X-Werte", style = "color:darkorange"), 4),
        br(),
        br(),
        br(),
        br(),
        sliderInput("n", div( "Anzahl der Stichprobe n:", style = "color:darkorange"),
                    min = 1,
                    max = 30,
                    value = 1,animate = animationOptions(interval = 300, loop = FALSE, playButton = NULL)),
        
        br(), br(),br(),br(),br(),br(),br(),br()
        
        )

#Benutzeroberflaeche zum Hochladen der CSV Datei konfigurieren
Aktienkurs.csv <- div(fileInput('Datei', div('Waehle eine CSV Datei aus', style = "color:darkorange"),
                                  accept=c('.csv'), buttonLabel = "Durchsuchen", placeholder = "keine Datei ausgewaehlt"),
                         br(),
                        radioButtons('Trenner', div("Trenner", style = "color:darkorange"),
                                     c(Komma=',',
                                       Semikolon=';',
                                       Tab='\t'),
                                     ';'),
                      br(),
                        numericInput("n", div("Anzahl der Stichprobe zum Zeigen:", style = "color:darkorange"), 18),
                      br(),br(), br(),br(),br(),br(),br(),br()
                      
                      
                        
                       )                     
  
 
  
#Benutzeroberflaeche fuer die Tabelle der Aktienkurse konfigurieren
  Aktienkurs1 <-  div(
    
    helpText(h4(div("Geben Sie eine gueltige Aktienname und Zeitdauer ein", style = "color:darkturquoise"))),
    helpText("Informationen stammen aus google/Yahoo finance."),
    dateRangeInput("dates", 
                   div("Zeitdauer",style = "color:darkorange"),
                   start = "2007-01-01", 
                   end = as.character(Sys.Date())),
    textInput("symb", div("Eingabe Wertpapiername ", style = "color:darkorange"), "AMZN"),
    radioButtons("source", div("Source:",style = "color:darkorange"),
                 c("YAHOO" = "yahoo",
                   "GOOG" = "google"
                 ), "yahoo") ,
    numericInput("obs", div("Anzahl der Stichprobe zum Zeigen:", style = "color:darkorange"), 15),
    br(),
    br(),
    tags$style(type='text/css', '#herunterladen_Stock {background-color: darkgreen; color: white;}'), 
    downloadButton('herunterladen_Stock', 'Herunterladen: Data')
    
    
  )
  
#Benutzeroberflaeche fuer Kurs-Rendite-Plot konfigurieren
KURS_Rendite_plot <- div( helpText(h4(div("Geben Sie eine gueltige Aktienname und Zeitdauer ein", style = "color:darkturquoise"))),
                            helpText("Informationen stammen aus google/Yahoo finance."),
                            
                            dateRangeInput("dates", 
                                           div("Zeitdauer",style = "color:darkorange"),
                                           start = "2007-01-01", 
                                           end = as.character(Sys.Date())),
                          br(),
                            textInput("symb", div("Gib Wertpapiername ein", style = "color:darkorange"), "AMZN"),
                          br(),
                          radioButtons("source", div("Source:",style = "color:darkorange"),
                                         c("YAHOO" = "yahoo",
                                           "GOOG" = "google"
                                         ), "yahoo") ,
                          br(),
                          br(),
                          
                          br(), br(),br(),br(),
                          tags$style(type='text/css', '#herunterladen_Plot{background-color: darkgreen; color: white;}'), 
                          downloadButton('herunterladen_Plot', 'Herunterladen: Plot 1'),
                          tags$style(type='text/css', '#herunterladen_Plot1{background-color: darkgreen; color: white;}'), 
                          downloadButton('herunterladen_Plot1', 'Herunterladen: Plot 2')
  )
   
#Benutzeroberflaeche fuer Konfidenzintervall konfigurieren
  Konfidenzintervall <- div(
    
    helpText(h4(div("Geben Sie eine gueltige Aktienname und Zeitdauer ein", style = "color:darkturquoise"))),
    helpText("Informationen stammen aus google/Yahoo finance."),
    textInput("symb", div("Gib Wertpapiername ein", style = "color:darkorange"), "AMZN"),
    dateRangeInput("dates", 
                   div("Zeitdauer",style = "color:darkorange"),
                   start = "2007-01-01", 
                   end = as.character(Sys.Date())),
    radioButtons("source", div("Source:",style = "color:darkorange"),
                 c("YAHOO" = "yahoo",
                   "GOOG" = "google"
                 ), "yahoo"),  
    
    sliderInput("bins",
                div("Anzahl der KI:", style = "color:darkorange"),
                min = 1,
                max = 100,
                value = 5, step = 1, animate=animationOptions(interval=100, loop=F)),  
    br(),
    
    sliderInput("Stichprobe",
                div("Stichprobenumfang:",style = "color:darkorange"),
                min = 1,
                max = 100,
                value = 5, step = 1,  animate=animationOptions(interval=100, loop=F)),
    br(), 
    br(),
    
    radioButtons("conf", div("Confidence level:", style = "color:darkorange"),
                 c("90%" = "a",
                   "95%" = "b",
                   "99%" = "c"))
  )   
  
  
  
  #Benutzeroberflaeche fuer die Tabelle des Hypothesentests konfigurieren
  Hypotest <- div(
    helpText(h4(div("Geben Sie eine gueltige Aktienname und Zeitdauer ein", style = "color:darkturquoise"))),
    helpText("Informationen stammen aus google/Yahoo finance."),
    textInput("symb", div(" Wertpapiername", style = "color:darkorange"), value = "AMZN"),
    dateRangeInput("dates", 
                   div("Zeitdauer", style = "color:darkorange"),
                   start = "2007-01-01", 
                   end = as.character(Sys.Date())),
    
    radioButtons("source", div("Source:",style = "color:darkorange"),
                 c("YAHOO" = "yahoo",
                   "GOOG" = "google"
                 ), "yahoo"), 
    
    numericInput("muh", div("Eingabe Erwartungswert", style = "color:darkorange"), value = 0.01),
    br(),
    radioButtons("conf", div("Alpha:", style = "color:darkorange"),
                 c("5%" = 0.05,
                   "1%" = 0.01,
                   "0.1%" = 0.001
                 )))
  
 #Benutzeroberflaeche fuer das Plot der Ueberschreitungswahrscheinlichkeit konfigurieren 
  p_Val <- div(
    
    helpText(h4(div("Geben Sie eine gueltige Aktienname und Zeitdauer ein", style = "color:darkturquoise"))),
    helpText("Informationen stammen aus google/Yahoo finance."),
    textInput("symb", div("Gib Wertpapiername ein", style = "color:darkorange"), "AMZN"),
    dateRangeInput("dates", 
                   div("Zeitdauer",style = "color:darkorange"),
                   start = "2007-01-01", 
                   end = as.character(Sys.Date())),
  
    radioButtons("source", div("Source:",style = "color:darkorange"),
                 c("YAHOO" = "yahoo",
                   "GOOG" = "google"
                 ), "yahoo"), 
    numericInput("muh", "giv mu", value = 0),
    sliderInput("alpha",
                div("Alpha:", style = "color:darkorange"),
                min = 0,
                max = 1,
                value = 0.05, step = 0.01, animate = animationOptions(interval = 200, loop = FALSE, playButton = NULL)))
  
  #Benutzeroberflaeche fuer die Tabelle der aktienkurse konfigurieren
  Aktienkurs2 <- div(
    helpText(h4(div("Geben Sie eine gueltige Aktienname und Zeitdauer ein", style = "color:darkturquoise"))),
    helpText("Informationen stammen aus google/Yahoo finance."),
    textInput("symb", div("Gib Wertpapiername ein", style = "color:darkorange"), "AMZN"),
    textInput("symb2", div("Gib Wertpapiername ein", style = "color:darkorange"), "FB"),
    dateRangeInput("dates", 
                   div("Zeitdauer",style = "color:darkorange"),
                   start = "2007-01-01", 
                   end = as.character(Sys.Date())),
    radioButtons("source", div("Source:",style = "color:darkorange"),
                 c("YAHOO" = "yahoo",
                   "GOOG" = "google"
                 ), "yahoo"), 
   
    numericInput("obs1", div("Number of observations to view:", style = "color:darkorange"), 15),
    br(),
    br(),
    tags$style(type='text/css', '#herunterladen_Stock{background-color: darkgreen; color: white;}'), 
    downloadButton('herunterladen_Stock', 'Herunterladen: Data 1'),
    tags$style(type='text/css', '#herunterladen_Stock1{background-color: darkgreen; color: white;}'), 
    downloadButton('herunterladen_Stock1', 'Herunterladen: Data 2')
  )
 
  #Benutzeroberflaeche der Rendite-Vergleivh-Plot konfigurieren
  RenditeVergleich_Plot <- div(
    
    helpText(h4(div("Geben Sie eine gueltige Aktienname und Zeitdauer ein", style = "color:darkturquoise"))),
    helpText("Informationen stammen aus google/Yahoo finance."),
    
    dateRangeInput("dates", 
                   div("Zeitdauer",style = "color:darkorange"),
                   start = "2007-01-01", 
                   end = as.character(Sys.Date())),
    br(),
    textInput("symb", div("Gib Wertpapiername ein", style = "color:darkorange"), "AMZN"),
    br(),
    textInput("symb2", div("Gib Wertpapiername ein", style = "color:darkorange"), "FB"),
    br(),
    radioButtons("source", div("Source:",style = "color:darkorange"),
                 c("YAHOO" = "yahoo",
                   "GOOG" = "google"
                 ), "yahoo") ,
    br(),
    
    tags$style(type='text/css', '#herunterladen_Plot1{background-color: darkgreen; color: white;}'),
    downloadButton('herunterladen_Plot1', 'Herunterladen: Plot 1'),
    tags$style(type='text/css', '#herunterladen_Plot2{background-color: darkgreen; color: white;}'), 
    downloadButton('herunterladen_Plot2', 'Herunterladen: Plot 2') 
  )
  
  #Benutzeroberflaeche fuer die Tabelle der Zweistichprobentest konfigurieren
  Hypotest1 <-div(
    helpText(h4(div("Geben Sie eine gueltige Aktienname und Zeitdauer ein", style = "color:darkturquoise"))),
    helpText("Informationen stammen aus google/Yahoo finance."),
    textInput("symb", div(" Wertpapiername", style = "color:darkorange"), value = "AMZN"),
    textInput("symb2", div(" Wertpapiername", style = "color:darkorange"), value = "FB"),
    br(),
    dateRangeInput("dates", 
                   div("Zeitdauer", style = "color:darkorange"),
                   start = "2007-01-01", 
                   end = as.character(Sys.Date())),
    br(),
    radioButtons("source", div("Source:",style = "color:darkorange"),
                 c("YAHOO" = "yahoo",
                   "GOOG" = "google"
                 ), "yahoo"),  
    br(),
    numericInput("muh", div("Eingabe Erwartungswert", style = "color:darkorange"), value = 0.01),
    br(),
    radioButtons("conf", div("Alpha:", style = "color:darkorange"),
                 c("5%" = 0.05,
                   "1%" = 0.01,
                   "0.1%" = 0.001
                 ))
  )
  
  #Die Quelle, die als Hilfe genommen wurden
  Hilfe <- div(     helpText(h5(div("Zur Erstellung dieses Apps wurden Die folgenden Quelle als Hilfe genommen", style = "color:darkturquoise"))),
    radioButtons("sourc", div("Quelle:",style = "color:darkorange"),
                             list("Shiny Tutorial" = "https://shiny.rstudio.com/tutorial/",
                               "An Introduction To R" = "https://cran.r-project.org/doc/manuals/r-release/R-intro.pdf",
                               "Shiny Package" = "https://cran.r-project.org/web/packages/shiny/shiny.pdf",
                               "Shiny Gallery" = "https://shiny.rstudio.com/gallery/"
                             ), selected = character(0)))
  
 #Aktienkurse aus dem Internet einlesen
Stock_Price <- reactive({
  getSymbols(input$symb, src = input$source, 
             from = input$dates[1],
             to = input$dates[2],
             auto.assign = FALSE)
  
})

#Aktienkurse aus dem Internet einlesen
Stock_Price1 <- reactive({
  getSymbols(input$symb2, src = input$source, 
             from = input$dates[1],
             to = input$dates[2],
             auto.assign = FALSE)
  
})

#Spalte der Schlusskurs
Input_stock <- reactive({
  
  data <- Stock_Price()
  close <- paste(input$symb,".Close",sep="")
  data <- data[, close, drop=F] 
})

#Spalte der Schlusskurs
Input_stock1 <- reactive({
  data <- Stock_Price1()
  close <- paste(input$symb2,".Close",sep="")
  data <- data[, close, drop=F]
})

#Aktien-Logrendite berechnen
Input_return <- reactive({
  obs <- Input_stock()
  obs <- as.numeric(na.omit(obs))
  obs <- diff(log(obs))
  obs <- data.frame(obs)
  obs <- obs[,1]
  obs <- obs[2:length(obs)]
})

#Aktien-Logrendite berechnen
Input_return1 <- reactive({
  obs <- Input_stock1()
  obs <- as.numeric(na.omit(obs))
  obs <- diff(log(obs))
  obs <- data.frame(obs)
  obs <- obs[,1]
  obs <- obs[2:length(obs)]
})

#Liste zweier Aktienkurse
Input_return2 <-reactive({
  obs <- list(Input_return(),Input_return1())
})

#Zusammengesetzte Tabelle der Aktienkurse und Aktienrendite
Stock_Price3 <-reactive({
  obs <- Input_stock()
  obs <- diff(log(obs))
  obs <- data.frame(obs)
  Return <- obs[,1]
  obs <-data.frame(Stock_Price(),Return)
  obs
})

runTTest <- reactive({
  obs <- Input_return()
  level <- 1-as.numeric(input$conf)
  test <- t.test(obs, conf.level=level, mu = input$muh) 
})
  
#Funktion der t-Test
runTTest1 <- reactive({
  obs <- Input_return2()
  level <- 1-as.numeric(input$conf)
  test <- t.test(obs[[1]], obs[[2]],var.equal = T, conf.level=level, mu = input$muh)
})

#Konfidenznivau festlegen
KI_Nivau <-reactive({
  
  switch(input$conf,
         a = 0.90,
         b = 0.95,
         c = 0.99,
         0.95)
})

#Plot zur Approximation der t-Verteilung durch Standard-Normalverteilung
output$Gauss_VS_t <- renderPlot({
  curve(dnorm(x), from= input$unter, to = input$ober, ylab = "f(x)", col = "blue",lwd =3, bty = "n", main = "Approximation von t-Verteilung durch standardnormalverteilung")
  
  curve(dt(x,df = input$n), from= input$unter, to = input$ober, col = "darkgreen", lwd = 3, add = TRUE)
  lines(c(0,0), c(0,0.4), lty = 2,lwd = 1)
  legend("topright", title = "Verteilung",inset=.11,
         legend = c("Normal", "t"), lwd = 5, 
         ncol = 1,  col = c("blue", "darkgreen"),bg = 'lightblue')
}, height = 540)

#CSV Datei Einlesen
output$Aktienkurs.csv <- renderTable({
  Datei <- input$Datei
  if (is.null(Datei))
    return(NULL)
  Datei <-read.csv(Datei$datapath,  sep=input$Trenner)
  head(Datei, input$n)
})

output$Aktienkurs1 <- renderTable({
  data <- Stock_Price3()
  head(data, n = input$obs)
})

#Datei Herunterladen
output$herunterladen_Stock <- downloadHandler(
  filename = function() { 
    paste(input$symb, '.csv', sep='')
  },
  content = function(file) {
    write.csv(Stock_Price(), file)
  }
)

output$herunterladen_Stock1 <- downloadHandler(
  filename = function() { 
    paste(input$symb2, '.csv', sep='')
  },
  
  content = function(file) {
    write.csv(Stock_Price1(), file)
  }
)

#Plot Herunterladen
output$herunterladen_Plot <- downloadHandler(
  filename = function() { paste(input$symb, '.png', sep='') },
  content = function(file) {
    png(file)
    chartSeries(Input_stock(),  name = input$symb)
    dev.off()  
  }
)

output$herunterladen_Plot1 <- downloadHandler(
  filename = function() { paste(input$symb, '.png', sep='') },
  content = function(file) {
    data <- Input_stock()
    data <-diff(log(data))
    png(file)
    chartSeries(data,  name = paste(input$symb,".Return"), up.col = "red")
    dev.off()  
  }
)

output$herunterladen_Plot2 <- downloadHandler(
  filename = function() { paste(input$symb, '.png', sep='') },
  content = function(file) {
    data <- Input_stock1()
    data <-diff(log(data))
    png(file)
    chartSeries(data,  name = paste(input$symb2,".Return"), up.col = "red")
    dev.off()
  }
)

#Aktienkurs Plot
output$KURS_Rendite_plot <- renderPlot({
  chartSeries(Input_stock(), name = input$symb)
}, height = 263)

#Rendite Plot
output$KURS_Rendite_plot_1 <- renderPlot({
    data <- Input_stock()
    data <-diff(log(data))
    chartSeries(data,  name = paste(input$symb,".Return"), up.col = "red")
  }, height = 263)
  
 #Konfidenzitervall Plot
output$Konfidenzintervall <- renderPlot({
    conf <- KI_Nivau()
    f(Input_return(),input$Stichprobe,input$bins, conf)
  }, height =660) 
  
#Zusammenfassung der eingelesenen Datei
inputSummary <- reactive({
  obs <- Input_return()
  means <- mean(obs)
  num_obs <- length(obs)
  vars <- var(obs)
  ses <- sqrt(vars)
  summary <- data.frame(
    "Aktien_Name" = c(paste(input$symb)),
    "Means" = means, 
    "Num_obs" = num_obs,
    "Variance" = vars, 
    "Standard_error" = ses)
  names(summary) <- c("Aktien_Name","Mittelwert", "Stichprobenanzahl", "Varianz", "Standardabweichung")
  summary
})

#Hypothesentest (Einfacher t-Test)
outputSummary <- reactive({
  test <- runTTest()
  alpha <- as.numeric(input$conf)
  summary <- data.frame("side" = test$alternative,
                        "h_0"   = test$null.value,
                        "pValue"=test$p.value, 
                        "DegFreedom"=test$parameter,
                        "tStatistic"=test$statistic)
  names(summary) <- c("Alternativ","Nullhypothese","P_Value", "Freiheitsgrad", "Test_Statistik")
  summary
  
})

#Konfidenzintervalle
ciSummary <- reactive({
  test <- runTTest()
  summary <- data.frame(
    "Alpha_pc"=100*as.numeric(input$conf),
    "LowerBound"= test$conf.int[1],
    "PointEstimate"=test$estimate,
    "UpperBound"=test$conf.int[2],
    "Intervalllaenge" = test$conf.int[2]-test$conf.int[1] 
  )
  names(summary) <- c("alpha", "Untere_Grenze", "Punktschaetzer", "Obere_Grenze", "Intervalllaenge")
  summary
})

ciMethode <- reactive({
  test <- runTTest()
  "Methode" = test$method
}) 

#Test-Methode (Zweiseitiger t-Test)
output$inmethod <- renderText({
  results <- ciMethode()
  results <-paste0("Testen des Erwartungswertes bei unbekannter Varianz (",results, ")")
  results 
})

output$inpSum <- renderTable({
  results <- inputSummary()
  data.frame(results)
},digits = 4)
output$inpSum01 <- renderTable({
  results <- inputSummary()
  data.frame(results)
},digits = 4)
output$outSum <- renderTable({
  results <- outputSummary()
  data.frame(results)
},digits = 4)

output$ciSum <- renderTable({
  results <- ciSummary()
}, digits = 4)

#Testsentscheidung
output$hypothesis <- renderPrint({
  test <- runTTest()
  alpha <- as.numeric(input$conf)
  if(test$p.value<alpha){
    results <- paste0("Erwartungswert = ",input$muh," kann bei einer irrtumswahrscheinlichkeit von " , alpha, " abgelehnt werden")}
  else{
    results <- paste0("Erwartungswert = ",input$muh," kann  bei einer irrtumswahrscheinlichkeit von " , alpha, " nicht abgelehnt werden")}
  results 
})

output$p_Value <- renderPlot({
  f1(Input_return(),input$alpha, input$muh)
}, height = 600)

output$Aktienkurs2 <- renderUI({
  table1 <- renderTable({
    data <- Stock_Price()
    head(data, n = input$obs1)
  })
  table2 <- renderTable({
    data <- Stock_Price1()
    head(data, n = input$obs1)
  })
  
tabsetPanel( 
    tabPanel(paste(input$symb), table1 ),
    tabPanel(paste(input$symb2), table2)
  )
})

output$RenditeVergleich_Plot <- renderPlot({
    data <- Input_stock()
    data <-diff(log(data))
    chartSeries(data,  name = paste(input$symb,".Return"), up.col = "red", theme = "black")
  }, height = 255)
  
output$RenditeVergleich_Plot1 <- renderPlot({
         data <-Input_stock1()
    data <-diff(log(data))
    chartSeries(data,  name = paste(input$symb2,".Return"), up.col = "green", theme = "black")
  }, height = 255)
  
#Zusammenfassung der Datei 
inputSummary1 <- reactive({
    obs <- Input_return2()
    means <- sapply(obs, mean)
    num_obs <- sapply(obs, length)
    vars <- sapply(obs, var)
    ses <- sqrt(vars)
    summary <- data.frame(
      "Aktien_Name" = c(paste(input$symb),paste(input$symb2)),
      "Means" = means, 
      "Num_obs" = num_obs,
      "Variance" = vars, 
      "Standard_error" = ses
      )
    names(summary) <- c("Aktien_Name","Mittelwert", "Stichprobenanzahl", "Varianz", "Standardabweichung")
    summary
  })
  
  #Hypothesentest (Zweiseitiger t-Test)
  outputSummary1 <- reactive({
    test <- runTTest1()
    alpha <- as.numeric(input$conf)
    summary <- data.frame("side" = test$alternative,
                          "h_0"   = test$null.value,
                          "pValue"=test$p.value, 
                          "DegFreedom"=test$parameter,
                          "tStatistic"=test$statistic
    )
    names(summary) <- c("Alternativ","Nullhypothese","P_Value", "Freiheitsgrad", "Test_Statistik")
    summary
  })
  
  #Konfidenzintervalle fuer die Differenz der erwarteten Rendite
  ciSummary1 <- reactive({
    test <- runTTest1()
    Summary <- data.frame(
      "Alpha_pc"=100*as.numeric(input$conf),
      "LowerBound"=test$conf.int[[1]],
      "PointEstimate"=-diff(test$estimate), 
      "UpperBound"=test$conf.int[[2]],
      "Intervalllaenge" = test$conf.int[2]-test$conf.int[1]
    )
    names(Summary) <- c("alpha", "Untere_Grenze", "Punktschaetzer", "Obere_Grenze", "Intervalllaenge")
    Summary
})
  
ciMethode1 <- reactive({
    test <- runTTest1()
    "Methode" = test$method
  })

#Test-Methode
output$inmethod1 <- renderText({
    results <- ciMethode1()
    results <-paste0("Testen der Gleichheit der Erwartungswerte zweier unabhaengiger 
normalverteilter Zufallsvariablen mit gleicher Varianz (",results, ")")
    results 
  })

output$inpSum1 <- renderTable({
    results <- inputSummary1()
    data.frame(results)
  },digits = 6)

output$outSum1 <- renderTable({
    results <- outputSummary1()
    data.frame(results)
  })
  
output$ciSum1 <- renderTable({
    results <- ciSummary1()
    data.frame(results)
  }, digits = 4)
   
 #Testentscheidung 
output$hypothesis1 <- renderPrint({
    test <- runTTest1()
    alpha <- as.numeric(input$conf)
    if(test$p.value<alpha){
      results <- paste0("Es liegt einen Unterschied zwischen den Erwartungswerten bei einem Konfidenznivau von ",(1- alpha), " vor" )}
    else{
      results <- paste0("Es liegt keinen Unterschied zwischen den Erwartungswerten bei einem Konfidenznivau von", (1-alpha), " vor")}
    results 
  })
  
#Funktion zum Aufrum einer Tnternet-Seite
getPage<-function(x) {
  browseURL(x)
}
  
output$Hilfe<-renderUI({
  getPage(input$sourc)
})
})
