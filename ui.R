
library(shiny)
shinyUI(fluidPage(
  #Hindergrund 
  theme = "bootstrap.css",
  h1("Renditeanalyse: Wertpapiere", 
     style = "font-family: 'Lobster', Harlow Solid Italic;
     line-height: 1.1; 
     color: green;"),
   
  sidebarLayout(
    sidebarPanel(
      #Aktuelle Uhrzeit
      tags$style(type='text/css', '#Aktuelle_Zeit{background-color: darkgreen; color: white;}'), 
      textOutput("Aktuelle_Zeit"),
      br(),
      br(),
      #Alle in Server Datei beschriebene Konfigurationen abrufen
    uiOutput("Liste")
    ),  
    
  mainPanel(
    #alle Schaltflaeche und dazugehoerige Funktionen abrufen
      tabsetPanel(position = "above", type = "pills", selected = NULL,           
                           tabPanel("Gauss_VS_t", style = "color:green", plotOutput("Gauss_VS_t")),
                           tabPanel("Aktienkurs.csv", tableOutput("Aktienkurs.csv")),
                           tabPanel("Aktienkurs1", tableOutput("Aktienkurs1")),
                           tabPanel("Konfidenzintervall", br(),h5( strong("Input summary",
                           style = "color:green")),tableOutput("inpSum01"),br(),br(),
                           plotOutput("Konfidenzintervall")) ,
                           tabPanel("KURS_Rendite_plot", plotOutput("KURS_Rendite_plot", 
                                    height = "10%"), plotOutput("KURS_Rendite_plot_1")),  
                           tabPanel("t-Test(1-Sample)",
                           br(),
                           h5( strong("Methode", style = "color:green")),
                           tags$style(type='text/css', '#inmethod {background-color:  mediumturquoise; color: black;}'), 
                           verbatimTextOutput("inmethod"),
                           br(),
                           br(),
                           h5( strong("Input summary",style = "color:green")),                           
                           tableOutput("inpSum" ),
                           br(),
                           h5( strong("t-Test Ergebnisse",style = "color:green")),
                           tableOutput("outSum"),
                           br(),                         
                           h5( strong("Konfidenzintervall fuer den Erwartungswert", style = "color:green")),
                           tableOutput("ciSum"),
                           br(),
                           h5( strong("Testentscheidung", style = "color:green")),
                           tags$style(type='text/css', '#hypothesis {background-color: darkred; color: white;}'), 
                           verbatimTextOutput("hypothesis")
                           ),
                           tabPanel("p_Value",  plotOutput("p_Value")) ,
                           tabPanel("Aktienkurs2", uiOutput("Aktienkurs2")),
                           tabPanel("RenditeVergleich Plot", br(), plotOutput("RenditeVergleich_Plot",height = "10%"), plotOutput("RenditeVergleich_Plot1")), 
                           tabPanel("t-Test(2-Sample)",
                           br(),
                           h5( strong("Methode", style = "color:green")),
                           tags$style(type='text/css', '#inmethod1 {background-color:  mediumturquoise; color: black;}'), 
                           verbatimTextOutput("inmethod1"),
                           br(),             
                           h5( strong("Input summary",style = "color:green")),
                           tableOutput("inpSum1" ),                        
                           br(),
                           h5( strong("t-Test Ergebnisse",style = "color:green")),                           
                           tableOutput("outSum1"),
                           br(),                        
                           h5( strong("Konfidenzintervall fuer die Differenz der Erwartungswerte", style = "color:green")),
                           tableOutput("ciSum1"),
                           br(),
                           h5( strong("Testentscheidung", style = "color:green")),
                           tags$style(type='text/css', '#hypothesis1 {background-color: darkred; color: white;}'), 
                           verbatimTextOutput("hypothesis1")                          
                  ),                  
                  tabPanel("Hilfe",  htmlOutput("Hilfe")), id = "tab")
    )
  )))
