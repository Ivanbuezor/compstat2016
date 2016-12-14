library(shiny) 
library(shinydashboard)
library(plotly)
library(pracma)
library(ggplot2)
library(DT)
library(invgamma)

ui <- dashboardPage( 
  dashboardHeader(title = "CompStat Fall 2016"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inverse Transform", tabName = "Tarea1", icon = icon("fa fa-bar-chart")),
      menuItem("Acept-Reject Method", tabName = "Tarea2", icon = icon("fa fa-bar-chart")),
      menuItem("MC Integration", tabName = "Tarea3", icon = icon("fa fa-bar-chart")),
      menuItem("Bayesian Regression", tabName = "Tarea4", icon = icon("fa fa-bar-chart"))
    ) 
  ), 
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "estilos.css")
    ),
    tabItems(
      # First tab content
      tabItem(tabName = "Tarea1",
              fluidRow(
                box(
                title = "Inputs", width = 12,  background = "black", solidHeader = TRUE,
                numericInput("lambda", "Introduzca un valor de lambda", 1),
                numericInput("Simulaciones", "Introduzca el numero de simulaciones", 10000),
                sliderInput("nbarras", label = "Numero de barras",min = 10, max = 100, value=10, step =10 )
               )
              ),
              fluidRow(
                box(title = "Inverse Transform Exponential",  background = "black", solidHeader = TRUE,
                    plotOutput("plot1", height = 250)),
                
                box(title = "Exponential Sample From R",   background = "black", solidHeader = TRUE,
                    plotOutput("plot2", height = 250))
              )
      ),
      
      # Second tab content
      tabItem(tabName = "Tarea2",
              fluidRow(
                box(
                title = "Inputs", background = "black", solidHeader = TRUE,
                numericInput("alpha", "Value of Alpha Parameter",3),
                numericInput("beta", "Value of Beta Parameter", 6),
                numericInput("nrv", "Number of Random Values", 10000)
                ),
                box(title = "Beta by Accept Reject Method",  background = "black", solidHeader = TRUE,
                    plotOutput("plotbeta", height = 250))
              )
              ),
      
      # Third tab content
      tabItem(tabName = "Tarea3", fluidRow(
        box(
          title = "Inputs", width = 12, background = "black", solidHeader = TRUE,
          textInput("funcion","Introduzca la funcion","x^2"),
          numericInput("ubound1", "Limite superior de la integral",1),
          numericInput("lBound1", "limite inferior de la integral",0)
        ),
        box(title = "Integral MC N = 10, 100, 1000, 10000, 100000",width = 12,  background = "black", solidHeader = TRUE,
            plotlyOutput("sinIntegral")
        ),
        box(title = "Integral Con Metodo del trapecio",width = 12,  background = "black", solidHeader = TRUE,
            uiOutput("trapinIntegral")
        )
      )
             
      ),
      # Fourth tab content
      tabItem(tabName ="Tarea4", fluidRow(
        box(title = "Dataset", width=12, background = "black", solidHeader = TRUE,
            selectInput("vd", "Variable Dependiente X:",
                        c("mpg" = "mpg",
                          "cyl" = "cyl",
                          "disp" = "disp",
                          "hp" = "hp",
                          "drat" = "drat",
                          "wt" = "wt",
                          "qsec" = "qsec")),
            
            selectInput("vi", "Variable independiente Y:",
                        c("hp" = "hp",
                          "cyl" = "cyl",
                          "disp" = "disp",
                          "mpg" = "mpg",
                          "drat" = "drat",
                          "wt" = "wt",
                          "qsec" = "qsec"
                          ))
            
            ,DT::dataTableOutput("table")
        ),
        box(title = "Scatter", background = "black", solidHeader = TRUE,
            plotlyOutput("mmscatter")
        ),
        box(title = "Parametros", background = "black", solidHeader = TRUE,
            sliderInput("cadenas", label = "Longitud de Cadena",min = 1000, max = 100000, value=1000, step =1000 ),
            plotOutput('apra'),
            plotOutput('aprb'),
            plotOutput('aprg')
        ),
        box(title = "Histogramas",width=12, background = "black", solidHeader = TRUE,
            plotOutput('histos')
        )
        
        
      ))
          
          
          
        )
        
      
        
        
        
        )
              
      )
  