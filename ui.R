library(shiny) 
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "CompStat Fall 2016"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inverse Transform", tabName = "Tarea1", icon = icon("fa fa-bar-chart")),
      menuItem("Acept-Reject Method", tabName = "Tarea2", icon = icon("fa fa-bar-chart")),
      menuItem("Tarea 3", tabName = "Tarea3", icon = icon("fa fa-bar-chart")),
      menuItem("Tarea 4", tabName = "Tarea4", icon = icon("fa fa-bar-chart"))
    ) 
  ), 
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Tarea1",
              fluidRow(
                box(
                title = "Inputs", width = 12,  background = "black", solidHeader = TRUE,
                numericInput("lambda", "Introduzca un valor de lambda", 1),
                numericInput("Simulaciones", "Introduzca el numero de simulaciones", 10000)
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
      tabItem(tabName = "Tarea3",
              h2("Tarea 3 va aqui")
      ),
      # Fourth tab content
      tabItem(tabName = "Tarea4",
              h2("Tarea 4 va aqui")
      )
    )
  ))