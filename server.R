library(shiny) 
library(shinydashboard)

 
shinyserver <- function(input, output) {

  output$plot1 <- renderPlot({
    #lamb <- (input$lambda)
    #simu <- (input$Simulaciones)
    uni <-runif(input$Simulaciones)
    expon <- -(1/input$lambda)*log(uni) 
    hist(expon,freq=F,main="Exp from Uniform")
    lines(density(expon),na.rm=TRUE)
  })
  output$plot2 <- renderPlot({
    #lamb <- (input$lambda)
    #simu <- (input$Simulaciones)
    expop <- rexp(input$Simulaciones,rate = input$lambda)
    hist(expop,freq=F,main="Exponentials from R")
    lines(density(expop),na.rm=TRUE)
    
  })
  output$plotbeta <- renderPlot({
    sampled <- data.frame(proposal = runif(input$nrv,0,1))
    sampled$targetDensity <- dbeta(sampled$proposal, input$alpha,input$beta)
    maxDens = max(sampled$targetDensity, na.rm = T)
    sampled$accepted = ifelse(runif(sampled$proposal,0,1) < sampled$targetDensity / maxDens, TRUE, FALSE)
    hist(sampled$proposal[sampled$accepted], freq = F, col = "grey", breaks = 100)
    curve(dbeta(x, input$alpha,input$beta),0,1, add =T, col = "red")
    
  })  
}

