library(shiny) 
library(shinydashboard)
library(plotly)
library(pracma)
library(ggplot2)
library(DT)

 
shinyserver <- function(input, output) {

  output$plot1 <- renderPlot({
    #lamb <- (input$lambda)
    #simu <- (input$Simulaciones)
    uni <-runif(input$Simulaciones)
    expon <- -(1/input$lambda)*log(uni) 
    hist(expon, breaks = input$nbarras, freq=F,main="Exp from Uniform")
    lines(density(expon),na.rm=TRUE)
  })
  output$plot2 <- renderPlot({
    #lamb <- (input$lambda)
    #simu <- (input$Simulaciones)
    expop <- rexp(input$Simulaciones,rate = input$lambda)
    hist(expop,breaks = input$nbarras,freq=F,main="Exponentials from R")
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
  
  output$sinIntegral<- renderPlotly({
    usFun <- parse(text=input$funcion)
    fun <- function (x) 
    { eval(usFun)
    }
    numvar1 <- 100000
    numvar2 <- 10000
    numvar3 <- 1000
    numvar4 <- 100
    numvar5 <- 10
    intefun <- function(a){
      univars <- runif(a, input$lBound1, input$ubound1)
      ev <- fun(univars)
      intest <- mean(ev)
      intest
    }
    a <- intefun(numvar5)
    b <- intefun(numvar4)
    c <- intefun(numvar3)
    d <- intefun(numvar2)
    e <- intefun(numvar1)
    
    combo1<- list(a = a,b = b,c = c,d = d,e = e)
    combo2<- c(1,2,3,4,5)
    c1 <- as.numeric(combo1)
    plot_ly(x = combo2, y = c1, type = "bar")
    
    
  })
  
  output$trapinIntegral<- renderUI({
    usFun <- parse(text=input$funcion)
    fun <- function (x) 
    { eval(usFun)
    }
    trapintg <- trapzfun(fun, input$lBound1, input$ubound1)
    print(trapintg[1])
    
    
  })
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- mtcars
    data
  }, rownames = TRUE))
  
  
  
  output$mmscatter<- renderPlotly({
    
    xdata <- input$vd
    ydata <- input$vi
    
    plot_ly(x = mtcars[,xdata], y = mtcars[,ydata], type = "scatter")
    
    
  })

##################################################################################
  ###############
  ##############
  #
  df <- reactive({
    f = mtcars
    f
  })
  
  data <- reactive({
    f = mtcars
    
      as.matrix(f)
  })
  
  output$data <- renderTable({
    data()
  })
  
  output$vdui <- renderUI({
    selectInput('vd', label = 'Variable dependiente', names(df()))
  })
  
  output$viui <- renderUI({
    selectInput('vi', label = 'Variable dependiente', names(df()))
  })
  
  output$varpt <- renderPlot({
    if(!is.null(df))
    {
      y <- df()[[input$vd]]
      x <- df()[[input$vi]]
      plot(x,y)
    }
  })
  
  nreg <- reactive({
    equis<-df()[[input$vi]]
    ye <- df()[[input$vd]]
    sp<-data.frame(equis,ye)
    splm<-lm(ye~equis,data=sp)
    summary_splm<-summary(splm)
    betas<-coefficients(summary_splm)
    list('betas' = betas, 'summary' = summary_splm)
  })
  
  ndist <- reactive({
    x <- seq(-100, 100, length=100)
    dnorm(x,round(nreg()$betas[1,1],digits=2),round(nreg()$betas[1,2],digits=2))
  })
  
  gdist <- reactive({
    x <- seq(-100, 100, length=100)
    dinvgamma(x,13.5,round(25*nreg()$summary$sigma,digits=2))
  })
  
  output$apra <- renderPlot({
    x <- seq(-100, 100, length=100)
    plot(x, ndist(), type="l", lty=2, xlab="x value",ylab="Density", main=paste('A priori a'))
  })
  
  output$aprb <- renderPlot({
    x <- seq(-100, 100, length=100)
    plot(x, ndist(), type="l", lty=2, xlab="x value",ylab="Density", main=paste('A priori b'))
  })
  
  output$aprg <- renderPlot({
    x <- seq(-100, 100, length=100)
    plot(x, gdist(), type="l", lty=2, xlab="x value",ylab="Density", main=paste('A priori eps'))
  })
  
  
  likelihood <- function(param){
    b1= param[1]
    b0 = param[2]
    sigma2 = param[3]
    pred = b1*df()[[input$vi]] + b0
    singlelikelihoods = dnorm(df()[[input$vd]], mean = pred, sd = sigma2**.5, log = T)
    sumll = sum(singlelikelihoods)
    return(sumll)
  }
  
  prior <- function(param){
    b1 = param[1]
    b0 = param[2]
    sigma2 = param[3]
    b1prior = dnorm(b1, mean=round(nreg()$betas[1,1],digits=2), sd=round(nreg()$betas[1,2]**.5,digits=2), log = T)
    b0prior = dnorm(b0, mean=round(nreg()$betas[2,1],digits=2), sd=round(nreg()$betas[2,2]**.5,digits=2), log = T)
    sigma2prior = dinvgamma(sigma2,14,round(25*nreg()$summary$sigma,digits=2),log = T)
    return(b1prior+b0prior+sigma2prior)
  }
  
  posterior <- function(param){
    return (likelihood(param) + prior(param))
  }
  
  #Metropolis
  
  proposalfunction <- function(param){
    return(rnorm(3,mean = param, sd= c(0.1,0.5,0.3)))
  }
  
  run_metropolis_MCMC <- function(startvalue, iterations){
    chain <- array(dim = c(iterations+1,3))
    chain[1,] <- startvalue
    for (i in 1:iterations){
      proposal <- proposalfunction(chain[i,])
      
      logprobab =posterior(proposal) - posterior(chain[i,])
      if (log(runif(1)) <= logprobab){
        chain[i+1,] = proposal
      }else{
        chain[i+1,] = chain[i,]
      }
    }
    return(chain)
  }
  
  mcmc <- reactive({
    startvalue = c(rnorm(1,0,1),rnorm(1,0,1),rinvgamma(1,1,1))
    chain = run_metropolis_MCMC(startvalue, input$cadenas)
    data.frame(b1=chain[,1],b0=chain[,2],s2=chain[,3])
  })
  
  output$sim <- renderDataTable({
    mcmc()
  })
  
  output$histos<-renderPlot({
    burnIn = input$cadenas*.20
    acceptance = 1-mean(duplicated(mcmc()[-(1:burnIn),]))
    par(mfrow = c(2,3))
    hist(mcmc()[-(1:burnIn),1],nclass=30,  main="Posterior of b1", xlab="Parametro" )
    abline(v = mean(mcmc()[-(1:burnIn),1]))
    hist(mcmc()[-(1:burnIn),2],nclass=30, main="Posterior of b0", xlab="Parametro")
    abline(v = mean(mcmc()[-(1:burnIn),2]))
    hist(mcmc()[-(1:burnIn),3],nclass=30, main="Posterior of sigma^2", xlab="Parametro")
    abline(v = mean(mcmc()[-(1:burnIn),3]) )
    plot(mcmc()[-(1:burnIn),1], type = "l", xlab="Iteraciones" , main = "Chain values of b1" )
    plot(mcmc()[-(1:burnIn),2], type = "l", xlab="Iteraciones" , main = "Chain values of b0")
    plot(mcmc()[-(1:burnIn),3], type = "l", xlab="Iteraciones" , main = "Chain values of sigma^2")
  })
  
  
  
}



