require(MASS)
require(manipulate)
data(anscombe)

shinyServer(function(input, output, session){
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot
  
  
  
  output$LOOplot <- renderPlot({
    whichpair <- substr(input$data,10,10)

eval(parse(text = paste("sortit <- order(anscombe$x", whichpair, ")", sep = "")))
eval(parse(text = paste("data.df <- data.frame(x=anscombe$x", whichpair, "[sortit], y=anscombe$y", whichpair, "[sortit])", sep = "")))
  
  ylims <- c(min(data.df$y)*0.8, max(data.df$y)*1.2)
  xlims <- c(min(data.df$x)*0.8, max(data.df$x)*1.2)
  ytext1 <- ylims[1] + 0.8 * (ylims[2]-ylims[1])
  ytext2 <- ylims[1] + 0.7 * (ylims[2]-ylims[1])
  xtext <- ylims[1] + 0.2 * (xlims[2]-xlims[1])
  
  
  #i <-  4
  cols = rep("green", 11)
  cols[input$i] <- "red"
  pchs <- rep(16, 11)
  pchs[input$i] <- 4
  par(bty = "n", xpd = NA, las = 1)
  plot(data.df$y~data.df$x, pch = pchs, col = cols, ylim = ylims, xlab = "x", ylab = "y", main = "Illustration of \n leave one out residuals", xlim = xlims)
  
  text(data.df$x, rep(4,11), round(studres(lm(data.df$y~data.df$x)),2), cex = 0.4)
  m1 <- lm(data.df$y[-input$i] ~ data.df$x[-input$i])
  sigma2 <- summary(m1)$sigma^2
  t1 <- bquote(hat(sigma)^2 == .(round(sigma2,6)))
  text(xtext, ytext2, t1)
  t3 <- bquote(paste(hat(beta)[1]==.(round(coef(m1)[1],2))," ", hat(beta)[2]==.(round(coef(m1)[2],2))))
  text(xtext, ytext1, t3)
  abline(m1, col = "blue")
  ##??pred.df <- data.frame(x=data.df$x[i])
  ##??yhat <- predict(m1, newdata=pred.df)
  yhat <- coef(m1)[1] + coef(m1)[2] * data.df$x[input$i]
  posit <- sign(yhat) + 2
  t2 <- bquote(e[-input$i] == .(round(data.df$y[input$i]-yhat, 2)))
  text(data.df$x[input$i], data.df$y[input$i], t2, col = "red", pos = posit, offset = 2)
  arrows(data.df$x[input$i], yhat, data.df$x[input$i], data.df$y[input$i], length = 0.01, col = "red")
  ##Sys.sleep(2)
})


})

