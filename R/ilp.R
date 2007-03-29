ilp <- function(mymodel){
  pii <- hat(model.matrix(mymodel))
  index <- 1:length(pii)
  k <- dim(model.matrix(mymodel))[2]
  n <- dim(model.matrix(mymodel))[1]
  th <- 2*k/n
  ##  
  plot1 <- xyplot(pii~index,
                  main="ilp \n index plot of leverage",
                  xlab="INDEX",
                  ylab="Leverage",
                  panel=myPanel <- function(x,y,...){
                    panel.xyplot(x,y,...)
                    panel.loess(x,y,...)
                  }
                  )
  print(plot1)
  trellis.focus("panel", 1, 1)
  grid.lines(x=unit(c(0,n), "native"), y=unit(c(th,th), "native"))
  panel.identify(labels=dimnames(model.matrix(mymodel))[[1]])
  }
