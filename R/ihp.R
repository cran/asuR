ihp <- function(mymodel){
   ## depends on libraries: lattice and grid
  ei <- rstudent(mymodel)
  k <- dim(model.matrix(mymodel))[2]-1 #minus 1 for the intercept
  pii <- hat(model.matrix(mymodel))
  di <- ei/(t(ei)%*%ei)
  ## calculations acording to Hadi-Script 5.68
  R <- k/(1-pii) * di^2/(1-di^2)
  P <- pii/(1-pii)
  Hadi <- R+P
  index <- 1:length(pii)
  ##  
  plot1 <- xyplot(Hadi~index,
                  main="ihp \n index plot of Hadi's influence",
                  xlab="INDEX",
                  ylab="Hadi's influence",
                  panel=myPanel <- function(x,y,...){
                    panel.xyplot(x,y,...)
                    panel.loess(x,y,...)
                  }
                  )
  print(plot1)
  trellis.focus("panel", 1, 1)
  panel.identify(labels=dimnames(model.matrix(mymodel))[[1]])
  }
