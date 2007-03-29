irp <- function(mymodel){
  ri <- rstudent(mymodel)
  index <- 1:length(ri)
  ##  
  plot1 <- xyplot(ri~index,
                  main="irp \n index plot of stud. residuals",
                  xlab="INDEX",
                  ylab="studentized Residuals",
                  panel=myPanel <- function(x,y,...){
                    panel.xyplot(x,y,...)
                    panel.abline(a=0)
                    panel.loess(x,y,...)
                  }
                  )
  print(plot1)
  ### ================================= IDENTIFICATION
  identified <- NULL
  trellis.focus("panel", 1, 1)
  identified <- c(identified, panel.identify(labels=dimnames(model.matrix(mymodel))[[1]]))
  identified
}
