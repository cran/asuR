xxp <- function(mydata){
  mydata <- mydata
  Xnr <- dim(mydata)[2]
  point.number <- dim(mydata)[1]
### Calculating the dimensions of the output
  ddim <- ceiling(sqrt(((Xnr*Xnr)-Xnr)/2))
  par(mfrow=c(ddim,ddim), mar=c(rep(0.1,4)),oma=(rep(0.5,4)))
### A loop for every individual plot
  for(i in 1:(Xnr-1)){
    for(j in (i+1):(Xnr)){
      ## Boxplots if one is numeric and the other a factor
      ##
      if(is.factor(mydata[,i])&&is.numeric(mydata[,j])){
        plot(mydata[,j] ~ mydata[,i],
             xlab=names(mydata)[j],
             ylab=names(mydata)[i],
             axes=FALSE,
             pch=".", cex=1.5)
        #axis(1, line=0, tick=FALSE)
        #axis(2, line=0, tick=FALSE)
        mtext(names(mydata)[j],side=1, line=-1.2)
        mtext(names(mydata)[i],side=2, line=-1.2)
      }
      my.cex=10*exp(-0.1*point.number)+1
      if(is.factor(mydata[,j])&&is.numeric(mydata[,i])){
        plot(mydata[,i] ~ mydata[,j],
             xlab=names(mydata)[j],
             ylab=names(mydata)[i],
             axes=FALSE,
             pch=1, cex=my.cex)
        axis(1, line=1)
        axis(2, line=1)
        mtext(names(mydata)[j],side=1, line=-1.2)
        mtext(names(mydata)[i],side=2, line=-1.2)
      }
      ## Scatterplot
      if(is.numeric(mydata[,j])&&is.numeric(mydata[,i])){
        plot(as.numeric(mydata[,i]) ~ mydata[,j],
             xlab=names(mydata)[j],
             ylab=names(mydata)[i],
             axes=FALSE,
             pch=1, cex = my.cex)
        #axis(1, line=1, labels=FALSE)
        #axis(2, line=1, labels=FALSE, at=c(0) )
        box()
        mtext(names(mydata)[j],side=1, line=-1.2)
        mtext(names(mydata)[i],side=2, line=-1.2)
      }
    }
  }
}
