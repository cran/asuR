ryp <- function(mymodel, id=c("none", "all")){
### ================================= checking and argument matching
  if(class(mymodel)[1]!="lm"){stop("ERROR: the model you want to inspect should be fitted with lm")}
  id <- match.arg(id)
### ================================= calculations
  ri <- rstudent(mymodel)
  yi <- predict(mymodel)
### ================================= plot
  plot1 <- xyplot(ri ~ yi,
                  main="ryp \n stud. residuals vs. predicted values",
                  ylab="studentized residuals",
                  xlab="predicted values",
                  panel=myPanel <- function(x,y,...){
                    panel.xyplot(x,y,...)
                    panel.abline(a=0)
                    panel.loess(x,y,...)
                  }
                  )
  print(plot1)
### ================================= IDENTIFICATION
  if(id=="all"){
    identified <- NULL
    trellis.focus("panel", 1, 1)
    identified <- panel.identify(labels=dimnames(model.matrix(mymodel))[[1]])
    return(identified)
  }
}

