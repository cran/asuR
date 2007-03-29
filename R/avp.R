app <- function(mymodel,myvar){
  library(car)
  av.plots(mymodel, variable=myvar)
}
