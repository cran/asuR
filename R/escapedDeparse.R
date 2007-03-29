 escapedDeparse <- function(vec.i){
    c.vec <- "c("
    for (i in seq(along=vec.i)){
      c.vec <- paste(c.vec, "\"", vec.i[i], "\"", sep="")
      if(i<length(vec.i)){c.vec <- paste(c.vec, ",", sep="")}
    }
    c.vec <- paste(c.vec, ")", sep="")
    return(c.vec)
  }
