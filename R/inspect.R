inspect <- function(mymodel, which=c("select","all"), id=c("all", "none"), ...){
standardGeneric("inspect")
}
setGeneric("inspect", def=inspect)
#############################################################################
###                                                                        LM
#############################################################################
inspect.lm <- function(mymodel, which=c("select","all"), id=c("all", "none"), ...){
  if(any(is.nan(rstudent(mymodel)))){stop("Residuals could not be studentized!!")}
  rpp.id <- rpp(mymodel, id)
  ryp.id <- ryp(mymodel, id)
  nrp.id <- nrp(mymodel)
  irp.id <- irp(mymodel)
  ilp.id <- ilp(mymodel)
  ihp.id <- ihp(mymodel)
  prp.id <- prp(mymodel)
#   return(list(rpp=rpp.id, nrp=nrp.id, irp=irp.id, ryp=ryp.id, ilp=ilp.id, prp=prp.id))
}
setMethod("inspect", "lm", inspect.lm)
#############################################################################
###                                                                       GLM
#############################################################################
inspect.glm <- function(mymodel, which=c("select","all"), id=c("all", "none"), ...){
  which <- match.arg(which)
  id <- match.arg(id)
#################################
### here is the place to add additional fuctions that should become available for inspect
  plot.names <- data.frame(rbind(
                                 cbind(fullname="deviance residuals vs. linear predictor",         short.function="dep",         long.function="devianceResidual.linearPredictor"),
                                 cbind(fullname="partial residual vs. each predictor",             short.function="rpp",          long.function="partialResidual.eachPredictor" ),
                                 cbind(fullname="linearized response vs. linear predictor",         short.function="lep",         long.function="linearizedResponse.linearPredictor"),
                                 cbind(fullname="half-normal quantiles vs. absolute stud. residuals", short.function="hnp",         long.function="halfNormalQuantiles.absoluteStudentizedResiduals")
                                 ## cbind(fullname="",         short.function="",         long.function=""),
                                 ))
#################################
  switch(which,
         ## which = "all"
         all = {print(!is.numeric(id)); print(id=="none")
           if(which=="all"){which.plots <- seq(along=row.names(plot.names))}
           for(i in seq(along=which.plots)){
             e.ask <- paste("if(!is.numeric(id)){user.selection <- menu(choices=c(\"yes\",\"no\",\"stop\"), title=\"***\n",plot.names[i,1], "\n   (functions: ",plot.names[i,2]," or ",plot.names[i,3],")\")}else{user.selection <- 1}",sep="")
#             e.deside <- paste("if(user.selection==0 | user.selection==3){break};if(user.selection==2){next}else{identified.list <- c(identified.list, list(",plot.names[i,2],".id = ", plot.names[i,2],"(mymodel, id=\"", id, "\")))}",sep="")
             e.deside <- paste("if(user.selection==0 | user.selection==3){break};if(user.selection==2){next}else{",plot.names[i,2],".id <- ", plot.names[i,2],"(mymodel, id=\"",id,"\")}",sep="")
             ##
             eval(parse(text=e.ask))
             eval(parse(text=e.deside))
           }
         },
         ## which = "select"
         select = {
           repeat{
             ## asking the user
             e.ask <- paste("user.selection <- menu(choices=", escapedDeparse(plot.names[,1]),", title=\"Select the number of the plot you want:\")",sep="")
             eval(parse(text=e.ask))
             ## the user selects now
             if(user.selection==0){break}
             e.deside <- paste(plot.names[user.selection,2],".id <- ", plot.names[user.selection,2],"(mymodel, id=\"",id,"\")",sep="")
             eval(parse(text=e.deside))
           }
         }
         )
  ##
  # creates a list of identified values, for every plot one slot
  # and one slot with all values that were selected at least once
  identified.list <- list()
  identified.all <- 0
  for (i in eval(parse(text=escapedDeparse(plot.names[,2])))){
    if(exists(paste(i, ".id", sep=""))){
      this.values <- eval(parse(text=paste(i, ".id", sep="")))#;print(this.values)
      identified.list <- c(identified.list, eval(parse(text=paste("list(", i, " = ", i, ".id)", sep=""))))
      ##identified.list <- c(identified.list, if(exists(paste(i, ".id", sep=""))){eval(parse(text=paste("list(", i, " = ", i, ".id)", sep="")))})
      identified.all <- c(identified.all, this.values[!this.values%in%identified.all])#c <- c(c, b[!b%in%c])
    }
  }
  identified.list <- c(identified.list, list(all=sort(identified.all[-1])))
  invisible(identified.list)
  ##
}
setMethod("inspect", "glm", inspect.glm)
#################################
###                           end
#################################








## switch(user.selection, 
## #  plot.names <- "c(\"partial residual vs. each predictor\", \"deviance residuals vs. linear predictor\")"
  



  

##     e.ask <- paste("if(!is.numeric(id)&&id==\"none\"){user.selection <- menu(choices=c(\"yes\",\"no\",\"stop\"), title=\")\")}else{user.selection <- 1}",sep="")
  
##   identified.list <- list()
##   for(i in seq(along=which.plots)){
##     e.ask <- paste("if(!is.numeric(id)&&id==\"none\"){user.selection <- menu(choices=c(\"yes\",\"no\",\"stop\"), title=\"",plot.names[i,1], "(",plot.names[i,2]," or ",plot.names[i,3],")\")}else{user.selection <- 1}",sep="")
##     e.deside <- paste("if(user.selection==0 | user.selection==3){break};if(user.selection==2){next}else{identified.list <- c(identified.list, list(",plot.names[i,2],".id = ", plot.names[i,2],"(mymodel, id)))}",sep="")
##     ##
##     print(i)
##     eval(parse(text=e.ask))
##     eval(parse(text=e.deside))
##   }
##   identified.list
##   #invisible(list())
## }
                    
##   dep.id <- dep(mymodel, id)
##   ##
##   eval(parse(text=))
##   ##

##   if(user.selection==1){
##     rpp.id <- rpp(mymodel, id)
##   }
##   ##   yep.id <- yep.glm(mymodel, id)
##   ##   nrp.id <- nrp.glm(mymodel, id)
##   ##  return(list(rep.glm=rep.id, rpp.glm=rpp.id, yep.glm=yep.id, nrp.glm=nrp.id))
##   ##  last.warning <- character()  # this did not help 
## }





## ## inspect <- function(mymodel, id=c("none","all")){
## ##   possible.classes <- "aov, lm, glm")
## ##   ##
## ##   this.class <- class(mymodel)[1]
## ##   LM.model <- FALSE
## ##   GLM.model <- FALSE
## ##   ##  
## ##   if( this.class=="lm" | this.class=="aov"){
## ##     LM.model <- TRUE
## ##   }else{
## ##     if( this.class=="glm" ){
## ##       GLM.model <- TRUE
## ##     }else{
## ##       stop(paste("inspect works only for models fitted with the functions: ", possible.classes, sep=", "))
## ##     }
## ##   }
