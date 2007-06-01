#############################################################################
###                                                                 WORKHORSE
#############################################################################
workhorse <- function(mymodel, which=c("select", "sequence", "all"), id=c("all", "none"), plot.names=plot.names){
  which <- match.arg(which)
  id <- match.arg(id)
#################################
  switch(which,
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
         },
         ## which = "sequence"
         sequence = {#print(!is.numeric(id)); print(id=="none")
                     which.plots <- seq(along=row.names(plot.names))
                     for(i in seq(along=which.plots)){
                       e.ask <- paste("if(!is.numeric(id)){user.selection <- menu(choices=c(\"yes\",\"no\",\"stop\"), title=\"***\n",plot.names[i,1], "\n   (functions: ",plot.names[i,2]," or ",plot.names[i,3],")\")}else{user.selection <- 1}",sep="")
                       ##             e.deside <- paste("if(user.selection==0 | user.selection==3){break};if(user.selection==2){next}else{identified.list <- c(identified.list, list(",plot.names[i,2],".id = ", plot.names[i,2],"(mymodel, id=\"", id, "\")))}",sep="")
                       e.deside <- paste("if(user.selection==0 | user.selection==3){break};if(user.selection==2){next}else{",plot.names[i,2],".id <- ", plot.names[i,2],"(mymodel, id=\"",id,"\")}",sep="")
                       ##
                       eval(parse(text=e.ask))
                       eval(parse(text=e.deside))
                     }
                   },
         ## which = "all"
         all = {#print(!is.numeric(id)); print(id=="none")
                if(which=="all"){which.plots <- seq(along=row.names(plot.names))}
                for(i in seq(along=which.plots)){
                  user.selection <- 1
                  e.deside <- paste("if(user.selection==0 | user.selection==3){break};if(user.selection==2){next}else{", plot.names[i,2],"(mymodel, id=\"none\")}",sep="")
                  ##
                  eval(parse(text=e.deside))
                }
              }
         )
  ##
  # creates a list of identified values, for every plot one slot
  # and one slot with all values that were selected at least once
  identified.list <- list()
  identified.all <- vector(mode="numeric", length=0)
  for (i in eval(parse(text=escapedDeparse(plot.names[,2])))){
    if(exists(paste(i, ".id", sep=""))){
      this.values <- eval(parse(text=paste(i, ".id", sep=""))) #;print(this.values)
      identified.list <- c(identified.list, eval(parse(text=paste("list(", i, " = ", i, ".id)", sep=""))))
      ##identified.list <- c(identified.list, if(exists(paste(i, ".id", sep=""))){eval(parse(text=paste("list(", i, " = ", i, ".id)", sep="")))})
      identified.all <- c(identified.all, this.values[!this.values%in%identified.all])#c <- c(c, b[!b%in%c])
    }else{
      eval(parse(text=paste(i, ".id <- vector(mode=\"numeric\", length=0)", sep="")))
    }
  }
  if(length(identified.all>0)){identified.all <- sort(identified.all[-1])}
  identified.list <- c(identified.list, list(all=identified.all))
  invisible(identified.list)
  ##
}
