#############################################################################
### RESIDUAL - GRUOP - PLOT
#############################################################################
rgp <- function(mymodel, id=c("all","none"), ...){
standardGeneric("rgp")
}
setGeneric("rgp", def=rgp)
#############################################################################
###                                                                   synonym
#############################################################################
halfNormalQuantiles.absoluteStudentizedResiduals <- rgp
#############################################################################
###                                                                      LMER
#############################################################################
rgp.lmer <- function(mymodel, id=c("all", "none"), ...){
### ================================= checking
  id <- match.arg(id)
### ================================= reading
  parseFormula(mymodel)
  ##
  ri <- resid(mymodel) # implemented as: working ...?
  ri.abs.max <- 1.1*max(abs(ri))
  #
  this.data <- cbind(my.data,ri)
  #rand.vars <- c(rand.vars, rand.vars.nested)
### ================================= makint the panels
  for ( i in seq(along=rand.vars)){
    ## i is a number for every random factor
    plot.eval <- paste("plot_",i,"<-bwplot(~ri|",rand.vars[i]," , this.data,
                     xlim=c(-ri.abs.max, ri.abs.max),
                     xlab=\"residuals\",
                     layout=c(1,length(levels(this.data[,\"",rand.vars[i],"\"]))),
                     strip=FALSE, strip.left=FALSE,
                     panel= function(x, y, subscripts=subscripts, ...){
                             if(length(subscripts)>10){panel.bwplot(x,y, ...)}else{panel.stripplot(x,y, jitter.data=TRUE,...)}
                             panel.abline(v=0)
                             grid.text(x=unit(-0.95*ri.abs.max,\"native\"), y=unit(0.05, \"lines\"),label=this.data[subscripts[1],\"",rand.vars[i],"\"], just=c(\"left\",\"bottom\"), gp=gpar(col=\"grey\"))
                     },
                     main=\"",rand.vars[i],"\"
                     )", sep="")
    eval(parse(text=paste(plot.eval)))
  }
### ================================= panel arangement
  n.panels <- length(rand.vars) #number of panesl
  if(n.panels<1){stop("ERROR: there should be at least one random factor in your model")}
  r.dim <- ceiling(sqrt(n.panels)) #number of panel rows
  c.dim <- ceiling(n.panels/r.dim) #number of panel cols
  panel.matrix <- matrix(c(1:n.panels, rep(0, r.dim*c.dim-n.panels)), nrow=r.dim, ncol=c.dim, byrow=TRUE)
  panel.matrix.logical <- panel.matrix>0
  ##
  grid.newpage()
  ##
  pushViewport(viewport())
  grid.text("rgp.lmer(), residuals at every level of each random factor ", y=unit(0.975, "npc"), just=c("center","bottom"), gp=gpar(col="grey", fontface="bold", fontsize=16 ))
  ## grid.text(x.label, y=unit(0.05, "npc"))
  ## grid.text(y.label, x=unit(0.05, "npc"), rot=90)
  pushViewport(viewport(width=unit(0.9, "npc"), height=unit(0.9, "npc"), just=c("center", "center"), layout=grid.layout(r.dim,c.dim)))
#    pushViewport(viewport(x=unit(4, "lines"), y=unit(4, "lines"), just=c("left", "bottom"), layout=grid.layout(r.dim,c.dim)))
  for (m in 1:r.dim){
    for (n in 1:c.dim){
      if(panel.matrix.logical[m,n]){
        pushViewport(viewport(layout.pos.col=n, layout.pos.row=m))
#        eval(parse(text=paste("grid.text(\"",predict.terms.numeric[panel.matrix[m,n]],"\",y=unit(0.9, \"npc\"))",sep="")))
        eval(parse(text=paste("print(plot_",panel.matrix[m,n],", newpage=FALSE)",sep="")))
        upViewport()
        }
      }
    }
### ================================= identification
  identifyControl(panel.matrix=trellis.currentLayout(), original.row.names=row.names(my.data), id="none")
}
### ================================= method
setMethod("rgp", "lmer", rgp.lmer)
#############################################################################

