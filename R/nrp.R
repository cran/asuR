#############################################################################
### NormalQQPlots of RESIDUALS
#############################################################################
nrp <- function(mymodel, id=c("all","none"), ...){
standardGeneric("nrp")
}
setGeneric("nrp", def=nrp)
#############################################################################
###                                                                   synonym
#############################################################################
Residuals.Fitted.CategoricalFixedEffects <- nrp
#############################################################################
###                                                                      LMER
#############################################################################
nrp.lmer <- function(mymodel, id=c("all", "none"), ...){
### ================================= checking
  id <- match.arg(id)
### ================================= reading
  parseFormula(mymodel)
  ##
  ri <- resid(mymodel) # implemented as: working ...?
  x.ax <- sort(ri)
  y.ax <- qnorm((seq(along.with=x.ax)-0.5)/length(x.ax))
### ================================= makint the panels
  for ( i in seq(along=fixed.terms.factor)){
    ## i is a number for every random factor
    plot.eval <- paste("plot_",i,"<-xyplot(y.ax ~ x.ax|",fixed.terms.factor[i]," , my.data,
                     xlab=\"fitted values\",
                     ylab=\"residuals\",
                     strip=TRUE, strip.left=FALSE,
                     main=\"",fixed.terms.factor[i],"\"
                     )", sep="")
    eval(parse(text=paste(plot.eval)))
  }
### ================================= panel arangement
  n.panels <- length(fixed.terms.factor) #number of panesl
  if(n.panels<1){stop("ERROR: there should be at least one fixed factor in your model")}
  r.dim <- ceiling(sqrt(n.panels)) #number of panel rows
  c.dim <- ceiling(n.panels/r.dim) #number of panel cols
  panel.matrix <- matrix(c(1:n.panels, rep(0, r.dim*c.dim-n.panels)), nrow=r.dim, ncol=c.dim, byrow=TRUE)
  panel.matrix.logical <- panel.matrix>0
  ##
  grid.newpage()
  ##
  pushViewport(viewport())
  grid.text("nrp.lmer(), normal quantile quantile plot of residuals by levels of categorical fixed effects", y=unit(0.975, "npc"), just=c("center","bottom"), gp=gpar(col="grey", fontface="bold", fontsize=16 ))
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
 ## currently not supported: id="none"
##   print("hallo")
##   return(0)
identifyControl(panel.matrix=trellis.currentLayout(), original.row.names=row.names(my.data), id="none")
}
### ================================= method
setMethod("nrp", "lmer", nrp.lmer)
#############################################################################

