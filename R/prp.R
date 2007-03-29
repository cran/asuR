prp <- function(mymodel){
  ## depends on libraries: lattice and grid
  ei <- rstudent(mymodel)
  k <- dim(model.matrix(mymodel))[2]-1 #minus 1 for the intercept
  pii <- hat(model.matrix(mymodel))
  di <- ei/(t(ei)%*%ei)
  ## calculations acording to Hadi-Script 5.68
  R <- k/(1-pii) * di^2/(1-di^2)
  P <- pii/(1-pii)
  plot.PR <- xyplot(R ~ P,
                    main="prp \n potential-residual plot",
                    xlab="potential",
                    ylab="residual")
  ## giving out the object
  print(plot.PR)
  trellis.focus("panel", 1, 1)
  grid.lines(x=unit(c(0.5,0.5), "npc"), y=unit(c(0,1), "npc"))
  grid.lines(x=unit(c(0,1), "npc"), y=unit(c(0.5,0.5), "npc"))
  xx <- panel.identify(labels=dimnames(model.matrix(mymodel))[[1]])
  upViewport()
  grid.text("labels within the plotting region refer to the row names in the data.frame", x=unit(0.5, "npc"), y=unit(0.01, "npc"))
return(xx)
}
