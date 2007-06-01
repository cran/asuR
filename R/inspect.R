inspect <- function(mymodel, which = c("select", "sequence", "all"), id=c("all", "none"), ...){
standardGeneric("inspect")
}
setGeneric("inspect", def=inspect)
#############################################################################
###                                                                        LM
#############################################################################
inspect.lm <- function(mymodel, which = c("select", "sequence", "all"), id=c("all", "none"), ...){
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
inspect.glm <- function(mymodel, which=c("select", "sequence", "all"), id=c("all", "none"), ...){
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
  workhorse(mymodel=mymodel, which=which, id=id, plot.names=plot.names)
}
setMethod("inspect", "glm", inspect.glm)
#############################################################################
###                                                                      LMER
#############################################################################
inspect.lmer <- function(mymodel, which=c("select", "sequence", "all"), id=c("all", "none"), ...){
#################################
### here is the place to add additional fuctions that should become available for inspect
  plot.names <- data.frame(rbind(
                                 cbind(fullname="Normal quantile quantile plot of residuals by levles of categorical fixed effects",         short.function="nrp",         long.function="NormalQuantiles.Residuals.CategoricalFixedEffects"),
                                 cbind(fullname="Box- or dotplot of residuals by levels, for each random factor",         short.function="rgp",         long.function="residuals.by.groups"),
                                 cbind(fullname="Residuals vs. fitted values for all categorical fixed effects",         short.function="rfp",         long.function="Residuals.Fitted.CategoricalFixedEffects"),
                                 cbind(fullname="Normal quantile quantile plot of random effects, for each factor",         short.function="nep",         long.function="NormalQuantiles.RandomEffectsQuantiles")
                                 ## cbind(fullname="",         short.function="",         long.function=""),
                                 ))
#################################
  workhorse(mymodel=mymodel, which=which, id=id, plot.names=plot.names)
}
setMethod("inspect", "lmer", inspect.lmer)
#setMethod("inspect", "glmer", inspect.lmer)
#############################################################################
###                                                                       END
#############################################################################
