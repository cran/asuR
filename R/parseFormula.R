#############################################################################
### PARSE FORMULA
#############################################################################
## This generic function should extract a number of elements from a
## fitted-model-object. For a given class it should always provide the same
## elements (they are listed at the beginning of the corresponding definition)

parseFormula <- function(mymodel, ...){
standardGeneric("parseFormula")
}
setGeneric("parseFormula", def=parseFormula)
#############################################################################
###                                                                        LM
#############################################################################
## terms.numeric
## response.var
## predictor.vars
parseFormula.lm <- function(mymodel, ...){
  ##  general comments on model objects
  #################################
  ## 1) data is in the 

  ## assumptions on the formula:
  ## 1) there is only one predictor
  #################################
  ## 'vars' are the names of the variables (response & predicotrs)
  my.vars <- all.vars(formula(mymodel)) # without transformation
  my.vars.class <- attr(mymodel$terms, "dataClasses")
  vars.numeric <- my.vars[my.vars.class=="numeric"]
  ## 'terms' are the names of all variables (response & predicots) WITH the transformation
  ##         e.g. log(Area)
  my.terms <- names(attr(mymodel$terms, "dataClasses"))
  terms.numeric <<-  my.terms[my.vars.class=="numeric"]  # the numeric predictors
  ## 
  response.var <<- my.vars[1]
  response.term <<- my.terms[1]
  predict.vars.numeric <<- vars.numeric[-1] # if none exists, e.g. in an ANOVA, it has lenght 0
  predict.terms.numeric <<- terms.numeric[-1]  
  ## for glm
  my.data.glm <<- mymodel$data
  eval(parse(text=paste("response.values.glm <<- my.data.glm$",response.var,sep="")))
  eval(parse(text=paste("response.values.lm <<- mymodel$model$", response.var, sep="")))
  ##
  intercept.logical <<- as.logical(attr(mymodel$terms, "intercept"))
}
setMethod("parseFormula", "lm", parseFormula.lm)
#############################################################################
###                                                                       GLM
#############################################################################
parseFormula.glm <- function(mymodel, ...){
  ## #################################
  ## general comments
  ## #################################
  ## returns 

  ## assumptions on the formula:
  ## 1) there is exactly one predictor
  #################################
  ## 'vars' are the names of the variables (response & predicotrs)
  my.vars <- all.vars(formula(mymodel)) # without transformation
  my.vars.class <- attr(mymodel$terms, "dataClasses")
  vars.numeric <- my.vars[my.vars.class=="numeric"]
  ## 'terms' are the names of all variables (response & predicots) WITH the transformation
  ##         e.g. log(Area)
  my.terms <- names(attr(mymodel$terms, "dataClasses"))
  terms.numeric <-  my.terms[my.vars.class=="numeric"]  # the numeric predictors
### response.var
  response.var <<- my.vars[1]
### response.term
  response.term <<- my.terms[1]
### predict.vars.numeric
  predict.vars.numeric <<- vars.numeric[-1] # if none exists, e.g. in an ANOVA, it has lenght 0
### predict.terms.numeric
  predict.terms.numeric <<- terms.numeric[-1]
### index_coef.terms.numeric
  index_coef.terms.numeric <<- match(predict.terms.numeric, names(coef(mymodel)))
### mydata
  my.data <<- mymodel$data
### response.values
  eval(parse(text=paste("response.values <<- my.data$",response.var,sep="")))
### intercept
  intercept.logical <<- as.logical(attr(mymodel$terms, "intercept"))
}
setMethod("parseFormula", "glm", parseFormula.glm)
#############################################################################
###                                                                      LMER
#############################################################################
## parseFormula.lmer <- function(mymodel, ...){
## ### ===========
## ### mydata
##   my.data <<- mymodel@frame
## ### response.values
## #  eval(parse(text=paste("response.values <<- my.data$",response.var,sep="")))
## ### intercept
##   intercept.logical <<- as.logical(attr(attr(mymodel@frame, "terms"), "intercept"))
## }
## ### =========== method
## setMethod("parseFormula", "lmer", parseFormula.lmer)
## #############################################################################
