#' Display sum of squares info
#'
#' @param linearmodel A linear model.
#' @param datavariable A variable.
#' @return Sum of squares info about the linear model.
#' @examples
#' sumofsquares(spruce.lm, spruce.df$Height)
#' sumofsquares(quad.lm, spruce.df$Height)
sumofsquares <- function(linearmodel,datavariable){
  fit = fitted(linearmodel)
  res = residuals(linearmodel)

  RSS=sum((datavariable-fit)^2)

  MSS = sum((fit-mean(datavariable))^2)


  TSS = sum((datavariable-mean(datavariable))^2)

  show(RSS)
  show(MSS)
  show(TSS)
  show(MSS/TSS)
}





