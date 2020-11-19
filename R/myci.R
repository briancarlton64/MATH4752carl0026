#' Calculate 95% confidence interval
#'
#' @param x Sample

#' @examples
#' myci(rnorm(23,1,4))
#' myci(c(1,2,5,6))
myci<-function(x){
  a=.05

  u<-mean(x)+qt(1-a/2,df=length(x)-1)*sd(x)/sqrt(length(x))

  l<-mean(x)-qt(1-a/2,df=length(x)-1)*sd(x)/sqrt(length(x))

  return(c(l,u))
}
