#' Display confidence interval
#'
#' @param x data vector
#' @param fun function to apply on vector
#' @param iter number of iterations
#' @param alpha (1-a)100 percent confidence interval
#' @param ... extra parameters for the barplot
#' @return A barplot of a confidence interval
#' @examples
#' myboot2(iter=10000,sam,fun="mean",alpha=0.05)
#' myboot2(sam,fun="mean",alpha=0.10)
myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,...){
  n=length(x)
  y=sample(x,n*iter,replace=TRUE) #A
  rs.mat=matrix(y,nr=n,nc=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun)
  ci=quantile(xstat,c(alpha/2,1-alpha/2)) #B
  para=hist(xstat,freq=FALSE,las=1,main="Histogram of Bootstrap sample statistics",...)
  mat=matrix(x,nr=length(x),nc=1,byrow=TRUE)
  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")# Vertical line
  segments(ci[1],0,ci[2],0,lwd=4)#Make the segment for the ci
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=3)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=3)
  text(pte,max(para$density)/2,round(pte,2),cex=3)
  return(list(ci=ci,fun=fun,x=x,xstat=xstat))# Some output to use if necessary


}
