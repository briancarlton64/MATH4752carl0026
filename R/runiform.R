#' Randomly generates a uniform distribution and displays it
#'
#' @param k Size of distribution
#' @param n Number of iterations
#' @param iter Number of iterations to display
#' @param time Time between graph displays
#' @return Graph of uniform distribution
#' @examples
#' runiform(10,100)
#' runiform(10,100, 5,0.2)
runiform=function(k,n, iter=10,time=0.5){
  for( i in 1:iter){
    s=sample(1:k,n,replace=TRUE)
    sf=factor(s,levels=1:k)
    barplot(table(sf)/n,beside=TRUE,col=rainbow(10),
            main=paste("Example sample()", " iteration ", i, " n= ", n,sep="") ,
            ylim=c(0,0.2)
    )
    Sys.sleep(time)
  }
}
