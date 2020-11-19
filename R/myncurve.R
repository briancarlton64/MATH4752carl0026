#' Display sum of squares info
#'
#' @param mu mean of a normal distribution
#' @param sigma standard deviation of a normal distribution
#' @return P(X<a)~Norm(mu,sigma)
#' @examples
#' myncurve(0,1,1)
#' myncurve(2,2,1)
myncurve = function(mu, sigma, a ){

  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve=seq(mu-3*sigma,a,length=10000)
  ycurve=dnorm(xcurve,mu,sigma)
  polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col = "black")
  prob=pnorm(a,mu,sigma)
  prob=round(prob,4)
  as.list(prob)
}

