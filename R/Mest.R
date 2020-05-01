#' M(r) function
#' 
#' Calculates the difference of the Ripley's L(r) of two point patterns
#' @param X1 A point pattern object (ppp)
#' @param X2 Another point pattern object (ppp)
#' @param r vector of distances at which to evaluate M(r)
#' @param rmax the maximum distance at which to evaluate M(r)
#' @return returns a vector of M values for a series of distances (r)
#' @export

Mest=function (X1, X2,r=NULL,rmax=NULL) {
  first=Lest2(X1,correction="none",r=r,rmax=rmax)
  second=Lest2(X2,correction="none",r=first$r)
  differ=first$un-second$un
  Mr=data.frame(cbind(first$r,differ))
  colnames(Mr)=c("r","Mr")
  return(Mr)
}
