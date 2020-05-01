#' Plotting function for M-test
#' 
#' Makes a pretty plot
#' @param mtest the output of m.test()
#' @param ... pass arguments to plot()
#' @return Plot the observed M(r) versus the randomization envelope
#' @export
#' 
Mplot=function(mtest, ...){
  lower=min(c(mtest$envelope$Down,mtest$Mest))
  upper=max(c(mtest$envelope$Up,mtest$Mest))
  plot(mtest$r,mtest$Mest,type="n",ylab="M(r)",xlab="Distance (r)",ylim=c(lower,upper),...)
  for (i in 1:length(mtest$r)){
    lines(c(mtest$r[i],mtest$r[i]),c(mtest$envelope$Up[i],mtest$envelope$Down[i]),lwd=2,col="grey")
  }
  abline(h=0)
  lines(mtest$r,mtest$envelope$Up,lty=2,lwd=1.5)
  lines(mtest$r,mtest$envelope$Down,lty=2,lwd=1.5)
  lines(mtest$r,mtest$Mest,col="red")
}