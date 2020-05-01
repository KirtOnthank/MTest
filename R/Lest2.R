#' Modified Ripley's L(r)
#' 
#' Placeholder documentation: A Modification of Ripley's L: sqrt(K/pi)-R
#' @param ... Passes arguments to Kest() from spatstat
#' @return A vector of L and r
#' @export
#' 
Lest2=function (...) { # the ... pass through definitions
  K=Kest(...) # straight to the Kest function
  nama = colnames(K)
  K=K[, !(nama %in% c("rip", "ls"))]
  R=K$r
  L=eval.fv(sqrt(K/pi)-R)
  L=rebadge.fv(L, substitute(L(r), NULL), "L")
  return(L) 
}