Mest=function (X1, X2, correct="none",r=NULL,rmax=NULL) {
  first=Lest2(X1,correction=correct,r=r,rmax=rmax)
  second=Lest2(X2,correction=correct,r=first$r)
  differ=first$iso-second$iso
  Mr=data.frame(cbind(first$r,differ))
  colnames(Mr)=c("r","M")
  return(Mr)
}
