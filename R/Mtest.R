#' M-test
#' 
#' Does that thang
#' @param X1 A point pattern object (ppp)
#' @param X2 Another point pattern object (ppp)
#' @param b number of bootstrap replicates to generate randomization envelope
#' @return Returns a list of things
#' @export

m.test=function (X1,X2,b=100){
  M.est=Mest(X1,X2)
  
  r=M.est$r
  
  # reprojecting data to have a common mean center point
  X1x=X1$x-mean(X1$x)
  X1y=X1$y-mean(X1$y)
  
  ## Finding the best rotation of second ppp to match first ppp.
  rot=matrix(c(1,1,-1,1,1,-1,-1,-1),ncol=2,byrow=T)
  align=numeric()
  
  for (i in 1:4){
    X2x=(X2$x-mean(X2$x))*rot[i,1]
    X2y=(X2$y-mean(X2$y))*rot[i,2]
    X1t=ppp(X1x,X1y,window=owin(c(min(c(X1x,X2x)),max(c(X1x,X2x))),c(min(c(X1y,X2y)),max(c(X1y,X2y)))))
    X2t=ppp(X2x,X2y,window=owin(c(min(c(X1x,X2x)),max(c(X1x,X2x))),c(min(c(X1y,X2y)),max(c(X1y,X2y)))))
    align[i]=sum(crossdist(X1t,X2t))
  }
  
  X2x=(X2$x-mean(X2$x))*rot[which.min(align),1]
  X2y=(X2$y-mean(X2$y))*rot[which.min(align),2]
  X2t=ppp(X2x,X2y,window=owin(c(min(c(X1x,X2x)),max(c(X1x,X2x))),c(min(c(X1y,X2y)),max(c(X1y,X2y)))))

  hold = matrix(0,b,length(r))
  M.dist=0
  n1=X1t$n
  n2=X2t$n
  allx=c(X1t$x,X2t$x)
  ally=c(X1t$y,X2t$y)
  tot=sum(n1,n2)
  for(i in 1:b){
    mix=sample(1:tot)
    temp1=ppp(allx[mix[1:n1]], ally[mix[1:n1]],
              window=owin(c(min(c(X1x,X2x)),max(c(X1x,X2x))),c(min(c(X1y,X2y)),max(c(X1y,X2y)))))
    temp2=ppp(allx[mix[(n1+1):tot]], ally[mix[(n1+1):tot]],
              window=owin(c(min(c(X1x,X2x)),max(c(X1x,X2x))),c(min(c(X1y,X2y)),max(c(X1y,X2y)))))
    differ=Mest(temp1,temp2,r=r)$Mr
    hold[i,]=differ
    M.dist[i]=sum(differ)
  }
  
  Mean = apply(hold, 2, mean) #mean
  Up = apply(hold, 2, quantile,probs=.975) #max
  Down = apply(hold, 2, quantile,probs=.025) #min
  
  envelope=data.frame(Mean,Up,Down)
  
  M.p=sum(M.dist>=sum(M.est$Mr))/length(M.dist)
  if (M.p==0){M.p=as.character(paste("<",1/length(M.dist),sep=""))}

  M.sum=sum(M.est$Mr)
  M.ci=quantile(M.dist,c(.025,.975))
  
  results=list(r=r,Mest=M.est$Mr,envelope=data.frame(Mean,Up,Down),p=M.p,M.sum=M.sum,ConfInt=M.ci)
  return(results)
}