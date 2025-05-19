#Metropolis-Hastings Algorithms
MH=function(ET1,ET,base,power){
  logtransRatio=logtreeRatio=log(1)
    if(ET1$method%in%c("Grow","Prune")){
    # 1. Transition ratio
    Cj=ET1$numNodes #number of terminal node
    #Q=ET1$q
    #P=ET1$p
    w=0                    # number of internal nodes which have two children terminal nodes.
    for(k in ET1$terminal){
      if(neighbourNode(k)%in%ET1$terminal){
        w = w+1
      }else{
        w = w
      }
    }
    w=w/2 #because neighbourNode is counted twice
    logtransRatio = log(Cj)-log(w) #+log(Q)+log(eta)+log(P) #canceled out by tree structure ratio

    # 2 Tree structure ratio
    if(ET1$method=="Grow"){
      depth0=floor(log2(ET1$STN))
    }else if(ET1$method=="Prune"){
      depth0=floor(log2(ET1$SIN))
    }
    depth1=depth0+1
    logtreeRatio = 2*log(1-prior.tree(base,power,depth1))+log(prior.tree(base,power,depth0))-log(1-prior.tree(base,power,depth0)) #-log(Q)-log(eta)-log(P) canceled out

    if(ET1$method=="Grow"){ #transRatio&treeRatio are computed for grow (above)
      logtransRatio=-logtransRatio #1/transRatio for prune
      logtreeRatio=-logtreeRatio   #1/treeRatio for prune
    }
  }

  #3. pseudo likelihood ratio
  logLR=ET1$loglik2-ET$loglik2

  #4. MH
  ET$MH="rejected"

  Ratio=min(logtransRatio+logtreeRatio+logLR,log(1))
  U=log(runif(1))
  if(!is.na(Ratio)){ #if lasso is no cov, then yhat is missing & Ratio is NA   
    if(Ratio>=U){
      ET=ET1
      ET$MH="accepted"
    }
  }

  return(ET)
}
