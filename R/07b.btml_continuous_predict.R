btml_continuous_predict=function(ET,ynew,xnew,znew){

  #1. Assign optimal ML under the tree structure ET
  n=length(ynew)            # number of subjects
  yhat=rep(NA,n)            # estimated probability (or outcome)
  eta=NA
  node.hat=rep(1,n)         # node number
  algorithm.hat=rep(NA,n)   # algorithm for subj in test data

  if(ET$numNodes==1){         #There is only 1 terminal node, i.e. root node
    algorithm.hat=rep(ET$algorithm[1],n)
    node.hat=rep(1,n)
  }else{
    for(i in ET$internal){
      # define index number and eta number
      idx=(node.hat==i)
      x.sel=unique(xnew[which(idx),ET$splitVariable[i]])
      eta[i]=length(x.sel)
      # Split node i into left & right
      left=which(idx & xnew[,ET$splitVariable[i]]<=ET$cutoff[i])
      right=which(idx & xnew[,ET$splitVariable[i]]>ET$cutoff[i])
      node.hat[left]=2*i
      node.hat[right]=2*i+1
      algorithm.hat[left]=ET$algorithm[2*i]                # selected algorithm for each subj
      algorithm.hat[right]=ET$algorithm[2*i+1]
    }
  }

  #2 Predicted probabilities
  for(j in ET$terminal){
    wh.j=which(node.hat==j)   # idx for subgroup j
    n.j=length(wh.j)
    if(length(wh.j)>0){
      y.j=ynew[wh.j]
      z.j=znew[wh.j,]                   # markers for subgroup j
      if(n.j==1)
        z.j=matrix(z.j,nrow=1)

      #compute yhat
      FUN=paste(ET$algoList[ET$algorithm[j]],"_predict", sep = "")
      yhat[wh.j]=continuousMLfit_predict(ET$algohat[[j]],z.j,FUN)
    }
  }
  return(list(node.hat=node.hat,algorithm.hat=algorithm.hat,yhat=yhat))
}
