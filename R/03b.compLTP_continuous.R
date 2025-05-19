#compLTP: likelihood, tree probability, posterior
compLTP_continuous=function(ET,base,power, btml_predict){

  y2=ET$y2
  x2=ET$x2
  z2=ET$z2
  n2=ET$n2

  ###################################################
  ###1. (pseudo) log likelihood
  ###################################################
  #1.1. fit ML for each terminal node using training data
  algohat=list()
  for(j in ET$terminal){
    tr.j=which(ET$node.hat1==j)   # idx train subgroup j

    y.j=ET$y1[tr.j]               # y for subgroup j training
    z.j=ET$z1[tr.j,]              # markers for subgroup j training

    #Select and fit one of the algorithms using jth terminal node
    FUN=ET$algoList[ET$algorithm[j]] #jth algorithm
    algohat[[j]]=continuousMLfit(y.j, z.j, FUN)
  }
  ET$algohat=algohat

  #1.2. fitted y on training & validation datasets
  fit2=btml_predict(ET,ynew=y2,xnew=x2,znew=z2)  # validation data
  ET$node.hat2=fit2$node.hat
  ET$algorithm.hat2=fit2$algorithm.hat
  ET$yhat2=fit2$yhat

  #1.3. pseudo log likelihood
  var2_new=mean((y2-ET$yhat2)^2)
  ET$loglik2=-n2/2*log(2*pi*var2_new)-n2/2

  ###################################################
  ##2. log tree prob
  ###################################################
  logTreeProb=0
  n.terminal=ET$terminal
  for(m in ET$terminal){
    depth=floor(log2(m))
    logTreeProb = logTreeProb+log(1-prior.tree(base,power,depth))+log(ET$dir.algorithm[ET$algorithm[m]])
                              #1-splitting prob                   # algo sel prob
  }

  if(ET$numNodes>1){
    for(m in ET$internal){
      depth=floor(log2(m))
      logTreeProb=logTreeProb+log(prior.tree(base,power,depth))+log(ET$dir.predictor[ET$splitVariable[m]])+log(ET$eta1[m])
    }
  }
  ET$logTreeProb=logTreeProb

  ###################################################
  ###3. log posterior
  ###################################################
  #ET$logPosterior1=ET$loglik1+ET$logTreeProb
  ET$logPosterior2=ET$loglik2+ET$logTreeProb # proportion of posterior since normal constant is ignored.

  return(ET)
}
