btml=function(y,x,z,ynew=NULL, xnew=NULL,znew=NULL,
             MLlist=c("lasso","rf","svm"),
             sparse=TRUE, nwarm=1000, niter=1000,
             minsample=20, base = 0.95, power = 0.8){

  ###
  #1. setup
  ###
  #1.1. outcome type
  type="c"
  if(is.factor(y))
    type="b" #binary

  if(type=="b"){
    compLTP=match.fun(compLTP_binary)
    Grow=match.fun(Grow_binary)
    Change=match.fun(Change_binary)
    btml_predict=match.fun(btml_binary_predict)
  }else if(type=="c"){
    compLTP=match.fun(compLTP_continuous)
    Grow=match.fun(Grow_continuous)
    Change=match.fun(Change_continuous)
    btml_predict=match.fun(btml_continuous_predict)
  }

  #1.2. ML list
  algoList=paste0(type,"_",MLlist) #rename

  #1.3. Split data into train and validation
  n=length(y)

  n1=floor(0.7* n) #80% tr / 20% val
  n2=n-n1
  train_ind <- sample(1:n, size = n1)
  y1=y[train_ind]  #traning
  x1=x[train_ind,]
  z1=z[train_ind,]

  y2=y[-train_ind] #test
  x2=x[-train_ind,]
  z2=z[-train_ind,]

  posterior.improved="no" #warm-up or posterior improved
  ###################################################
  ## A. Warm Up
  ###################################################
  #A1. Initial tree, uniform prior for x; compute LTP (likelihood, tree probability, posterior)
  ET=InitialTree(y1,x1,z1,y2,x2,z2, algoList)
  ET$dir.predictor=rep(1/ET$q,ET$q) #priors
  ET$dir.algorithm=rep(1/ET$p,ET$p)
  #ET$dir.marker=rep(1/ET$r,ET$r) #not used

  ET=compLTP(ET,base,power, btml_predict)                                       # Compute log-likelihood, tree probability and log-posterior

  #A2. Warm up tree
  m.predictor=NA #selected predictors
  m.algorithm=NA #selected algorithm

  #numNodes=logPosterior=loglik=NA
  for(warm in 1:nwarm){
    if(warm %% 100==0)
      message(paste0("Number of warm-up: ", warm, " of total ", nwarm))

    ET1=UpdateTree(ET,minsample, Grow,Change) #stochastic search: growth, prune, change, swap, assign
  #  numNodes[warm]=ET1$numNodes
  #  logPosterior[warm]=ET1$logPosterior2
  #  loglik[warm]=ET1$loglik2

    if(ET1$size.cond){ #n for each terminal node is large enough
      ET1=compLTP(ET1,base,power, btml_predict)   #compute LTP
      ET=MH(ET1,ET,base,power)
      #if(ET$MH=="accepted"){
        posterior.improved="yes"
        #Store the marker and predictor information
        m.predictor=append(m.predictor, ET$splitVariable[ET$internal])
        m.algorithm=append(m.algorithm, ET$algorithm[ET$terminal])
      #}
    }
  }
  m.predictor=m.predictor[!is.na(m.predictor)]
  m.algorithm=m.algorithm[!is.na(m.algorithm)]

  #A3. Dirchlet priors
  if(sparse==TRUE){
    for(j in 1:ET$q)
      ET$dir.predictor[j]=ET$dir.predictor[j]+sum(m.predictor==j)
    ET$dir.predictor=ET$dir.predictor/sum(ET$dir.predictor)

    for(j in 1:ET$p)
      ET$dir.algorithm[j]=ET$dir.algorithm[j]+sum(m.algorithm==j)
    ET$dir.algorithm=ET$dir.algorithm/sum(ET$dir.algorithm)
  }

  ###################################################
  ## B. Update Tree
  ###################################################
  #B1. initial value for evaluation
  ET2=ET

  #B2. update tree
  for(iter in 1:niter){
    if(iter %% 100==0)
      message(paste0("Number of update: ", iter, " of total ", niter))

    ET1=UpdateTree(ET,minsample, Grow,Change)             # stochastic search: growth, prune, change, swap, assign
  #  numNodes[warm+iter]=ET1$numNodes
  #  logPosterior[warm+iter]=ET1$logPosterior2
  #  loglik[warm+iter]=ET1$loglik2
    if(ET1$size.cond){                       # n for each terminal node is large enough
      ET1=compLTP(ET1,base,power, btml_predict)            # Compute log-likelihood, tree probability and log-posterior
      ET=MH(ET1,ET,base,power)               # Metropolis-Hastings
      if(ET$MH=="accepted"){        #choose the one with the highest posterior
        if(ET$logPosterior2>ET2$logPosterior2){ #2 for validation (1 for training)
          posterior.improved="yes"
          ET2=ET
        }
      }
    }
  }

  if(posterior.improved=="no"){
    warning("Increase nwarm and/or niter\n")
  }else{
    ET=ET2 #choose the one with the highest posterior

    #B3. summarize result
    fit_pred=btml_predict(ET, ynew=y, xnew=x, znew=z) #combined yhat for training and validation datasets. (overfitted)
    ET$y.hat=fit_pred$yhat
    ET$node.hat=fit_pred$node.hat

    if(type=="b"){
      y_num=as.numeric(y==1)
      ET$bs=mean((y_num-ET$y.hat)^2)
      ET$roc=ET$auc=NA
      try_roc=try(ET$roc<-pROC::roc(y~ET$y.hat,direction="<",levels=c(0,1)),silent=TRUE)
      if(!inherits(try_roc,'try-error'))
        ET$auc=ET$roc$auc
    }else if(type=="c"){
      ET$mse=mean((y-ET$y.hat)^2)
    }

    if(is.null(ynew)==FALSE & is.null(xnew)==FALSE & is.null(znew)==FALSE){
      fit_new=btml_predict(ET, ynew=ynew, xnew=xnew, znew=znew)
      ET$y.hat.new=fit_new$yhat
      ET$node.hat.new=fit_new$node.hat

      if(type=="b"){
        y_num_new=as.numeric(ynew==1)
        ET$bs.new=mean((y_num_new-ET$y.hat.new)^2)
        ET$roc.new=ET$auc.new=NA
        try_roc=try(ET$roc.new<-pROC::roc(ynew~ET$y.hat.new,direction="<",levels=c(0,1)),silent=TRUE)
        if(!inherits(try_roc,'try-error'))
          ET$auc.new=ET$roc.new$auc
      }else if(type=="c"){
        ET$mse.new=mean((ynew-ET$y.hat.new)^2)
      }
    }

    ###
    #C. further summary
    ###
    #C1. rename
    ET$algoList=sub('*..', '', ET$algoList) #remove b./c. (binary / continuous)
    names(ET)[names(ET)=="algoList"]="MLlist"

    names(ET)[names(ET)=="algohat"]="fitML"
    names(ET)[names(ET)=="algorithm"]="selML"
    ET$selML=ET$MLlist[ET$selML]

    #C2. remove unnecessariy info during MH search
    if(sum(ET$terminal!=1)){
      #terminal
      nt=max(ET$terminal,na.rm=TRUE)
      ET$selML=ET$selML[1:nt]
      ntt=1:nt
      for(s in setdiff(ntt,ET$terminal)){
        ET$selML[s]=NA
        if(s<=nt){
          ET$fitML[[s]]=NA
        }else{
          ET$fitML[[s]]=NULL
        }
      }

      #internal
      ni=max(ET$internal,na.rm=TRUE)
      ET$splitVariable=ET$splitVariable[1:ni]
      ET$cutoff=ET$cutoff[1:ni]
    }

    #C3 print
    btml.print(ET)

    #C4. remove some variabes
    ET$MLlist=NULL

    ET$n1=ET$n2=ET$p=ET$q=ET$r=NULL
    ET$y1=ET$x1=ET$z1=ET$y2=ET$x2=ET$z2=NULL
    ET$eta1=ET$eta2=NULL
    ET$numNodes=NULL

    ET$dir.algorithm=ET$dir.predictor=NULL
    ET$loglik2=ET$logPosterior2=ET$logTreeProb=NULL
    ET$node.hat1=ET$node.hat2=NULL
    ET$algorithm.hat1=ET$algorithm.hat2=NULL

    ET$MH=ET$STN=ET$SIN=ET$method=ET$size.cond=NULL
    ET$yhat=ET$yhat2=NULL
    ET$node.hat1=ET$node.hat2=NULL

    ET$MLfit=ET$algohat
    ET$algohat=NULL

    #C5. result
    #return(list(ET=ET,numNodes=numNodes,logPosterior=logPosterior,loglik=loglik))
    return(ET)
  }
}

