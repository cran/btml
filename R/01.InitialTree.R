InitialTree=function(y1,x1,z1,
                     y2,x2,z2, algoList){ #initial tree
  ET=list()

  ET$n1=length(y1)      # number of samples
  ET$n2=length(y2)      # number of samples
  ET$q=ncol(x1)         # number of features
  ET$r=ncol(z1)         # number of markers
  ET$p=length(algoList) # number of candidate ML algorithms

  ET$y1=y1     # outcome train
  ET$x1=x1     # features train
  ET$z1=z1     # markers train
  ET$y2=y2     # outcome validation
  ET$x2=x2     # features validation
  ET$z2=z2     # markers validation


  ET$terminal=1
  ET$internal=NA
  ET$splitVariable=NA
  ET$cutoff=NA

  ET$eta1=ET$eta2=NA #the number of available values which could be select to split the chosen terminal node
  ET$numNodes=length(ET$terminal)

  ET$algoList=algoList
  ET$algorithm=sample(1:ET$p,1)              # selected algorithm

  ET$node.hat1=ET$node.hat2=rep(ET$terminal,ET$n1)              # estimated terminal node, i.e. subgroup, for each subject
  ET$algorithm.hat1=ET$algorithm.hat2=rep(ET$algorithm,ET$n1)   # estimated algorithm for each subj

  return(ET)
}
