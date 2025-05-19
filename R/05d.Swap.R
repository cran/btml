Swap=function(ET,SIN,minsample){

  ####
  #1. Initial value
  ####
  n1=ET$n1
  y1=ET$y1
  x1=ET$x1

  splitVariable=ET$splitVariable
  cutoff=ET$cutoff
  algorithm=ET$algorithm

  node.hat1=ET$node.hat1
  algorithm.hat1=ET$algorithm.hat1

  internal=ET$internal
  terminal=ET$terminal

  eta1=ET$eta1

  ####
  # 2. Update tree structure
  ####
  # 2.1 select feature
  splitVariable=swapSplit(splitVariable, SIN, parentNode(SIN))
  cutoff=swapSplit(cutoff, SIN, parentNode(SIN))

  eta1=eta1[1]
  node.hat1=rep(1,n1)
  reverse=FALSE
  for(i in internal){
    # define index number and eta number
    idx=(node.hat1==i)
    x.sel=unique(x1[which(idx),splitVariable[i]])
    eta1[i]=length(x.sel)

    # Split node i into left & right
    left=which(idx & x1[,splitVariable[i]]<=cutoff[i])
    right=which(idx & x1[,splitVariable[i]]>cutoff[i])

    node.hat1[left]=2*i
    node.hat1[right]=2*i+1

    cond1=min(table(y1,node.hat1))>minsample         # subgroup sample size is large enough
    cond2=length(left)>0                           # table above does not work if left or right is empty
    cond3=length(right)>0
    cond4=length(table(y1,node.hat1))>1              # exclude the cases table contains NA values
    size.cond=cond1&cond2&cond3&cond4

    if(!size.cond){
      reverse=TRUE
      break
    }else{
      algorithm.hat1[left]=algorithm[2*i]               # selected algorithm for each subj
      algorithm.hat1[right]=algorithm[2*i+1]
    }
  }

  ####
  # 3. summary
  ####
  if(reverse){                        # if we did not do "SWAP"
    ET$size.cond=FALSE
  }else{
    ET$eta1=eta1
    ET$splitVariable=splitVariable
    ET$cutoff=cutoff

    ET$algorithm=algorithm
    ET$node.hat1=node.hat1
    ET$algorithm.hat1=algorithm.hat1

    ET$internal=internal
    ET$terminal=terminal

    ET$numNodes=length(ET$terminal)
    ET$size.cond=TRUE
  }

  return(ET)
}
