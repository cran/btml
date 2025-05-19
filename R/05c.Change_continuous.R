Change_continuous=function(ET,SIN,minsample){

  ####
  #1. Initial value
  ####
  n1=ET$n1
  q=ET$q
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
  #2.1. select feature
  splitVariable[SIN]=sample(1:q,1,prob = ET$dir.predictor)

  #2.2. all idx number in node SIN
  subTreeNode=NA
  for (i in 1:(floor(log2(max(terminal)))-floor(log2(SIN)))){
    subadd = 2^i*SIN+0:(2^i-1)
    subTreeNode=sort(c(subTreeNode,subadd))
  }
  idx=node.hat1 %in% subTreeNode                          # idx, i.e. those who belong to node SIN
  x.sel=unique(x1[which(idx),splitVariable[SIN]])         # selected x for those belong to node SIN
  cutoff[SIN]=sample(x.sel,1)                            # define new cutoff value

  #2.3. tree structure with new predictor and cutoff value
  eta1=eta1[1]
  node.hat1=rep(1,n1)
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

    cond2=length(left)>minsample                           # table above does not work if left or right is empty
    cond3=length(right)>minsample
    size.cond=cond2&cond3
    if(size.cond){
      algorithm.hat1[left]=algorithm[2*i]               # selected algorithm for each subj
      algorithm.hat1[right]=algorithm[2*i+1]
    }else{
      break
    }
  }

  ####
  # 3. summary
  ####
  ET$eta1=eta1
  ET$splitVariable=splitVariable
  ET$cutoff=cutoff

  ET$algorithm=algorithm
  ET$node.hat1=node.hat1
  ET$algorithm.hat1=algorithm.hat1

  ET$internal=internal
  ET$terminal=terminal

  ET$numNodes=length(ET$terminal)
  ET$size.cond=size.cond

  return(ET)
}
