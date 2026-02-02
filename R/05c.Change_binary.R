Change_binary=function(ET,SIN,minsample){

  ####
  #1. Initial value
  ####
  n1=ET$n1
  q=ET$q
  y1=ET$y1
  x1=ET$x1
  y2=ET$y2
  x2=ET$x2

  splitVariable=ET$splitVariable
  cutoff=ET$cutoff
  algorithm=ET$algorithm

  node.hat1=ET$node.hat1
  node.hat2=ET$node.hat2
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
    idx1=(node.hat1==i)
    idx2=(node.hat2==i)
    x.sel=unique(x1[which(idx1),splitVariable[i]])
    eta1[i]=length(x.sel)

    # Split node i into left & right
    left1=which(idx1 & x1[,splitVariable[i]]<=cutoff[i])
    left2=which(idx2 & x2[,splitVariable[i]]<=cutoff[i])
    right1=which(idx1& x1[,splitVariable[i]]>cutoff[i])
    right2=which(idx2& x2[,splitVariable[i]]>cutoff[i])

    node.hat1[left1]=2*i
    node.hat2[left2]=2*i
    node.hat1[right1]=2*i+1
    node.hat2[right2]=2*i+1

    cond1=min(table(y1,node.hat1))>minsample         # subgroup sample size is large enough
    cond2=length(left1)>minsample                           # table above does not work if left or right is empty
    cond3=length(right1)>minsample
    cond4=min(table(y2,node.hat2))>=2  
    cond5=length(left2)>=2          
    cond6=length(right2)>=2
    size.cond=cond1&cond2&cond3&cond4&cond5&cond6
    if(size.cond){
      algorithm.hat1[left1]=algorithm[2*i]               # selected algorithm for each subj
      algorithm.hat1[right1]=algorithm[2*i+1]
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
