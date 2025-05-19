Grow_continuous=function(ET,STN,minsample){

  ####
  #1. Initial value
  ####
  p=ET$p
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
  splitVariable[STN]=sample(1:q,1,prob = ET$dir.predictor)

  idx=(node.hat1==STN)                            # idx, i.e. those who belong to terminal node=STN
  x1.sel=unique(x1[which(idx),splitVariable[STN]]) # selected x for those who belong to terminal node=STN
  cutoff[STN]=sample(x1.sel,1)
  eta1[STN]=length(x1.sel)                         # the number of available values which could be select to split the chosen terminal node

  #2.2. Split STN into left & right
  left=which(idx & x1[,splitVariable[STN]]<=cutoff[STN])
  right=which(idx & x1[,splitVariable[STN]]>cutoff[STN])

  LC.STN=2*STN   #left child node number for STN
  RC.STN=2*STN+1 #right child node number

  node.hat1[left]=LC.STN
  node.hat1[right]=RC.STN

  cond2=length(left)>minsample
  cond3=length(right)>minsample
  size.cond=cond2&cond3

  if(size.cond){
    #2.3. select algorithms
    sel.algorithm.L=sample(1:p,1,prob = ET$dir.algorithm)
    sel.algorithm.R=sample(1:p,1,prob = ET$dir.algorithm)

    algorithm[LC.STN]=sel.algorithm.L
    algorithm[RC.STN]=sel.algorithm.R

    algorithm.hat1[left]=algorithm[LC.STN]       # selected algorithm for each subj
    algorithm.hat1[right]=algorithm[RC.STN]

    #2.4. updated internal & terminal nodes
    internal=sort(c(internal,STN))               # NA is removed by sort, if internal is NA
    terminal=sort(c(setdiff(terminal,internal),LC.STN,RC.STN))
            #setdiff(sort(c(terminal,LC.STN,RC.STN)),internal)

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
  }

  ET$size.cond=size.cond
  ET$STN=STN

  return(ET)
}
