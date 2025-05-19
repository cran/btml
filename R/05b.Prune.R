Prune = function(ET,SIN){

  ####
  #1. Initial value
  ####
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
  RC.SIN=2*SIN+1 #right child node number for SIN
  LC.SIN=2*SIN   #left

  #2.1. delete feature and cutoff value for SIN
  splitVariable[SIN]=NA
  cutoff[SIN]=NA
  idx=(node.hat1==RC.SIN|node.hat1==LC.SIN)        #idx, i.e. those who belong to terminal node=2*STN or 2*SIN+1
  eta1[SIN]=NA
  node.hat1[idx]=SIN

  #2.2. select marker
  sel.algorithm=algorithm[SIN]                         #prune and previous selected marker
  algorithm[LC.SIN]=algorithm[RC.SIN]=sel.algorithm
  algorithm.hat1[idx]=algorithm[SIN]                    #select marker for each subject

  #2.3. updated internal & terminal nodes
  internal=setdiff(internal,SIN)
  terminal=sort(c(setdiff(terminal,c(LC.SIN,RC.SIN)),SIN))

  ####
  # 3. summary
  ####
  ET$splitVariable=splitVariable
  ET$cutoff=cutoff

  ET$algorithm=algorithm
  ET$node.hat1=node.hat1
  ET$algorithm.hat1=algorithm.hat1

  ET$internal=internal
  ET$terminal=terminal

  ET$numNodes=length(ET$terminal)

  ET$SIN=SIN
  ET$size.cond=TRUE

  return(ET)
}

