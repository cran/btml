Assign=function(ET,STN){
  ####
  #1. Initial value
  ####
  p=ET$p
  algorithm=ET$algorithm
  node.hat=ET$node.hat
  algorithm.hat1=ET$algorithm.hat1
  dir.algorithm=ET$dir.algorithm

  ####
  # 2. Update tree structure
  ####
  #2.1 subject index for node STN
  idx=(node.hat==STN)

  #2.2 select algorithm
  sel.algorithm=sample(1:p,1,prob=ET$dir.algorithm)
  algorithm[STN]=sel.algorithm
  algorithm.hat1[idx]=algorithm[STN]

  ####
  # 3. summary
  ####
  ET$algorithm=algorithm
  ET$node.hat=node.hat
  ET$algorithm.hat1=algorithm.hat1
  ET$size.cond=TRUE

  return(ET)
}
