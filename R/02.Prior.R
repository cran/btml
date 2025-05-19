#prior for tree
prior.tree=function(base,power,depth){ #alpha is base; beta is power
  base/((1+depth)^power)
}
