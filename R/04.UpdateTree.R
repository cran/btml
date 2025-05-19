UpdateTree=function(ET,minsample, Grow,Change){

  method=sample(c("Grow", "Prune","Change","Swap","Assign"),1,prob = c(1/5,1/5,1/5,1/5,1/5))
  ET$method=method

  ET$size.cond=FALSE
  if(method=="Grow"){           # Grow, Assign are applicable for both single or many trees
	  STN=resample(ET$terminal,1)                # select one terminal node to "grow"
    ET=Grow(ET,STN,minsample)
  }else if(method=="Assign"){
	  STN=resample(ET$terminal,1)                # select one terminal node to "assign"
    ET=Assign(ET,STN)
  }else if(ET$numNodes>1){      # Prune, Change, Swap are applicable for single tree only
    if(method=="Prune"){
      pruneNodePool=NULL        # get node pool (w2): number of internal node having two child nodes
      for(k in ET$terminal){
        if(neighbourNode(k) %in% ET$terminal){ #true if neighbour node is also terrminal node
          pruneNodePool=c(pruneNodePool, parentNode(k))
        }
      }
      pruneNodePool=unique(pruneNodePool) #duplicated: one terminal nodel has neighbor terminal node, or vice versa

      SIN=resample(pruneNodePool,1)            # select one internal node to "prune"
      ET=Prune(ET,SIN)

    }else if(method=="Change"){
        SIN=resample(ET$internal,1)              # select one internal node to "change"
        ET=Change(ET,SIN,minsample)

    }else if(ET$numNodes>2){    # Swap are only applicable for node number larger than 2
      if(method=="Swap"){
        SIN=resample(setdiff(ET$internal,1),1) # select one internal node to "swap"
        ET=Swap(ET,SIN,minsample)
      }
    }
  }
  return(ET)
}
