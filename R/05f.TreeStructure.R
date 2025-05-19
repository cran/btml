#in order to avoid there is one number in the pool, we need to redefine resample function
resample <- function(x, ...) x[sample.int(length(x), ...)]

#parent node to compute ...
parentNode <- function(x){
  if(x %% 2==0){
    return(x/2)
  }else{
    return((x-1)/2)
  }
}

# neighbor node to compute w for Grown, Prune, ...
neighbourNode = function(x){
  if(x %% 2==0){
    return(x+1)
  }else{
    return(x-1)
  }
}

# Define swap function for SWAP
swapSplit <- function(x,i,j) {
  x[c(i,j)] <- x[c(j,i)];
  x
}

# child nodes
childNodes=function(x){
  x=c(2*x,2*x+1)
  return(x)
}
