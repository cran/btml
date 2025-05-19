\name{btml}
\alias{btml}
\title{Bayeisan Treed Machine Learning}
\description{The treed model generalizes the Bayesian classification and regression tree (BCART) model by partitioning subjects into terminal nodes and tailoring machine learning model to each terminal node.}
\usage{
  btml(y,x,z,ynew,xnew,znew,MLlist,sparse,nwarm,niter,minsample,base,power)
}
\arguments{
  \item{y}{Response vector. If a factor codied as 0 or 1, classification is assumed. Otherwise, regression is assumed.}
  \item{x}{Data.frame or matrix of predictors that is used to estimate a tree structure.}
  \item{z}{Data.frame or matrix of predictors that is used in terminal node specific ML models. See the description below about the difference between x and z.}
  \item{ynew}{Response vector for the test set corresponding to y (default ynew=NULL).}
  \item{xnew}{Data.frame or matrix for the test set corresponding to x (default xnew=NULL).}
  \item{znew}{Data.frame or matrix for the test set corresponding to z (default znew=NULL).}
  \item{MLlist}{Candidate ML models that can be assigned to each terminal node (default MLlist=c("lasso","rf","svm")). Any other ML models can be included. See the details below.}
  \item{sparse}{Whether to perform variable and machine learning model selections based on a sparse Dirichlet prior rather than simply uniform (default sparse=TRUE).}
  \item{nwarm}{Number of warm-up (default nwarm=1000).}
  \item{niter}{Number of iteration (defaut niter=1000).}
  \item{minsample}{The number of minimum sample size per each node, i.e., length(y)>min_sample if y is continuous and min(length(y==1),length(y==0))>min_sample (default min_sample=20).}
  \item{base}{Base parameter for tree prior (default base=0.95).}
  \item{power}{Power parameter for tree prior (default power=0.8).}
}
\details{This treed model uses stochastic search to find the optimal decision-tree based rule that partitions subjects into distinct terminal nodes and assigns the most effective ML model to each terminal node. For high-dimensional variables, increase nwarm=10000 and niter=10000, or more; and increase minsample.

Ideally, there are two sets of predictors, x and z, e.g., demographic variables and biomarkers, where x is used to split trees, and z is assigned to each terminal node. However, if this is not possible, it allows us to use the same x and z in the btml function, e.g., btml(y=y, x=x, z=x, ...).

Regarding the node numbers, an internal node s has left and right child nodes 2*s and 2*s+1, respectively, where node 1 is a root node; nodes 2 and 3 are left and right child nodes of node 1; nodes 4 and 5 are left and right nodes of node 2; and so on.

Currently, lasso(), randomForest(), and svm(...,kernel="radial") functions from R packages cv.glmnet, randomForest, and e1071 are supported, but any ML models can be flexibly added to terminal nodes, e.g., see the example #3 below.
}

\value{An object of class btml, which is a list with the following components:
\item{terminal}{Node numbers in terminal nodes.}
\item{internal}{Node numbers in internal nodes.}
\item{splitVariable}{Variable (i.e., x[,u] if splitVariable[k]=u) used to split the internal node k.}
\item{cutoff}{cutoff[k] is the cutoff value to split the internal node k.}
\item{selML}{ML model assigned to the terminal node t.}
\item{fitML}{fitML[[t]] is the fitted ML model at the terminal node t \eqn{\in} terminal.}
\item{y.hat}{Estimated y (or estimated probability) on the training set if y is continuous (or binary).}
\item{node.hat}{Estimated node on the training set.}
\item{mse}{Training MSE.}
\item{bs}{Training Brier Score.}
\item{roc}{Training ROC curve.}
\item{auc}{Training AUC.}
\item{y.hat.new}{Estimated y (or estimated probability) on the test set if y is continuous (or binary).}
\item{node.hat.new}{Estimated node on the test set.}
\item{mse.new}{Test MSE.}
\item{bs.new}{Test Brier Score.}
\item{roc.new}{Test ROC curve.}
\item{auc.new}{Test AUC.}
}

\references{Yaliang Zhang and Yunro Chung, Bayesian treed machine learning model (in preperation)}
\author{Yaliang Zhang [aut], Yunro Chung [aut, cre]}

\examples{
\donttest{
set.seed(123)
###
#1. continuous y
###
n=200*2 #n=200 & 200 for training & test sets

x=matrix(rnorm(n*10),n,10) #10 predictors
z=matrix(rnorm(n*10),n,10) #10 biomarkers

xcut=median(x[,1])
subgr=1*(x[,1]<xcut)+2*(x[,1]>=xcut) #2 subgroups

lp=rep(NA,n)
for(i in 1:n)
  lp[i]=1+3*z[i,subgr[i]]
y=lp+rnorm(n,0,1)

idx.nex=sample(1:n,n*1/2,replace=FALSE)
ynew=y[idx.nex]
xnew=x[idx.nex,]
znew=z[idx.nex,]

y=y[-idx.nex]
x=x[-idx.nex,]
z=z[-idx.nex,]

fit1=btml(y,x,z,ynew=ynew,xnew=xnew,znew=znew)
fit1$mse.new
plot(fit1$y.hat.new~ynew,ylab="Predicted y",xlab="ynew")

###
#2. binary y
###
x=matrix(rnorm(n*10),n,10) #10 predictors
z=matrix(rnorm(n*10),n,10) #10 biomarkers

xcut=median(x[,1])
subgr=1*(x[,1]<xcut)+2*(x[,1]>=xcut) #2 subgroups

lp=rep(NA,n)
for(i in 1:n)
  lp[i]=1+3*z[i,subgr[i]]
prob=1/(1+exp(-lp))
y=rbinom(n,1,prob)
y=as.factor(y)

idx.nex=sample(1:n,n*1/2,replace=FALSE)
ynew=y[idx.nex]
xnew=x[idx.nex,]
znew=z[idx.nex,]

y=y[-idx.nex]
x=x[-idx.nex,]
z=z[-idx.nex,]

fit2=btml(y,x,z,ynew=ynew,xnew=xnew,znew=znew)
fit2$auc.new
plot(fit2$roc.new)

###
#3. add new ML models
#   1) write two functions:
#      c_xx & c_xx_predict if y is continuous or
#      b_xx & b_xx.predict if y is binary
#   2) MLlist includes xx, not c.xx nor b.xx.
#   3) run btml using the updated MLlist.
#   The below is an example of adding ridge regression.
###
#3.1. ridge regression for continuous y.
c_ridge=function(y,x){
  x=data.matrix(x)
  fit=NULL
  suppressWarnings(try(fit<-glmnet::cv.glmnet(x,y,alpha=0),silent=TRUE))
  return(fit)
}
c_ridge_predict=function(fit,xnew){
  y.hat=rep(NA,nrow(xnew))
  if(!is.null(fit)){
    xnew=data.matrix(xnew)
    y.hat=as.numeric(predict(fit,newx=xnew,s="lambda.min",type="response"))
  }
  return(y.hat)
}

#3.2. ridge regression for binary y.
b_ridge=function(y,x){
  x=data.matrix(x)
  fit=NULL
  suppressWarnings(try(fit<-glmnet::cv.glmnet(x,y,alpha=1,family="binomial"),silent=TRUE))
  return(fit)
}
b_ridge_predict=function(fit,xnew){
  y.hat=rep(NA,nrow(xnew))
  if(!is.null(fit)){
    xnew=data.matrix(xnew)
    y.hat=as.numeric(predict(fit,newx=xnew,s="lambda.min",type="response"))
  }
  return(y.hat)
}

#3.3. update MLlist
MLlist=c("lasso","ridge")
fit3=btml(y,x,z,ynew=ynew,xnew=xnew,znew=znew,MLlist=MLlist)
fit3$auc.new
plot(fit3$roc.new)
}
}
