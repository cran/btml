#training & test based ROC
binaryMLfit= function(y, x, FUN){
  #y: binary vector (0/1)
  #x: predictor matrix
  #FUN: name of ML algorithm with binary outcomes

  #1. initial parameter
  FUN=match.fun(FUN)
  #2. train
  res=FUN(y,x)
  #4. return
  return(res=res)
}

# predict function
binaryMLfit_predict = function(model, newx, FUN){
  #1. initial parameter
  FUN=match.fun(FUN)
  #2. train and validation
  res=FUN(model, newx)
  #4. return
  return(res=res)
}

###
#A. ML with binary outcomes (models with errors/warnings will not be considered)
#   y is factor & x is data.frame
###
#A1 lasso
b_lasso=function(y,x){
  fit=NULL
  x=data.matrix(x)
  suppressWarnings(try(fit<-glmnet::cv.glmnet(x,y,alpha=1,family="binomial"),silent=TRUE))
  return(fit)
}
b_lasso_predict=function(fit,newx){
  y.hat=rep(NA,nrow(newx))
  if(!is.null(fit)){
    #if(sum(abs(as.numeric(coef(fit)))>0)>1){ #at least one of the coef + int are not zero
      newx=data.matrix(newx)
      y.hat=as.numeric(predict(fit,newx=newx,s="lambda.min",type="response"))
    #}
  }
  return(y.hat)
}

#A2 random forest
b_rf=function(y,x){
  df=data.frame(x=x, y=y)
  colnames(df)=c(paste0("x",1:ncol(x)),"y")
  fit=NULL
  suppressWarnings(try(fit<-randomForest::randomForest(y~.,data=df),silent=TRUE))
  return(fit)
}
b_rf_predict=function(fit,newx){
  y.hat=rep(NA,nrow(newx))
  if(!is.null(fit)){
    df=data.frame(x=newx)
    colnames(df)=paste0("x",1:ncol(newx))
    y.hat=as.numeric(predict(fit, df, type = "prob")[,2])
  }
  return(y.hat)
}

#A3. svm
b_svm = function(y, x, kernel="radial"){
  df=data.frame(x=x, y=y)
  colnames(df)=c(paste0("x",1:ncol(x)),"y")
  fit=NULL
  suppressWarnings(try(fit<-e1071::svm(y~.,data = df, kernel=kernel, probability = TRUE),silent=TRUE))
  return(fit)
}
b_svm_predict=function(fit,newx){
  y.hat=rep(NA,nrow(newx))
  if(!is.null(fit)){
    df=data.frame(x=newx)
    colnames(df)=paste0("x",1:ncol(newx))
    predictedValue=predict(fit, df, probability = TRUE)
    predictedValue2=attr(predictedValue, "probabilities")
    k=which(colnames(predictedValue2)=="1")
    y.hat=as.numeric(as.numeric(predictedValue2[,k]))
  }
  return(y.hat)
}
