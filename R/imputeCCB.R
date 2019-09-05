# Credits -----------------------------------------------------------------
# Copyright (c) Peking University Cancer Hosptial, All Rights Reserved.
# Author: Du, Yang
# Discription: imputation functions
# Create date: 4 Sep 2019.
# Last update date: 4 Sep 2019.

# use data filter for all functions
filterByNA<-function(data,filterRule=1,dims=1){
	data.na<-apply(data,dims,is.na)/ncol(data)
	data.filtered<-data[data.na < filterRule,]
	return(data.filtered)
}

# knn
imputeCCB.knn<-function(data,filterRule=1,k = 10, rowmax = 0.5, colmax = 0.8, 
	maxp = 1500, rng.seed=362436069)
	
	require(impute,quietly = T)
	data.filtered<-filterByNA(data,filterRule)
	data.imputed <- impute.knn(as.matrix(data.filtered),k=k,rowmax=rowmax,colmax=colmax,maxp=maxp,rng.seed=rng.seed)
	return(data.imputed$data)
}

# multiple imputation
imputeCCB.mi<-function(data,filterRule=1,m=5){

	require(mice,quietly = T)

	data.filtered<-filterByNA(data,filterRule)
	imp<-mice(data.filtered, m = m)
	dat.list<-lapply(1:m, function(x){complete(imp,x)})
	return(dat.list)
}

# seqknn
# package too old

# MissForest
imputeCCB.missForest<-function(data,filterRule=1,ntree=100,maxiter=10){

	require(missForest,quietly = T)
	data.filtered<-filterByNA(data,filterRule)
	data.imp <- missForest(data.filtered, xtrue = iris, verbose = TRUE,ntree=ntree,maxiter = maxiter)
	return(data.imp)
}

# LLSimpute
imputeCCB.llsImpute<-function(data,filterRule=1,k=10,correlation="pearson"){

	require(pcaMethods,quietly = T)
	data.filtered<-filterByNA(data,filterRule)
	result <- llsImpute(data.filtered, k = k, correlation=correlation, allVariables=TRUE)
	return(completeObs(result))
}

# SVD
imputeCCB.svd<-function(data,filterRule=1,nPcs = 2){


	require(pcaMethods,quietly = T)
	data.filtered<-filterByNA(data,filterRule)
	result<-svdImpute(data.filtered,nPcs = nPcs)
	cObs <- completeObs(result)
	return(cObs)
}

# MLE

imputeCCB.mle<-function(data,filterRule=1){

	require(pcaMethods,quietly = T)
	data.filtered<-filterByNA(data,filterRule)
	data.mle<-impute.wrapper.MLE(data.filtered)
	return(data.mle)
}

# minProb
imputeCCB.minProb<-function(data,filterRule=1,q = 0.01,tune.sigma = 1){
	require(imputeLCMD,quietly = T)
	data.filtered<-filterByNA(data,filterRule)
	data.minProb<-impute.MinProb(data.filtered, q = q, tune.sigma = tune.sigma)
	return(data.minProb)
}

# minDet
imputeCCB.minDet<-function(data,filterRule=1){
	require(imputeLCMD,quietly = T)
	data.filtered<-filterByNA(data,filterRule)
	data.minDet <- impute.MinDet(data.filtered)
	return(data.minDet)
}

# QRILC
imputeCCB.QRILC<-function(data,filterRule=1,tune.sigma = 1){

	require(imputeLCMD,quietly = T)
	data.filtered<-filterByNA(data,filterRule)
	data.QRILC <- impute.QRILC(data.filtered, tune.sigma = tune.sigma)
	return(data.QRILC[[1]])
}

# zero
imputeCCB.zero<-function(data,filterRule=1){
	require(imputeLCMD,quietly = T)
	data.filtered<-filterByNA(data,filterRule)
	data.zero <- impute.ZERO(data.filtered)
	return(data.zero)
}

impute.model <-function(){}

impute.ccb <-function(data,filterRule=1,impute.model=NULL){}
