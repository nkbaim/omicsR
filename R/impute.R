
# Credits -----------------------------------------------------------------
# Copyright (c) Peking University Cancer Hosptial, All Rights Reserved.
# Discription: this script do imputation for missing values in expression data.
# Author: Yang Du,
# Create date: 22 Aug 2019.
# Last update date: 22 Aug 2019.



impute.ccb<-function(data.matrix=NULL,na.sample=0.8,na.feature=0.9, impute.model=c('MAR','MAR_MNAR'),
	MAR.method=c("KNN","MLE","SVD"), MNAR.method=c("MinDet", "MinProb","QRILC", "ZERO")){

	require(imputeLCMD)


	if(is.null(data.matrix)){
		stop("data.matrix should not be null")
	}else{

		data.matrix<-as.matrix(data.matrix)

		if(na.sample<0 | na.sample>1){
			stop("na.sample should between 0-1")
		}
		
		if(na.feature<0 | na.feature>1){
			stop("na.feature should between 0-1")
		}


		data.na.feature <- apply(is.na(data.matrix),1,sum)
		data.na.sample <- apply(is.na(data.matrix),2,sum)

		data.matrix.filtered <- data.matrix[data.na.feature <= na.feature*ncol(data.matrix),
			data.na.sample <= na.sample*nrow(data.matrix)]


		impute.model = match.arg(impute.model)
		MAR.method = match.arg(MAR.method)
		MNAR.method = match.arg(MNAR.method)

		# run model.Selector
		# model Selector 方法可以重建
		m.s = model.Selector(data.matrix.filtered)

		if(impute.model == "MAR"){
			if(MAR.method == "KNN"){
				data.matrix.imputed = impute.wrapper.KNN(dataSet.MCAR, K = 15)
			}else if(MAR.method == "MLE"){
				data.matrix.imputed = impute.wrapper.MLE(data.matrix.filtered)
			}else if(MAR.method == "SVD")
				data.matrix.imputed = impute.wrapper.SVD(data.matrix.filtered,K = 2)

		}else if(impute.model == "MAR_MNAR"){
			data.matrix.imputed = impute.MAR.MNAR(data.matrix.filtered, m.s, method.MAR = MAR.method, 
				method.MNAR = MNAR.method)
		}

		return(data.matrix.imputed)
	}

}
