
# Credits -----------------------------------------------------------------
# Copyright (c) Peking University Cancer Hosptial, All Rights Reserved.
# Author: Yang Du,
# Create date: 5 Sep 2019.
# Last update: 5 Sep 2019.
# 

mergeReplicates<-function(data.matrix,run.info=NULL,method=c('mean','median')){

	method=match.arg(method)

	# 循环sample
	# colnames(run.info)<-c('run','sample')

	samples<-unique(run.info$sample)

	result<-matrix(0,nrow=nrow(data.matrix),ncol=length(samples))

	for(i in 1:length(samples)){

		run_ids<-run.info[run.info$sample==samples[i],]$run
		data.sample<-as.matrix(data.matrix[,run_ids])
		if(method =='mean'){
			result[,i]<-apply(data.sample,1,mean)
		}else if(method =='median'){
			result[,i]<-apply(data.sample,1,median)
		}
	}
	colnames(result)<-samples
	rownames(result)<-rownames(data.matrix)
	return(result)

}
