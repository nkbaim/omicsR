
# Credits -----------------------------------------------------------------
# Copyright (c) Peking University Cancer Hosptial, All Rights Reserved.
# Discription: this script make jitter boxplot with a matrix
# Author: Yang Du,
# Create date: 22 Aug 2019.
# Last update date: 22 Aug 2019.



fpkm2tpm<-function(data.fpkm){

	t1<-Sys.time()
	data.tpm <- apply(data.fpkm,2,function(x){x/sum(x)*1000000})
	t2<-Sys.time()
	print(t2-t1)
	return(data.tpm)
}
