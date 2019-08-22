
# Credits -----------------------------------------------------------------
# Copyright (c) Peking University Cancer Hosptial, All Rights Reserved.
# Discription: this script make jitter boxplot with a matrix
# Author: Yang Du,
# Create date: 22 Aug 2019.
# Last update date: 22 Aug 2019.



## make boxplot of gene expression, input matrix, data.frame, column samples,row genes.

boxplot.jitter<-function(data.matrix=NULL,xlab="",ylab="Expression value distribution",title=""){
	require(ggplot2)
	require(reshape2)


	if(!is.null(data.matrix)){

		# plot distribution (in samples) for selected genes

		d.melt<-melt(data.frame(id=rownames(data.matrix),data.matrix),id.vars = 'id')

		p <-ggplot(d.melt, aes(x=variable, y=value, color=variable)) + 
			geom_boxplot(outlier.shape = NA) + 
			labs(title=title, x=xlab, y = ylab) + 
			theme_classic() + 
			theme(plot.title = element_text(hjust = 0.5)) +
			geom_jitter(shape=1, position=position_jitter(0.2), aes(colour = variable))

  		return(p)

	}

}

boxplot.matrix<-function(data=NULL,out="output.png",width=1000,height=1000,ylim=NULL,breaks=NULL,x.ticks=FALSE,title=NULL,
	xlab=NULL,ylab=NULL){
	
	# data matrix: row, genes; col, samples;


	require(ggplot2)
	require(reshape2)
	data.matrix <- data
	data.matrix$feature=rownames(data)
	data.melt<-melt(data.matrix,id="feature")
	colnames(data.melt)<-c('feature','run','value')

	png(out,width=width,height = height)

	p<-ggplot(data.melt,aes(run,value))+geom_boxplot(alpha=0.5,fill="#C7CEB2",outlier.colour=NA)+theme_bw()+theme(panel.grid=element_blank(),
		axis.text.x = element_text(angle=90, hjust=1, vjust=.5,size = 0, color = "black", face = "bold"),
		axis.text.y =element_text(size = 12, color = "black", face = "bold"),
	  	axis.title.x =element_text(size = 16, color = "black", face = "bold"),
	  	axis.title.y =element_text(size = 16, color = "black", face = "bold"),
	  	title=element_text(size = 16, color = "black", face = "bold"))

	if(!is.null(ylim)){
		p<-p+scale_y_continuous(limits = ylim)
	}

	if(!is.null(breaks)){
		p<-p+scale_y_continuous(breaks = ylim)
	}
	if(x.ticks){
		p<-p+theme(axis.ticks.x = element_blank())
	}
	if(!is.null(title)){
		p<-p+ggtitle(title)
	}
	if(!is.null(xlab)){
		p<-p+xlab(xlab)
	}
	if(!is.null(ylab)){
		p<-p+ylab(ylab)
	}

	print(p)
	dev.off()

}

