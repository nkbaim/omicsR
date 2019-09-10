plot.coeffcient<-function(x=NULL,y=NULL,xlab="x",ylab="y",main="",){

	cor.xy<-cor.test(x,y)
	plot(x,y,pch=20,col='royalblue',xlab="",ylab="",main=main,
		xlim=c(min(x,y),max(x,y)),ylim=c(min(x,y),max(x,y)))

	text(x=min(x,y),y=max(x,y)-0.5,labels=paste0('r=',round(cor.xy$estimate,2),', ','p-value=',format(cor.xy$p.value, 
		scientific=TRUE,digits=2)),font=3,adj=0,cex=1) 
	
	mtext(xlab,side=1,line=2.5,cex=1)
	mtext(ylab,side=2,line=2.5,cex=1)

	abline(lm(y ~ x),col='indianred2',lwd=2)
	dev.off()
}
