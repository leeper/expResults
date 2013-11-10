trplot <- function(dv=NULL,tr=NULL,results=NULL,mean=TRUE,conf.int=NULL,distrib=FALSE,scatter=NULL){
	# function to plot treatment group means and, optionally, SEs, CIs, and data dispersions
	# convert results table if supplied
	if(is.null(results))
		results <- expResults(dv,tr)
	
	# store values
	means <- results[,1]
	ses <- results[,4]
	if(!is.null(rownames(results))){
		labels <- rownames(results)
	}
	else{
		labels <- seq(1:length(means))
	}
	groups <- length(means)
	min <- range(means)[1]
	max <- range(means)[2]
	range <- range(means)[2]-range(means)[1]
	lower <- min(min-(.1*range),min(means-(2*ses))-(.1*range))
	upper <- max(max+(.1*range),max(means+(2*ses))+(.1*range))
	
	# make plot
	plot(seq(.5,groups/2,.5),results[,1],xlim=c(0,(groups/2)+(.05*groups)),ylim=c(lower,upper),bty="n",ylab=NA,yaxt="n",xlab=NA,xaxt="n",pch=20)
	axis(1,at=seq(.5,groups/2,.5),labels=labels,las=1)
	axis(2,las=2)
	if(distrib==TRUE){
		maxvals <- by(dv,tr,max,na.rm=TRUE)
		minvals <- by(dv,tr,min,na.rm=TRUE)
		for(i in 1:length(means)){
			lines(x=c((i/2),(i/2)),y=c(minvals[i],maxvals[i]),lwd=1,col="gray20")
		}
	}
	if(!is.null(conf.int)){
		if(!conf.int==FALSE){
			for(i in 1:length(means)){
				lines(x=c((i/2),(i/2)),y=c(means[i]-(conf.int*ses[i]),means[i]+(conf.int*ses[i])),lwd=2,col="gray40")
			}
		}
	}
	if(mean==TRUE){
		for(i in 1:length(means)){
			lines(x=c((i/2)-.05,(i/2)+.05),y=c(means[i],means[i]),lwd=4,col="black")
		}
	}
}




