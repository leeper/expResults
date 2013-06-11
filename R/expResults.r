# FUNCTION TO PRODUCE A SIMPLE EXPERIMENTAL RESULTS TABLE
# FOUR-COLUMN TABLE WITH TREATMENT GROUP MEAN, SD, N, AND SE

# Copyright (C) 2011  Thomas J. Leeper
# This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.


# dv is a vector containing outcome values
# tr is a vector containing treatment group indicators
# labels is an optional call to a vector (by default, called ".groupNames"), which - if specified - labels resulting table rows
# object.name, if specified, stores the results table as an R object (e.g., to be used later for plotting)
# digits rounds all values to specified number of digits (default is 2)
# NAs handles NAs, by default removing them in calculating means and SDs

expResults <- function(dv,tr,labels=.groupNames,object.name=NULL,digits=2,NAs="na.rm"){
	# function to produce a simple treatment group results table
	if(!is.null(NAs) && NAs=="na.rm"){
		results <- cbind(round(by(dv,tr,mean,na.rm=TRUE),digits),
						round(by(dv,tr,sd,na.rm=TRUE),digits),
						by(dv,tr,length)) #treatment group means, SDs, and n
		for(i in 1:length(results[,3])){
			results[i,3] <- results[i,3]-sum(by(dv,tr,is.na)[[i]])
		}
	}
	else {
		results <- cbind(round(by(dv,tr,mean),2),
						round(by(dv,tr,sd),2),
						by(dv,tr,length)) #treatment group means, SDs, and n
	}
	# detect indicator vars for calculating SD of proportion
	if(dim(table(dv))==2){
		if(sum(names(table(dv)) %in% c("0","1"))==2) { # check if indicator
			sdprop <- function(x){
				sd <- sqrt(as.numeric(prop.table(table(x))[1]*prop.table(table(x))[1]))
			}
			results[,2] <- round(by(dv,tr,sdprop),digits)
		}
	}
	# combine and add standard errors
	results <- cbind(results,round(results[,2]/sqrt(results[,3]),2))
	colnames(results) <- c("Mean","SD","N","SE")
	# add treatment group labels, if applicable
	if(is.null(labels)){
		if(exists(".groupNames")==TRUE){
			if(length(.groupNames)==dim(results)[1])
				rownames(results) <- .groupNames
		}
	}
	else {
		if(length(labels)==dim(results)[1])
			rownames(results) <- labels
	}
	# store results in an R object, if requested
	if(!is.null(object.name)){
		label<- c(paste("Results stored in: ",object.name,sep=""))
		write.table(label,quote=FALSE,row.names=FALSE,col.names=FALSE)
		assign(object.name,results, envir = .GlobalEnv)
	}
	return(results) # return results matrix
}
