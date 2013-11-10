groupImpute <- function(dataframe,...,method='mean'){
	dataframe$row <- 1:nrow(dataframe)
    dfs <- split(dataframe,as.list(...))
    imputeddfs <- 
    lapply(dfs, function(d) {
        for(i in 1:length(d)){
            if(is.numeric(d[,i])){
                if(method=='mean')
                    d[is.na(d[,i]),i] <- mean(d[,i],na.rm=TRUE)
                if(method=='median')
                    d[is.na(d[,i]),i] <- median(d[,i],na.rm=TRUE)
                if(method=='draw1')
                    d[is.na(d[,i]),i] <- sample(na.omit(d[,i]),sum(is.na(d[,i])),TRUE)
                if(method=='draw2')
                    d[is.na(d[,i]),i] <- sample(na.omit(d[,i]),sum(is.na(d[,i])),FALSE)
            }
        }
        return(d)
    })
    out <- do.call(rbind,imputeddfs)
    out <- out[order(out$row),]
    out$row <- NULL
    rownames(out) <- 1:nrow(out)
    return(out)
}
