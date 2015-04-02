##Program finds the lowests moratlity rate hospital for the given state and outcome

best<- function(state,outcome){
        
        data<-dataread()                                        ##reads the data from the file
        state_list<-as.data.frame(table(data$col7))             ##finds the total number of states present in the file 
        if(!(state %in% state_list[,1])){                       ##checks if the provided state is valid 
                stop("invalid state")
        }
        if(!(outcome %in% as.list(c("heart attack","heart failure", "pneumonia")))){            ##checks if the provided outcome is valid
                
                stop("invalid outcome")
                
        }
     
        data<-subset(data,col7==state,select=c(col2,col13,col19,col25))                 ##subset the data to only the data required
        if(outcome=="heart attack"){
                min_value=min(data[,2],na.rm=TRUE)                                      ##find the minimum mortality rate
                data<-subset(data,col13==min_value)                                     ##subset the data to only the rows with minimum mortality rate
                if(length(data)!=1){                                                    ##check if the result has more than one value 
                        data<-sort(as.character(data[,1]))                              ##if yes then sort the list on alphabetic order and return the first value
                        return(data[1])
                }
                return(as.character(data[,1]))
        }
        else if(outcome=="heart failure"){
                min_value=min(data[,3],na.rm=TRUE)
                data<-subset(data,col19==min_value)
                if(length(data)!=1){
                        data<-sort(as.character(data[,1]))
                        return(data[1])
                }
                return(as.character(data[,1]))
        }
        else if(outcome=="pneumonia"){
                min_value=min(data[,4],na.rm=TRUE)
                data<-subset(data,col25==min_value)
                if(length(data)!=1){
                        data<-sort(as.character(data[,1]))
                        return(data[1])
                }
                return(as.character(data[,1]))
        }   
        
        
}

##function reads the data required by the best function
dataread<-function(){
        
        data<-read.csv("outcome-of-care-measures.csv")
        data[11:46]<-lapply(data[11:46],function(x){as.numeric(as.character(x))})               ##convert the read data into numeric value for the col required
        x = rep("col", dim(data)[2])                                                            ##prep stage to change the colname
        y = 1:dim(data)[2]
        colnames(data) = paste(x, y, sep="")                                                    ##change the colnames to simpler variable for easy processing
        data
}