rankhospital<-function(state,outcome,rank){
        
        data<-dataread()                                        ##reads the data from the file
        state_list<-as.data.frame(table(data$col7))             ##finds the total number of states present in the file 
        if(!(state %in% state_list[,1])){                       ##checks if the provided state is valid 
                stop("invalid state")
        }
        if(!(outcome %in% as.list(c("heart attack","heart failure", "pneumonia")))){            ##checks if the provided outcome is valid
                
                stop("invalid outcome")
                
        }
        
        data<-subset(data,col7==state,select=c(col2,col13,col19,col25))                 ##subset the data to only the data required
        index<-findindex(data,rank)
        if(index=="NA"){
                return("NA")
        }
        
        if(outcome=="heart attack"){
                
                
                temp_list<-sort(data[,2])        
                
                data<-subset(data,col13<=temp_list[index])
                data<-data[with(data,order(col13,col2)),]
                
                return(as.character(data[index,1]))
        
        }
        else if(outcome=="heart failure"){
                
                temp_list<-sort(na.omit(data[,3]))        
                data<-subset(data,col19<=temp_list[index])
                data<-data[with(data,order(col19,col2)),]
                return(as.character(data[index,1]))
        
        }
        else if(outcome=="pneumonia"){
                
                temp_list<-sort(na.omit(data[,4]))        
                data<-subset(data,col25<=temp_list[index])
                data<-data[with(data,order(col19,col2)),]
                return(as.character(data[index,1]))
        }        
        
        
}




findindex<-function(data,rank){
        if(length(data)>rank){
                return("NA")
        }
        else if(rank=="best"){
                return(1)
        }
        else if(rank=="worst"){
                return(nrow(na.omit(data)))
        }
        else if(is.numeric(rank)){
                return(rank)
        }
        else stop("invalid rank")
}

##function reads the data required by the best function
dataread<-function(){
        
        data<-read.csv("outcome-of-care-measures.csv")
        data[,11:46]<-lapply(data[,11:46],function(x){as.numeric(as.character(x))})               ##convert the read data into numeric value for the col required
        x = rep("col", dim(data)[2])                                                            ##prep stage to change the colname
        y = 1:dim(data)[2]
        colnames(data) = paste(x, y, sep="")                                                    ##change the colnames to simpler variable for easy processing
        data
}