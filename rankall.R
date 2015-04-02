rankall<- function(outcome,num="best"){
        
        data<-dataread()                                        ##reads the data from the file
       
        if(!(outcome %in% as.list(c("heart attack","heart failure", "pneumonia")))){            ##checks if the provided outcome is valid
                
                stop("invalid outcome")
                
        }
        
        data<-subset(data,select=c(col2,col7,col11,col17,col23)) 
        data[,1:2]<-lapply(data[,1:2],function(x){as.character(x)})
        state_list<-as.data.frame(table(data$col7))
        state_list<-lapply(state_list,function(x){as.character(x)})
        final<-data.frame(col2=character(),col7=character(),stringsAsFactors = FALSE)
        final_row<-1
        if(outcome=="heart attack"){
                
                for(i in state_list$Var1){
                        sub_data<-subset(data,col7==i)
                        sub_data<-subset(sub_data,col11!="NA")
                        sub_data<-sub_data[with(sub_data,order(col11,col2)),]
                        
                        index<-findindex(sub_data,num)
                        if(index=="NA"){
                                final[final_row,]<-list("NA",i)
                                
                        }
                        else{
                        final[final_row,]<-sub_data[index,1:2]           
                        }
                        
                        final_row<-final_row+1
                }
             
                
        }
        else if(outcome=="heart failure"){
                
                for(i in state_list$Var1){
                        sub_data<-subset(data,col7==i)
                        sub_data<-subset(sub_data,col17!="NA")
                        sub_data<-sub_data[with(sub_data,order(col17,col2)),]
                        
                        index<-findindex(sub_data,num)
                        if(index=="NA"){
                                final[final_row,]<-list("NA",i)
                                
                        }
                        else{
                                final[final_row,]<-sub_data[index,1:2]           
                        }
                        
                        final_row<-final_row+1
                }
               
                
                
        }
        else if(outcome=="pneumonia"){
                
                
                for(i in state_list$Var1){
                        sub_data<-subset(data,col7==i)
                        sub_data<-subset(sub_data,col23!="NA")
                        sub_data<-sub_data[with(sub_data,order(col23,col2)),]
                        
                        index<-findindex(sub_data,num)
                        if(index=="NA"){
                                final[final_row,]<-list("NA",i)
                                
                        }
                        else{
                                final[final_row,]<-sub_data[index,1:2]           
                        }
                        
                        final_row<-final_row+1
                }
                
                
        }    
        
        row.names(final)<-final[,2]
        colnames(final)<-c("hospital","state")
        return(final)
        
        
}


dataread<-function(){
        
        data<-read.csv("outcome-of-care-measures.csv")
        data[,11:46]<-lapply(data[,11:46],function(x){as.numeric(as.character(x))})               ##convert the read data into numeric value for the col required
        x = rep("col", dim(data)[2])                                                            ##prep stage to change the colname
        y = 1:dim(data)[2]
        colnames(data) = paste(x, y, sep="")                                                    ##change the colnames to simpler variable for easy processing
        data
}

findindex<-function(data,rank){
        
        if(rank=="best"){
                return(1)
        }
        else if(rank=="worst"){
                return(nrow(data))
        }
        else if(is.numeric(rank)&&nrow(data)<rank){
                return("NA")
        }
        else if(is.numeric(rank)){
                return(rank)
        }
        
        else stop("invalid rank")
}
