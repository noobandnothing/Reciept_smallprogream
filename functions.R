DEV_MODE = FALSE
library(crayon)

#function 0
Read_file<-function(){
  while(TRUE){
    tryCatch(
      {   
        print("Please select the database on which you wish to work")
        input_file<-read.csv(file.choose())
        break
      },
      error=function(cond) {
        if(DEV_MODE)
          cat(red(paste("E0: Invalid FILE OR ",cond,"\n")))
        else
          cat(red("E0: Invalid FILE\n"))
      }
    )
  }
  return(input_file)
}


#function 1
Message<-function(File){
  print(paste("Enter the item code from 1 to",max(File$Item)))
  print("If You finshed press 0 ")
}

#function 2
Found <- function(item_id,File){
  tryCatch(
    {   
      if(item_id %in% File$Item)
        return(TRUE)
      else if(is.na(item_id)){
        print("Invalid Input")
        return(NA)
      }else
        return(FALSE)
    },error=function(cond) {
      if(DEV_MODE)
        cat(red(paste("E2: Invalid Input OR ",cond,"\n")))
      else
        cat(red("E2: Invalid Input\n"))
      return(NA)
    },warning=function(cond) {
      if(DEV_MODE)
        cat(red(paste("W2: Invalid Input OR ",cond,"\n")))
      else
        cat(red("W2: Invalid Input\n"))
      return(NA)
    }
  ) 
}

#function 3
Get_quantity<-function(tmp){
  tmp_res <-aggregate(tmp$Item, by=list(tmp$Item), FUN=length)
  colnames(tmp_res)<-c("Item","Quantity")
  return(tmp_res)
}

#function 4
Loop<-function(File){
  tmp = {}
  while(TRUE){
    tryCatch(
      {   
        Message(File)
        choice <- as.integer(readline())
        if(choice == 0){
          if(length(tmp) > 0){
            tmp <-data.frame(tmp)
            colnames(tmp)<-('Item')
          }
          break
        }else{
          if(Found(choice,File)){
            tmp<-append(tmp,choice)
            print(paste("Input ",choice, " was added"))
          }else
            cat(red("O4-1: Invalid Input (Item is not found)\n"))
        }
      },
      error=function(cond) {
        if(DEV_MODE)
          cat(red(paste("E4: Invalid Input OR ",cond,"\n")))
        else
          cat(red("E4: Invalid Input\n"))
        return(NA)
      },
      warning=function(cond) {
        if(DEV_MODE)
          cat(red(paste("W4: Invalid Input OR ",cond,"\n")))
        else
          cat(red("W4: Invalid Input\n"))
        return(NA)
      }
    ) 
  }
  return(tmp)
}

#function 5
Print_item<-function(dataset,File){
  tryCatch(
    {   
    	 output<-logo()
    	 total<-sum(File[dataset[,1],]$Price)
    	 dataset=Get_quantity(dataset)
    	 no_items=sum(dataset$Quantity)
    	 Price<-File[dataset$Item,]$Price
    	 dataset<-cbind(dataset,Price)
    	 Mean<-mean(dataset$Price)
    	 Min<-min(dataset$Price)
    	 Max<-max(dataset$Price)
    	 output<-paste(output,"                              Price","\n")
	     for(i in 1:length(dataset$Item)){
      	     output<-paste(output,dataset[i,]$Quantity," x item ", dataset[i,]$Item,"                ",dataset[i,]$Price,"\n")
      	     dataset[i,]
    	 }    
       output<-paste(output,"Number of items              ",no_items,"\n")
    	 output<-paste(output,"mean price of all items      ",Mean,"\n")
	     output<-paste(output,"min item price               ",Min,"\n")
	     output<-paste(output,"max item price               ",Max,"\n")
    	 output<-paste(output,"Required to pay              ",total,"\n")
	     cat(green(paste(output,"\n"))) 
    },error=function(cond) {
      if(DEV_MODE)
        cat(red(paste("E5 : DataSet is empty OR ",cond,"\n")))
      else
        cat(red("E5 : DataSet is empty\n"))
    },warning=function(cond) {
      if(DEV_MODE)
        cat(red(paste("W5 : DataSet is empty OR ",cond,"\n")))
      else
        cat(red("W5 : DataSet is empty\n"))
    }
  ) 
} 

logo<-function(){
  out<-paste("           _    __  __ __  __ \n",
             "         / \\  |  \\/  |  \\/  |\n",
             "        / _ \\ | |\\/| | |\\/| |\n",
             "       / ___ \\| |  | | |  | |\n",
             "      /_/   \\_\\_|  |_|_|  |_|\n")
  writeLines(out)
}

# Main Function
main<-function(){
  File<-Read_file()
  select_items<-Loop(File)
  if(length(select_items) != 0)
    Print_item(select_items,File)
}
