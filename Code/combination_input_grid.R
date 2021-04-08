


# this function returns a matrix with all combination of neurons : note vectors with 1 0 0 0 .. are still in the output
# but 100 ...1 not 


combination_input_grid=function(maxlayer,maxneuron)
{ 
 starttime=Sys.time()
  
 input = vector(mode = "list", length = maxlayer)
 input[[1]]=1:maxneuron
 if( maxlayer > 1){for(i in 2:maxlayer){input[[i]]=0:maxneuron}}

 grid=expand.grid( input ) 
 

 truevec=rep(TRUE,dim(grid)[1])
 for(row in 1:dim(grid)[1])
 {
   for(col in (2:(dim(grid)[2]-1)) )
   {
     if  ( grid[row,col]< grid[row,col+1] & grid[row,col]==0){truevec[row]=FALSE } 
   }
 }
 combmat=grid[truevec,]
 
 print(paste("duration: " ,Sys.time()-starttime))
   
   
 return(combmat)
} 


input_grid <- function(n=3, l=3) {
   anz <- n^(1:l)
   mat <- matrix(0, nrow=sum(anz), ncol=l)
   
   
   i_end <- cumsum(anz)
   i_start <- anz-1
   i_start <- i_end - i_start
   
   
   for(j in 0:(length(anz)-1)) {
      for (i in (1+j):l) {
         mat[i_start[i]:i_end[i], i-j] <- rep(1:n, rep(n^(j), n))
      }
   }
   return(as.data.frame(mat))
}