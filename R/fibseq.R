#'@name fibseq
#'@title Generates a vector containing the Fibonacci sequence to length 'n'
#'
#'
#'
#'@param  n A numeric value specifiying the length of vector to generate
#'
#'@return
#'
#'@export





fibseq<-
  function(n){
    
    
    if(n <= 0){
      stop('0 is an invalid list length, choose a number of iterations')
      }
    
    
    out<-c(0,1)
    
    if(n==1){  out<-out[n]  }
    
    if(n==2){  out          }
    
    if(n>2){
    
    for(i in seq(1,(n-2))){   out<-c(out,out[i]+out[i+1])  }
    
    }
    
    return(out)
    
  }































