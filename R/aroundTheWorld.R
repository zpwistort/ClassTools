#'@name aroundTheWorld
#'@title Randomly selects from a list of greetings from around the world, lists the greeting and country of origin
#'
#'
#'
#'@return
#'
#'@export





aroundTheWorld<-
  function(){
    
  hello<- 
    data.frame(
    
     country  = c('USA','China', 'France', 'India','Japan', 'Zambia')
    ,greeting = c('Howdy', 'Nin Hao', 'Bonjour', 'Namaste', 'Ohayo', 'Bwanji')
  
    )
  
  out<- hello[sample(length(hello[,1]),1),]
    
  return(out)
  
  }































