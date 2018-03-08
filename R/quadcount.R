#'@name quadcount
#'@title Plots a map
#'
#'@description A function to subdivide a given area of points and sum the values of points in each box. It is necessary to supply a data.frame with columns 
#'titled'x' and 'y'. Must indicate which column contains the values to sum ('valNum' parameter). Generates a df titled 'pixels'
#'test.
#'
#'
#'@param  df A data.frame
#'@param  xbreak A numeric value to subdivide the x-axis
#'@param  ybreak A numeric value to subdivide the x-axis
#'@param  val A character string naming a vector of values in df to sum.
#'@param  pixSize A numeric value to size the points in the generate plot
#'@param  pixAlpha A numeric value, from 0 to 1. Controls the opacity of plotted points
#'@param  pix A logic value, default TRUE. Controls whether points are drawn in plot.
#'            Setting False will increase plot speed of large areas.
#'
#'@return List item 1= list of boxes and pixel counts; List item 2= ggplot of quads and counts
#'
#'@export


library(tidyverse)

quadcount<-
  function(df,val,xbreak,ybreak,pixSize=2,pixAlpha=1, pix=T){


## Error message if df$x not found
    
  # if(is.null(df$x) == T){
  #   
  #   stop(paste('
  #              
  #              x-coords must be in a column titled "x"'),'
  #        
  #        ')
  #   
  # }

## Error message if df$y not found
  if(is.null(df$y) == T){
      
    stop(paste('
               
               y-coords must be in a column titled "y"'),'
         
         ')
      
  }

## Error message if val is not a column in df
  if(length((which(colnames(df)==val))) == 0){
      
      stop(paste('
                 
                 val must be a column in df'),'
           
           ')
      
  }

    
    
## variable list

# df       = pixel10
# xbreak   = 5
# ybreak   = 5
# rangeX   = range(pixel10$x)
# rangeY   = range(pixel10$y)
# pixSize  = 2
# pixAlpha = 1

rangeX      = range(df$x)
rangeY      = range(df$y)
nBox        = (rangeY[2]/ybreak)*(rangeX[2]/xbreak)
test        = data.frame()
out         = c()
pixelNumber = c()
boxNumber   = c()
x           = c()
y           = c()
labX        = c()
labY        = c()



##  Error message if num of boxes does not pixels evenly x
if((rangeX[2]/xbreak)%%1 != 0){
  
  
  stop(paste('
             
              Num of boxes must evenly divide pixels: ','(xbreak)  ',rangeX[2],'/',xbreak,'=',(rangeX[2]/xbreak),'
             
             '))
  
}

##  Error message if num of boxes does not pixels evenly y
if((rangeY[2]/ybreak)%%1 != 0){
  
  
  stop(paste('
             
             Num of boxes must evenly divide pixels: ','(ybreak)  ',rangeY[2],'/',ybreak,'=',(rangeY[2]/ybreak),'
             
             '))
  
}





## builds windows
for(i in seq(0,(rangeY[2]-ybreak),ybreak)){
  
  #bin the y's 
  box <- df%>%
    
    filter(
      
      y >= i+1 &
      y <= i+ybreak
      
    )
  
  for(j in seq(0,(rangeX[2]-xbreak),xbreak)){
    
    box2<-
      box%>%
      filter(
        x >= j+1 &
        x <= j+xbreak
      )
    
    test <- rbind(test,box2)
  }
  
}

test<-
  test%>%
  mutate(
    
    id=factor(rep(seq(1,nBox),each=length(test[,1])/nBox))
  )









##Count function
for(i in seq(1,nBox)){
  
  pixelNumber <-
  
     c(pixelNumber, sum(test[,val][which(test$id==i)]))
  
  
  boxNumber <-
    
     c(boxNumber, i)
  
  
}

out<- data.frame(boxNumber,pixelNumber)

# out








## poly data.frame for polygon graphic





for(i in seq(0,(rangeY[2]-ybreak),ybreak)){
  
  
  for(j in seq(0,(rangeX[2]-xbreak),xbreak)){
    
    y<-c(y, (i), (i), i+(ybreak), i+(ybreak) )
    x<-c(x, (j), j+(xbreak), j+(xbreak), (j) )
  
  }


}




poly2 <- data.frame(
        #bottom left       top right         bottom right          top left
#  x = c(1,  5,  5, 1,     1, 5,  5,  1,      5, 10, 10, 5,        5, 10, 10,  5)
# ,y = c(1,  1,  5, 5,     5, 5, 10, 10,      1,  1,  5, 5,        5,  5, 10, 10)
  x
 ,y
 ,val = rep(seq(1,nBox),each=length(x)/nBox)
 ,id  = factor(rep(seq(1,nBox),each=length(x)/nBox))
 ,lab = rep(out[,2], each=length(x)/nBox)
)






for(i in seq(1,length(poly2$x))){

  poly2$x[i] <- poly2$x[i] + 0.5#(xbreak/10)

  poly2$y[i] <- poly2$y[i] + 0.5#(ybreak/10)


}







## labels for count


for(i in seq(1,nBox)){
  
  
  labX[i] <- test$x[median(which(test$id == i))]
  
  labY[i] <- test$y[median(which(test$id == i))]
  
  
}



label<-
  data.frame(

     labX

    ,labY

    ,lab = out[,2]


  )














## plot polygon boxes with points


  if(pix==T){
    p<-
      ggplot(data=test, aes(x=x,y=y))+
    
        geom_point(aes(color=valFact),size =pixSize, alpha=pixAlpha, shape=15)+ #, alpha=0.4)+
        geom_polygon(data=poly2, aes(x=x,y=y,group=id), fill=NA, size=1, color='black')+
        geom_text(data=label,aes(label=lab,x=labX,y=labY))+
        coord_fixed(0.95)+
    
        ggtitle(paste('Function poly test: ', rangeX[2],' x ', rangeY[2], sep=''))+
        guides(color = guide_legend(title = "Value"))+
        labs(caption = paste(' (','pixel size ', pixSize,')', sep=''))
  }else{
    p<-
      ggplot(data=test, aes(x=x,y=y))+
      
      #geom_point(aes(color=valFact),size =pixSize, alpha=pixAlpha, shape=15)+ #, alpha=0.4)+
      geom_polygon(data=poly2, aes(x=x,y=y,group=id), fill=NA, size=1, color='black')+
      geom_text(data=label,aes(label=lab,x=labX,y=labY))+
      coord_fixed(0.95)+
      
      ggtitle(paste('Function poly test: ', rangeX[2],' x ', rangeY[2], sep=''))+
      guides(color = guide_legend(title = "Value"))+
      labs(caption = paste(' (','pixel size ', pixSize,')', sep=''))    
    
    
  }
    
    
    
    

return(list(out, p))

}





## generates a test plot titled 'pixels',   10 x 10 size
valNum = sample(c(0,1),replace = T, 150)
pixel10 <- data.frame(
  
  x   = rep(seq(1,15,1),each = 10)
  
  ,y   = rep(seq(10,1,-1),15)
  
  ,valNum
  
  ,valFact = factor(valNum)
  
)




quadcount(pixel10,val='valNum',xbreak=5,ybreak=5) #,pixSize = 13.5, pixAlpha = 0.75)




























