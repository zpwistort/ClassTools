#'@name density2
#'@title Density function
#'
#'@description Takes a data.frame and column of that data.frame to build a density plot using ggplot.
#'
#'@param  df A dataframe
#'@param  var1 A character string of the column name
#'
#'@return Density Plot of input column
#'
#'@export


library(tidyverse)


density2<-
  function(df,var1){


    ggplot(data=df, aes(df[,var1]))+
      geom_density()+
      scale_x_continuous()+
      xlab(var1)

  }





