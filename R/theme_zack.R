#'@name theme_zack
#'@title Removes gridlines
#'
#'
#'
#'
#'@export

library(tidyverse)

theme_zack<-
  function(){

    theme_bw()+

    theme(

      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()

      )

  }








