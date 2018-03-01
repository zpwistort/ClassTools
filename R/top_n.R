#'@name top_n
#'@title What is the top n things
#'
#'@description Takes a data.frame and produces the top n of a grouping.
#'
#'@param  df A dataframe
#'@param  groupings A character string of column to group
#'@param  n A number to clip count
#'@param  count A TRUE/FALSE, includes the count column of output data.frame, excludes count column, respectively
#'
#'
#'@export

library(tidyverse)

top_n<-
  function(df, groupings, n, count=F){

    top<-
    df%>%
      group_by_at(vars(one_of(groupings)))%>%
      summarize(

        Count=n()

      )%>%
      arrange(desc(Count))


    ##select whether to display count or not
    if(count==T){
      head(top,n)
    }else{
      head(top[,-2],n)
    }

  }





















