#'@name mapIt
#'@title Plots a map
#'
#'@param  df A data.frame
#'@param  plotLat A character string name of the latitude column from the data.frame
#'@param  plotLong A character string name of the longitude column from the data.frame
#'@param  col A character string name of a column for a color ( aes(color=?) ), default to NULL
#'@param  latRange A tuple vector with the min then max latitudes desired, default  c(-90,90)
#'@param  longRange A tuple vector with the min then max longitudes desired, default c(-180,180)
#'
#'@return Map plot
#'
#'@export

library(tidyverse)
library(maps)

mapIt<-
  function(df,plotLat, plotLong, col=NULL, latRange=c(-90,90), longRange=c(-200,200)){


    ## pull world map polygons from 'Maps' package
    world<-map_data('world')


    ## pull max and mins from latitude and longitude variables
    latmin<-latRange[[1]]
    latmax<-latRange[[2]]
    longmin<-longRange[[1]]
    longmax<-longRange[[2]]

    ## reduce map to latitude and longitude requested
    reduced<-filter(world, lat>=latmin
                    & lat<=latmax
                    & long<=longmax
                    & long>=longmin)

    #colblind<-c('yellow',"#0072B2","#000000", "#E69F00", "#56B4E9", "#009E73",
    #            "#F0E442", "#D55E00", "#CC79A7")



    ## decide what to sequence by for the gridline breaks
    if(as.integer((latmax-latmin)/5) > 10){
      by1=10
      #print(paste('by1',by1,sep='='))
    }else{
      by1=5
      #print(paste('by1',by1,sep='='))
    }

    if(as.integer((latmax-latmin)/5) > 10){
      by2=25
      #print(paste('by2',by2,sep = '='))
    }else{
      by2=10
      #print(paste('by2',by2,sep='='))
    }





    p<-
      ggplot()+

      ## set up plot: layer north america as well as outline and sandwich gridlines between
      geom_polygon(data = reduced, aes(x=long, y = lat,group=group),
                   fill='white',color='black') +
      geom_polygon(data = reduced, aes(x=long, y = lat,group=group),
                   fill='light green',color='black', alpha=0.9) +
      geom_hline(yintercept=seq(latmin, latmax, by=by1), color='white') +
      geom_vline(xintercept=seq(longmin, longmax, by=by2), color='white') +
      geom_polygon(data = reduced, aes(x=long, y = lat,group=group),
                   fill=NA,color='black') +




      if(is.null(col)){
        geom_point(
          data=df,
          aes(x=df[,plotLong],y=df[,plotLat]),
          size=2

        )

      }else{

        geom_point(
          data=df,
          aes(x=df[,plotLong],y=df[,plotLat], color=df[,col]),
          size=2

        )
      }

    p<-
      p+

      scale_x_continuous(limits=c(longmin, longmax),breaks=seq(longmin, longmax, by=by2),expand = c(0, 0)) +
      scale_y_continuous(limits=c(latmin, latmax),breaks=seq(latmin, latmax, by=by1), expand = c(0, 0))+
      coord_fixed(1.3)+



      ylab('Latitude')+
      xlab('Longitude')+
      #scale_color_manual(values=colblind)+

      ## set legend
      guides(color = guide_legend(title = paste(col,'Type')))+

      theme_bw()+

      theme(

        axis.title = element_text(size=12),
        axis.text =  element_text(size=10),

        ## color background, remove gridlines
        panel.background = element_rect(fill ="deepskyblue",
                                        colour = "black", size = 0.5,
                                        linetype = "solid"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),

        ## format legend
        legend.text = element_text(size=10),
        legend.title= element_text(face='bold')

      )

    ##prints plot
    p

    ## does not work, it tries to facet from map polygon data instead of scatter data

    # if(facet==T){
    #
    #   f<-df[,col]
    #
    #   p+
    #     facet_wrap(~f)
    #
    # }else{
    #
    #   p
    #
    # }


  }
