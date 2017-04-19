
#' Ranking Plot
#'
#' This function makes a plot that is intented to be used for
#' comparison of the scores in a single variable
#'
#'@param data a dataframe with the info
#'@param name the name of a column to be used as the names of the elements
#'@param num the name of the numeric variable to be compared
#'@param limits range of the plot should be in the format c(min,max)
#'@param colores name of a column giving information of the color of each point
#'@param n show the top n cases
#'@param paleta colors to be used in the points
#'@return a ggplot graph
#'@author Andrés Mejía
#'@details
#'This function takes a dataframe and two columns, one of the name
#' and the other of a numeric attribute, this function orders the
#' data and makes a horizontal point plot that makes easy compare
#' the results between units.
#' @export
#'


rankingPlot<-function(
        data,name,num,limits,colores,n,paleta){

        if(!missing(n)){
                data<-data %>%
                arrange_(paste0("desc(",num,")")) %>%head(n)
        }
        if(missing(limits)){
          eps=(max(data[,num])-min(data[,num]))/1000
          limits=c(min(data[,num])-eps,max(data[,num])+eps)
        }
        data<-data %>%
                arrange_(paste0("desc(",num,")"))
        data[,name]=factor(data[,name],
                levels = data[,name] %>% rev,
                ordered=T)
        if(missing(colores)){
                data[,"colores"]="black"
                paleta="black"
                colores="colores"
        }
        if(!missing(colores)){
          if(missing(paleta)){
                  paleta=topo.colors(length(unique(data[,colores])))
          } else if(length(unique(colores))>length(paleta)){
          paleta=topo.colors(length(unique(data[,colores])))
          }
        }
        data[,colores]=as.factor(data[,colores])
        ploti<-ggplot(data=data,aes_string(x=name,y=num))+
         geom_point(aes_string(colour=colores))+lims(y=limits)+
         coord_flip()+
         scale_color_manual(values=paleta)+
         theme(panel.grid.major =element_blank()) +
         theme(panel.grid.minor=element_blank()) +
         theme(text=element_text(family = "Verdana" ))+
         theme(plot.title = element_text(lineheight=.8,  size=25))+
         theme(axis.title.x=element_blank())+
         theme(axis.title.y=element_blank())

        if(missing(colores)){
         ploti<-ploti+theme(legend.position="none")
        }
        ploti
}
