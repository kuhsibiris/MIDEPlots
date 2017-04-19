#' Colors used in the MIDE documentation
#'
#' List of Colors used in the MIDE documentation

MIDEpallete<-c("#7f7f7f", "#F17F80","#877DA3","#50C0C2","#6AB8A5",
               "#FAAB51","#F9C838")


#' Mide Barplots
#'
#' This function takes one observation and makes varplots for
#' multiple variales, if allows comparison if a second observation is
#' passed as shadowdata
#'
#' @param data a dataframe with only one row
#' @param vars a character vector with the names of the columns to plot
#' @param varlabels character vector with the names to be used in the plot
#' @param barcolor list of colors to be used in the bars, defaults to MIDEpallete
#' @param shadowdata a second observation to plot, this one has a low alpha, recomended for highest case
#' @param horizontal Should the plot be horizontal?, defaults to FALSE
#' @return a ggplot graph
#' @author Andrés Mejía
#' @details
#' This function takes a single observation in a dataframe,
#' makes a barplot using as x values the variables gives in vars,
#' and as y values the values in the variables themselves. If a second
#' observation is given in shadowvar this one is shown with a low alpha
#' to be used as comparison.
#' @export
#'
#'
MIDEbar<-function(data,vars,varlabels,barcolor=MIDEpallete,shadowdata,
                  horizontal=FALSE){

        if(!missing(shadowdata)){
                if(length(data)!=length(shadowdata)){
                stop("data and shadowdata must have same length")
                }
                if(sum(vars %in% colnames(shadowdata))<length(vars)){
                        stop("vars not in shadowdata")}
                if(dim(shadowdata)[1]!=1)
                        {stop("Can only take one observation(shadowdata)")}
        }
        if(sum(vars %in% colnames(data))<length(vars)){
                stop("vars not in data")
        }

        if(missing(varlabels))varlabels=vars

       if(length(vars)>length(MIDEpallete)){
            warning("More variables than colors, adding colors")
               barcolor=c(barcolor,
                          topo.colors(length(vars)-length(MIDEpallete)))
        } else{barcolor=MIDEpallete[1:length(vars)]}

        if(horizontal){
                barcolor=rev(barcolor)
        }
        if(dim(data)[1]!=1)
                {stop("Can only take one observation (data)")}

        data2<-data[vars]
        data2<-data2 %>% gather(var,valor)
        if(!missing(shadowdata)){
                data3<-shadowdata[vars]
                data3<-data3 %>% gather(var,shadow)
                data2<-left_join(data2,data3,by="var")
        }
        plot1<-ggplot(data2, aes(x=var,y=valor))+
                geom_bar(aes(fill=var),color="white",
                         stat="identity",alpha=1, size=1)+
                labs(x=element_blank(), y=element_blank()) +
                theme(panel.background = element_blank()) +
                theme(panel.grid.major =element_blank()) +
                theme(panel.grid.minor=element_blank()) +
                theme(text=element_text(family = "Verdana" ))+
                theme(plot.title = element_text(lineheight=.8,  size=25))+
                theme(axis.title.x=element_blank())+
                theme(axis.title.y=element_blank())+
                scale_x_discrete(breaks=vars,
                                 labels=varlabels)+
                scale_fill_manual(values=barcolor)+
                theme(axis.text.y=element_text(size=20))+
                theme(legend.position="none")

        if(horizontal)plot1<-plot1+  coord_flip()
        if(!missing(shadowdata)){
                plot1<-plot1+
                        geom_bar(aes(y=shadow,fill=vars),
                                 stat="identity", color="#8e8e8e",
                                 alpha=0.3,size=0.5)
        }
        plot1
}

