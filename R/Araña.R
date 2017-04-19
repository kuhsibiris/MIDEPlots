#' @import tidyr
#' @import ggplot2
#' @import dplyr
#' @import purrr
#' @import extrafont
#'

coord_radar <- function (theta = "x", start = 0, direction = 1)
{
        theta <- match.arg(theta, c("x", "y"))
        r <- if (theta == "x")
                "y"
        else "x"
        ggproto("CoordRadar", CoordPolar, theta = theta, r = r, start = start,
                direction = sign(direction),
                is_linear = function(coord) TRUE)
}

#' Spider Plot
#'
#' Does a list of circular plots showing the position of the n-th
#' entry in a ranking, the
#' fist places being outside (a spider)
#'
#' @param data a data frame
#' @param vars variables to use
#' @param varlables labels to use in the plot in case those differ
#'  from the colnames in data
#' @param pos index of the cases to plot, defaults to all
#' @return A list of ggplot graphs
#' @author Andrés Mejía
#' @details This function takes a dataframe and makes the spider plots
#' (a circular ranking plot) of the elements listed.
#' @export
SpiderPlot <-function(data,vars,varlabels,pos=1:dim(data)[1]){
        if(missing(vars)) error("Select variables to use")
        if(missing(varlabels))varlabels=vars
        mimax=dim(data)[1]
        data<-data[,vars]
        data2<-map_df(data,function(x){length(x)-rank(x)+1})
        out<-map(pos,~defSpiderPlot(data2,vars,.x,varlabels,mimax))
}

defSpiderPlot<-function(data2,vars,pos,varlabels,mimax){
        data3=  gather(data2[pos,],variable,Puesto)
        data3$variable=factor(data3$variable,levels=vars,ordered=T)
        data3$group="a"
        ggplot(data3, aes(x=variable,y=Puesto))+
                #coordenadas polares
                coord_polar(theta = "x", direction = 1) +
                coord_radar()+
                #quita el fondo
                theme(panel.background = element_blank())+
                #dibula la linea
                geom_polygon(aes(y=mimax-Puesto+1,group="a"),
                             alpha=0.005, size=3.5,colour="black")+
                #puntos coloreados
                geom_point(size=8, aes(y=mimax-Puesto+1),
                           colour="darkgreen")+
                # Numeros en la grafica
                geom_text(size=10, color="blue", fontface="bold",
                          hjust=-1, vjust=1,angle = 0,
                          aes(y=mimax-Puesto+1, label=Puesto))+
                #Graficas con el minimo y el maximo, son invisibles para mantener la escala
                ylim(1,mimax)+
                #-Etiquetas de los Ejes
                labs(x=element_blank(), y=element_blank())+
                theme(panel.grid.major = element_line(
                        colour = "black", linetype="dotted",size=0.4)) +
                theme(panel.background = element_blank()) +
                theme(panel.grid.minor = element_line(colour="blue")) +
                theme(text=element_text(family = "Verdana" ))+
                scale_x_discrete(breaks=vars,
                                 labels=varlabels) +
                scale_colour_manual(values=
                                            c("#238b45", "#df65b0", "#6a51a3",
                                              "gray", "#3182bd", "orange"))+
                theme(plot.title = element_text(lineheight=.8,
                                                size=25)) +
                theme(axis.title.y=element_blank(),
                      axis.title.x=element_blank(),
                      axis.text.x=element_text(size=15,
                                               color="black"),
                      axis.text.y=element_blank())
}
