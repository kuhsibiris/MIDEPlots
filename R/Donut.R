

#' Donut Plots
#'
#' Plots values as a filled area of a given max
#'
#'  @param data a data frame that contains the values to plot
#'  @param valuevar name of the value to use
#'  @param maxi value that represents a full indicator, can be missing in that case it is calculated as the sum of valuevar.
#'  @param shadowvar value to be plotted in the background, can be missing
#'  @param valuecenter value shown in the center of the plot
#'  @param nivel how far away from the center should the plot be made
#'  @param shadowaalpha indicates the transparency of the shadow
#'  @return a ggplot graph
#'  @details
#'  This makes a ring plot of a variable.
#'  @export
#'

donutplot<-function(data,valuevar,maxi,shadowvar,
                    nivel=3,shadowalpha=0.5,valuecenter){
        if(missing(valuevar))
               stop("valuevar missing")
        if(missing(maxi)){
                maxi<-NA
        }
        if(missing(valuecenter)){
                valuecenter=""
        }
        data$fill=as.factor(seq_len(dim(data)[1]))
        ploti<-ggplot(data)+
                donut_level(valuevar,maxi,nivel) +xlim(c(0,NA))+ylim(c(0,maxi))+
                geom_text(color="black",angle = 0, size=6,y=0,x=0,aes(label=valuecenter,inherit.aes=F))+
                   geom_vline(aes(xintercept=nivel-0.5),color="gray")+geom_vline(aes(xintercept=nivel+0.5),color="gray")
        if(!missing(shadowvar))
                ploti<-ploti+geom_rect(aes_(ymax=shadowvar,alpha=0.5))
        ploti
}


donut_level<-function(valuevar,maxi,nivel){
        list(   geom_col(aes_string(y=valuevar,x=nivel,fill="fill"),alpha=1,width = 1),
                coord_polar(theta="y"),
                labs(x=element_blank(), y=element_blank()),
                theme_bw(),
                theme(panel.grid=element_blank()),
                theme(axis.text=element_blank()) ,
                theme(axis.ticks=element_blank()) ,
                theme(axis.title.x=element_blank()),
                theme(axis.title.y=element_blank()),
                theme(text=element_text(family = "Verdana" )),
                theme(legend.position="none")
        )
}
