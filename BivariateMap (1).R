install.packages("cowplot")
install.packages("biscale")

library(sf)
library(ggplot2)
library(dplyr)
library(cowplot)
library(biscale)
Ung<-read_sf("./UngujaTreeAndBuildingCover.shp")

# create classes
quantiles_PrcBldC <- quantile(Ung$PrcBldC, probs = seq(0, 1, by = 0.25), na.rm = TRUE)
quantiles_MdnTrCv <- quantile(Ung$MdnTrCv, probs = seq(0, 1, by = 0.25), na.rm = TRUE)

data2 <- bi_class(Ung, x = MdnTrCv, y = PrcBldC, style = "quantile", dim=3)


maptheme<-theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
                axis.text=element_text(color="black",size=15),
                axis.title=element_text(color="black",size=22),
                legend.key.size = unit(1.5,'cm'),
                legend.title = element_text(color="black",size=18),
                legend.text = element_text(color="black",size=16))

map<-ggplot() +
  geom_sf(data = data2, mapping = aes(fill = bi_class),show.legend = FALSE) +
  bi_scale_fill(pal = "PurpleGrn", dim = 3, na.value= "white") +
  geom_sf(data=PAs,color="red",fill=NA,linewidth=1)+
  #geom_sf_label(data=PAs,aes(label=NAME),nudge_x = c(.1,.07,0.08))+
  scale_color_manual(values="red",name=NULL)+
  scale_x_continuous(breaks=c(39.3,39.5),name=NULL)+
  scale_y_continuous(breaks=c(-6.4,-6.2,-6.0,-5.8),name=NULL)+
  theme_bw()+maptheme

legend <- bi_legend(
  pal = "PurpleGrn",
  dim = 3,
  xlab = "More trees",
  ylab = "More buildings",
  size = 8)


finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.55, 0.65, 0.25, 0.25)

finalPlot

 ggsave(filename = "Bivariate.png",  finalPlot,  width = 8, height = 9,units="in", dpi = 300)
 
 