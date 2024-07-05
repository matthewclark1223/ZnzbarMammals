library(sf)
library(tidyverse)

setwd("./Mammals")
Ung<-read_sf("./UngujaTreeAndBuildingCover.shp")
Mamm<-read.csv("./Present.Species.median.csv")
PAs<-read_sf("WDPA_WDOECM_May2024_Public_TZA_shp-polygons.shp")
PAs2<-read_sf("PA_level1.shp")
PAs3 <- read_sf("Export_KNV_FOREST_RESERVE.shp")
PAs <- rbind(PAs, PAs2)

# Ensure CRS matches
st_crs(PAs) <- st_crs(Ung)
PAs3 <- st_transform(PAs3, st_crs(PAs))

# Filter PAs based on Ung and marine condition
PAs <- PAs[Ung, ] %>% filter(MARINE != 2)

# cleaning up the names
PAs3 <- PAs3 %>% rename(NAME = clases)
PAs3$NAME <- "KNV Forest Reserve"
PAs3 <- st_cast(PAs3, "MULTIPOLYGON")

PAs <- bind_rows(PAs, PAs3)
PAs$PA<-"Protected area"

PAs<-PAs[-5, ]
PAs$NAME <- gsub("KNV Forest Reserve", "Kidikotundu-Nongwe-Vundwe", PAs$NAME)
PAs$NAME <- gsub("Masingini Catchment Forest", "Masingini", PAs$NAME)
PAs[5, "NAME"] <- NA


maptheme<-theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
                axis.text=element_text(color="black",size=15),
                axis.title=element_text(color="black",size=22),
                legend.key.size = unit(1.5,'cm'),
                legend.title = element_text(color="black",size=18),
                legend.text = element_text(color="black",size=16))
              
                
buildmap<- ggplot(Ung) +
  geom_sf(aes(fill = PrcBldC), na.rm = TRUE) + 
  geom_sf(data=PAs,aes(color=PA),fill=NA,linewidth=1, show.legend = FALSE)+
  #geom_sf_label(data=PAs,aes(label=NAME),nudge_x = c(.1,.07,0.08))+
  labs(fill = "Building cover", scale)+
  scale_color_manual(values="red",name=NULL)+
  scale_x_continuous(breaks=c(39.2,39.4,39.6),name=NULL)+
  scale_y_continuous(breaks=c(-6.4,-6.2,-6.0,-5.8),name=NULL)+
  scale_fill_distiller(palette = "Purples",direction=0,trans="log10",labels = scales::label_percent())+
  #viridis::scale_fill_viridis(option = "D", direction = -1, trans = "log10", labels = scales::label_percent()) +
  theme_bw()+maptheme
buildmap

ggsave(filename = "BuildingsMap.png",  buildmap,  width = 18, height = 9,units="in", dpi = 300)


tree_map<- ggplot(Ung) +
  geom_sf(aes(fill = MdnTrCv*0.01 ),na.rm = TRUE) + 
  geom_sf(data=PAs,aes(color=PA),fill=NA,linewidth=1,show.legend = "line")+
  #geom_sf_label(data=PAs,aes(label=NAME),nudge_x = c(.1,.07,0.08))+
  labs(fill = "Median tree\ncover", scale)+
  scale_color_manual(values="red",name=NULL)+
  scale_x_continuous(breaks=c(39.2,39.4,39.6),name=NULL)+
  scale_y_continuous(breaks=c(-6.4,-6.2,-6.0,-5.8),name=NULL)+
  scale_fill_distiller(palette = "Greens",direction=0,labels = scales::label_percent())+
  #viridis::scale_fill_viridis(option = "D", direction = -1, labels = scales::label_percent()) +
  theme_bw()+maptheme

tree_map

ggsave(filename = "TreesMap.png",  tree_map,  width = 18, height = 9,units="in", dpi = 300)

install.packages("patchwork")
library(patchwork)
combined_plot <-  buildmap + tree_map +
  plot_annotation(tag_levels = 'A')  # Automatically labels with "A." and "B."

print(combined_plot)
ggsave(filename = "combined_B+T_plot.png",  combined_plot,  width = 18, height = 9,units="in", dpi = 300)

######## MAMMAL diversity
Mamm<-read.csv("./Present.Species.median.csv")
Mamm<-Mamm%>%distinct() #There are some dupicated. Remove those. 

mammal_diversity <- Mamm %>%
  mutate_at (c(4:8,11:26), ~ifelse(. != 0, 1, 0)) %>%  #Removing large cats (none on the island) and domestic cats
  rowwise() %>%
  mutate(total_species = sum(c_across(c(4:8,11:26)))) %>%      #Sum eht presence/absence
  ungroup()

mammal_diversity <- mammal_diversity %>%
  select(SHEHIA, total_species)


Mam_Ung <- merge(Ung, mammal_diversity, by = "SHEHIA",all.x=T)%>%distinct() #There are some dupicated. Remove those. 

MamDiv<-ggplot(Mam_Ung) +
  geom_sf(aes(fill = total_species ),na.rm = TRUE) + 
  geom_sf(data=PAs,aes(color=PA),fill=NA,linewidth=1,show.legend = "line")+
  geom_sf_label(data=PAs,aes(label=NAME), nudge_x = c(-0.039,0.088,0.09,-0.1), nudge_y = c(-0.03,0,0,-0.03), size = 3)+
  labs(fill = "Species observed")+
  scale_color_manual(values="red",name=NULL)+
  scale_x_continuous(breaks=c(39.2,39.4,39.6),name=NULL)+
  scale_y_continuous(breaks=c(-6.4,-6.2,-6.0,-5.8),name=NULL)+
  scale_fill_distiller(palette = "PuBuGn",direction=0)+
  #viridis::scale_fill_viridis(option = "mako", direction = 1) +
  theme_bw()+maptheme

MamDiv

ggsave(filename = "SpeciesCountMap.png",  MamDiv,  width = 18, height = 9,units="in", dpi = 300)

library(gridExtra)
# Define MamDivnolab with the legend removed
MamDivnolab <- ggplot(Mam_Ung) +
  geom_sf(aes(fill = total_species), na.rm = TRUE, show.legend = FALSE) + 
  geom_sf(data = PAs, aes(color = PA), fill = NA, linewidth = 1, show.legend = FALSE) + # No legend for PA
  #geom_sf_label(data = PAs, aes(label = NAME), nudge_x = c(.1, .07, 0.08)) +
  labs(fill = "Species observed") +
  scale_color_manual(values = "red", name = NULL) +
  scale_x_continuous(breaks = c(39.2, 39.4, 39.6), name = NULL) +
  scale_y_continuous(breaks = c(-6.4, -6.2, -6.0, -5.8), name = NULL) +
  scale_fill_distiller(palette = "PuBuGn", direction = 0) +
  # viridis::scale_fill_viridis(option = "mako", direction = 1) +
  theme_bw() +
  maptheme +
  theme(legend.position = "none")  # Remove all legends

MamDivnolab
combined_div <- grid.arrange(MamDivnolab, MamDiv, ncol = 2)
ggsave(filename = "Combined_Diversity.png",  combined_div,  width = 18, height = 9,units="in", dpi = 300)



