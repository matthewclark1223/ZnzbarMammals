library(tidyverse)
install.packages("brms")
data<-read.csv("./Present.Species.median.csv")

data<-na.omit(data)
data <- data %>%
  select (- "Large.Cats")     ### removing Large cats from the dataset
head(data[4:25])

data<-data%>%pivot_longer(4:25,names_to = "Species",values_to = "Presence" )

data<-data%>%mutate(Occurs=ifelse(Presence>0,1,0))
data$Species <- gsub("Genet", "Servaline genet", data$Species, perl = TRUE)
data$Species <- gsub("Small.Cats", "Domestic \n Cats", data$Species, perl = TRUE)
data$Species <- gsub("Elephant.Shrews", "Elephant \n shrews", data$Species, perl = TRUE)
data$Species <- gsub("Shrews", "Crocidura \n shrews", data$Species, perl = TRUE)
data$Species <- gsub("Blue.Monkey", "Zanzibar blue \n monkey", data$Species, perl = TRUE)
data$Species <- gsub("Bushbaby", "Zanzibar \n bushbaby", data$Species, perl = TRUE)
data$Species <- gsub("Bats", "Mixed \n bats", data$Species, perl = TRUE)
data$Species <- gsub("Hyrax", "Eastern tree \n hyrax", data$Species, perl = TRUE)
data$Species <- gsub("Rats.Mice", "Rats \n and mice", data$Species, perl = TRUE)
data$Species <- gsub("Red.Colobus", "Red Colobus \n monkey", data$Species, perl = TRUE)
data$Species <- gsub("Pouched.Rat", "Pouched rat", data$Species, perl = TRUE)
data$Species <- gsub("Mongoose.B.T", "Bushy-tailed \n mongoose", data$Species, perl = TRUE)
data$Species <- gsub("Mongoose.Slender", "Slender \n mongoose", data$Species, perl = TRUE)
data$Species <- gsub("Mongoose.Banded", "Banded \n mongoose", data$Species, perl = TRUE)
data$Species <- gsub("African.Civet", "African \n civet", data$Species, perl = TRUE)
data$Species <- gsub("Duiker.Aders", "Aders's\n duiker", data$Species, perl = TRUE)
data$Species <- gsub("Duiker.Blue", "Blue \n duiker", data$Species, perl = TRUE)
data$Species <- gsub("Duiker.Suni", "Suni \n duiker", data$Species, perl = TRUE)
data$Species <- gsub("Zanj.Sun.Squirrel", "Zanj \n sun squirrel", data$Species, perl = TRUE)
data$Species <- gsub("Red.Bush.Squirrel", "Red bush \n squirrel", data$Species, perl = TRUE)
data$Species <- gsub("Vervet.Monkey", "Vervet \n monkey", data$Species, perl = TRUE)


#library(lme4)

stdize<-function(x){
  (x-mean(x,na.rm=T))/(2*sd(x,na.rm=T))}

data<-data%>%mutate(std_MdnTrCv=stdize(MdnTrCv),std_PrcBldC=stdize(PrcBldC))



fit<-brms::brm(Occurs~(std_PrcBldC+std_MdnTrCv|Species),
               family="bernoulli",data=data, chains=4,cores = 4 )

brms::mcmc_plot(fit)

brms::prior_summary(fit)

buildingEff<-as.data.frame(coef(fit,prob=c(0.05,0.25,0.75,0.95) )$Species[,,2]) #building cover effect, CI = 90%
buildingEff$Species<-row.names(buildingEff)
row.names(buildingEff)<-NULL


mytheme=theme(text=element_text( face= "plain"),
              axis.text.y  = element_text(face= "plain"),
              axis.title = element_text(color="black",size=20),
              axis.text=element_text(color="black",size=13))

#building cover
BuildEFF<-buildingEff %>%
  arrange(desc(Estimate)) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(Species=factor(Species, levels=Species)) %>%
ggplot(.,aes(x=Species,y=Estimate,ymin=Q25,ymax=Q75))+
  geom_linerange(aes(ymin=Q5,ymax=Q95),color="darkgrey", linewidth=1) +##change deets
  geom_pointrange(size=1,alpha=0.99,color="black",linewidth=1.2)+
  #scale_y_continuous(breaks=c(-2,0,2),limits=c(-2.2,2.2))+
  # scale_x_discrete(breaks=c(names(postdf)[c(92:93,95:97)]),
  #   labels=c("Mangroves\ndeclining","Theft","REDD","Committee\nmember","Male"))+
  coord_flip()+geom_hline(yintercept=0,linetype="dashed",color="darkgrey" )+
  ggthemes::theme_clean()+mytheme+scale_x_discrete(expand=c(0.01,0.0 ))+
  ylab("Standardized effect of building cover\non reported observation")+xlab("Species")

BuildEFF

ggsave(filename = "BuildingEffects.png",  BuildEFF,  width = 8, height = 9,units="in", dpi = 300)


treeEff<-as.data.frame(coef(fit,prob=c(0.05,0.25,0.75,0.95) )$Species[,,3]) #building cover effect, CI = 90%
treeEff$Species<-row.names(treeEff)
row.names(treeEff)<-NULL
#building cover
TreeEFF<-treeEff %>%
  arrange(Estimate) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(Species=factor(Species, levels=Species)) %>%
  ggplot(.,aes(x=Species,y=Estimate,ymin=Q25,ymax=Q75))+
  geom_linerange(aes(ymin=Q5,ymax=Q95),color="darkgrey", linewidth=1) +##change deets
  geom_pointrange(size=1.2,alpha=0.99,color="black",linewidth=1.2)+
  #scale_y_continuous(breaks=c(-2,0,2),limits=c(-2.2,2.2))+
  # scale_x_discrete(breaks=c(names(postdf)[c(92:93,95:97)]),
  #   labels=c("Mangroves\ndeclining","Theft","REDD","Committee\nmember","Male"))+
  coord_flip()+geom_hline(yintercept=0,linetype="dashed",color="darkgrey" )+
  ggthemes::theme_clean()+mytheme+scale_x_discrete(expand=c(0.01,0.0 ))+
  ylab("Standardized effect of tree cover\non reported observation")+xlab("Species")

TreeEFF

ggsave(filename = "TreeEffects.png",  TreeEFF,  width = 8, height = 9,units="in", dpi = 300)


TreeMarg<-marginaleffects::plot_predictions(fit,condition = "std_MdnTrCv")+theme_bw()+
  labs(x = "Mean tree cover %",
       y = "Probability of reported observation",
       fill = "Credibility\ninterval")+
  scale_x_continuous(breaks=c( range(data$std_MdnTrCv)),labels = c("0","100"))+
  theme(axis.text = element_text(color="black",size=10),
        axis.title = element_text(color="black",size=16))

TreeMarg

ggsave(filename = "TreeMarg.png",  TreeMarg,  width = 8, height = 9,units="in", dpi = 300)


#check range of building cover


BuildMarg<-marginaleffects::plot_predictions(fit,condition = "std_PrcBldC")+
  labs(x = "Shehia building cover (%)",
       y = "Probability of reported observation",
       fill = "Credibility\ninterval")+theme_bw()+
  scale_x_continuous(breaks=c( range(data$std_PrcBldC)),labels = c("0.05","46"))+
  mytheme+
  theme(axis.text = element_text(color="black",size=13),
        axis.title = element_text(color="black",size=20))


BuildMarg


ggsave(filename = "BuildMarg.png",  BuildMarg,  width = 8, height = 9,units="in", dpi = 300)

 

library(marginaleffects)


##Building conditional effects

pred$Species <- gsub("Znzbr", "Zanzibar", pred$Species, perl = TRUE)

pred <- marginaleffects::predictions(fit,
                                     newdata = datagrid(std_PrcBldC = data$std_PrcBldC,
                                                        std_MdnTrCv = 0, 
                                                        Species = unique(data$Species))) |> 
  marginaleffects::posterior_draws()

ff<-lm(std_PrcBldC~PrcBldC,data=data)
predict(ff,data.frame(PrcBldC=45)) # 2.736452 
predict(ff,data.frame(PrcBldC=22.5)) # 1.245562
predict(ff,data.frame(PrcBldC=.1)) # -0.2387013 

BuildingConditional<-ggplot(pred, aes(x = std_PrcBldC, y = draw)) +
  tidybayes::stat_lineribbon() +
  scale_fill_brewer(palette = "Reds") +
  facet_wrap(~ Species, ncol = 6) +
  labs(x = "Shehia building cover (%)",
       y = "Probability of reported observation",
       fill = "Credibility\ninterval")+theme_bw()+
  scale_x_continuous(breaks=c(-0.2387013, 1.245562, 2.736452),labels = c("0.1","22.5", "45"))+
  scale_y_continuous(n.breaks=4)+
  theme_linedraw() +
  theme(axis.text = element_text(color="black",size=12),
        axis.title = element_text(color="black",size=16),
        panel.grid.major = element_line(color="lightgrey"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white", color = NA),
        strip.text = element_text(size = 10, face = "bold", color = "black"),
        legend.key.size = unit(1.5,'cm'),
        legend.title = element_text(color="black",size=18),
        legend.text = element_text(color="black",size=16),
        strip.text.x = element_text(margin = margin(0.05, 0.05, 0.05, 0.05, "cm")))
BuildingConditional

ggsave(filename = "BuildingConditional.png",  BuildingConditional,  width = 13, height = 9,units="in", dpi = 300)

### Tree conditional effects
pred <- marginaleffects::predictions(fit,
                                     newdata = datagrid(std_MdnTrCv = data$std_MdnTrCv,
                                                        std_PrcBldC = 0, 
                                                        Species = unique(data$Species))) |> 
  marginaleffects::posterior_draws()

ff2<-lm(std_MdnTrCv~MdnTrCv,data=data)
ff2
predict(ff2,data.frame(MdnTrCv=100)) # 0.9673069
predict(ff2,data.frame(MdnTrCv=50)) # 0.06732676
predict(ff2,data.frame(MdnTrCv=0)) # -0.8326534


TreeConditional<-ggplot(pred, aes(x = std_MdnTrCv, y = draw)) +
  tidybayes::stat_lineribbon() +
  scale_fill_brewer(palette = "Reds") +
  facet_wrap(~ Species, ncol= 6) +
  labs(x = "Median tree cover (%)",
       y = "Probability of reported observation",
       fill = "Credibility\ninterval")+theme_bw()+
  scale_x_continuous(breaks=c(-0.8326534, 0.06732676, 0.9673069),labels = c ("0","50", "100"))+
  scale_y_continuous(n.breaks=4)+
  theme_linedraw() +
  theme(axis.text = element_text(color="black",size=12),
        axis.title = element_text(color="black",size=16),
        panel.grid.major = element_line(color="lightgrey"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white", color = NA),
        strip.text = element_text(size = 10, face = "bold", color = "black"),
        legend.key.size = unit(1.5,'cm'),
        legend.title = element_text(color="black",size=18),
        legend.text = element_text(color="black",size=16),
        strip.text.x = element_text(margin = margin(0.05, 0.05, 0.05, 0.05, "cm")))   

TreeConditional

ggsave(filename = "TreeConditional.png",  TreeConditional,  width = 13, height = 9,units="in", dpi = 300)

