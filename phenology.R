#code to analyze phenology at the APSU farm
#written by Joe Endris
#with input from Evan Rehm

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggfortify)
library(ggtext)
library(multcomp)
library(multcompView)
library(lubridate)
library(readxl)
library(gridExtra)
library(MuMIn)

# # # # # # # # # # # #
#data preparation ----
# # # # # # # # # # # #

#read in phenology observations
phenology<-read_excel("phenology.xlsx")

#create column for year
phenology <- mutate(phenology, year=year(date))

#create column for month
phenology <- mutate(phenology, month=month(date))

#create column for julian date
phenology$julian_date <- yday(phenology$date)

#omit any blank spots in the mean_phenology column
#phenology <- phenology[complete.cases(phenology[,4]),]

# # # # # # # # # # # # # # # # # # #
# Phenology statistical analysis ----
# # # # # # # # # # # # # # # # # # #

#global model
phenology_model <- glm(phenology ~ species * date * year, data=phenology, family = poisson, na.action="na.fail")
summary(phenology_model)
dredge(phenology_model)

# # # # # # # # # # # #
# Phenology plots ----
# # # # # # # # # # # #

#calculate mean phenology by julian date
phenology <- phenology%>%
  group_by(species, year, julian_date) %>%
  dplyr::mutate(mean_phenology=mean(phenology))

#calculate SD for phenology
phenology <- phenology%>%
  group_by(species, year, julian_date) %>%
  mutate(pheno_sd = sd(phenology, na.rm=TRUE))

maple_phenology<-ggplot(data=subset(phenology, species=="Acer saccharum"), aes(x = julian_date, y=mean_phenology, color=factor(year))) +
  geom_point()+
  geom_errorbar(aes(y = mean_phenology, ymin = mean_phenology - pheno_sd, ymax = mean_phenology + pheno_sd), alpha = .2) +
  geom_line()+
  labs(x="", y="", colour = "Year")+
  scale_color_manual(values = c("2021" = "blue", "2022" = "red", "2023" = "black", "2024" = "purple"))+
  ylim(-1, 5)+
  theme_bw()+
  theme(axis.title.x = element_markdown())+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        text=element_text(size=14),
        legend.background = element_blank(),
        legend.box.background = element_blank(),legend.spacing.y = unit(0, "cm"),
        legend.position=c("0.08","0.65"))+
  annotate("text", x=40,y=4.5,label= expression(italic("Acer saccharum")), hjust=0, size=5)

maple_phenology

beech_phenology<-ggplot(data=subset(phenology, species=="Fagus grandifolia"), aes(x = julian_date, y=mean_phenology, color=factor(year))) +
  geom_point()+
  geom_errorbar(aes(y = mean_phenology, ymin = mean_phenology - pheno_sd, ymax = mean_phenology + pheno_sd), alpha = .2) +
  geom_line()+
  labs(x="", y="Phenology Code", colour = "Year")+
  scale_color_manual(values = c("2021" = "blue", "2022" = "red", "2023" = "black", "2024" = "purple"))+
  ylim(-1, 5)+
  theme_bw()+
  theme(legend.position="none")+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        text=element_text(size=14))+
  annotate("text", x=40,y=4,label= expression(italic("Fagus grandifolia")), hjust=0, size=5)

beech_phenology

poplar_phenology<-ggplot(data=subset(phenology, species=="Liriodendron tulipifera"), aes(x = julian_date, y=mean_phenology, color=factor(year))) +
  geom_point()+
  geom_errorbar(aes(y = mean_phenology, ymin = mean_phenology - pheno_sd, ymax = mean_phenology + pheno_sd), alpha = .2) +
  geom_line()+
  labs(x="Julian Date", y="", colour = "Year")+
  scale_color_manual(values = c("2021" = "blue", "2022" = "red", "2023" = "black", "2024" = "purple"))+
  ylim(-1, 5)+
  theme_bw()+
  theme(legend.position="none")+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        text=element_text(size=14))+
  annotate("text", x=40,y=4.5,label= expression(italic("Liriodendron tulipifera")), hjust=0, size=5)

poplar_phenology

hornbeam_phenology<-ggplot(data=subset(phenology, species=="Ostrya virginiana"), aes(x = julian_date, y=mean_phenology, color=factor(year))) +
  geom_point()+
  geom_errorbar(aes(y = mean_phenology, ymin = mean_phenology - pheno_sd, ymax = mean_phenology + pheno_sd), alpha = .2) +
  geom_line()+
  labs(x="Julian Date", y="", colour = "Year")+
  scale_color_manual(values = c("2021" = "blue", "2022" = "red", "2023" = "black", "2024" = "purple"))+
  ylim(-1, 5)+
  theme_bw()+
  theme(legend.position="none")+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        text=element_text(size=14))+
  annotate("text", x=40,y=4.5,label= expression(italic("Ostrya virginiana")), hjust=0, size=5)

hornbeam_phenology

oak_phenology<-ggplot(data=subset(phenology, species=="Quercus alba"), aes(x = julian_date, y=mean_phenology, color=factor(year))) +
  geom_point()+
  geom_errorbar(aes(y = mean_phenology, ymin = mean_phenology - pheno_sd, ymax = mean_phenology + pheno_sd), alpha = .2) +
  geom_line()+
  labs(x="Julian Date", y="", colour = "Year")+
  scale_color_manual(values = c("2021" = "blue", "2022" = "red", "2023" = "black", "2024" = "purple"))+
  ylim(-1, 5)+
  theme_bw()+
  theme(legend.position="none")+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        text=element_text(size=14))+
  annotate("text", x=40,y=4.5,label= expression(italic("Quercus alba")), hjust=0, size=5)

oak_phenology

grid.arrange(maple_phenology, beech_phenology, poplar_phenology, hornbeam_phenology, oak_phenology, nrow=5)
