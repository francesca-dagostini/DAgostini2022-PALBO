setwd("/Users/francescadagostini/desktop")
data<-read.table("Phytoliths2019.csv",sep=",",header=TRUE)

library(ggplot2)
library(ggpubr)
library(ggrepel)
library(tidyverse)
library(svglite)
library(MASS)

###MEAN&SD
#For the remaining morphotypes change the variable 'TriCONCE' to the variable corresponding to the morphotype, e.g. SaddleCONCE, CrossCONCE...
with(data, tapply(data$TriCONCE, list(data$Species, data$Treatment), mean))
with(data, tapply(data$TriCONCE, list(data$Species, data$Treatment), sd))

with(data, tapply(data$Concentration,list(data$Treatment), mean))
with(data, tapply(data$Concentration,list(data$Treatment), sd))

###GLM
#For the remaining morphotypes change the variable 'TriCONCE' to the variable corresponding to the morphotype, e.g. SaddleCONCE, CrossCONCE...
glm1<- glm(family= gaussian, data$TriCONCE ~ data$Treatment, data=data)
summary(glm1)

glm2<- glm(family= gaussian, data$TriCONCE ~ data$Species, data=data)
summary(glm2)

glm3<- glm(family= gaussian, data$Concentration ~ data$Species+ data$Genotype+ data$TWT+ data$TE +data$TWA+ data$LastTranspiration, data=data)
summary(glm3)

###ANOVA2WAY###
aov_twu_2way <- aov(data$Concentration ~ data$Species + data$Treatment, data = data)
TukeyHSD(aov_twu_2way)

###LINEAR REGRESSION### 
#For Ratio sensitive to fixed run the code by changing the variable "Concentration" to the variable "RatioS/F"
Regre<-ggplot(data = data, aes(x = TWT, y=log(Concentration))) + 
  facet_wrap(~Species, scales="free_x", nrow= 1) +
  labs(title="",
       x="Total water transpired (L)",
       y="Phytolith concentration") +
  geom_point(aes(color= Treatment))+
  geom_smooth(method="lm", color = "black", size=0.5, se = TRUE)+
  stat_cor(label.y = 2)+
  jpeg("CONCE_TWT.jpeg", width=2500, height=2000, res=300)
Regre +theme_bw()+ scale_color_brewer(palette="Paired")+
  theme(legend.title=element_blank(), legend.position="bottom", aspect.ratio=1,
        text=element_text(size=15), axis.title=element_text(size=13))
dev.off()

###BOXPLOT SPECIES###
#For ratio sensitive to fixed run the code by changing the variable "Concentration" to the variable "RatioS/F"
boxplot1<-ggplot(data, aes(x=Species, y=log(Concentration), fill=Treatment)) + 
  geom_boxplot() +
  facet_wrap(~Species, scales="free_x") +
  labs(title="",
       x="",
       y="Phytolith concentration") +
  stat_summary(fun="mean", aes(group=Treatment), position=position_dodge(0.75),
               geom="point", shape=23, size=3, fill="white")
jpeg("CONCE_SPE.jpeg", width=2500, height=2000, res=300)
boxplot1+theme_bw()+scale_fill_brewer(palette= "BuPu")+
  theme(legend.title=element_blank(), legend.position="bottom",
        text=element_text(size=15), axis.title=element_text(size=13),
        axis.text.x=element_blank())
dev.off()

###BOXPLOT LANDRACES
#For ratio sensitive to fixed run the code by changing the variable "Concentration" to the variable "RatioS/F"
boxplot1<-ggplot(data, aes(x=Genotype, y=log(Concentration), fill=Treatment)) + 
  geom_boxplot() +
  facet_wrap(~Species, scales="free_x") +
  labs(title="",
       x="",
       y="Phytolith concentration") +
  stat_summary(fun="mean", aes(group=Treatment), position=position_dodge(0.75),
               geom="point", shape=23, size=3, fill="white")
jpeg("CONCE_GENO.jpeg", width=2500, height=2000, res=300)
boxplot1+theme_bw()+scale_fill_brewer(palette= "BuPu")+
  theme(legend.title=element_blank(), legend.position="bottom",
        text=element_text(size=15), axis.title=element_text(size=13),
        axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

###LOGISTIC REGRESSION###

#MODERNO
data<-read.table("Modern.csv",sep=",",header=TRUE)
data[, c(3)] <- sapply(data[, c(3)], as.numeric)
data$Treatment [data$Treatment == "1"] <- 0
data$Treatment [data$Treatment == "2"] <- 1
data$Treatment<- as.integer(data$Treatment)

#1
full.model <- glm(Treatment ~ TriPER + SaddlePER + RondelPER + BulliParalPER + BulliFlabPER+ StomaPER + CrossPER + BiloPER + PolyPER, 
                  data = data, family = binomial(link="logit"))
summary(full.model)

# 2 Stepwise selection
step.model<- stepAIC(full.model, direction = "both", 
                     trace = FALSE)
summary(step.model)

#ARCHEO
dataset<-read.csv(file="Archaeological.csv", sep=",")
dataset[, c(1,3,4)] <- sapply(dataset[, c(1,3,4)], as.numeric)
X<-dataset[,c(5:14)]

#5 Predicting
p <- predict(step.model, X,  type="response")

output <- cbind(dataset, p)

#6 Plotting results
Regre<-ggplot(output, aes(x = output$Origin_Spe, y = output$p, label=output$Sample)) + 
  geom_point(aes(color=factor(output$Origin)), size=3)+
  facet_wrap(~Species, scales="free_x") +
  geom_text_repel(size=2)+
  labs(title="",
       x="",
       y="probability of WW") 
jpeg("Prob.jpg", width=2500, height=2000, res=300)
Regre +theme_bw()+ scale_color_brewer(palette="Set2")+
  theme(legend.title=element_blank(), legend.position="bottom", aspect.ratio=1,
        text=element_text(size=15), axis.title=element_text(size=13), 
        axis.text.x=element_blank(), axis.ticks.x=element_blank())
dev.off()


