#setwd('C:/Users/jshur/OneDrive/Desktop')  #FOR PCs
setwd('C:/Users/jshur/OneDrive - UC San Diego/Desktop/Work Desktop Dec 2009/Active Projects/Pyromania/')
library(doBy)
library(ggplot2)
library(nlme)
library(lme4)
library(reshape2)
library(tidyr)
library(plyr)
library(mgcv)

rm(list=ls())

x<-read.csv("GHG.csv")
names(x)


trt<-read.csv("treatments.csv")
x<-merge(x, trt, by="Tank", all=T)

x<-subset(x, Week!="AA")
x<-subset(x, Week!="CH4")
x<-subset(x, Gas=="CH4"|Gas=="CO2")
x$Treatment<-as.factor(x$Treatment)

CO2<-subset(x, Gas=="CO2")
CO2_1<-subset(CO2, Week== "WK1")


p1<-ggplot(x,aes(y=ppm, x=plant.mass..g, color = factor(Treatment))) +
  geom_point(size=2)+
  facet_grid(rows=vars(Gas), cols=vars(Week), scales="free_y")+
  labs(x = "G plants added", y="ppm", title = "Greenhouse gases")+
  geom_smooth(method = loess)
p1
x$plant.mass..g<-as.numeric(x$plant.mass..g)
x$Week<-as.factor(x$Week)

CH4<-subset(x, Gas=="CH4")
CO2<-subset(x, Gas=="CO2")

timepoint<-"WK3"
CO21 <- gam(ppm ~ s(plant.mass..g, k=5, bs="tp") +
            s(plant.mass..g, Treatment, k=5, bs="re"), data=subset(CO2, Week %in% timepoint), method="REML", family="gaussian")
CO22 <- gam(ppm ~ s(plant.mass..g, k=5, bs="tp"), data=subset(CO2, Week %in% timepoint), method="REML", family="gaussian")

AIC(CO21,CO22) 
summary(CO21) 
summary(CO22) 


timepoint<-"WK1"
CH41 <- gam(ppm ~ s(plant.mass..g, k=5, bs="tp") +
              s(plant.mass..g, Treatment, k=5, bs="re"), data=subset(CH4, Week %in% timepoint), method="REML", family="gaussian")======
CH42 <- gam(ppm ~ s(plant.mass..g, k=5, bs="tp"), data=subset(CH4, Week %in% timepoint), method="REML", family="gaussian")

AIC(CH41,CH42) 
summary(CH41) 
summary(CH42) 


p2<-ggplot(CO2,aes(y=ppm, x=plant.mass..g, color = factor(Treatment))) +
  geom_point(size=2)+
  facet_grid(rows=vars(Gas), cols=vars(Week), scales="free_y")+
  labs(x = "G plants added", y="ppm", title = "Greenhouse gases")+
  geom_smooth(method = loess)
p2

