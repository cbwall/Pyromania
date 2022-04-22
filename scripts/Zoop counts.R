#setwd('C:/Users/jshur/OneDrive/Desktop')  #FOR PCs
setwd('C:/Users/jshur/OneDrive - UC San Diego/Desktop/Work Desktop Dec 2009/Active Projects/Pyromania/zoop count data')
library(doBy)
library(ggplot2)
library(nlme)
library(lme4)
library(reshape2)
library(tidyr)
library(plyr)
library(mgcv)
rm(list=ls())

x<-read.csv("Master zoop data pyro.csv")
names(x)

levels(as.factor(x$DATE.COLLECTED))
x$DATE.COLLECTED[x$DATE.COLLECTED=="2022-11-03"]<-"2021-11-03"  # fixing dates that have wrong year
x$DATE.COLLECTED[x$DATE.COLLECTED=="2022-11-15"]<-"2021-11-15"
x$DATE.COLLECTED[x$DATE.COLLECTED=="2022-12-06"]<-"2021-12-06"

table(x$DATE.COLLECTED, x$Time.point)

vols<-read.csv("sample volumes.csv")
names(vols)<-c("DATE.COLLECTED","Time.point","TREATMENT","Plant.mass.g","TANK","VOLUME.SAMPLED")
levels(as.factor(vols$DATE.COLLECTED))

x<-merge(x, vols, by=c("TANK","DATE.COLLECTED","Time.point"), all=T)
levels(as.factor(x$DATE.COLLECTED))
summary(x)

table(x$Time.point, x$TANK)
table(x$COUNTED.BY, x$TANK)

levels(as.factor(x$SPECIES))

x$SPECIES[x$SPECIES=="Epliphium"]<-"Ephipium"
x$SPECIES[x$SPECIES=="Mayfly larva"]<-"Mayfly"
x$SPECIES[x$SPECIES=="Mayfly larvae"]<-"Mayfly"
x$SPECIES[x$SPECIES=="Mosquito Larvae"]<-"Mosquito"
x$SPECIES[x$SPECIES=="Mosquito Pupae ?"]<-"Mosquito"
x$SPECIES[x$SPECIES=="Mosquito larvae"]<-"Mosquito"

# Celia code (the above didn't work for me)
x<-x %>%
  mutate(SPECIES = fct_recode(`SPECIES`,
                              "Mosquito"    = "Mosquito Larvae",
                              "Mosquito"    = "Mosquito Pupae ?",
                              "Mosquito"    = "Mosquito larvae",
                              "Mayfly"    = "Mayfly larvae",
                              "Mayfly"    = "Mayfly larva",
                              "Ephipium"  = "Epliphium"))

levels(as.factor(x$SPECIES))


x$spp <- casefold(x$SPECIES, upper = FALSE)
levels(as.factor(x$spp))
str(x)
x$DENSITY<-1000*x$X..OF.INDIVIDUALS/x$VOLUME.SAMPLED

levels(as.factor(x$DATE.COLLECTED))

table(x$TANK[x$DATE.COLLECTED=="2021-11-03"], x$spp[x$DATE.COLLECTED=="2021-11-03"])

names(x)
y<-x[,-c(4:9,12)]
z<-summaryBy(DENSITY ~ TANK + DATE.COLLECTED + Time.point + TREATMENT + Plant.mass.g +
               spp, data = y, FUN = mean)
z1<-pivot_wider(z, names_from="spp", values_from="DENSITY.mean")
z1[is.na(z1)]<-0
names(z1)[21]<-"burst_daphnia"
z1$all_daphnia<-z1$daphnia+z1$burst_daphnia

#Daphnia

p1<-ggplot(z1,aes(y=all_daphnia, x=Plant.mass.g, color = factor(TREATMENT))) +
  geom_point(size=2)+
  facet_grid(rows=vars(DATE.COLLECTED), scales="free_y")+
  labs(x = "G plants added", y="density (#/L)", title = "Daphnia")+
   geom_smooth(method = loess)
p1
z1$Plant.mass.g<-as.numeric(z1$Plant.mass.g)
z1$TREATMENT<-as.factor(z1$TREATMENT)

timepoint<-"T2"
M1 <- gam(all_daphnia ~ s(Plant.mass.g, k=5, bs="tp") +
          s(Plant.mass.g, TREATMENT, k=5, bs="re"), data=subset(z1, Time.point %in% timepoint), method="REML", family="gaussian")
M2 <- gam(all_daphnia ~ s(Plant.mass.g, k=5, bs="tp"), data=subset(z1, Time.point %in% timepoint), method="REML", family="gaussian")

AIC(M1,M2)
summary(M2)
summary(M1)

# T0
  # No difference between Daphnia abundance along burned/unburned gradients
  # No effect of plant mass on Daphnia abundance p=0.83
# T1
  #No difference between Daphnia abundance along burned/unburned gradients
  # No effect of plant mass on Daphnia abundance p=0.06
# T2
  # No difference between Daphnia abundance along burned/unburned gradients
  # SIGNIFICANT effect of plant mass on Daphnia abundance p=0.04
# T3
  # SIGNIFICANT difference between Daphnia abundance along burned/unburned gradients plantmass*treatment, p=0.009
# T4
  # No difference between Daphnia abundance along burned/unburned gradients
  # No effect of plant mass on Daphnia abundance p=0.13

#Mosquito
p2<-ggplot(z1,aes(y=mosquito, x=Plant.mass.g, color = factor(TREATMENT))) +
  geom_point(size=2)+
  facet_grid(rows=vars(DATE.COLLECTED), scales="free_y")+
  labs(x = "G plants added",y="density (#/L)", title = "Mosquitoes")+
  geom_smooth(method = loess)
p2

timepoint<-"T4"
M3 <- gam(mosquito ~ s(Plant.mass.g, k=5, bs="tp") +
            s(Plant.mass.g, TREATMENT, k=12, bs="re"), data=subset(z1, Time.point %in% timepoint), method="REML", family="gaussian")
M4 <- gam(mosquito ~ s(Plant.mass.g, k=5, bs="tp"), data=subset(z1, Time.point %in% timepoint), method="REML", family="gaussian")

AIC(M3,M4) 
summary(M3) 
summary(M4) 

# T0
  # No mosquitos
# T1
  #SIGNIFICANT difference between mosquito abundance along burned/unburned gradients plantmass*treatment, p=0.0004
# T2
  #SIGNIFICANT difference between mosquito abundance along burned/unburned gradients plantmass*treatment, p=0.004
# T3
  # SIGNIFICANT difference between mosquito abundance along burned/unburned gradients plantmass*treatment, p=0.008
# T4
  # SIGNIFICANT difference between mosquito abundance along burned/unburned gradients plantmass*treatment, p=0.01

#Calanoid
p3<-ggplot(z1,aes(y=calanoid, x=Plant.mass.g, color = factor(TREATMENT))) +
  geom_point(size=2, aes(colour=factor(TREATMENT)))+
  facet_grid(rows=vars(DATE.COLLECTED), scales="free_y")+
  labs(x = "G plants added",y="density (#/L)", title = "calanoid")+
  geom_smooth(method = loess)
p3


timepoint<-"T4"
M5 <- gam(calanoid ~ s(Plant.mass.g, k=5, bs="tp") +
            s(Plant.mass.g, TREATMENT, k=5, bs="re"), data=subset(z1, Time.point %in% timepoint), method="REML", family="gaussian")
M6 <- gam(calanoid ~ s(Plant.mass.g, k=5, bs="tp"), data=subset(z1, Time.point %in% timepoint), method="REML", family="gaussian")

AIC(M5,M6) 
summary(M5) 
summary(M6) 


#Cyclopoid
p4<-ggplot(z1,aes(y=cyclopoid, x=Plant.mass.g, color = factor(TREATMENT))) +
  geom_point(size=2, aes(colour=factor(TREATMENT)))+
  facet_grid(rows=vars(DATE.COLLECTED), scales="free_y")+
  labs(x = "G plants added",y="density (#/L)", title = "cyclopoid")+
  geom_smooth(method = loess)
p4

timepoint<-"T3"
M7 <- gam(cyclopoid ~ s(Plant.mass.g, k=5, bs="tp") +
            s(Plant.mass.g, TREATMENT, k=5, bs="re"), data=subset(z1, Time.point %in% timepoint), method="REML", family="gaussian")
M8 <- gam(cyclopoid ~ s(Plant.mass.g, k=5, bs="tp"), data=subset(z1, Time.point %in% timepoint), method="REML", family="gaussian")

AIC(M7,M8) 
summary(M7) 
summary(M8) 


#Kellicottia
p5<-ggplot(z1,aes(y=kellicottia, x=Plant.mass.g, color = factor(TREATMENT))) +
  geom_point(size=2)+
  facet_grid(rows=vars(DATE.COLLECTED), scales="free_y")+
  labs(x = "G plants added",y="density (#/L)", title = "kellicottia")+
  geom_smooth(method = loess)
p5

#Nauplii
p6<-ggplot(z1,aes(y=nauplii, x=Plant.mass.g, color = factor(TREATMENT))) +
  geom_point(size=2)+
  facet_grid(rows=vars(DATE.COLLECTED), scales="free_y")+
  labs(x = "G plants added",y="density (#/L)", title = "nauplii")+
  geom_smooth(method = loess)
p6

p6<-ggplot(z1,aes(y=chironomid, x=Plant.mass.g, color = factor(TREATMENT))) +
  geom_point(size=2)+
  facet_grid(rows=vars(DATE.COLLECTED), scales="free_y")+
  labs(x = "G plants added",y="density (#/L)", title = "chironomid")+
  geom_smooth(method = loess)
p6

z2<-summaryBy(DENSITY.mean ~ spp, data = z, FUN = c(mean, max, min))
z3<-subset(z, spp=="calanoid"|spp=="ceriodaphnia"|spp=="chironomid"|spp=="cyclopoid"|
             spp=="daphnia"|spp=="kellicottia"|spp=="keratella"|spp=="mayfly"|spp=="mosquito"|
             spp=="nauplii")
levels(as.factor(z$spp))


p7<-ggplot(z3,aes(fill=spp, y=DENSITY.mean, x=Plant.mass.g)) +
  geom_bar(stat="identity", position="fill", width = 12)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(rows=vars(DATE.COLLECTED), cols=vars(TREATMENT))+
  labs(x = "Treatment")
p7


p8<-ggplot(z3,aes(fill=spp, y=DENSITY.mean, x=Plant.mass.g)) +
  geom_bar(stat="identity", width = 12)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(rows=vars(DATE.COLLECTED), cols=vars(TREATMENT), scales="free")+
  labs(x = "Treatment")
p8 #position="fill",

z4<-pivot_wider(z3, names_from = spp, values_from = DENSITY.mean)
z4[is.na(z4)]<-0
names(z4)


PCA1 <- prcomp(subset(z4, Time.point %in% "T1")[,c(6:15)], center = TRUE)
PCA2 <- prcomp(subset(z4, Time.point %in% "T2")[,c(6:15)], center = TRUE)
PCA3 <- prcomp(subset(z4, Time.point %in% "T3")[,c(6:15)], center = TRUE)
PCA4 <- prcomp(subset(z4, Time.point %in% "T4")[,c(6:15)], center = TRUE)

#TIME 1
scores1<-as.data.frame(PCA1$x[,c(1,2)])
scores1<-cbind(scores1,as.data.frame(subset(z4, Time.point %in% "T1"))[,c(1,4,5)] )
ecor1<-as.data.frame(PCA1$rotation[,c(1:2)])
jj1<-summary(PCA1)
scores1$mass.rank<-trunc(rank(scores1$Plant.mass.g))


PCplot1<-ggplot() +
  geom_point(data=scores1, aes(PC1, PC2, colour=TREATMENT, fill=TREATMENT, size=mass.rank*2), alpha=0.3) + 
  geom_segment(data = ecor1,aes(x = 0, y = 0, xend = PC1*15, yend = PC2*15), 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
                             type = "closed"),linetype=1, size=0.6,colour = "black")+
  geom_text(data = ecor1,aes(PC1*15,PC2*15,label=row.names(ecor1)))+
  ggtitle("Time point 1")+
  labs(x=paste("PC 1 (", format(100 *jj1$importance[2,1], digits=4), "%)", sep=""),
       y=paste("PC 2 (", format(100 *jj1$importance[2,2], digits=4), "%)", sep=""))
PCplot1

adonis(subset(z4, Time.point %in% "T1")[,c(6:15)]~scores1$TREATMENT*scores1$Plant.mass.g)

#TIME 2
scores2<-as.data.frame(PCA2$x[,c(1,2)])
scores2<-cbind(scores2,as.data.frame(subset(z4, Time.point %in% "T2"))[,c(1,4,5)] )
ecor2<-as.data.frame(PCA2$rotation[,c(1:2)])
jj2<-summary(PCA2)
scores2$mass.rank<-trunc(rank(scores2$Plant.mass.g))

PCplot2<-ggplot() +
  geom_point(data=scores2, aes(PC1, PC2, colour=TREATMENT, fill=TREATMENT, size=mass.rank), alpha=0.3) + 
  geom_segment(data = ecor2,aes(x = 0, y = 0, xend = PC1*15, yend = PC2*15), 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
                             type = "closed"),linetype=1, size=0.6,colour = "black")+
 geom_text(data = ecor2,aes(PC1*15,PC2*15,label=row.names(ecor2)))+
  ggtitle("Time point 2")+
  labs(x=paste("PC 1 (", format(100 *jj2$importance[2,1], digits=4), "%)", sep=""),
       y=paste("PC 2 (", format(100 *jj2$importance[2,2], digits=4), "%)", sep=""))
PCplot2

adonis(subset(z4, Time.point %in% "T2")[,c(6:15)]~scores2$TREATMENT*scores2$Plant.mass.g)


#TIME 3
scores3<-as.data.frame(PCA3$x[,c(1,2)])
scores3<-cbind(scores3,as.data.frame(subset(z4, Time.point %in% "T3"))[,c(1,4,5)] )
ecor3<-as.data.frame(PCA3$rotation[,c(1:2)])
jj3<-summary(PCA3)
scores3$mass.rank<-trunc(rank(scores3$Plant.mass.g))


PCplot3<-ggplot() +
  geom_point(data=scores3, aes(PC1, PC2, colour=TREATMENT, fill=TREATMENT, size=mass.rank), alpha=0.3) + 
  geom_segment(data = ecor3,aes(x = 0, y = 0, xend = PC1*15, yend = PC2*15), 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
                             type = "closed"),linetype=1, size=0.6,colour = "black")+
  geom_text(data = ecor3,aes(PC1*15,PC2*15,label=row.names(ecor3)))+
  ggtitle("Time point 3")+
  labs(x=paste("PC 1 (", format(100 *jj3$importance[2,1], digits=4), "%)", sep=""),
       y=paste("PC 2 (", format(100 *jj3$importance[2,2], digits=4), "%)", sep=""))
PCplot3

adonis(subset(z4, Time.point %in% "T3")[,c(6:15)]~scores3$TREATMENT*scores3$Plant.mass.g)



#TIME 4
scores4<-as.data.frame(PCA4$x[,c(1,2)])
scores4<-cbind(scores4,as.data.frame(subset(z4, Time.point %in% "T4"))[,c(1,4,5)] )
ecor4<-as.data.frame(PCA4$rotation[,c(1:2)])
jj4<-summary(PCA4)
scores4$mass.rank<-trunc(rank(scores4$Plant.mass.g))


PCplot4<-ggplot() +
  geom_point(data=scores4, aes(PC1, PC2, colour=TREATMENT, fill=TREATMENT, size=mass.rank), alpha=0.3) + 
  geom_segment(data = ecor4,aes(x = 0, y = 0, xend = PC1*15, yend = PC2*15), 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
                             type = "closed"),linetype=1, size=0.6,colour = "black")+
  geom_text(data = ecor4,aes(PC1*15,PC2*15,label=row.names(ecor4)))+
  ggtitle("Time point 4")+
  labs(x=paste("PC 1 (", format(100 *jj3$importance[2,1], digits=4), "%)", sep=""),
       y=paste("PC 2 (", format(100 *jj3$importance[2,2], digits=4), "%)", sep=""))
PCplot4

adonis(subset(z4, Time.point %in% "T4")[,c(6:15)]~scores4$TREATMENT*scores4$Plant.mass.g)










# With Hellinger transformations

zoop1<-decostand(subset(z4, Time.point %in% "T1")[,c(6:15)], "hell")
PCA1 <- prcomp(zoop1, center = TRUE)
zoop2<-decostand(subset(z4, Time.point %in% "T2")[,c(6:15)], "hell")
PCA2 <- prcomp(zoop2, center = TRUE)
zoop3<-decostand(subset(z4, Time.point %in% "T3")[,c(6:15)], "hell")
PCA3 <- prcomp(zoop3, center = TRUE)
zoop4<-decostand(subset(z4, Time.point %in% "T4")[,c(6:15)], "hell")
PCA4 <- prcomp(zoop4, center = TRUE)

#TIME 1
scores1<-as.data.frame(PCA1$x[,c(1,2)])
scores1<-cbind(scores1,as.data.frame(subset(z4, Time.point %in% "T1"))[,c(1,4,5)] )
jj1<-summary(PCA1)
ecor1<-as.data.frame(PCA1$rotation[,c(1:2)])
scores1$mass.rank<-trunc(rank(scores1$Plant.mass.g))


PCplot1<-ggplot() +
  geom_point(data=scores1, aes(PC1, PC2, colour=TREATMENT, fill=TREATMENT, size=mass.rank*2), alpha=0.3) + 
  geom_segment(data = ecor1,aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
                             type = "closed"),linetype=1, size=0.6,colour = "black")+
  geom_text(data = ecor1,aes(PC1,PC2,label=row.names(ecor1)))+
  ggtitle("Time point 1")+
  labs(x=paste("PC 1 (", format(100 *jj1$importance[2,1], digits=4), "%)", sep=""),
       y=paste("PC 2 (", format(100 *jj1$importance[2,2], digits=4), "%)", sep=""))
PCplot1

apply(zoop1, 1, function(x) !all(x==0)) # Need to remove rows of all 0s for the permanova
adonis(zoop1[-5,]~scores1$TREATMENT[-5]*scores1$Plant.mass.g[-5])

library(vegan)
t1zoop<-dbrda(zoop1~scores1$TREATMENT*scores1$Plant.mass.g) 
anova(t1zoop)
anova(t1zoop, by="terms", permu=800)



# Significant effect of plant mass

#TIME 2
scores2<-as.data.frame(PCA2$x[,c(1,2)])
scores2<-cbind(scores2,as.data.frame(subset(z4, Time.point %in% "T2"))[,c(1,4,5)] )
ecor2<-as.data.frame(PCA2$rotation[,c(1:2)])
jj2<-summary(PCA2)
scores2$mass.rank<-trunc(rank(scores2$Plant.mass.g))

PCplot2<-ggplot() +
  geom_point(data=scores2, aes(PC1, PC2, colour=TREATMENT, fill=TREATMENT, size=mass.rank), alpha=0.3) + 
  geom_segment(data = ecor2,aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
                             type = "closed"),linetype=1, size=0.6,colour = "black")+
  geom_text(data = ecor2,aes(PC1,PC2,label=row.names(ecor2)))+
  ggtitle("Time point 2")+
  labs(x=paste("PC 1 (", format(100 *jj2$importance[2,1], digits=4), "%)", sep=""),
       y=paste("PC 2 (", format(100 *jj2$importance[2,2], digits=4), "%)", sep=""))
PCplot2

t2zoop<-dbrda(zoop2~scores2$TREATMENT*scores2$Plant.mass.g) 
anova(t2zoop)
anova(t2zoop, by="terms", permu=800)

#TIME 3
scores3<-as.data.frame(PCA3$x[,c(1,2)])
scores3<-cbind(scores3,as.data.frame(subset(z4, Time.point %in% "T3"))[,c(1,4,5)] )
ecor3<-as.data.frame(PCA3$rotation[,c(1:2)])
jj3<-summary(PCA3)
scores3$mass.rank<-trunc(rank(scores3$Plant.mass.g))


PCplot3<-ggplot() +
  geom_point(data=scores3, aes(PC1, PC2, colour=TREATMENT, fill=TREATMENT, size=mass.rank), alpha=0.3) + 
  geom_segment(data = ecor3,aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
                             type = "closed"),linetype=1, size=0.6,colour = "black")+
  geom_text(data = ecor3,aes(PC1,PC2,label=row.names(ecor3)))+
  ggtitle("Time point 3")+
  labs(x=paste("PC 1 (", format(100 *jj3$importance[2,1], digits=4), "%)", sep=""),
       y=paste("PC 2 (", format(100 *jj3$importance[2,2], digits=4), "%)", sep=""))
PCplot3

t3zoop<-dbrda(zoop3~scores3$TREATMENT*scores3$Plant.mass.g) 
anova(t3zoop)
anova(t3zoop, by="terms", permu=800)


#TIME 4
scores4<-as.data.frame(PCA4$x[,c(1,2)])
scores4<-cbind(scores4,as.data.frame(subset(z4, Time.point %in% "T4"))[,c(1,4,5)] )
ecor4<-as.data.frame(PCA4$rotation[,c(1:2)])
jj4<-summary(PCA4)
scores4$mass.rank<-trunc(rank(scores4$Plant.mass.g))


PCplot4<-ggplot() +
  geom_point(data=scores4, aes(PC1, PC2, colour=TREATMENT, fill=TREATMENT, size=mass.rank), alpha=0.3) + 
  geom_segment(data = ecor4,aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
                             type = "closed"),linetype=1, size=0.6,colour = "black")+
  geom_text(data = ecor4,aes(PC1,PC2,label=row.names(ecor4)))+
  ggtitle("Time point 4")+
  labs(x=paste("PC 1 (", format(100 *jj3$importance[2,1], digits=4), "%)", sep=""),
       y=paste("PC 2 (", format(100 *jj3$importance[2,2], digits=4), "%)", sep=""))
PCplot4

t4zoop<-dbrda(zoop4~scores4$TREATMENT*scores4$Plant.mass.g) 
anova(t4zoop)
anova(t4zoop, by="terms", permu=800)

