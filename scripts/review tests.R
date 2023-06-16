pH<-read.csv("data/GH.gases/raw and info/ph comparison.csv")
pH$Treatment<-as.factor(pH$Treatment)
pH$Time.point<-as.factor(pH$Time.point)

pH.plot.compare<-ggplot(data=pH, aes(x=pH.calc, y=pH.meas))+
geom_point(aes(color=Treatment, size=plant.mass..g), alpha=0.5) +
geom_point(shape = 1, colour = "black", aes(size=plant.mass..g))+
scale_color_manual(values = c("brown1", "mediumseagreen")) + 
geom_smooth(method=lm, color="orchid", se=FALSE, show.legend = FALSE)+
coord_cartesian(ylim=c(7.5, 9.1), xlim=c(7, 9)) +
  theme_bw() + Fig.formatting +
  theme(axis.text.y=element_text(
    margin=unit(c(0.5, 0.5, 0.5, 0.5), "cm"), colour="black", size=8))

mod<-lm(pH.meas~pH.calc, data=pH)
summary(mod)
anova(mod)

pH.plot.compare
ggsave("figures/review tests/pH.plot.compare.pdf", width=6, height=4)


############################################
########  15N-sage and N added
# summary from the elemental analysis
plant.nut<-read.csv("data/Pyro_plant material_elemental.csv")

cols<-c("type", "plant", "treatment") # columns to make factors
plant.nut[cols] <- lapply(plant.nut[cols], factor) # make all these factors

plant.sum.trt<-aggregate(N~treatment, plant.nut, FUN=mean)


#### test plot: looking at %N
N.box.test<- ggplot(plant.nut, aes(x=treatment:plant, y=N, fill=treatment)) +
  geom_boxplot(alpha=0.7) +
  scale_fill_manual(values = c("brown1", "mediumseagreen")) +
  geom_dotplot(binaxis='y', stackdir='center', alpha=0.5, dotsize=0.5,
               position=position_dodge(0.75))+
  xlab("Treatment")+
  ylab("%N")+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

N.box.test
ggsave("figures/review tests/percent.N.box.alt.pdf", encod="MacRoman", height=4, width=4)


##### new test df
topes.trt.test<-topes.trt

# if all things equal, use the %N of sage and willow (stem and leaf) to determine the g of N added
topes.trt.test$plant.mass..g.N<- ifelse(topes.trt.test$Treatment =="burned", topes.trt.test$plant.mass..g*(1.296545/100), topes.trt.test$plant.mass..g*(1.177917/100))

# plot relationship
biomass.plant.N<-ggplot(topes.trt.test, aes(x=plant.mass..g, y=plant.mass..g.N, color=Treatment))+
  geom_point()+
  geom_line() + theme_classic()

biomass.plant.N
ggsave("figures/review tests/biomass.plant.N.pdf", encod="MacRoman", height=3, width=4)
  

# test models T1
m1.T1.sage.test <- gam(percent.sage ~ Treatment + Type +
                    s(plant.mass..g.N, by=Treatment), 
                  subset = Time.point=="T1", data=topes.trt.test, method="REML", family="gaussian")

m2.T1.sage.test <- gam(percent.sage ~ Treatment + Type +
                         s(plant.mass..g.N), 
                       subset = Time.point=="T1", data=topes.trt.test, method="REML", family="gaussian")

m3.T1.sage.test <- gam(percent.sage ~
                    s(plant.mass..g.N), 
                  subset = Time.point=="T1", data=topes.trt.test, method="REML", family="gaussian")

AIC.sage.T1<-AIC(m1.T1.sage.test, m2.T1.sage.test, m3.T1.sage.test)

# anova for best model
anova.gam(m1.T1.sage.test)

# smooth fit for plot
msmooth.T1<- gam(percent.sage ~ Treatment +
                        s(plant.mass..g.N, by=Treatment), 
                      subset = Time.point=="T1", data=topes.trt.test, method="REML", family="gaussian")

per.Sage.T1.mod.plot.test<-
  plot_smooths(
    model = msmooth.T1,
    series = plant.mass..g.N,
    comparison = Treatment
  )  + theme(legend.position = "none") +
  geom_point(data=topes.trt.test[(topes.trt.test$Time.point=="T1"),], 
             aes(x=plant.mass..g.N, y=percent.sage, color=Treatment, shape=Type)) +
  scale_shape_manual(name="Plankton", values = c(17, 16), 
                     labels = c(expression(paste("> 63"~mu,"m")), 
                                expression(paste("< 63"~mu,"m")))) +
  scale_color_manual(values = c("brown1", "mediumseagreen")) +
  ylab("% Sage")+
  xlab("plant N added (g)") +
  ggtitle("Time-1") +
  coord_cartesian(ylim=c(0, 100)) +
  Fig.formatting +
  theme(legend.key.size = unit(1,"line"))


### T2
# test models
m1.T2.sage.test <- gam(percent.sage ~ Treatment + Type +
                         s(plant.mass..g.N, by=Treatment), 
                       subset = Time.point=="T2", data=topes.trt.test, method="REML", family="gaussian")

m2.T2.sage.test <- gam(percent.sage ~ Treatment + Type +
                         s(plant.mass..g.N), 
                       subset = Time.point=="T2", data=topes.trt.test, method="REML", family="gaussian")

m3.T2.sage.test <- gam(percent.sage ~
                         s(plant.mass..g.N), 
                       subset = Time.point=="T2", data=topes.trt.test, method="REML", family="gaussian")

AIC.sage.T2<-AIC(m1.T2.sage.test, m2.T2.sage.test, m3.T2.sage.test)

# anova for best model
anova.gam(m1.T2.sage.test)

# smooth fit for plot
msmooth.T2<- gam(percent.sage ~ Treatment +
                   s(plant.mass..g.N, by=Treatment), 
                 subset = Time.point=="T2", data=topes.trt.test, method="REML", family="gaussian")

per.Sage.T2.mod.plot.test<-
  plot_smooths(
    model = msmooth.T2,
    series = plant.mass..g.N,
    comparison = Treatment
  )  + theme(legend.position = "none") +
  geom_point(data=topes.trt.test[(topes.trt.test$Time.point=="T2"),], 
             aes(x=plant.mass..g.N, y=percent.sage, color=Treatment, shape=Type)) +
  scale_shape_manual(name="Plankton", values = c(17, 16), 
                     labels = c(expression(paste("> 63"~mu,"m")), 
                                expression(paste("< 63"~mu,"m")))) +
  scale_color_manual(values = c("brown1", "mediumseagreen")) +
  ylab("% Sage")+
  xlab("plant N added (g)") +
  ggtitle("Time-2") +
  coord_cartesian(ylim=c(0, 100)) +
  Fig.formatting +
  theme(legend.key.size = unit(1,"line"))


#### combine 
sage.mix.model.alt<- plot_grid(per.Sage.T1.mod.plot.test + theme(legend.position = "none"), 
                               per.Sage.T2.mod.plot.test + theme(legend.position = "none"),
                           extract.legend.mix,
                           rel_widths = c(8,8,3), ncol=3, labels=c('A', 'B', ''), label_size=8)
sage.mix.model.alt

ggsave("figures/review tests/Isotope.mixmodel.alt.pdf", encod="MacRoman", height=4, width=8)



#######################################################
###### probe pH effects in response to loading
# data = YSI

YSI.pH<-YSI

cols<-c("Time.point", "Treatment", "Tank", "Date", "Dawn..Dusk") # columns to make factors
YSI.pH[cols] <- lapply(YSI.pH[cols], factor) # make all these factors
YSI.pH$plant.mass..g<-as.numeric(YSI.pH$plant.mass..g)


YSI.pH.T1<-YSI.pH[(YSI.pH$Date=="2021-11-15"),]
YSI.pH.T2<-YSI.pH[(YSI.pH$Time.point=="T2"),]
YSI.pH.T2<-YSI.pH.T2[!(YSI.pH.T2$Dawn..Dusk=="dawn" & YSI.pH.T2$Date=="2021-12-06"),]
YSI.pH.T3<-YSI.pH[(YSI.pH$Date=="2022-01-03"),]


################## simple facet plots
# T1
pH.plot.YSI.T1<-ggplot(data=YSI.pH.T1, aes(x=plant.mass..g, y=pH))+
  geom_point(aes(color=Treatment)) +
  coord_cartesian(ylim=c(7, 9.5)) +
  ggtitle("T1")+ 
  geom_smooth(method="loess", aes(group=Treatment, color=Treatment, fill=Treatment), alpha=0.2)+
  scale_color_manual(values = c("brown1", "mediumseagreen")) + 
  scale_fill_manual(values = c("brown1", "mediumseagreen")) + 
  theme_bw()+
  theme(axis.text.y=element_text(
    margin=unit(c(0.5, 0.5, 0.5, 0.5), "cm"), colour="black", size=8)) +
  facet_wrap(~ Dawn..Dusk)

# T2
pH.plot.YSI.T2<-ggplot(data=YSI.pH.T2, aes(x=plant.mass..g, y=pH))+
  geom_point(aes(color=Treatment)) +
  coord_cartesian(ylim=c(7, 9.5)) +
  geom_smooth(method="loess", aes(group=Treatment, color=Treatment, fill=Treatment), alpha=0.2)+
  scale_color_manual(values = c("brown1", "mediumseagreen")) + 
  scale_fill_manual(values = c("brown1", "mediumseagreen")) + 
  ggtitle("T2")+ 
  theme_bw()+
  theme(axis.text.y=element_text(
    margin=unit(c(0.5, 0.5, 0.5, 0.5), "cm"), colour="black", size=8)) +
  facet_wrap(~ Dawn..Dusk)

# T3
pH.plot.YSI.T3<-ggplot(data=YSI.pH.T3, aes(x=plant.mass..g, y=pH))+
  geom_point(aes(color=Treatment)) +
  coord_cartesian(ylim=c(7, 9.5)) +
  geom_smooth(method="loess", aes(group=Treatment, color=Treatment, fill=Treatment), alpha=0.2)+
  scale_color_manual(values = c("brown1", "mediumseagreen")) + 
  scale_fill_manual(values = c("brown1", "mediumseagreen")) + 
  ggtitle("T3")+ 
  theme_bw()+
  theme(axis.text.y=element_text(
    margin=unit(c(0.5, 0.5, 0.5, 0.5), "cm"), colour="black", size=8)) +
  facet_wrap(~ Dawn..Dusk)



######################## ######################## ######################## 
######################## test models
######################## 

########### Dawn 1 
m1.pH.T1.dawn<-gam(pH ~ Treatment + s(plant.mass..g, by= Treatment), subset = Dawn..Dusk=="dawn", data = YSI.pH.T1, method = "REML")

m2.pH.T1.dawn<-gam(pH ~ Treatment + s(plant.mass..g), subset = Dawn..Dusk=="dawn", data = YSI.pH.T1, method = "REML")

m3.pH.T1.dawn<-gam(pH ~ s(plant.mass..g), subset = Dawn..Dusk=="dawn", data = YSI.pH.T1, method = "REML")

T1.dawn.AIC<-AIC(m1.pH.T1.dawn, m2.pH.T1.dawn, m3.pH.T1.dawn)
# model with treatment and global smooth best

summary(m1.pH.T1.dawn)
anova.gam(m1.pH.T1.dawn)
gam.check(m1.pH.T1.dawn, rep=1000)
draw(m1.pH.T1.dawn)
concrvity(m1.pH.T1.dawn)
par(mfrow = c(2, 2))
plot(m1.pH.T1.dawn, all.terms = TRUE, page=1)

# model predictions
dawn.diff.T1<-plot_difference(
  m1.pH.T1.dawn,
  series = plant.mass..g,
  difference = list(Treatment = c("burned", "unburned"))
)

###########  
#plot for the model output on rawdata
dawn.T1.mod.plot<-
  plot_smooths(
    model = m1.pH.T1.dawn,
    series = plant.mass..g,
    comparison= Treatment
  ) + 
  geom_point(data=YSI.pH.T1, aes(x=plant.mass..g, y=pH, color=Treatment))+
  scale_color_manual(values = c("brown1", "mediumseagreen")) + 
  coord_cartesian(ylim=c(7, 9.5)) +
  ggtitle("T1-dawn")+
  theme(axis.text.y=element_text(
    margin=unit(c(0.5, 0.5, 0.5, 0.5), "cm"), colour="black", size=8))+
  Fig.formatting

  

########################## ######################## ######################## 
########### Dusk 1 
m1.pH.T1.dusk<-gam(pH ~ Treatment + s(plant.mass..g, by= Treatment), subset = Dawn..Dusk=="dusk", data = YSI.pH.T1, method = "REML")

m2.pH.T1.dusk<-gam(pH ~ Treatment + s(plant.mass..g), subset = Dawn..Dusk=="dusk", data = YSI.pH.T1, method = "REML")

m3.pH.T1.dusk<-gam(pH ~ s(plant.mass..g), subset = Dawn..Dusk=="dusk", data = YSI.pH.T1, method = "REML")

T1.dusk.AIC<-AIC(m1.pH.T1.dusk, m2.pH.T1.dusk, m3.pH.T1.dusk)
# model with treatment and global smooth best

summary(m1.pH.T1.dusk)
anova.gam(m1.pH.T1.dusk)
gam.check(m1.pH.T1.dusk, rep=1000)
draw(m1.pH.T1.dusk)
concrvity(m1.pH.T1.dusk)
par(mfrow = c(2, 2))
plot(m1.pH.T1.dusk, all.terms = TRUE, page=1)

# model predictions
dusk.diff.T1<-plot_difference(
  m1.pH.T1.dusk,
  series = plant.mass..g,
  difference = list(Treatment = c("burned", "unburned"))
)

###########  
#plot for the model output on rawdata
dusk.T1.mod.plot<-
  plot_smooths(
    model = m1.pH.T1.dusk,
    series = plant.mass..g,
    comparison= Treatment
  ) + 
  geom_point(data=YSI.pH.T1, aes(x=plant.mass..g, y=pH, color=Treatment))+
  scale_color_manual(values = c("brown1", "mediumseagreen")) + 
  coord_cartesian(ylim=c(7, 9.5)) +
  ggtitle("T1-dusk")+
  theme(axis.text.y=element_text(
    margin=unit(c(0.5, 0.5, 0.5, 0.5), "cm"), colour="black", size=8)) +
  Fig.formatting


########### Dawn T2 
m1.pH.T2.dawn<-gam(pH ~ Treatment + s(plant.mass..g, by= Treatment), subset = Dawn..Dusk=="dawn", data = YSI.pH.T2, method = "REML")

m2.pH.T2.dawn<-gam(pH ~ Treatment + s(plant.mass..g), subset = Dawn..Dusk=="dawn", data = YSI.pH.T2, method = "REML")

m3.pH.T2.dawn<-gam(pH ~ s(plant.mass..g), subset = Dawn..Dusk=="dawn", data = YSI.pH.T2, method = "REML")

T2.dawn.AIC<-AIC(m1.pH.T2.dawn, m2.pH.T2.dawn, m3.pH.T2.dawn)
# model with global smooth best

summary(m3.pH.T2.dawn)
anova.gam(m3.pH.T2.dawn)
gam.check(m3.pH.T2.dawn, rep=1000)
draw(m3.pH.T2.dawn)
concrvity(m3.pH.T2.dawn)
par(mfrow = c(2, 2))
plot(m3.pH.T2.dawn, all.terms = TRUE, page=1)

# model predictions
dawn.diff.T2<-plot_difference(
  m3.pH.T2.dawn,
  series = plant.mass..g,
  difference = list(Treatment = c("burned", "unburned"))
)

###########  
#plot for the model output on rawdata
dawn.T2.mod.plot<-
  plot_smooths(
    model = m3.pH.T2.dawn,
    series = plant.mass..g,
  ) + 
  geom_point(data=YSI.pH.T2, aes(x=plant.mass..g, y=pH, color=Treatment))+
  scale_color_manual(values = c("brown1", "mediumseagreen")) + 
  coord_cartesian(ylim=c(7, 9.5)) +
  ggtitle("T2-dawn")+ 
  theme(axis.text.y=element_text(
    margin=unit(c(0.5, 0.5, 0.5, 0.5), "cm"), colour="black", size=8)) +
  Fig.formatting



########################## ######################## ######################## 
########### Dusk T2 
m1.pH.T2.dusk<-gam(pH ~ Treatment + s(plant.mass..g, by= Treatment), subset = Dawn..Dusk=="dusk", data = YSI.pH.T2, method = "REML")

m2.pH.T2.dusk<-gam(pH ~ Treatment + s(plant.mass..g), subset = Dawn..Dusk=="dusk", data = YSI.pH.T2, method = "REML")

m3.pH.T2.dusk<-gam(pH ~ s(plant.mass..g), subset = Dawn..Dusk=="dusk", data = YSI.pH.T2, method = "REML")

T2.dusk.AIC<-AIC(m1.pH.T2.dusk, m2.pH.T2.dusk, m3.pH.T2.dusk)
# model with treatment and global smooth best

summary(m1.pH.T2.dusk)
anova.gam(m1.pH.T2.dusk)
gam.check(m1.pH.T2.dusk, rep=1000)
draw(m1.pH.T2.dusk)
concrvity(m1.pH.T2.dusk)
par(mfrow = c(2, 2))
plot(m1.pH.T2.dusk, all.terms = TRUE, page=1)

# model predictions
dusk.diff.T2<-plot_difference(
  m1.pH.T2.dusk,
  series = plant.mass..g,
  difference = list(Treatment = c("burned", "unburned"))
)

###########  
#plot for the model output on rawdata
dusk.T2.mod.plot<-
  plot_smooths(
    model = m1.pH.T2.dusk,
    series = plant.mass..g,
    comparison= Treatment
  ) + 
  geom_point(data=YSI.pH.T2, aes(x=plant.mass..g, y=pH, color=Treatment))+
  scale_color_manual(values = c("brown1", "mediumseagreen")) + 
  coord_cartesian(ylim=c(7, 9.5)) +
  ggtitle("T2-dusk")+ 
  theme(axis.text.y=element_text(
    margin=unit(c(0.5, 0.5, 0.5, 0.5), "cm"), colour="black", size=8))+
  Fig.formatting



########### ########### ########### ########### ########### ########### 
########### Dawn T3 
m1.pH.T3.dawn<-gam(pH ~ Treatment + s(plant.mass..g, by= Treatment), subset = Dawn..Dusk=="dawn", data = YSI.pH.T3, method = "REML")

m2.pH.T3.dawn<-gam(pH ~ Treatment + s(plant.mass..g), subset = Dawn..Dusk=="dawn", data = YSI.pH.T3, method = "REML")

m3.pH.T3.dawn<-gam(pH ~ s(plant.mass..g), subset = Dawn..Dusk=="dawn", data = YSI.pH.T3, method = "REML")

T3.dawn.AIC<-AIC(m1.pH.T3.dawn, m2.pH.T3.dawn, m3.pH.T3.dawn)
# model with treatment and global smooth best

summary(m1.pH.T3.dawn)
anova.gam(m1.pH.T3.dawn)
gam.check(m1.pH.T3.dawn, rep=1000)
draw(m1.pH.T3.dawn)
concrvity(m1.pH.T3.dawn)
par(mfrow = c(2, 2))
plot(m1.pH.T3.dawn, all.terms = TRUE, page=1)

# model predictions
dawn.diff.T3<-plot_difference(
  m1.pH.T3.dawn,
  series = plant.mass..g,
  difference = list(Treatment = c("burned", "unburned"))
)

###########  
#plot for the model output on rawdata
dawn.T3.mod.plot<-
  plot_smooths(
    model = m1.pH.T3.dawn,
    series = plant.mass..g,
    comparison= Treatment
  ) + 
  geom_point(data=YSI.pH.T3, aes(x=plant.mass..g, y=pH, color=Treatment))+
  scale_color_manual(values = c("brown1", "mediumseagreen")) + 
  coord_cartesian(ylim=c(7, 9.5)) +
  ggtitle("T3-dawn")+
  theme(axis.text.y=element_text(
    margin=unit(c(0.5, 0.5, 0.5, 0.5), "cm"), colour="black", size=8))+
Fig.formatting


########################## ######################## ######################## 
########### Dusk 1 
m1.pH.T3.dusk<-gam(pH ~ Treatment + s(plant.mass..g, by= Treatment), subset = Dawn..Dusk=="dusk", data = YSI.pH.T3, method = "REML")

m2.pH.T3.dusk<-gam(pH ~ Treatment + s(plant.mass..g), subset = Dawn..Dusk=="dusk", data = YSI.pH.T3, method = "REML")

m3.pH.T3.dusk<-gam(pH ~ s(plant.mass..g), subset = Dawn..Dusk=="dusk", data = YSI.pH.T3, method = "REML")

T3.dusk.AIC<-AIC(m1.pH.T3.dusk, m2.pH.T3.dusk, m3.pH.T3.dusk)
# model with treatment and global smooth best

summary(m1.pH.T3.dusk)
anova.gam(m1.pH.T3.dusk)
gam.check(m1.pH.T3.dusk, rep=1000)
draw(m1.pH.T3.dusk)
concrvity(m1.pH.T3.dusk)
par(mfrow = c(2, 2))
plot(m1.pH.T3.dusk, all.terms = TRUE, page=1)

# model predictions
dusk.diff.T3<-plot_difference(
  m1.pH.T3.dusk,
  series = plant.mass..g,
  difference = list(Treatment = c("burned", "unburned"))
)

###########  
#plot for the model output on rawdata
dusk.T3.mod.plot<-
  plot_smooths(
    model = m1.pH.T3.dusk,
    series = plant.mass..g,
    comparison= Treatment
  ) + 
  geom_point(data=YSI.pH.T3, aes(x=plant.mass..g, y=pH, color=Treatment))+
  scale_color_manual(values = c("brown1", "mediumseagreen")) + 
  coord_cartesian(ylim=c(7, 9.5)) +
  ggtitle("T3-dusk")+
  theme(axis.text.y=element_text(
    margin=unit(c(0.5, 0.5, 0.5, 0.5), "cm"), colour="black", size=8))+
  Fig.formatting

### combine plots
pH.plots<-plot_grid(dawn.T1.mod.plot+ theme(legend.position = "none"),
          dawn.T2.mod.plot+ theme(legend.position = "none"),
          dawn.T3.mod.plot+ theme(legend.position = "none"),
          dusk.T1.mod.plot+ theme(legend.position = "none"),
          dusk.T2.mod.plot+ theme(legend.position = "none"),
          dusk.T3.mod.plot+ theme(legend.position = "none"),
          ncol=3)

ggsave("figures/review tests/pHplots.pdf", height=7, width=10, encod="MacRoman")


############## pH day-night difference
################## simple facet plots
trts<-cbind(YSI.pH[c(1:30),c(4:6)]) # columns for treatments
YSI.diff<-rbind(trts, trts, trts) # to give 3 runs of these columns

x<-rep(c("T1", "T2", "T3"), each=30)
YSI.diff$Time<-as.factor(x)

# time specifc dfs already made in main code
# Time 2 needs amending due to a few weird pH readings
T2.Dawn2<-YSI.pH.T2[(YSI.pH.T2$Dawn..Dusk=="dawn"),]
T2.Dusk1<-YSI.pH.T2[(YSI.pH.T2$Dawn..Dusk=="dusk"),]

# bind dawn into new column
YSI.diff$pH.dawn<-(pH.dawn=unlist(c(T1.Dawn1$pH, T2.Dawn2$pH, T3.Dawn1$pH), use.names = FALSE))

YSI.diff$pH.dusk<-(pH.dusk=unlist(c(T1.Dusk$pH, T2.Dusk$pH, T3.Dusk$pH), use.names = FALSE))

#calculate change in pH
YSI.diff$pH.DIFF<-YSI.diff$pH.dusk-YSI.diff$pH.dawn

cols<-c("Time", "Treatment", "Tank") # columns to make factors
YSI.diff[cols] <- lapply(YSI.diff[cols], factor) # make all these factors
YSI.diff$plant.mass..g<-as.numeric(YSI.diff$plant.mass..g)

########## calculate change in pH over the day-night
pH.diurnal.change<-ggplot(data=YSI.diff, aes(x=plant.mass..g, y=pH.DIFF))+
  geom_point(aes(color=Treatment)) +
  #coord_cartesian(ylim=c(7, 9.5)) +
  ggtitle("pH change (dusk to dawn: ++ = dusk > pH)")+ 
  geom_smooth(method="loess", aes(group=Treatment, color=Treatment, fill=Treatment), alpha=0.2)+
  scale_color_manual(values = c("brown1", "mediumseagreen")) + 
  scale_fill_manual(values = c("brown1", "mediumseagreen")) + 
  theme_bw()+
  theme(axis.text.y=element_text(
    margin=unit(c(0.5, 0.5, 0.5, 0.5), "cm"), colour="black", size=8)) +
  facet_wrap(~ Time)

ggsave("figures/review tests/pH.diurnal.change.pdf", height=6, width=10, encod="MacRoman")

