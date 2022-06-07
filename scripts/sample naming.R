getwd()

samples<-read.csv("flowcytom.csv")
head(samples)
samples$names<-interaction(samples$Time, samples$Tank, samples$fluorescence, sep="_")

write.csv(samples, "samples.csv")
