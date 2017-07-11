library(readr)
library(lattice)
library(RColorBrewer)
library(ggplot2)
library(hypervolume)
library(tidyr)
library(taxize)
library(brranching)
library(picante)
library(ape)
library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(sp)
library(raster)
library(phytools)
library(geiger)
library(plotrix)

myColours <- brewer.pal(6,"Set2")

my.settings <- list(
  superpose.polygon=list(col=myColours[1:4], border="transparent"),
  strip.background=list(col=myColours[6]),
  strip.border=list(col="black")
)


#Import amniote database
#(Replaced -999 with NA in the .csv document itself prior to importing)
Amniote_Database_Aug_2015 <- read_csv("C:/Users/Cecina/Desktop/Amniote_Database_Aug_2015.csv")
Amniote_Database_Aug_2015$weaning_weight_g<-as.numeric(Amniote_Database_Aug_2015$weaning_weight_g)

#How many species have trait information for the following traits?

#female_maturity_d, litter_or_clutch_size_n, litter_or_clutches_per_y, adult_body_mass_g, longevity_y
sum(complete.cases(Amniote_Database_Aug_2015[,c(8:11,20)]))
#2770

#Create columns for invariant traits

#R=average reproductive allocation per unit time
#For mammals:
##R=litter_or_clutch_size_n*litters_or_clutches_y*weaning_weight_g
#For birds:
##R=litter_or_clutch_size_n*litters_or_clutches_y*fledging_mass_g
#For reptiles:
##R=litter_or_clutch_size_n*litters_or_clutches_y*egg_mass_g

for (i in 1:nrow(Amniote_Database_Aug_2015)) {
  print(i)
  if(Amniote_Database_Aug_2015$class[i]=='Mammalia') {
    Amniote_Database_Aug_2015[i, 'R'] <- Amniote_Database_Aug_2015[i,"litter_or_clutch_size_n"]*Amniote_Database_Aug_2015[i,"litters_or_clutches_per_y"]*Amniote_Database_Aug_2015[i,"weaning_weight_g"]
  }
  else if(Amniote_Database_Aug_2015$class[i]=='Reptilia') {
    Amniote_Database_Aug_2015[i, 'R'] <- Amniote_Database_Aug_2015[i,"litter_or_clutch_size_n"]*Amniote_Database_Aug_2015[i,"litters_or_clutches_per_y"]*Amniote_Database_Aug_2015[i,"birth_or_hatching_weight_g"]
  }
  else if(Amniote_Database_Aug_2015$class[i]=='Aves') {
    Amniote_Database_Aug_2015[i, 'R'] <- Amniote_Database_Aug_2015[i,"litter_or_clutch_size_n"]*Amniote_Database_Aug_2015[i,"litters_or_clutches_per_y"]*Amniote_Database_Aug_2015[i,"fledging_mass_g"]
  }
}

#How many non-NA values for R?
length(Amniote_Database_Aug_2015$R[!is.na(Amniote_Database_Aug_2015$R)])
#How many birds?
length(Amniote_Database_Aug_2015$R[!is.na(Amniote_Database_Aug_2015$R) & Amniote_Database_Aug_2015$class=="Aves"])
#How many mammals?
length(Amniote_Database_Aug_2015$R[!is.na(Amniote_Database_Aug_2015$R) & Amniote_Database_Aug_2015$class=="Mammalia"])
#How many reptiles?
length(Amniote_Database_Aug_2015$R[!is.na(Amniote_Database_Aug_2015$R) & Amniote_Database_Aug_2015$class=="Reptilia"])


#C=reproductive effort
#C=R/m

Amniote_Database_Aug_2015$C<-Amniote_Database_Aug_2015$R/Amniote_Database_Aug_2015$adult_body_mass_g

#How many non-NA values for C?
length(Amniote_Database_Aug_2015$C[!is.na(Amniote_Database_Aug_2015$C)])


#Calculate C*E

Amniote_Database_Aug_2015$C_E<-Amniote_Database_Aug_2015$C*Amniote_Database_Aug_2015$longevity_y

#How many non-NA values for C*E?
length(Amniote_Database_Aug_2015$C_E[!is.na(Amniote_Database_Aug_2015$C_E)])
#How many birds?
length(Amniote_Database_Aug_2015$C_E[!is.na(Amniote_Database_Aug_2015$C_E) & Amniote_Database_Aug_2015$class=="Aves"])
#How many mammals?
length(Amniote_Database_Aug_2015$C_E[!is.na(Amniote_Database_Aug_2015$C_E) & Amniote_Database_Aug_2015$class=="Mammalia"])
#How many reptiles?
length(Amniote_Database_Aug_2015$C_E[!is.na(Amniote_Database_Aug_2015$C_E) & Amniote_Database_Aug_2015$class=="Reptilia"])



#Calculate E/alpha

Amniote_Database_Aug_2015$E_alpha<-Amniote_Database_Aug_2015$longevity_y*365/Amniote_Database_Aug_2015$female_maturity_d

#How many non-NA values for E/alpha?
length(Amniote_Database_Aug_2015$E_alpha[!is.na(Amniote_Database_Aug_2015$E_alpha)])
#How many birds?
length(Amniote_Database_Aug_2015$E_alpha[!is.na(Amniote_Database_Aug_2015$E_alpha) & Amniote_Database_Aug_2015$class=="Aves"])
#How many mammals?
length(Amniote_Database_Aug_2015$E_alpha[!is.na(Amniote_Database_Aug_2015$E_alpha) & Amniote_Database_Aug_2015$class=="Mammalia"])
#How many reptiles?
length(Amniote_Database_Aug_2015$E_alpha[!is.na(Amniote_Database_Aug_2015$E_alpha) & Amniote_Database_Aug_2015$class=="Reptilia"])


#Calculate I/m

#I=size of offspring at independence
#For mammals:
##I=weaning_weight_g
#For birds:
##I=fledging_mass_g
#For reptiles:
##I=birth_or_hatching_weight_g

for (i in 1:nrow(Amniote_Database_Aug_2015)) {
  print(i)
  if(Amniote_Database_Aug_2015$class[i]=='Mammalia') {
    Amniote_Database_Aug_2015[i, 'I'] <- Amniote_Database_Aug_2015[i,"weaning_weight_g"]
  }
  else if(Amniote_Database_Aug_2015$class[i]=='Reptilia') {
    Amniote_Database_Aug_2015[i, 'I'] <- Amniote_Database_Aug_2015[i,"birth_or_hatching_weight_g"]
  }
  else if(Amniote_Database_Aug_2015$class[i]=='Aves') {
    Amniote_Database_Aug_2015[i, 'I'] <- Amniote_Database_Aug_2015[i,"fledging_mass_g"]
  }
}

#How many non-NA values for I?
length(Amniote_Database_Aug_2015$I[!is.na(Amniote_Database_Aug_2015$I)])
#How many birds?
length(Amniote_Database_Aug_2015$I[!is.na(Amniote_Database_Aug_2015$I) & Amniote_Database_Aug_2015$class=="Aves"])
#How many mammals?
length(Amniote_Database_Aug_2015$I[!is.na(Amniote_Database_Aug_2015$I) & Amniote_Database_Aug_2015$class=="Mammalia"])
#How many reptiles?
length(Amniote_Database_Aug_2015$I[!is.na(Amniote_Database_Aug_2015$I) & Amniote_Database_Aug_2015$class=="Reptilia"])


Amniote_Database_Aug_2015$I_m<-Amniote_Database_Aug_2015$I/Amniote_Database_Aug_2015$adult_body_mass_g

#How many non-NA values for I/m?
length(Amniote_Database_Aug_2015$I_m[!is.na(Amniote_Database_Aug_2015$I_m)])
#How many birds?
length(Amniote_Database_Aug_2015$I_m[!is.na(Amniote_Database_Aug_2015$I_m) & Amniote_Database_Aug_2015$class=="Aves"])
#How many mammals?
length(Amniote_Database_Aug_2015$I_m[!is.na(Amniote_Database_Aug_2015$I_m) & Amniote_Database_Aug_2015$class=="Mammalia"])
#How many reptiles?
length(Amniote_Database_Aug_2015$I_m[!is.na(Amniote_Database_Aug_2015$I_m) & Amniote_Database_Aug_2015$class=="Reptilia"])


#How many species have values for E/alpha, C*E, and I/m?
sum(complete.cases(Amniote_Database_Aug_2015[,c(39,41,42)]))
#How many birds?
length(Amniote_Database_Aug_2015$species[complete.cases(Amniote_Database_Aug_2015[,c(39,41,42)]) & Amniote_Database_Aug_2015$class=="Aves"])
#How many mammals?
length(Amniote_Database_Aug_2015$species[complete.cases(Amniote_Database_Aug_2015[,c(39,41,42)]) & Amniote_Database_Aug_2015$class=="Mammalia"])
#How many reptiles?
length(Amniote_Database_Aug_2015$species[complete.cases(Amniote_Database_Aug_2015[,c(39,41,42)]) & Amniote_Database_Aug_2015$class=="Reptilia"])

#Create bargraph with the number of species with values for each of the traits
traitcoverage<-data.frame(Class=c("Aves","Mammalia","Reptilia"),C_E=numeric(3),E_alpha=numeric(3),I_m=numeric(3),All=numeric(3))
for(i in 1:3){
  traitcoverage$C_E[i]<-length(Amniote_Database_Aug_2015$C_E[!is.na(Amniote_Database_Aug_2015$C_E) & Amniote_Database_Aug_2015$class==traitcoverage$Class[i]])
}
for(i in 1:3){
  traitcoverage$E_alpha[i]<-length(Amniote_Database_Aug_2015$E_alpha[!is.na(Amniote_Database_Aug_2015$E_alpha) & Amniote_Database_Aug_2015$class==traitcoverage$Class[i]])
}
for(i in 1:3){
  traitcoverage$I_m[i]<-length(Amniote_Database_Aug_2015$I_m[!is.na(Amniote_Database_Aug_2015$I_m) & Amniote_Database_Aug_2015$class==traitcoverage$Class[i]])
}
for(i in 1:3){
  traitcoverage$All[i]<-length(Amniote_Database_Aug_2015$species[complete.cases(Amniote_Database_Aug_2015[,c(39,41,42)]) & Amniote_Database_Aug_2015$class==traitcoverage$Class[i]])
}

traitcoverage$C_E<-as.numeric(traitcoverage$C_E)
traitcoverage$E_alpha<-as.numeric(traitcoverage$E_alpha)
traitcoverage$I_m<-as.numeric(traitcoverage$I_m)
traitcoverage$All<-as.numeric(traitcoverage$All)

traitcoverage$Class<-as.character(traitcoverage$Class)
#Add a row of totals
traitcoverage<-rbind(traitcoverage,c("Total",colSums(traitcoverage[,2:5])))
traitcoverage[4,1]<-"Total"

speciesperinvariant<-gather(traitcoverage,key = Class)
colnames(speciesperinvariant)<-c("Class","Trait","Count")

#Grouped bargraph of the number of species with values for each invariant
barchart(Count~Class,data=speciesperinvariant,groups=Trait,ylab="Number of Species",auto.key=list(space="top",columns=4),par.settings=my.settings)


#Determine how many non-NA values there are for each trait:
availabletraits=NULL
for (i in 8:36) {
  availabletraits$Trait[i]<-colnames(Amniote_Database_Aug_2015)[i]
  availabletraits$Available[i]<-length(which(!is.na(Amniote_Database_Aug_2015[,i])))
}
availabletraits<-as.data.frame(availabletraits)
availabletraits<-availabletraits[8:36,]
#order by the number of non-NA values
availabletraits<-availabletraits[order(-availabletraits$Available),]
View(availabletraits)


#Histograms of traits

#Histogram of body mass
hist(log(Amniote_Database_Aug_2015$adult_body_mass_g[!is.na(Amniote_Database_Aug_2015$adult_body_mass_g)]),xlab="Log(Body Mass)",main="")
hist(log(Amniote_Database_Aug_2015$adult_body_mass_g[!is.na(Amniote_Database_Aug_2015$adult_body_mass_g)]),breaks=200,xlab="Log(Body Mass)",main="",col = myColours[6])


ggplot(data=Amniote_Database_Aug_2015,aes(x=log(adult_body_mass_g)))+
  geom_histogram(binwidth = 0.1)
#Stacked histogram by class
ggplot(data=Amniote_Database_Aug_2015[!is.na(Amniote_Database_Aug_2015$adult_body_mass_g),],aes(x=log(adult_body_mass_g),colour=class))+
  geom_freqpoly(binwidth = 0.1)
ggplot(data=Amniote_Database_Aug_2015)+
  geom_histogram(aes(x=log(adult_body_mass_g),fill=class,alpha=0.2),data=subset(Amniote_Database_Aug_2015,class=="Aves"))+
  geom_histogram(aes(x=log(adult_body_mass_g),fill=class,alpha=0.2),data=subset(Amniote_Database_Aug_2015,class=="Mammalia"))+
  geom_histogram(aes(x=log(adult_body_mass_g),fill=class,alpha=0.2),data=subset(Amniote_Database_Aug_2015,class=="Reptilia"))



#Histogram of C*E
hist(log(Amniote_Database_Aug_2015$C_E[!is.na(Amniote_Database_Aug_2015$C_E)]),xlab="Log(C*E)",main="")
hist(log(Amniote_Database_Aug_2015$C_E[!is.na(Amniote_Database_Aug_2015$C_E)]),breaks=200,xlab="Log(C*E)",main="",col = myColours[2])

ggplot(data=Amniote_Database_Aug_2015,aes(x=log(C_E)))+
  geom_histogram(binwidth = 0.1)
#Stacked histogram by class
#dotted lines at predicted approximate locations of (altricial) birds and mammals (Charnov 2002)
ggplot(data=Amniote_Database_Aug_2015[!is.na(Amniote_Database_Aug_2015$C_E),],aes(x=log(C_E),colour=class))+
  geom_freqpoly(binwidth = 0.1)+
  geom_vline(aes(xintercept=log(5)),color=gg_color_hue(3)[1],lty=2)+
  geom_vline(aes(xintercept= log(1.7)),color=gg_color_hue(3)[2],lty=2)
ggplot(data=Amniote_Database_Aug_2015)+
  geom_histogram(aes(x=log(C_E),fill=class,alpha=0.2),data=subset(Amniote_Database_Aug_2015,class=="Aves"))+
  geom_histogram(aes(x=log(C_E),fill=class,alpha=0.2),data=subset(Amniote_Database_Aug_2015,class=="Mammalia"))+
  geom_histogram(aes(x=log(C_E),fill=class,alpha=0.2),data=subset(Amniote_Database_Aug_2015,class=="Reptilia"))


#Histogram of E/alpha
hist(log(Amniote_Database_Aug_2015$E_alpha[!is.na(Amniote_Database_Aug_2015$E_alpha)]),xlab="Log(E/alpha)",main="")
hist(log(Amniote_Database_Aug_2015$E_alpha[!is.na(Amniote_Database_Aug_2015$E_alpha)]),breaks=200,xlab="Log(E/alpha)",main="",col = myColours[3])

ggplot(data=Amniote_Database_Aug_2015,aes(x=log(E_alpha)))+
  geom_histogram(binwidth = 0.1)
#Stacked histogram by class
#dotted lines at predicted approximate locations of (altricial) birds and mammals (Charnov 2002)
ggplot(data=Amniote_Database_Aug_2015[!is.na(Amniote_Database_Aug_2015$E_alpha),],aes(x=log(E_alpha),colour=class))+
  geom_freqpoly(binwidth = 0.1)+
  geom_vline(aes(xintercept=log(3)),color=gg_color_hue(3)[1],lty=2)+
  geom_vline(aes(xintercept= log(1.35)),color=gg_color_hue(3)[2],lty=2)

#Histogram of I/m
hist(log(Amniote_Database_Aug_2015$I_m[!is.na(Amniote_Database_Aug_2015$I_m)]),xlab="Log(I/m)",main="")
hist(log(Amniote_Database_Aug_2015$I_m[!is.na(Amniote_Database_Aug_2015$I_m)]),breaks=200,xlab="Log(I/m)",main="",col = myColours[4])

ggplot(data=Amniote_Database_Aug_2015,aes(x=log(I_m)))+
  geom_histogram(binwidth = 0.1)
#Stacked histogram by class
#dotted lines at predicted approximate locations of (altricial) birds and mammals (Charnov 2002)
ggplot(data=Amniote_Database_Aug_2015[!is.na(Amniote_Database_Aug_2015$I_m),],aes(x=log(I_m),colour=class))+
  geom_freqpoly(binwidth = 0.1)+
  geom_vline(aes(xintercept=log(1)),color=gg_color_hue(3)[1],lty=2)+
  geom_vline(aes(xintercept= log(0.3)),color=gg_color_hue(3)[2],lty=2)


#Histograms with bats
#Body mass
ggplot(data=Amniote_Database_Aug_2015)+
  geom_freqpoly(aes(x=log(adult_body_mass_g),color=class),binwidth = 0.1,data=subset(Amniote_Database_Aug_2015,class=="Aves"))+
  geom_freqpoly(aes(x=log(adult_body_mass_g),color=class),binwidth = 0.1,data=subset(Amniote_Database_Aug_2015,class=="Mammalia"))+
  geom_freqpoly(aes(x=log(adult_body_mass_g),color=class),binwidth = 0.1,data=subset(Amniote_Database_Aug_2015,class=="Reptilia"))+
  geom_freqpoly(aes(x=log(adult_body_mass_g),color=order),binwidth = 0.1,data=subset(Amniote_Database_Aug_2015,order=="Chiroptera"))
  
ggplot(data=Amniote_Database_Aug_2015)+
  geom_histogram(aes(x=log(adult_body_mass_g),fill=class,alpha=0.9),data=subset(Amniote_Database_Aug_2015,class=="Aves"))+
  geom_histogram(aes(x=log(adult_body_mass_g),fill=class,alpha=0.9),data=subset(Amniote_Database_Aug_2015,class=="Mammalia"))+
  geom_histogram(aes(x=log(adult_body_mass_g),fill=class,alpha=0.9),data=subset(Amniote_Database_Aug_2015,class=="Reptilia"))+
  geom_histogram(aes(x=log(adult_body_mass_g),fill=order,alpha=0.9),data = subset(Amniote_Database_Aug_2015,order=="Chiroptera"))

#C*E
ggplot(data=Amniote_Database_Aug_2015)+
  geom_freqpoly(aes(x=log(C_E),color=class),binwidth = 0.1,data=subset(Amniote_Database_Aug_2015,class=="Aves"))+
  geom_freqpoly(aes(x=log(C_E),color=class),binwidth = 0.1,data=subset(Amniote_Database_Aug_2015,class=="Mammalia"))+
  geom_freqpoly(aes(x=log(C_E),color=class),binwidth = 0.1,data=subset(Amniote_Database_Aug_2015,class=="Reptilia"))+
  geom_freqpoly(aes(x=log(C_E),color=order),binwidth = 0.1,data=subset(Amniote_Database_Aug_2015,order=="Chiroptera"))

ggplot(data=Amniote_Database_Aug_2015)+
  geom_histogram(aes(x=log(C_E),fill=class,alpha=0.9),data=subset(Amniote_Database_Aug_2015,class=="Aves"))+
  geom_histogram(aes(x=log(C_E),fill=class,alpha=0.9),data=subset(Amniote_Database_Aug_2015,class=="Mammalia"))+
  geom_histogram(aes(x=log(C_E),fill=class,alpha=0.9),data=subset(Amniote_Database_Aug_2015,class=="Reptilia"))+
  geom_histogram(aes(x=log(C_E),fill=order,alpha=0.9),data = subset(Amniote_Database_Aug_2015,order=="Chiroptera"))

#E/alpha
ggplot(data=Amniote_Database_Aug_2015)+
  geom_freqpoly(aes(x=log(E_alpha),color=class),binwidth = 0.1,data=subset(Amniote_Database_Aug_2015,class=="Aves"))+
  geom_freqpoly(aes(x=log(E_alpha),color=class),binwidth = 0.1,data=subset(Amniote_Database_Aug_2015,class=="Mammalia"))+
  geom_freqpoly(aes(x=log(E_alpha),color=class),binwidth = 0.1,data=subset(Amniote_Database_Aug_2015,class=="Reptilia"))+
  geom_freqpoly(aes(x=log(E_alpha),color=order),binwidth = 0.1,data=subset(Amniote_Database_Aug_2015,order=="Chiroptera"))

ggplot(data=Amniote_Database_Aug_2015)+
  geom_histogram(aes(x=log(E_alpha),color=class,alpha=0.2),fill="white",data=subset(Amniote_Database_Aug_2015,class=="Aves"))+
  geom_histogram(aes(x=log(E_alpha),color=class,alpha=0.2),fill="white",data=subset(Amniote_Database_Aug_2015,class=="Mammalia"))+
  geom_histogram(aes(x=log(E_alpha),color=class,alpha=0.2),fill="white",data=subset(Amniote_Database_Aug_2015,class=="Reptilia"))+
  geom_histogram(aes(x=log(E_alpha),color=order,alpha=0.2),fill="white",data = subset(Amniote_Database_Aug_2015,order=="Chiroptera"))

#I/m
ggplot(data=Amniote_Database_Aug_2015)+
  geom_freqpoly(aes(x=log(I_m),color=class),binwidth = 0.1,data=subset(Amniote_Database_Aug_2015,class=="Aves"))+
  geom_freqpoly(aes(x=log(I_m),color=class),binwidth = 0.1,data=subset(Amniote_Database_Aug_2015,class=="Mammalia"))+
  geom_freqpoly(aes(x=log(I_m),color=class),binwidth = 0.1,data=subset(Amniote_Database_Aug_2015,class=="Reptilia"))+
  geom_freqpoly(aes(x=log(I_m),color=order),binwidth = 0.1,data=subset(Amniote_Database_Aug_2015,order=="Chiroptera"))

ggplot(data=Amniote_Database_Aug_2015)+
  geom_histogram(aes(x=log(I_m),fill=class,alpha=0.9),data=subset(Amniote_Database_Aug_2015,class=="Aves"))+
  geom_histogram(aes(x=log(I_m),fill=class,alpha=0.9),data=subset(Amniote_Database_Aug_2015,class=="Mammalia"))+
  geom_histogram(aes(x=log(I_m),fill=class,alpha=0.9),data=subset(Amniote_Database_Aug_2015,class=="Reptilia"))+
  geom_histogram(aes(x=log(I_m),fill=order,alpha=0.9),data = subset(Amniote_Database_Aug_2015,order=="Chiroptera"))


#Histograms with primates
#Body mass
ggplot(data=Amniote_Database_Aug_2015)+
  geom_freqpoly(aes(x=log(adult_body_mass_g),color=class),binwidth = 0.1,data=subset(Amniote_Database_Aug_2015,class=="Aves"))+
  geom_freqpoly(aes(x=log(adult_body_mass_g),color=class),binwidth = 0.1,data=subset(Amniote_Database_Aug_2015,class=="Mammalia"))+
  geom_freqpoly(aes(x=log(adult_body_mass_g),color=class),binwidth = 0.1,data=subset(Amniote_Database_Aug_2015,class=="Reptilia"))+
  geom_freqpoly(aes(x=log(adult_body_mass_g),color=order),binwidth = 0.1,data=subset(Amniote_Database_Aug_2015,order=="Primates"))

ggplot(data=Amniote_Database_Aug_2015)+
  geom_histogram(aes(x=log(adult_body_mass_g),fill=class,alpha=0.9),data=subset(Amniote_Database_Aug_2015,class=="Aves"))+
  geom_histogram(aes(x=log(adult_body_mass_g),fill=class,alpha=0.9),data=subset(Amniote_Database_Aug_2015,class=="Mammalia"))+
  geom_histogram(aes(x=log(adult_body_mass_g),fill=class,alpha=0.9),data=subset(Amniote_Database_Aug_2015,class=="Reptilia"))+
  geom_histogram(aes(x=log(adult_body_mass_g),fill=order,alpha=0.9),data = subset(Amniote_Database_Aug_2015,order=="Primates"))

#Apes
ggplot(data=Amniote_Database_Aug_2015)+
  #lorises
  geom_histogram(aes(x=log(adult_body_mass_g),fill=family,alpha=0.9),data = subset(Amniote_Database_Aug_2015,family=="Lorisidae"))+
  #Galagos
  geom_histogram(aes(x=log(adult_body_mass_g),fill=family,alpha=0.9),data = subset(Amniote_Database_Aug_2015,family=="Galagidae"))+
  #Tarsiers
  geom_histogram(aes(x=log(adult_body_mass_g),fill=family,alpha=0.9),data = subset(Amniote_Database_Aug_2015,family=="Tarsiidae"))+
  #Lemurs
  geom_histogram(aes(x=log(adult_body_mass_g),fill=family,alpha=0.9),data = subset(Amniote_Database_Aug_2015,family=="Cheirogaleidae" | family=="Daubentoniidae" | family=="Indridae" |  family=="Lemuridae" |  family=="Lepilemuridae"))+
  #Old World monkeys
  geom_histogram(aes(x=log(adult_body_mass_g),fill=family,alpha=0.9),data = subset(Amniote_Database_Aug_2015,family=="Cheirogaleidae"))+
  #New World monkeys
  geom_histogram(aes(x=log(adult_body_mass_g),fill=family,alpha=0.9),data = subset(Amniote_Database_Aug_2015,family=="Cheirogaleidae" | family=="Cebidae" | family=="Aotidae" |  family=="Pitheciidae" |  family=="Atelidae"))+
  #Apes
  geom_histogram(aes(x=log(adult_body_mass_g),fill=family,alpha=0.9),data = subset(Amniote_Database_Aug_2015,family=="Hominidae" | family=="Hylobatidae"))
  


#C*E
ggplot(data=Amniote_Database_Aug_2015)+
  geom_freqpoly(aes(x=log(C_E),color=class),binwidth = 0.1,data=subset(Amniote_Database_Aug_2015,class=="Aves"))+
  geom_freqpoly(aes(x=log(C_E),color=class),binwidth = 0.1,data=subset(Amniote_Database_Aug_2015,class=="Mammalia"))+
  geom_freqpoly(aes(x=log(C_E),color=class),binwidth = 0.1,data=subset(Amniote_Database_Aug_2015,class=="Reptilia"))+
  geom_freqpoly(aes(x=log(C_E),color=order),binwidth = 0.1,data=subset(Amniote_Database_Aug_2015,order=="Primates"))

ggplot(data=Amniote_Database_Aug_2015)+
  geom_histogram(aes(x=log(C_E),fill=class,alpha=0.9),data=subset(Amniote_Database_Aug_2015,class=="Aves"))+
  geom_histogram(aes(x=log(C_E),fill=class,alpha=0.9),data=subset(Amniote_Database_Aug_2015,class=="Mammalia"))+
  geom_histogram(aes(x=log(C_E),fill=class,alpha=0.9),data=subset(Amniote_Database_Aug_2015,class=="Reptilia"))+
  geom_histogram(aes(x=log(C_E),fill=order,alpha=0.9),data = subset(Amniote_Database_Aug_2015,order=="Primates"))

#E/alpha
ggplot(data=Amniote_Database_Aug_2015)+
  geom_freqpoly(aes(x=log(E_alpha),color=class),binwidth = 0.1,data=subset(Amniote_Database_Aug_2015,class=="Aves"))+
  geom_freqpoly(aes(x=log(E_alpha),color=class),binwidth = 0.1,data=subset(Amniote_Database_Aug_2015,class=="Mammalia"))+
  geom_freqpoly(aes(x=log(E_alpha),color=class),binwidth = 0.1,data=subset(Amniote_Database_Aug_2015,class=="Reptilia"))+
  geom_freqpoly(aes(x=log(E_alpha),color=order),binwidth = 0.1,data=subset(Amniote_Database_Aug_2015,order=="Primates"))

ggplot(data=Amniote_Database_Aug_2015)+
  geom_histogram(aes(x=log(E_alpha),fill=class,alpha=0.2),data=subset(Amniote_Database_Aug_2015,class=="Aves"))+
  geom_histogram(aes(x=log(E_alpha),fill=class,alpha=0.2),data=subset(Amniote_Database_Aug_2015,class=="Mammalia"))+
  geom_histogram(aes(x=log(E_alpha),fill=class,alpha=0.2),data=subset(Amniote_Database_Aug_2015,class=="Reptilia"))+
  geom_histogram(aes(x=log(E_alpha),fill=order,alpha=0.2),data = subset(Amniote_Database_Aug_2015,order=="Primates"))

#I/m
ggplot(data=Amniote_Database_Aug_2015)+
  geom_freqpoly(aes(x=log(I_m),color=class),binwidth = 0.1,data=subset(Amniote_Database_Aug_2015,class=="Aves"))+
  geom_freqpoly(aes(x=log(I_m),color=class),binwidth = 0.1,data=subset(Amniote_Database_Aug_2015,class=="Mammalia"))+
  geom_freqpoly(aes(x=log(I_m),color=class),binwidth = 0.1,data=subset(Amniote_Database_Aug_2015,class=="Reptilia"))+
  geom_freqpoly(aes(x=log(I_m),color=order),binwidth = 0.1,data=subset(Amniote_Database_Aug_2015,order=="Primates"))

ggplot(data=Amniote_Database_Aug_2015)+
  geom_histogram(aes(x=log(I_m),fill=class,alpha=0.9),data=subset(Amniote_Database_Aug_2015,class=="Aves"))+
  geom_histogram(aes(x=log(I_m),fill=class,alpha=0.9),data=subset(Amniote_Database_Aug_2015,class=="Mammalia"))+
  geom_histogram(aes(x=log(I_m),fill=class,alpha=0.9),data=subset(Amniote_Database_Aug_2015,class=="Reptilia"))+
  geom_histogram(aes(x=log(I_m),fill=order,alpha=0.9),data = subset(Amniote_Database_Aug_2015,order=="Primates"))


#Some species have values of I/m greater than 1 (juvenile larger than adult)
which(log(Amniote_Database_Aug_2015$I_m)>0)

I_m_over1<-Amniote_Database_Aug_2015[which(log(Amniote_Database_Aug_2015$I_m)>0),c(1,4,5,7,11,23:25,40,41)]
View(I_m_over1)
write.csv(I_m_over1,"I_m_over1.csv")



#Subset of database including only species for all of the invariants
desiredcolumns<-c(1:7,11,39,40,42)
completecase_species<-Amniote_Database_Aug_2015[complete.cases(Amniote_Database_Aug_2015$adult_body_mass_g,Amniote_Database_Aug_2015$C_E,Amniote_Database_Aug_2015$I_m,Amniote_Database_Aug_2015$E_alpha),desiredcolumns]
#Log transform
completecase_species$log_bodymass<-log(completecase_species$adult_body_mass_g)
completecase_species$log_C_E<-log(completecase_species$C_E)
completecase_species$log_I_m<-log(completecase_species$I_m)
completecase_species$log_E_alpha<-log(completecase_species$E_alpha)
#Scale the log transformed traits
completecase_species$scale_log_bodymass<-scale(completecase_species$log_bodymass)
completecase_species$scale_log_C_E<-scale(completecase_species$log_C_E)
completecase_species$scale_log_I_m<-scale(completecase_species$log_I_m)
completecase_species$scale_log_E_alpha<-scale(completecase_species$log_E_alpha)

#Create hypervolumes for each class of amniotes


#Bird Gaussian hypervolume

#Log transform bird hypervolume
completebirds_gaussian<-hypervolume_gaussian(data = completecase_species[completecase_species$class=="Aves",12:15],
                                             name = "completebirds_gaussian")
completebirds_gaussian@Volume

#Scale log transform bird hypervolume
completebirds_scale_gaussian<-hypervolume_gaussian(data = completecase_species[completecase_species$class=="Aves",16:19],
                                             name = "completebirds_scale_gaussian")
completebirds_scale_gaussian@Volume

#Plot bird hypervolume
#Log transform bird hypervolume
plot(completebirds_gaussian,point.dark.factor=1,color=gg_color_hue(3)[1])
plot(completebirds_gaussian,show.3d=TRUE,plot.3d.axes.id=2:4,cex.random=3,cex.data=6,
     show.legend=TRUE,point.alpha.min=0.5,point.dark.factor=1)
#Scale log transform bird hypervolume
plot(completebirds_scale_gaussian,point.dark.factor=1,color=gg_color_hue(3)[1])
plot(completebirds_scale_gaussian,show.3d=TRUE,plot.3d.axes.id=2:4,cex.random=3,cex.data=6,
     show.legend=TRUE,point.alpha.min=0.5,point.dark.factor=1)

#Plot different orders
#Accipitriformes
plot(completebirds_gaussian,point.dark.factor=1,color=gg_color_hue(3)[1],
     plot.function.additional=function(i,j) {
       points(x=complete_data[complete_data$order=="Accipitriformes",i],y=complete_data[complete_data$order=="Accipitriformes",j],col="seagreen3",pch=19) 
     })
#Anseriformes
plot(completebirds_gaussian,point.dark.factor=1,color=gg_color_hue(3)[1],
     plot.function.additional=function(i,j) {
       points(x=complete_data[complete_data$order=="Anseriformes",i],y=complete_data[complete_data$order=="Anseriformes",j],col="seagreen3",pch=19) 
     })
#Apodiformes
plot(completebirds_gaussian,point.dark.factor=1,color=gg_color_hue(3)[1],
     plot.function.additional=function(i,j) {
       points(x=complete_data[complete_data$order=="Apodiformes",i],y=complete_data[complete_data$order=="Apodiformes",j],col="seagreen3",pch=19) 
     })
#Caprimulgiformes
plot(completebirds_gaussian,point.dark.factor=1,color=gg_color_hue(3)[1],
     plot.function.additional=function(i,j) {
       points(x=complete_data[complete_data$order=="Caprimulgiformes",i],y=complete_data[complete_data$order=="Caprimulgiformes",j],col="seagreen3",pch=19) 
     })
#Charadriiformes
plot(completebirds_gaussian,point.dark.factor=1,color=gg_color_hue(3)[1],
     plot.function.additional=function(i,j) {
       points(x=complete_data[complete_data$order=="Charadriiformes",i],y=complete_data[complete_data$order=="Charadriiformes",j],col="seagreen3",pch=19) 
     })
#Passeriformes
plot(completebirds_gaussian,point.dark.factor=1,color=gg_color_hue(3)[1],
     plot.function.additional=function(i,j) {
       points(x=complete_data[complete_data$order=="Passeriformes",i],y=complete_data[complete_data$order=="Passeriformes",j],col="seagreen3",pch=19) 
     })
#Procellariiformes
plot(completebirds_gaussian,point.dark.factor=1,color=gg_color_hue(3)[1],
     plot.function.additional=function(i,j) {
       points(x=complete_data[complete_data$order=="Procellariiformes",i],y=complete_data[complete_data$order=="Procellariiformes",j],col="seagreen3",pch=19) 
     })
#Sphenisciformes
plot(completebirds_gaussian,point.dark.factor=1,color=gg_color_hue(3)[1],
     plot.function.additional=function(i,j) {
       points(x=complete_data[complete_data$order=="Sphenisciformes",i],y=complete_data[complete_data$order=="Sphenisciformes",j],col="seagreen3",pch=19) 
     })
#Strigiformes
plot(completebirds_gaussian,point.dark.factor=1,color=gg_color_hue(3)[1],
     plot.function.additional=function(i,j) {
       points(x=complete_data[complete_data$order=="Strigiformes",i],y=complete_data[complete_data$order=="Strigiformes",j],col="seagreen3",pch=19) 
     })

plot(completebirds_gaussian,point.dark.factor=1,color=gg_color_hue(3)[1],
     plot.function.additional=function(i,j) {
       points(x=complete_data[complete_data$order=="Passeriformes",i],y=complete_data[complete_data$order=="Passeriformes",j],col="green",pch=19)
       points(x=complete_data[complete_data$order=="Charadriiformes",i],y=complete_data[complete_data$order=="Charadriiformes",j],col="red",pch=19)
       points(x=complete_data[complete_data$order=="Procellariiformes",i],y=complete_data[complete_data$order=="Procellariiformes",j],col="blue",pch=19)
     })



#Mammal Gaussian hypervolume

#Log transform mammal hypervolume
completemammals_gaussian<-hypervolume_gaussian(data = completecase_species[completecase_species$class=="Mammalia",12:15],
                                             name = "completemammals_gaussian")
completemammals_gaussian@Volume
#Scaled log transform mammal hypervolume
completemammals_scale_gaussian<-hypervolume_gaussian(data = completecase_species[completecase_species$class=="Mammalia",16:19],
                                               name = "completemammals_scale_gaussian")
completemammals_scale_gaussian@Volume

#Plot mammal hypervolume
#Log transform mammal hypervolume
plot(completemammals_gaussian,point.dark.factor=1,color=gg_color_hue(3)[2])
plot(completemammals_gaussian,show.3d=TRUE,plot.3d.axes.id=2:4,cex.random=3,cex.data=6,
     show.legend=TRUE,point.alpha.min=0.5,point.dark.factor=1)
#Scaled log transform mammal hypervolume
plot(completemammals_scale_gaussian,point.dark.factor=1,color=gg_color_hue(3)[2])
plot(completemammals_scale_gaussian,show.3d=TRUE,plot.3d.axes.id=2:4,cex.random=3,cex.data=6,
     show.legend=TRUE,point.alpha.min=0.5,point.dark.factor=1)


#Reptile Gaussian hypervolume

#Log transform reptile hypervolume
completereptiles_gaussian<-hypervolume_gaussian(data = completecase_species[completecase_species$class=="Reptilia",12:15],
                                               name = "completereptiles_gaussian")
completereptiles_gaussian@Volume
#Scaled log transform reptile hypervolume
completereptiles_scale_gaussian<-hypervolume_gaussian(data = completecase_species[completecase_species$class=="Reptilia",16:19],
                                                name = "completereptiles_scale_gaussian")
completereptiles_scale_gaussian@Volume

#Plot reptile hypervolume
#Log transformed reptile hypervolume
plot(completereptiles_gaussian,point.dark.factor=1,color=gg_color_hue(3)[3])
plot(completereptiles_gaussian,show.3d=TRUE,plot.3d.axes.id=2:4,cex.random=3,cex.data=6,
     point.alpha.min=0.5,point.dark.factor=1)
#Scaled log transformed reptile hypervolume
plot(completereptiles_scale_gaussian,point.dark.factor=1,color=gg_color_hue(3)[3])
plot(completereptiles_scale_gaussian,show.3d=TRUE,plot.3d.axes.id=2:4,cex.random=3,cex.data=6,
     point.alpha.min=0.5,point.dark.factor=1)
#Visualize the three reptile orders
#Crocodilia
plot(completereptiles_gaussian,point.dark.factor=1,color=gg_color_hue(3)[3],
     plot.function.additional=function(i,j) {
       points(x=complete_data[complete_data$order=="Crocodilia",i],y=complete_data[complete_data$order=="Crocodilia",j],col="red",pch=19) 
     })
#Squamata
plot(completereptiles_gaussian,point.dark.factor=1,color=gg_color_hue(3)[3],
     plot.function.additional=function(i,j) {
       points(x=complete_data[complete_data$order=="Squamata",i],y=complete_data[complete_data$order=="Squamata",j],col="red",pch=19) 
     })
#Testudines
plot(completereptiles_gaussian,point.dark.factor=1,color=gg_color_hue(3)[3],
     plot.function.additional=function(i,j) {
       points(x=complete_data[complete_data$order=="Testudines",i],y=complete_data[complete_data$order=="Testudines",j],col="red",pch=19) 
     })
#View all three orders
plot(completereptiles_gaussian,point.dark.factor=1,color=gg_color_hue(3)[3],
     plot.function.additional=function(i,j) {
       points(x=complete_data[complete_data$order=="Crocodilia",i],y=complete_data[complete_data$order=="Crocodilia",j],col="red",pch=19)
       points(x=complete_data[complete_data$order=="Squamata",i],y=complete_data[complete_data$order=="Squamata",j],col="purple",pch=19)
       points(x=complete_data[complete_data$order=="Testudines",i],y=complete_data[complete_data$order=="Testudines",j],col="darkgreen",pch=19)
     })



#Plotting all three hypervolumes together
#Log transformed hypervolumes
plot(hypervolume_join(completebirds_gaussian,completemammals_gaussian,completereptiles_gaussian),
     colors = c(gg_color_hue(3)[1],gg_color_hue(3)[2],gg_color_hue(3)[3]))
plot(hypervolume_join(completebirds_gaussian,completemammals_gaussian,completereptiles_gaussian),
     show.3d=TRUE,plot.3d.axes.id=2:4,
     colors = c(gg_color_hue(3)[1],gg_color_hue(3)[2],gg_color_hue(3)[3]),point.alpha.min = 0.5,cex.random=3,cex.data=6)
#Scaled log transformed hypervolumes
plot(hypervolume_join(completebirds_scale_gaussian,completemammals_scale_gaussian,completereptiles_scale_gaussian),
     colors = c(gg_color_hue(3)[1],gg_color_hue(3)[2],gg_color_hue(3)[3]))
plot(hypervolume_join(completebirds_gaussian,completemammals_gaussian,completereptiles_gaussian),
     show.3d=TRUE,plot.3d.axes.id=2:4,
     colors = c(gg_color_hue(3)[1],gg_color_hue(3)[2],gg_color_hue(3)[3]),point.alpha.min = 0.5,cex.random=3,cex.data=6)

#Overlap statistics
#Birds and mammals:
hypervolume_overlap_statistics(hypervolume_set(completebirds_gaussian,completemammals_gaussian,check.memory = FALSE))
#Birds and reptiles:
hypervolume_overlap_statistics(hypervolume_set(completebirds_gaussian,completereptiles_gaussian,check.memory = FALSE))
#Mammals and reptiles:
hypervolume_overlap_statistics(hypervolume_set(completemammals_gaussian,completereptiles_gaussian,check.memory = FALSE))




#Bat hypervolumes
completebats_gaussian<-hypervolume_gaussian(data = completecase_species[completecase_species$order=="Chiroptera",12:15],
                                                name = "completebats_gaussian")
completebats_gaussian@Volume

plot(completebats_gaussian,point.dark.factor=1,color=gg_color_hue(4)[4])
#Plot bats with birds and mammals
plot(hypervolume_join(completebirds_gaussian,completemammals_gaussian,completebats_gaussian),
     colors = c(gg_color_hue(3)[1],gg_color_hue(3)[2],gg_color_hue(4)[4]))

complete_data<-completecase_species[,c(12:15,1:5)]
complete_data<-as.data.frame(complete_data)
#Add bat points to mammal hypervolume
plot(completemammals_gaussian,colors=gg_color_hue(3)[2],plot.function.additional=function(i,j) {
     points(x=complete_data[complete_data$order=="Chiroptera",i],y=complete_data[complete_data$order=="Chiroptera",j],col="red",pch=19) 
     })
plot(hypervolume_join(completebirds_gaussian,completemammals_gaussian),colors=c(gg_color_hue(3)[1],gg_color_hue(3)[2]),plot.function.additional=function(i,j) {
  points(x=complete_data[complete_data$order=="Chiroptera",i],y=complete_data[complete_data$order=="Chiroptera",j],col="darkviolet",pch=19) 
})

#Add primate points
plot(completemammals_gaussian,colors=gg_color_hue(3)[2],plot.function.additional=function(i,j) {
  points(x=complete_data[complete_data$order=="Primates",i],y=complete_data[complete_data$order=="Primates",j],col="red",pch=19) 
})
#Add cetacean points
plot(completemammals_gaussian,colors=gg_color_hue(3)[2],plot.function.additional=function(i,j) {
  points(x=complete_data[complete_data$order=="Cetacea",i],y=complete_data[complete_data$order=="Cetacea",j],col="red",pch=19) 
})
#Add marsupial points
plot(completemammals_gaussian,colors=gg_color_hue(3)[2],plot.function.additional=function(i,j) {
  points(x=complete_data[complete_data$order=="Dasyuromorphia" | complete_data$order=="Didelphimorphia" | complete_data$order=="Peramelemorphia" | complete_data$order=="Diprotodontia",i],y=complete_data[complete_data$order=="Dasyuromorphia" | complete_data$order=="Didelphimorphia" | complete_data$order=="Peramelemorphia" | complete_data$order=="Diprotodontia",j],col="red",pch=19) 
})





#Bat overlaps
#Birds and bats:
hypervolume_overlap_statistics(hypervolume_set(completebirds_gaussian,completebats_gaussian,check.memory = FALSE))
#Mammals and bats:
hypervolume_overlap_statistics(hypervolume_set(completemammals_gaussian,completebats_gaussian,check.memory = FALSE))


#Pair plots of mammals split up by orders
cols<-character(nrow(completecase_species))
cols[]<-"black"

for(i in 1:length(unique(completecase_species$order[completecase_species$class=="Mammalia"]))){
  cols[completecase_species$order==unique(completecase_species$order[completecase_species$class=="Mammalia"])[i]]<-rainbow(length(unique(completecase_species$order[completecase_species$class=="Mammalia"])))
}


pairs(completecase_species[completecase_species$class=="Mammalia",12:15],lower.panel = NULL,col=cols)

splom(~completecase_species[completecase_species$class=="Mammalia",12:15] | order, completecase_species[completecase_species$class=="Mammalia",12:15])


#Estimating A for mammals as in Charnov book table 5.2
A_mammals<-Amniote_Database_Aug_2015[which(!is.na(Amniote_Database_Aug_2015$female_maturity_d) & !is.na(Amniote_Database_Aug_2015$adult_body_mass_g) & Amniote_Database_Aug_2015$class=="Mammalia"),]
plot(log(A_mammals$female_maturity_d)~log(A_mammals$adult_body_mass_g),ylab="Ln(Female Maturity)",xlab="Ln(Adult Body Mass)")
points(log(A_mammals$female_maturity_d[A_mammals$order=="Primates"])~log(A_mammals$adult_body_mass_g[A_mammals$order=="Primates"]),col="red")
A_model<-lm(log(A_mammals$female_maturity_d)~log(A_mammals$adult_body_mass_g))
summary(A_model)
abline(A_model)
#Remove primates and recalculate alpha
A_mammals_noprimates<-Amniote_Database_Aug_2015[which(!is.na(Amniote_Database_Aug_2015$female_maturity_d) & !is.na(Amniote_Database_Aug_2015$adult_body_mass_g) & !is.na(Amniote_Database_Aug_2015$weaning_d) & Amniote_Database_Aug_2015$class=="Mammalia" & Amniote_Database_Aug_2015$order!="Primates"),]
A_mammals_noprimates$alpha5 <- A_mammals_noprimates$female_maturity_d - as.numeric(A_mammals_noprimates$weaning_d)
plot(log(A_mammals_noprimates$alpha5)~log(A_mammals_noprimates$adult_body_mass_g),ylab="Ln(Alpha)",xlab="Ln(Adult Body Mass)")
A_model_noprimates<-lm(log(A_mammals_noprimates$alpha5)~log(A_mammals_noprimates$adult_body_mass_g))
summary(A_model_noprimates)
abline(A_model_noprimates)
#just primates
plot(log(A_mammals$female_maturity_d[A_mammals$order=="Primates"])~log(A_mammals$adult_body_mass_g[A_mammals$order=="Primates"]),ylab="Ln(Female Maturity)",xlab="Ln(Adult Body Mass)")
A_model_primates<-lm(log(A_mammals$female_maturity_d[A_mammals$order=="Primates"])~log(A_mammals$adult_body_mass_g[A_mammals$order=="Primates"]))
summary(A_model_primates)
abline(A_model)




#Phylogenetic analyses

#Mammals
mammaltrees<-read.nexus("fritztree2009.txt")
#Pick just the tree with the best date estimate
mammaltree_best<-mammaltrees$mammalST_MSW05_bestDates
#want to prune to just the mammals with trait data
#named vector including all the mammal species with complete trait data
bmvec_mammal<-completecase_species$adult_body_mass_g[completecase_species$class=="Mammalia"]
names(bmvec_mammal)<-completecase_species$taxaname[completecase_species$class=="Mammalia"]
pruned_mammaltree_best<-prune.missing(x=bmvec_mammal, phylo=mammaltree_best)
pruned_mammaltree_best<-pruned_mammaltree_best$tree

#Birds
bigbirdtree<-read.newick("C:/Users/Cecina/Desktop/BigBird.All.NewNames.7000Taxa.tre.gz")
birdtree<-bigbirdtree[1]
birdtree<-birdtree[[1]]
birdtree$tip.label<-gsub("^(.*?_.*?_)","",birdtree$tip.label)

#pruning birds
bmvec_bird<-completecase_species$adult_body_mass_g[completecase_species$class=="Aves"]
names(bmvec_bird)<-completecase_species$taxaname[completecase_species$class=="Aves"]
pruned_birdtree<-prune.missing(x=bmvec_bird, phylo=birdtree)
pruned_birdtree<-pruned_birdtree$tree


#Reptiles



#adding traits to mammal tree
#Body mass
mammal_log_bodymass<-completecase_species$log_bodymass[completecase_species$class=="Mammalia"]
names(mammal_log_bodymass)<-completecase_species$taxaname[completecase_species$class=="Mammalia"]
mammal_log_bodymass_tiporder<-mammal_log_bodymass[pruned_mammaltree_best$tip.label]

plot(pruned_mammaltree_best,no.margin = TRUE,type="fan",show.tip.label = FALSE)
tiplabels(pch=19,col=color.scale(mammal_log_bodymass_tiporder,extremes=c("blue","red")))
color.legend(-255,-125,-155,-115,legend=c(0.85,18.82),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")


#C*E
mammal_log_C_E<-completecase_species$log_C_E[completecase_species$class=="Mammalia"]
names(mammal_log_C_E)<-completecase_species$taxaname[completecase_species$class=="Mammalia"]
mammal_log_C_E_tiporder<-mammal_log_C_E[pruned_mammaltree_best$tip.label]

plot(pruned_mammaltree_best,no.margin = TRUE,type="fan",show.tip.label = FALSE)
tiplabels(pch=19,col=color.scale(mammal_log_C_E_tiporder,extremes=c("blue","red"),xrange=c(-2.760842,5.378637)))
color.legend(-255,-125,-155,-115,legend=c(-2.76,5.38),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

#I/m
mammal_log_I_m<-completecase_species$log_I_m[completecase_species$class=="Mammalia"]
names(mammal_log_I_m)<-completecase_species$taxaname[completecase_species$class=="Mammalia"]
mammal_log_I_m_tiporder<-mammal_log_I_m[pruned_mammaltree_best$tip.label]

plot(pruned_mammaltree_best,no.margin = TRUE,type="fan",show.tip.label = FALSE)
tiplabels(pch=19,col=color.scale(mammal_log_I_m_tiporder,extremes=c("blue","red")))
color.legend(-255,-125,-155,-115,legend=c(-3.77,2.59),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

#E/alpha
mammal_log_E_alpha<-completecase_species$log_E_alpha[completecase_species$class=="Mammalia"]
names(mammal_log_E_alpha)<-completecase_species$taxaname[completecase_species$class=="Mammalia"]
mammal_log_E_alpha_tiporder<-mammal_log_E_alpha[pruned_mammaltree_best$tip.label]

plot(pruned_mammaltree_best,no.margin = TRUE,type="fan",show.tip.label = FALSE)
tiplabels(pch=19,col=color.scale(mammal_log_E_alpha_tiporder,extremes=c("blue","red")))
color.legend(-255,-125,-155,-115,legend=c(-1.80,4.20),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

#make mammal tree dichotomous
pruned_mammaltree_di<-multi2di(pruned_mammaltree_best,random=FALSE)

#Create Brownian motion, OU, etc. models
#For body mass
mammal_bodymass_fit.ou<-fitContinuous(pruned_mammaltree_di,mammal_log_bodymass_tiporder,model="OU")
mammal_bodymass_fit.ou
mammal_bodymass_fit.bm<-fitContinuous(pruned_mammaltree_di,mammal_log_bodymass_tiporder,model="BM")
mammal_bodymass_fit.bm
mammal_bodymass_fit.lambda<-fitContinuous(pruned_mammaltree_di,mammal_log_bodymass_tiporder,model="lambda")
mammal_bodymass_fit.lambda
mammal_bodymass_fit.white<-fitContinuous(pruned_mammaltree_di,mammal_log_bodymass_tiporder,model="white")
mammal_bodymass_fit.white
#For C*E
mammal_C_E_fit.ou<-fitContinuous(pruned_mammaltree_di,mammal_log_C_E_tiporder,model="OU")
mammal_C_E_fit.ou
mammal_C_E_fit.bm<-fitContinuous(pruned_mammaltree_di,mammal_log_C_E_tiporder,model="BM")
mammal_C_E_fit.bm
mammal_C_E_fit.lambda<-fitContinuous(pruned_mammaltree_di,mammal_log_C_E_tiporder,model="lambda")
mammal_C_E_fit.lambda
mammal_C_E_fit.white<-fitContinuous(pruned_mammaltree_di,mammal_log_C_E_tiporder,model="white")
mammal_C_E_fit.white
#For I/m
mammal_I_m_fit.ou<-fitContinuous(pruned_mammaltree_di,mammal_log_I_m_tiporder,model="OU")
mammal_I_m_fit.ou
mammal_I_m_fit.bm<-fitContinuous(pruned_mammaltree_di,mammal_log_I_m_tiporder,model="BM")
mammal_I_m_fit.bm
mammal_I_m_fit.lambda<-fitContinuous(pruned_mammaltree_di,mammal_log_I_m_tiporder,model="lambda")
mammal_I_m_fit.lambda
mammal_I_m_fit.white<-fitContinuous(pruned_mammaltree_di,mammal_log_I_m_tiporder,model="white")
mammal_I_m_fit.white
#For E/alpha
mammal_E_alpha_fit.ou<-fitContinuous(pruned_mammaltree_di,mammal_log_E_alpha_tiporder,model="OU")
mammal_E_alpha_fit.ou
mammal_E_alpha_fit.bm<-fitContinuous(pruned_mammaltree_di,mammal_log_E_alpha_tiporder,model="BM")
mammal_E_alpha_fit.bm
mammal_E_alpha_fit.lambda<-fitContinuous(pruned_mammaltree_di,mammal_log_E_alpha_tiporder,model="lambda")
mammal_E_alpha_fit.lambda
mammal_E_alpha_fit.white<-fitContinuous(pruned_mammaltree_di,mammal_log_E_alpha_tiporder,model="white")
mammal_E_alpha_fit.white

#Body mass
mammal_bodymass_lam_tree<-rescale(pruned_mammaltree_best,model="lambda", mammal_bodymass_fit.lambda$opt$lambda)
mammal_bodymass_lam_fastAnc<-fastAnc(mammal_bodymass_lam_tree, mammal_log_bodymass_tiporder)

#C*E
mammal_C_E_lam_tree<-rescale(pruned_mammaltree_best,model="lambda", mammal_C_E_fit.lambda$opt$lambda)
mammal_C_E_lam_fastAnc<-fastAnc(mammal_C_E_lam_tree, mammal_log_C_E_tiporder)

#E/alpha
mammal_E_alpha_ou_tree<-rescale(pruned_mammaltree_best,model="OU", mammal_E_alpha_fit.ou$opt$alpha)
mammal_E_alpha_ou_fastAnc<-fastAnc(mammal_E_alpha_ou_tree, mammal_log_E_alpha_tiporder)

#I/m
mammal_I_m_lam_tree<-rescale(pruned_mammaltree_best,model="lambda", mammal_I_m_fit.lambda$opt$lambda)
mammal_I_m_lam_fastAnc<-fastAnc(mammal_I_m_lam_tree, mammal_log_I_m_tiporder)




#Playing around with PGLS
C_E.I_m_pglsmodel<-gls(log_C_E~log_I_m,correlation = corBrownian(phy=pruned_mammaltree_di),data = as.data.frame(mammaltraitmatrix),method = "ML")


#tree with mapped continuous character
mammalcont_C_E<-contMap(pruned_mammaltree_best,mammal_log_C_E_tiporder,plot = FALSE)
plot(mammalcont_C_E,type="fan",no.margin=TRUE,show.tip.label=FALSE)

#phylomorphospace
mammaltraits<-completecase_species[completecase_species$taxaname%in%pruned_mammaltree_best$tip.label,c(20,12:15)]
mammaltraitmatrix<-as.matrix(mammaltraits[,2:5])
mammaltraitmatrix<-mammaltraitmatrix[pruned_mammaltree_best$tip.label,]
rownames(mammaltraitmatrix)<-mammaltraits$taxaname
phylomorphospace(pruned_mammaltree_best,mammaltraitmatrix[,c(2,4)])

#Adding traits to bird tree
#Body mass
bird_log_bodymass<-completecase_species$log_bodymass[completecase_species$class=="Aves"]
names(bird_log_bodymass)<-completecase_species$taxaname[completecase_species$class=="Aves"]
bird_log_bodymass_tiporder<-bird_log_bodymass[pruned_birdtree$tip.label]

plot(pruned_birdtree,no.margin = TRUE,show.tip.label = FALSE)
tiplabels(pch=19,col=color.scale(bird_log_bodymass_tiporder,extremes=c("blue","red")))
color.legend(0,100,0.05,105,legend=c(2.65,6.53),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

#C*E
bird_log_C_E<-completecase_species$log_C_E[completecase_species$class=="Aves"]
names(bird_log_C_E)<-completecase_species$taxaname[completecase_species$class=="Aves"]
bird_log_C_E_tiporder<-bird_log_C_E[pruned_birdtree$tip.label]

plot(pruned_birdtree,no.margin = TRUE,show.tip.label = FALSE)
tiplabels(pch=19,col=color.scale(bird_log_C_E_tiporder,extremes=c("blue","red")))
color.legend(0,100,0.05,105,legend=c(2.65,6.53),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

#I/m
bird_log_I_m<-completecase_species$log_I_m[completecase_species$class=="Aves"]
names(bird_log_I_m)<-completecase_species$taxaname[completecase_species$class=="Aves"]
bird_log_I_m_tiporder<-bird_log_I_m[pruned_birdtree$tip.label]

plot(pruned_birdtree,no.margin = TRUE,show.tip.label = FALSE)
tiplabels(pch=19,col=color.scale(bird_log_I_m_tiporder,extremes=c("blue","red")))
color.legend(5,5,40,10,legend=c(-1.94,3.50),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

#E/alpha
bird_log_E_alpha<-completecase_species$log_E_alpha[completecase_species$class=="Aves"]
names(bird_log_E_alpha)<-completecase_species$taxaname[completecase_species$class=="Aves"]
bird_log_E_alpha_tiporder<-bird_log_E_alpha[pruned_birdtree$tip.label]

plot(pruned_birdtree,no.margin = TRUE,show.tip.label = FALSE)
tiplabels(pch=19,col=color.scale(bird_log_E_alpha_tiporder,extremes=c("blue","red")))
color.legend(5,5,40,10,legend=c(0.54,3.57),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

#heat map of all traits
birdtraits<-completecase_species[completecase_species$taxaname%in%pruned_birdtree$tip.label,c(20,12:15)]
birdtraitmatrix<-as.matrix(birdtraits[,2:5])
rownames(birdtraitmatrix)<-birdtraits$taxaname
phylo.heatmap(pruned_birdtree,birdtraitmatrix,fsize=c(0.5,0.7,1),standardize = TRUE)

#Create Brownian motion, OU, etc. models
#For body mass
bird_bodymass_fit.ou<-fitContinuous(pruned_birdtree,bird_log_bodymass_tiporder,model="OU")
bird_bodymass_fit.ou
bird_bodymass_fit.bm<-fitContinuous(pruned_birdtree,bird_log_bodymass_tiporder,model="BM")
bird_bodymass_fit.bm
bird_bodymass_fit.lambda<-fitContinuous(pruned_birdtree,bird_log_bodymass_tiporder,model="lambda")
bird_bodymass_fit.lambda
bird_bodymass_fit.white<-fitContinuous(pruned_birdtree,bird_log_bodymass_tiporder,model="white")
bird_bodymass_fit.white
#For C*E
bird_C_E_fit.ou<-fitContinuous(pruned_birdtree_di,bird_log_C_E_tiporder,model="OU")
bird_C_E_fit.ou
bird_C_E_fit.bm<-fitContinuous(pruned_birdtree_di,bird_log_C_E_tiporder,model="BM")
bird_C_E_fit.bm
bird_C_E_fit.lambda<-fitContinuous(pruned_birdtree_di,bird_log_C_E_tiporder,model="lambda")
bird_C_E_fit.lambda
bird_C_E_fit.white<-fitContinuous(pruned_birdtree_di,bird_log_C_E_tiporder,model="white")
bird_C_E_fit.white
#For I/m
bird_I_m_fit.ou<-fitContinuous(pruned_birdtree_di,bird_log_I_m_tiporder,model="OU")
bird_I_m_fit.ou
bird_I_m_fit.bm<-fitContinuous(pruned_birdtree_di,bird_log_I_m_tiporder,model="BM")
bird_I_m_fit.bm
bird_I_m_fit.lambda<-fitContinuous(pruned_birdtree_di,bird_log_I_m_tiporder,model="lambda")
bird_I_m_fit.lambda
bird_I_m_fit.white<-fitContinuous(pruned_birdtree_di,bird_log_I_m_tiporder,model="white")
bird_I_m_fit.white
#For E/alpha
bird_E_alpha_fit.ou<-fitContinuous(pruned_birdtree_di,bird_log_E_alpha_tiporder,model="OU")
bird_E_alpha_fit.ou
bird_E_alpha_fit.bm<-fitContinuous(pruned_birdtree_di,bird_log_E_alpha_tiporder,model="BM")
bird_E_alpha_fit.bm
bird_E_alpha_fit.lambda<-fitContinuous(pruned_birdtree_di,bird_log_E_alpha_tiporder,model="lambda")
bird_E_alpha_fit.lambda
bird_E_alpha_fit.white<-fitContinuous(pruned_birdtree_di,bird_log_E_alpha_tiporder,model="white")
bird_E_alpha_fit.white





#Add trophic level for mammals
MammalDIET_v1_0$taxaname<-paste(MammalDIET_v1_0$Genus,MammalDIET_v1_0$Species,sep="_")
completemammal_trophic<-merge(completecase_species,MammalDIET_v1_0[,c("taxaname","TrophicLevel")],by="taxaname")

#carnivore hypervolume
mammalcarnivore_gaussian<-hypervolume_gaussian(data = completemammal_trophic[completemammal_trophic$TrophicLevel=="Carnivore",13:16],
                                               name = "mammalcarnivore_gaussian")
#omnivore hypervolume
mammalomnivore_gaussian<-hypervolume_gaussian(data = completemammal_trophic[completemammal_trophic$TrophicLevel=="Omnivore",13:16],
                                               name = "mammalomnivore_gaussian")
#herbivore hypervolume
mammalherbivore_gaussian<-hypervolume_gaussian(data = completemammal_trophic[completemammal_trophic$TrophicLevel=="Herbivore",13:16],
                                              name = "mammalherbivore_gaussian")

plot(hypervolume_join(mammalcarnivore_gaussian,mammalomnivore_gaussian,mammalherbivore_gaussian))

#trophic overlap
#carnivore and herbivore
hypervolume_overlap_statistics(hypervolume_set(mammalcarnivore_gaussian,mammalherbivore_gaussian,check.memory=FALSE))
#carnivore and omnivore
hypervolume_overlap_statistics(hypervolume_set(mammalcarnivore_gaussian,mammalomnivore_gaussian,check.memory=FALSE))
#herbivore and omnivore
hypervolume_overlap_statistics(hypervolume_set(mammalherbivore_gaussian,mammalomnivore_gaussian,check.memory=FALSE))

#trophic hypervolume volumes
mammalcarnivore_gaussian@Volume
mammalherbivore_gaussian@Volume
mammalomnivore_gaussian@Volume

#Intersection of carnivores and herbivores
carnherb_set<-hypervolume_set(mammalcarnivore_gaussian,mammalherbivore_gaussian,check.memory = FALSE)
carnherb_int<-carnherb_set@HVList$Intersection
#overlap of omnivores with this intersection
hypervolume_overlap_statistics(hypervolume_set(mammalomnivore_gaussian,carnherb_int,check.memory = FALSE))


#GIS code

#save an object containing the coordinate reference system of wgs84
P4S.latlon <- CRS("+proj=longlat +datum=WGS84")  
#This CRS needs to be supplied when loading in a shapefile(or else added later).  If you don't supply this you can't transform to a new crs
#Note that some shapefiles contain added metadata containing the CRS.

#Import shape polygons from IUCN range maps
mammal_polys<-readShapePoly("C:/Users/Cecina/Desktop/HypervolumeFiles/TERRESTRIAL_MAMMALS/TERRESTRIAL_MAMMALS.shp",proj4string = P4S.latlon)
#Taxonomic information as well as information on whether a shapefile corresponds to native vs invader range, and whether the species is there seasonally vs constantly:
mammal_data<-mammal_polys@data

#change to an equal area projection
ea_tf<-CRS("+proj=cea +units=m")
mammals_ea<-spTransform(x = mammal_polys,CRSobj = ea_tf)

#create a raster: specify extent and resolution (size of grid cells) in meters
r<-raster(ext= extent(mammals_ea),resolution=100000)

