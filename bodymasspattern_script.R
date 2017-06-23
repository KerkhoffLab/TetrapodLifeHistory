library(readr)
library(lattice)
library(RColorBrewer)
library(ggplot2)
library(hypervolume)
library(tidyr)

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

#Great

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

#Add bat points to mammal hypervolume
plot(completemammals_gaussian,colors=gg_color_hue(3)[2])
plot(hypervolume_join(completemammals_gaussian,completebats_gaussian))

completebatdata<-completecase_species[completecase_species$order=="Chiroptera",12:15]
op = par(no.readonly = T)

par(mfrow=c(3, 3))
par(mar=c(0,0,0,0))
par(oma=c(0.5,0.5,0.5,0.5))
for(i in 1:2){
  for(j in 1:2){
    if (j > i){
      points(completebatdata[,j],completebatdata[,i])
    }
  }
}



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


##The following code is from hypervolume_code.R

#GIS code

#save an object containing the coordinate reference system of wgs84
P4S.latlon <- CRS("+proj=longlat +datum=WGS84")  
#This CRS needs to be supplied when loading in a shapefile(or else added later).  If you don't supply this you can't transform to a new crs
#Note that some shapefiles contain added metadata containing the CRS.

#Import shape polygons from IUCN range maps
mammals<-readShapePoly("C:/Users/Cecina/Desktop/TERRESTRIAL_MAMMALS/TERRESTRIAL_MAMMALS.shp",proj4string = P4S.latlon)
#Taxonomic information as well as information on whether a shapefile corresponds to native vs invader range, and whether the species is there seasonally vs constantly:
mammal_data<-mammals@data

#change to an equal area projection
ea_tf<-CRS("+proj=cea +units=m")
mammals_ea<-spTransform(x = mammals,CRSobj = ea_tf)

#create a raster: specify extent and resolution (size of grid cells) in meters
r<-raster(ext= extent(mammals_ea),resolution=100000)

#create columns for traits (hopefully) not correlated with body mass (invariants)
#b: number of daughters per year = (litter size x litters/yr)/2
amniote_mammals$b<-(amniote_mammals$litter_or_clutch_size_n*amniote_mammals$litters_or_clutches_per_y)/2
#alpha_b: female maturity x b
amniote_mammals$alpha_b<-amniote_mammals$female_maturity_d*amniote_mammals$b
#E_alpha: longevity/female maturity
amniote_mammals$E_alpha<-amniote_mammals$longevity_y/amniote_mammals$female_maturity_d
#S: 1/(longevity*b)
amniote_mammals$S<-1/(amniote_mammals$longevity_y*amniote_mammals$b)
#unfortunately S is equal to 1/(E_alpha*alpha_b) so you can't use all three of these


#Create list of mammal species with complete cases for top -- trait values

#number of species with values for the top 10 traits=45
sum(complete.cases(amniote_mammals[,c(8:14,22,29,35)]))
#top 9 traits=1164 (longevity_y instead of maximum_longevity_y)
sum(complete.cases(amniote_mammals[,c(8:11,13:15,20,29)]))
#top 8 traits=1297
sum(complete.cases(amniote_mammals[,c(8:11,13,14,20,29)]))
#top 7 traits=1375
sum(complete.cases(amniote_mammals[,c(9:14,29)]))
#top 4 traits=2192
sum(complete.cases(amniote_mammals[,c(11,29,9,20)]))
#body mass, litter size, longevity, gestation, and litters per year
sum(complete.cases(amniote_mammals[,c(11,9,20,13,10)]))
#body mass and invariant traits=1548
sum(complete.cases(amniote_mammals[,c(11,38:40)]))


#list of mammal species in database with values for body mass, alpha_b, and E_alpha
amniote_mammals_complete<-amniote_mammals[complete.cases(amniote_mammals[,c(11,38,39)]),]
#create new genusspecies column to match with IUCN data
amniote_mammals_complete<-unite(amniote_mammals_complete, genusspecies, genus, species, sep=" ", remove=FALSE)
#contains 1548 species with all 4 trait values


