# Libraries ---------------------------------------------------------------
library(readr)
library(plyr)
library(dplyr)
library(lattice)
library(RColorBrewer)
library(ggplot2)
library(hypervolume)
library(tidyr)
library(taxize)
library(brranching)
library(picante)
library(maptools)
library(mapproj)
library(rgeos)
library(sp)
library(raster)
library(phytools)
library(geiger)
library(plotrix)
library(mosaic)
library(PhyloOrchard)
library(phangorn)
library(magick)


# Amphibian Data ----------------------------------------------------------

#Import AmphiBIO database
#Downloaded from https://doi.org/10.6084/m9.figshare.4644424
AmphiBIO_v1 <- read.csv("C:/Users/cecin/Desktop/AmphiBIO_v1/AmphiBIO_v1.csv")
AmphiBIO_v1$class<-"Amphibia"
# AmphiBIO_v1$species<-NA
# AmphiBIO_v1$subspecies<-NA
# AmphiBIO_v1$common_name<-NA
colnames(AmphiBIO_v1)[2]<-"order"
colnames(AmphiBIO_v1)[3]<-"family"
colnames(AmphiBIO_v1)[4]<-"genus"
AmphiBIO_v1$taxaname<-gsub(" ","_",AmphiBIO_v1$Species)

#Calculate average clutch size
AmphiBIO_v1$Litter_size_avg_n<-(AmphiBIO_v1$Litter_size_max_n+AmphiBIO_v1$Litter_size_min_n)/2
#Calculate average age at maturity
AmphiBIO_v1$Age_at_maturity_avg_y<-(AmphiBIO_v1$Age_at_maturity_max_y+AmphiBIO_v1$Age_at_maturity_min_y)/2

#Use allometry equations to calculate mass at independence
AmphiBIO_v1$Offspring_size_min_g<-NA
AmphiBIO_v1$Offspring_size_max_g<-NA
#For Anura
AmphiBIO_v1$Offspring_size_min_g[AmphiBIO_v1$order=="Anura"]<-10^-4.324*(AmphiBIO_v1$Offspring_size_min_mm[AmphiBIO_v1$order=="Anura"])^3.189
AmphiBIO_v1$Offspring_size_max_g[AmphiBIO_v1$order=="Anura"]<-10^-4.324*(AmphiBIO_v1$Offspring_size_max_mm[AmphiBIO_v1$order=="Anura"])^3.189
#For Caudata
AmphiBIO_v1$Offspring_size_min_g[AmphiBIO_v1$order=="Caudata"]<-10^-3.98*(AmphiBIO_v1$Offspring_size_min_mm[AmphiBIO_v1$order=="Caudata"])^2.644
AmphiBIO_v1$Offspring_size_max_g[AmphiBIO_v1$order=="Caudata"]<-10^-3.98*(AmphiBIO_v1$Offspring_size_max_mm[AmphiBIO_v1$order=="Caudata"])^2.644

#Calculate average mass at independence
AmphiBIO_v1$Offspring_size_avg_g<-(AmphiBIO_v1$Offspring_size_min_g+AmphiBIO_v1$Offspring_size_max_g)/2

#Create columns for invariant traits

#R=average reproductive allocation per unit time
#For amphibians:
##R=Litter_size_avg_n*Reproductive_output_y*Offspring_size_avg_g
AmphiBIO_v1$R<-AmphiBIO_v1$Litter_size_avg_n*AmphiBIO_v1$Reproductive_output_y*AmphiBIO_v1$Offspring_size_avg_g

#C=reproductive effort
#C=R/m
AmphiBIO_v1$C<-AmphiBIO_v1$R/AmphiBIO_v1$Body_mass_g

#Calculate C*E
#E=max longevity-avg. age at maturity
AmphiBIO_v1$C_E<-AmphiBIO_v1$C*(AmphiBIO_v1$Longevity_max_y-AmphiBIO_v1$Age_at_maturity_avg_y)

#Calculate E/alpha
AmphiBIO_v1$E_alpha<-(AmphiBIO_v1$Longevity_max_y-AmphiBIO_v1$Age_at_maturity_avg_y)/AmphiBIO_v1$Age_at_maturity_avg_y

#Calculate I/m
AmphiBIO_v1$I_m<-AmphiBIO_v1$Offspring_size_avg_g/AmphiBIO_v1$Body_mass_g

# Amniote Data ------------------------------------------------------------

#Import amniote database
#Downloaded from http://esapubs.org/archive/ecol/E096/269/#data
#Metadata available at http://esapubs.org/archive/ecol/E096/269/metadata.php
#(Replaced -999 with NA in the .csv document itself prior to importing)
Amniote_Database_Aug_2015 <- read_csv("C:/Users/cecin/Desktop/Amniote/Amniote_Database_Aug_2015.csv")
Amniote_Database_Aug_2015$weaning_weight_g<-as.numeric(Amniote_Database_Aug_2015$weaning_weight_g)

#Create columns for invariant traits

#R=average reproductive allocation per unit time
#For mammals:
##R=litter_or_clutch_size_n*litters_or_clutches_y*weaning_weight_g
#For birds:
##R=litter_or_clutch_size_n*litters_or_clutches_y*fledging_mass_g
#For reptiles:
##R=litter_or_clutch_size_n*litters_or_clutches_y*egg_mass_g

for (i in 1:nrow(Amniote_Database_Aug_2015)) {
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

#C=reproductive effort
#C=R/m
Amniote_Database_Aug_2015$C<-Amniote_Database_Aug_2015$R/Amniote_Database_Aug_2015$adult_body_mass_g


#Calculate C*E
#using E=maximum longevity
Amniote_Database_Aug_2015$C_E<-Amniote_Database_Aug_2015$C*(Amniote_Database_Aug_2015$maximum_longevity_y-(Amniote_Database_Aug_2015$female_maturity_d/365))

#Calculate E/alpha
Amniote_Database_Aug_2015$E_alpha<-(Amniote_Database_Aug_2015$maximum_longevity_y*365-Amniote_Database_Aug_2015$female_maturity_d)/Amniote_Database_Aug_2015$female_maturity_d


#Calculate I/m

#I=size of offspring at independence
#For mammals:
##I=weaning_weight_g
#For birds:
##I=fledging_mass_g
#For reptiles:
##I=birth_or_hatching_weight_g

for (i in 1:nrow(Amniote_Database_Aug_2015)) {
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

#Add taxaname column
Amniote_Database_Aug_2015$taxaname<-paste(Amniote_Database_Aug_2015$genus,Amniote_Database_Aug_2015$species,sep="_")

Amniote_Database_Aug_2015$I_m<-Amniote_Database_Aug_2015$I/Amniote_Database_Aug_2015$adult_body_mass_g


# Allen et al. Reptile Data -----------------------------------------------

#Import reptile data from Allen et al. 2017
#Downloaded from http://datadryad.org/resource/doi:10.5061/dryad.2d7b0
Allen_etal_reptiledata <- read_csv("C:/Users/cecin/OneDrive/Documents/Kenyon College/Kerkhoff Lab/Summer Science 2017/bodymasspatterns/data/Allen_etal_reptiledata.csv")

#Data cleaning
#Crocodylus_johnsoni
Allen_etal_reptiledata$Species[Allen_etal_reptiledata$Species=="Crocodylus_johnstoni"]<-"Crocodylus_johnsoni"
#Chilabothrus_fordii
Amniote_Database_Aug_2015$species[Amniote_Database_Aug_2015$taxaname=="Chilabothrus_fordi"]<-"fordii"
Amniote_Database_Aug_2015$taxaname[Amniote_Database_Aug_2015$taxaname=="Chilabothrus_fordi"]<-"Chilabothrus_fordii"
#Erythrolamprus spp.
Allen_etal_reptiledata$Species[Allen_etal_reptiledata$Species=="Erythrolamprus_bizonus"]<-"Erythrolamprus_bizona"
Allen_etal_reptiledata$Species[Allen_etal_reptiledata$Species=="Erythrolampus_almadensis"]<-"Erythrolamprus_almadensis"
Allen_etal_reptiledata$Species[Allen_etal_reptiledata$Species=="Erythrolampus_maryellenae"]<-"Erythrolamprus_maryellenae"
Allen_etal_reptiledata$Species[Allen_etal_reptiledata$Species=="Erythrolampus_mossoroensis"]<-"Erythrolamprus_mossoroensis"
Allen_etal_reptiledata$Species[Allen_etal_reptiledata$Species=="Erythrolampus_poecilogyrus"]<-"Erythrolamprus_poecilogyrus"
Allen_etal_reptiledata$Species[Allen_etal_reptiledata$Species=="Erythrolampus_reginae"]<-"Erythrolamprus_reginae"
Allen_etal_reptiledata$Species[Allen_etal_reptiledata$Species=="Erythrolampus_typhlus"]<-"Erythrolamprus_typhlus"
Allen_etal_reptiledata$Species[Allen_etal_reptiledata$Species=="Erythrolampus_viridis"]<-"Erythrolamprus_viridis"
#Philodryas_argentea
Amniote_Database_Aug_2015$species[Amniote_Database_Aug_2015$taxaname=="Philodryas_argenteus"]<-"argentea"
Amniote_Database_Aug_2015$taxaname[Amniote_Database_Aug_2015$taxaname=="Philodryas_argenteus"]<-"Philodryas_argentea"
#Tomodon_dorsatus
Allen_etal_reptiledata$Species[Allen_etal_reptiledata$Species=="Tomodon_dorsatum"]<-"Tomodon_dorsatus"
#Stenodactylus_leptocosymbotes
Allen_etal_reptiledata$Species[Allen_etal_reptiledata$Species=="Stenodactylus_leptocosymbotus"]<-"Stenodactylus_leptocosymbotes"
#Proctoporus_oreades
Allen_etal_reptiledata$Species[Allen_etal_reptiledata$Species=="Euspondylus_oreades"]<-"Proctoporus_oreades"
#Liolaemus_quilmes
Allen_etal_reptiledata$Species[Allen_etal_reptiledata$Species=="Liolaemus_espinozai"]<-"Liolaemus_quilmes"
#Regina_grahamii
Allen_etal_reptiledata$Species[Allen_etal_reptiledata$Species=="Regina_grahami"]<-"Regina_grahamii"
#Phrynosoma_douglasii
Allen_etal_reptiledata$Species[Allen_etal_reptiledata$Species=="Phrynosoma_douglassii"]<-"Phrynosoma_douglasii"
#Anomalopus_verreauxii
Amniote_Database_Aug_2015$species[Amniote_Database_Aug_2015$taxaname=="Anomalopus_verreauxi"]<-"verreauxii"
Amniote_Database_Aug_2015$taxaname[Amniote_Database_Aug_2015$taxaname=="Anomalopus_verreauxi"]<-"Anomalopus_verreauxii"
#Melanops_loveridgei
Allen_etal_reptiledata$Species[Allen_etal_reptiledata$Species=="Melanops_emmrichi"]<-"Melanops_loveridgei"
#Tiliqua_rugosa
Allen_etal_reptiledata$Species[Allen_etal_reptiledata$Species=="Tiliqua_rugosus"]<-"Tiliqua_rugosa"
#Aspidoscelis_flagellicauda
Allen_etal_reptiledata$Species[Allen_etal_reptiledata$Species=="Aspidoscelis_flagellicaudus"]<-"Aspidoscelis_flagellicauda"
#Aspidoscelis_lineattissima
Allen_etal_reptiledata$Species[Allen_etal_reptiledata$Species=="Aspidoscelis_lineattissimus"]<-"Aspidoscelis_lineattissima"
#Vermicella_calonotus
Amniote_Database_Aug_2015$species[Amniote_Database_Aug_2015$taxaname=="Neelaps_calonotus"]<-"calonotus"
Amniote_Database_Aug_2015$taxaname[Amniote_Database_Aug_2015$taxaname=="Neelaps_calonotus"]<-"Vermicella_calonotus"
#Simoselaps_bimaculatus
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Neelaps_bimaculatus"]<-"Simoselaps"
Amniote_Database_Aug_2015$taxaname[Amniote_Database_Aug_2015$taxaname=="Neelaps_bimaculatus"]<-"Simoselaps_bimaculatus"
#Broadleysaurus_major
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Gerrhosaurus_major"]<-"Broadleysaurus"
Amniote_Database_Aug_2015$taxaname[Amniote_Database_Aug_2015$taxaname=="Gerrhosaurus_major"]<-"Broadleysaurus_major"
#Chelonoidis_niger
Allen_etal_reptiledata$Species[Allen_etal_reptiledata$Species=="Chelonoidis_nigra"]<-"Chelonoidis_niger"

#create dataframe of just the reptiles from the amniote database
amniote_reptiledata<-Amniote_Database_Aug_2015[Amniote_Database_Aug_2015$class=="Reptilia",]
#need to convert age at sexual maturity to days so it corresponds to amniote database
Allen_etal_reptiledata$SM<-Allen_etal_reptiledata$SM*365

#combined reptile data
combined_reptiledata<-amniote_reptiledata[,-c(37:41)]
combined_reptiledata<-combined_reptiledata[,-c(38)]

#fill in possible missing trait values for the species in the amniote database
for(i in 1:nrow(combined_reptiledata)){
  species_i<-combined_reptiledata$taxaname[i]
  if(species_i%in%Allen_etal_reptiledata$Species){
    if(is.na(combined_reptiledata$female_maturity_d[i])){
      combined_reptiledata$female_maturity_d[i]<-Allen_etal_reptiledata$SM[Allen_etal_reptiledata$Species==species_i]
    }
    if(is.na(combined_reptiledata$litter_or_clutch_size_n[i])){
      combined_reptiledata$litter_or_clutch_size_n[i]<-Allen_etal_reptiledata$CS[Allen_etal_reptiledata$Species==species_i]
    }
    if(is.na(combined_reptiledata$litters_or_clutches_per_y[i])){
      combined_reptiledata$litters_or_clutches_per_y[i]<-Allen_etal_reptiledata$CY[Allen_etal_reptiledata$Species==species_i]
    }
    if(is.na(combined_reptiledata$adult_body_mass_g[i])){
      combined_reptiledata$adult_body_mass_g[i]<-Allen_etal_reptiledata$BM[Allen_etal_reptiledata$Species==species_i]
    }
    if(is.na(combined_reptiledata$birth_or_hatching_weight_g[i])){
      combined_reptiledata$birth_or_hatching_weight_g[i]<-Allen_etal_reptiledata$HM[Allen_etal_reptiledata$Species==species_i]
    }
    if(is.na(combined_reptiledata$maximum_longevity_y[i])){
      combined_reptiledata$maximum_longevity_y[i]<-Allen_etal_reptiledata$LG[Allen_etal_reptiledata$Species==species_i]
    }
  }
}

#Add the species unique to Allen into the combined_reptiledata
Allen_uniquespecies<-Allen_etal_reptiledata[!Allen_etal_reptiledata$Species%in%amniote_reptiledata$taxaname,]
#standardize columns
Allen_uniquespecies$class<-"Reptilia"
colnames(Allen_uniquespecies)[1:2]<-c("order","family")
Allen_uniquespecies<-separate(Allen_uniquespecies,Species, c("genus", "species"), "_")
Allen_uniquespecies$subspecies<-NA
Allen_uniquespecies$common_name<-NA
colnames(Allen_uniquespecies)[13]<-"female_maturity_d"
colnames(Allen_uniquespecies)[11]<-"litter_or_clutch_size_n"
colnames(Allen_uniquespecies)[12]<-"litters_or_clutches_per_y"
colnames(Allen_uniquespecies)[9]<-"adult_body_mass_g"
colnames(Allen_uniquespecies)[14]<-"maximum_longevity_y"
Allen_uniquespecies$longevity_y<-NA
Allen_uniquespecies$gestation_d<-NA
Allen_uniquespecies$weaning_d<-NA
colnames(Allen_uniquespecies)[10]<-"birth_or_hatching_weight_g"
Allen_uniquespecies$weaning_weight_g<-NA
Allen_uniquespecies$egg_mass_g<-NA
Allen_uniquespecies$incubation_d<-NA
Allen_uniquespecies$fledging_age_d<-NA

Allen_uniquespecies$male_maturity_d<-NA
Allen_uniquespecies$inter_litter_or_interbirth_interval_y<-NA
Allen_uniquespecies$female_body_mass_g<-NA
Allen_uniquespecies$male_body_mass_g<-NA
Allen_uniquespecies$no_sex_body_mass_g<-NA
Allen_uniquespecies$egg_width_mm<-NA
Allen_uniquespecies$egg_length_mm<-NA
Allen_uniquespecies$fledging_mass_g<-NA
Allen_uniquespecies$adult_svl_cm<-NA
Allen_uniquespecies$male_svl_cm<-NA
Allen_uniquespecies$female_svl_cm<-NA
Allen_uniquespecies$birth_or_hatching_svl_cm<-NA
Allen_uniquespecies$female_svl_at_maturity_cm<-NA
Allen_uniquespecies$female_body_mass_at_maturity_g<-NA
Allen_uniquespecies$no_sex_svl_cm<-NA
Allen_uniquespecies$no_sex_svl_cm<-NA
Allen_uniquespecies$no_sex_maturity_d<-NA
Allen_uniquespecies$taxaname<-paste(Allen_uniquespecies$genus,Allen_uniquespecies$species,sep="_")

#reorder columns to match combined_reptiledata
Allen_uniquespecies<-Allen_uniquespecies[colnames(combined_reptiledata)]

#row bind the unique species
combined_reptiledata<-rbind(combined_reptiledata,Allen_uniquespecies)

#Create columns for invariant traits

#R
combined_reptiledata$R<-combined_reptiledata$litter_or_clutch_size_n*combined_reptiledata$litters_or_clutches_per_y*combined_reptiledata$birth_or_hatching_weight_g
#C
combined_reptiledata$C<-combined_reptiledata$R/combined_reptiledata$adult_body_mass_g
#C*E
combined_reptiledata$C_E<-combined_reptiledata$C*(combined_reptiledata$maximum_longevity_y-(combined_reptiledata$female_maturity_d/365))

#E/alpha
combined_reptiledata$E_alpha<-(combined_reptiledata$maximum_longevity_y*365-combined_reptiledata$female_maturity_d)/combined_reptiledata$female_maturity_d

#I
combined_reptiledata$I<-combined_reptiledata$birth_or_hatching_weight_g
#I/m
combined_reptiledata$I_m<-combined_reptiledata$I/combined_reptiledata$adult_body_mass_g

#create augmented Amniote database for use in following calculations
augmented_amniote_database<-Amniote_Database_Aug_2015[Amniote_Database_Aug_2015$class!="Reptilia",]
augmented_amniote_database<-rbind(augmented_amniote_database,combined_reptiledata)

# write.csv(augmented_amniote_database, file = "./data/augmented_amniote_database.csv")
# augmented_amniote_database<-read.csv(file = "./data/augmented_amniote_database.csv")


# Complete Case Data ------------------------------------------------------

#Subset of database including only species for all of the invariants
desiredcolumns<-c(1:8,11,12,38,39,40,41,42,43)
completecase_species<-augmented_amniote_database[complete.cases(augmented_amniote_database$adult_body_mass_g,augmented_amniote_database$C_E,augmented_amniote_database$I_m,augmented_amniote_database$E_alpha),desiredcolumns]
#remove otter
completecase_species<-completecase_species[completecase_species$taxaname!="Enhydra_lutris",]
#remove Acanthis hornemani
completecase_species<-completecase_species[completecase_species$taxaname!="Acanthis_hornemanni",]
#Log transform
completecase_species$log_bodymass<-log(completecase_species$adult_body_mass_g)
completecase_species$log_C_E<-log(completecase_species$C_E)
#removing species with negative values for C*E:
completecase_species<-completecase_species[!is.na(completecase_species$log_C_E),]
#removing species with zero values for C*E:
completecase_species<-completecase_species[!is.infinite(completecase_species$log_C_E),]

completecase_species$log_I_m<-log(completecase_species$I_m)
completecase_species$log_E_alpha<-log(completecase_species$E_alpha)


#complete case amphibians
desiredcolumns_amph<-c(39,2:5,42,23,29,47:49,45,40,50)
completecase_amph<-AmphiBIO_v1[complete.cases(AmphiBIO_v1$Body_mass_g,AmphiBIO_v1$C_E,AmphiBIO_v1$I_m,AmphiBIO_v1$E_alpha),desiredcolumns_amph]
colnames(completecase_amph)[7]<-"adult_body_mass_g"
colnames(completecase_amph)[8]<-"maximum_longevity_y"
colnames(completecase_amph)[12]<-"I"
completecase_amph$female_maturity_d<-completecase_amph$Age_at_maturity_avg_y*365
completecase_amph$log_bodymass<-log(completecase_amph$adult_body_mass_g)
completecase_amph$log_C_E<-log(completecase_amph$C_E)
#removing species with negative values for C*E:
completecase_amph<-completecase_amph[!is.na(completecase_amph$log_C_E),]

completecase_amph$log_I_m<-log(completecase_amph$I_m)
completecase_amph$log_E_alpha<-log(completecase_amph$E_alpha)

#complete cases of amniotes and amphibians
completecase_am<-rbind.fill(completecase_species,completecase_amph)
#Make class a factor
completecase_am$class<-as.factor(completecase_am$class)
#Reorder factor in reverse evolutionary order (birds, mammals, reptiles, amphibians)
completecase_am$class = factor(completecase_am$class,levels(completecase_am$class)[c(2:4,1)])

# write.csv(completecase_am, file = "./data/completecase_am.csv")
# completecase_am<-read.csv(file = "./data/completecase_am.csv")


# Histograms of Traits ----------------------------------------------------

#Body mass
ggplot(data=completecase_am,aes(x=log(adult_body_mass_g),colour=class, ..density..,))+
  geom_freqpoly(binwidth=0.5,lwd=1.3)+
  scale_color_brewer(name="Class",palette="Set1")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.text=element_text(size=18), legend.title = element_text(size=18))+
  labs(x = "Log(Body Mass (g) )", y="Density")

ggplot(data=completecase_am,aes(x=log(adult_body_mass_g),colour=class, ..density..))+
  geom_boxplot(aes(y=log(adult_body_mass_g),x=class))+
  scale_color_brewer(name="Class",palette="Set1")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.text=element_text(size=18), legend.title = element_text(size=18))+
  labs(y = "Log(Body Mass (g)", x = "")

#C*E
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


#Make histograms of body mass and three invariants for all 4 classes:
#body mass
ggplot(data=completecase_am,aes(x=log(adult_body_mass_g),colour=class, ..density..))+
  geom_freqpoly(binwidth=0.5,lwd=1.3)+
  scale_color_brewer(name="Class",palette="Set1")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=12),axis.title = element_text(size=14))+
  theme(legend.text=element_text(size=18), legend.title = element_text(size=18),legend.position = c(0.8,0.5))+
  labs(x = "Log(Body Mass (g) )", y="Density")
#C*E
ggplot(data=completecase_am,aes(x=log_C_E,..density..,colour=class))+
  geom_freqpoly(binwidth=0.5,lwd=1.3,show.legend = F)+
  scale_color_brewer(palette = "Set1")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=12),axis.title = element_text(size=14))+
  labs(x = "Log(Lifetime Reproductive Effort)", y="Density")
#E/alpha
ggplot(data=completecase_am,aes(x=log_E_alpha,..density..,colour=class))+
  geom_freqpoly(binwidth=0.5,lwd=1.3,show.legend = F)+
  scale_color_brewer(palette="Set1")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=12),axis.title = element_text(size=14))+
  labs(x = "Log(Relative Reproductive Lifespan)",y="Density")
#I/m
ggplot(data=completecase_am,aes(x=log_I_m,..density..,colour=class))+
  geom_freqpoly(binwidth=0.5,lwd=1.3,show.legend = F)+
  scale_color_brewer(palette = "Set1")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=12),axis.title = element_text(size=14))+
  labs(x = "Log(Relative Offspring Size)",y="Density")

anova(lm(log_bodymass~class, data = completecase_am))
TukeyHSD(lm(log_bodymass~class, data = completecase_am))
favstats(log_bodymass~class, data = completecase_am)

anova(lm(log_C_E~class, data = completecase_am))
TukeyHSD(lm(log_C_E~class, data = completecase_am))
favstats(log_C_E~class, data = completecase_am)

anova(lm(log_E_alpha~class, data = completecase_am))
TukeyHSD(lm(log_E_alpha~class, data = completecase_am))
favstats(log_E_alpha~class, data = completecase_am)

anova(lm(log_I_m~class, data = completecase_am))
TukeyHSD(lm(log_I_m~class, data = completecase_am))
favstats(log_I_m~class, data = completecase_am)


#Histograms with bats
#Body mass
ggplot()+
  geom_freqpoly(data=completecase_am[completecase_am$class=="Aves"|completecase_am$class=="Mammalia",],aes(x=log_bodymass,..density..,colour=class),
                binwidth=0.5, lwd=1.3,show.legend = F)+
  scale_color_brewer(palette="Set1")+
  geom_freqpoly(data=completecase_am[completecase_am$order=="Chiroptera",],aes(x=log_bodymass,..density..),
                binwidth=0.5, lwd=1.3, show.legend = F,colour=brewer.pal(n=5, "Set1")[5])+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=12),axis.title = element_text(size=14))+
  labs(x = "Log(Body Mass (g))", y="Density")+
  theme(legend.text=element_text(size=18), legend.title = element_text(size=18),legend.position = c(0.8,0.5))

legend("topright",legend=c("Aves","Mammalia","Chiroptera"),lwd=1.9,col=c(brewer.pal(n=5,"Set1")[1],brewer.pal(n=5,"Set1")[2],brewer.pal(n=5,"Set1")[5]))

#C*E
ggplot()+
  geom_freqpoly(data=completecase_am[completecase_am$class=="Aves"|completecase_am$class=="Mammalia",],aes(x=log_C_E,..density..,colour=class),
                binwidth=0.5, lwd=1.3,show.legend = F)+
  scale_color_brewer(palette="Set1")+
  geom_freqpoly(data=completecase_am[completecase_am$order=="Chiroptera",],aes(x=log_C_E,..density..),
                binwidth=0.5, lwd=1.3, show.legend = F,colour=brewer.pal(n=5, "Set1")[5])+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=12),axis.title = element_text(size=14))+
  labs(x = "Log(Lifetime Reproductive Effort)", y="Density")
  
#E/alpha
ggplot()+
  geom_freqpoly(data=completecase_am[completecase_am$class=="Aves"|completecase_am$class=="Mammalia",],aes(x=log_E_alpha,..density..,colour=class),
              binwidth=0.5, lwd=1.3,show.legend = F)+
  scale_color_brewer(palette="Set1")+
  geom_freqpoly(data=completecase_am[completecase_am$order=="Chiroptera",],aes(x=log_E_alpha,..density..),
                binwidth=0.5, lwd=1.3, show.legend = F,colour=brewer.pal(n=5, "Set1")[5])+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=12),axis.title = element_text(size=14))+
  labs(x = "Log(Relative Reproductive Lifespan)", y="Density")

#I/m
ggplot()+
  geom_freqpoly(data=completecase_am[completecase_am$class=="Aves"|completecase_am$class=="Mammalia",],aes(x=log_I_m,..density..,colour=class),
                binwidth=0.5, lwd=1.3,show.legend = F)+
  scale_color_brewer(palette="Set1")+
  geom_freqpoly(data=completecase_am[completecase_am$order=="Chiroptera",],aes(x=log_I_m,..density..),
                binwidth=0.5, lwd=1.3, show.legend = F,colour=brewer.pal(n=5, "Set1")[5])+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=12),axis.title = element_text(size=14))+
  labs(x = "Log(Relative Offspring Size)", y="Density")

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




# Hypervolumes ------------------------------------------------------------

#Bird Gaussian hypervolume

#Log transform bird hypervolume
completebirds_gaussian<-hypervolume_gaussian(data = completecase_am[completecase_am$class=="Aves",c(18:20,17)],
                                             name = "completebirds_gaussian")
completebirds_gaussian@Volume
#Plot bird hypervolume
plot(completebirds_gaussian,point.dark.factor=1,color=gg_color_hue(3)[1])
plot(completebirds_gaussian,show.3d=TRUE,plot.3d.axes.id=2:4,cex.random=3,cex.data=6,
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
completemammals_gaussian<-hypervolume_gaussian(data = completecase_am[completecase_am$class=="Mammalia",c(18:20,17)],
                                             name = "completemammals_gaussian")
completemammals_gaussian@Volume


#Plot mammal hypervolume
#Log transform mammal hypervolume
plot(completemammals_gaussian,point.dark.factor=1,color=gg_color_hue(3)[2])
plot(completemammals_gaussian,show.3d=TRUE,plot.3d.axes.id=2:4,cex.random=3,cex.data=6,
     show.legend=TRUE,point.alpha.min=0.5,point.dark.factor=1)


#Reptile Gaussian hypervolume

#Log transform reptile hypervolume
completereptiles_gaussian<-hypervolume_gaussian(data = completecase_am[completecase_am$class=="Reptilia",c(18:20,17)],
                                               name = "completereptiles_gaussian")
completereptiles_gaussian@Volume

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
plot(completereptiles_gaussian,point.dark.factor=1,num.points.max.random=6000,contour.lwd=1.5,color=brewer.pal(n=3,"Set1")[3],show.legend=FALSE,
           plot.function.additional=function(i,j) {
               points(x=complete_data[complete_data$order=="Squamata",i],y=complete_data[complete_data$order=="Squamata",j],col=brewer.pal(n=7,"Set1")[4],pch=19)
               points(x=complete_data[complete_data$order=="Testudines",i],y=complete_data[complete_data$order=="Testudines",j],col=brewer.pal(n=6,"Accent")[5],pch=19)
               points(x=complete_data[complete_data$order=="Crocodilia",i],y=complete_data[complete_data$order=="Crocodilia",j],col=brewer.pal(n=6,"Accent")[6],pch=19)
             })
legend("bottomleft",legend = c("Crocodilia","Squamata","Testudines"),text.col=c(brewer.pal(n=6,"Accent")[6],brewer.pal(n=7,"Set1")[4],brewer.pal(n=6,"Accent")[5]),bty="n",cex=1.1,text.font=2)


#Amphibian hypervolume
completeamph_gaussian<-hypervolume_gaussian(data = completecase_am[completecase_am$class=="Amphibia",c(18:20,17)],
                                                name = "completeamph_gaussian")
completeamph_gaussian@Volume

#Plot amphibian hypervolume
#Log transform amphibian hypervolume
plot(completeamph_gaussian,point.dark.factor=1,color=gg_color_hue(4)[4])
plot(completeamph_gaussian,show.3d=TRUE,plot.3d.axes.id=2:4,cex.random=3,cex.data=6,
     show.legend=TRUE,point.alpha.min=0.5,point.dark.factor=1)


#Plotting all three amniote hypervolumes together
#Log transformed hypervolumes
plot(hypervolume_join(completebirds_gaussian,completemammals_gaussian,completereptiles_gaussian),num.points.max.random=6000,contour.lwd=1.5,colors=c(brewer.pal(n=3,"Set1")[1],brewer.pal(n=3,"Set1")[2],brewer.pal(n=3,"Set1")[3]),show.legend=FALSE)
legend("bottomleft",legend = c("Birds","Mammals","Reptiles"),text.col=c(brewer.pal(n=3,"Set1")[1],brewer.pal(n=3,"Set1")[2],brewer.pal(n=3,"Set1")[3]),bty="n",cex=1.1,text.font=2)

plot(hypervolume_join(completebirds_gaussian,completemammals_gaussian,completereptiles_gaussian),
     show.3d=TRUE,plot.3d.axes.id=2:4,
     colors = c(gg_color_hue(3)[1],gg_color_hue(3)[2],gg_color_hue(3)[3]),point.alpha.min = 0.5,cex.random=3,cex.data=6)

#Plotting all four hypervolumes together
#Log transformed hypervolumes
plot(hypervolume_join(completebirds_gaussian,completemammals_gaussian,completereptiles_gaussian,completeamph_gaussian),
     num.points.max.random=6000,contour.lwd=1.5,colors=c(brewer.pal(n=3,"Set1")[1],brewer.pal(n=3,"Set1")[2],brewer.pal(n=3,"Set1")[3],brewer.pal(n=4,"Set1")[4]),
     names=c("log(LRE)","log(ROS)", "log(RRL)", "log(Body Mass)"),show.legend=FALSE)
legend("bottomleft",legend = c("Birds","Mammals","Reptiles", "Amphibians"),text.col=c(brewer.pal(n=3,"Set1")[1],brewer.pal(n=3,"Set1")[2],brewer.pal(n=3,"Set1")[3],brewer.pal(n=4,"Set1")[4]),bty="n",cex=1.1,text.font=2)

plot(hypervolume_join(completebirds_gaussian,completemammals_gaussian,completereptiles_gaussian,completeamph_gaussian),
     show.3d=TRUE,plot.3d.axes.id=1:3,
     colors = c(brewer.pal(n=3,"Set1")[1],brewer.pal(n=3,"Set1")[2],brewer.pal(n=3,"Set1")[3],brewer.pal(n=4,"Set1")[4]),
     names=c("log(LRE)", "log(ROS)", "log(RRL)"),show.legend=FALSE,point.alpha.min = 0.5,cex.random=3,cex.data=6)

open3d()
plot(hypervolume_join(completebirds_gaussian,completemammals_gaussian,completereptiles_gaussian,completeamph_gaussian),
     show.3d=TRUE,plot.3d.axes.id=c(1,3,2),
     colors = c(brewer.pal(n=3,"Set1")[1],brewer.pal(n=3,"Set1")[2],brewer.pal(n=3,"Set1")[3],brewer.pal(n=4,"Set1")[4]),
     names=c("log(LRE)", "log(RRL)", "log(ROS)"),show.legend=FALSE,point.alpha.min = 0.5,cex.random=3,cex.data=6)

#play3d(spin3d(axis = c(0, 0, 1), rpm = 10))
movie3d(spin3d(axis = c(0,0,1),rpm=10),duration = 15,movie = "spinninghypervolume", dir = "C:/Users/cecin/OneDrive/Documents/Kenyon College/Kerkhoff Lab/Summer Science 2017/bodymasspatterns/gif_folder")

#Overlap statistics
#Birds and mammals:
hypervolume_overlap_statistics(hypervolume_set(completebirds_gaussian,completemammals_gaussian,check.memory = FALSE))
#Birds and reptiles:
hypervolume_overlap_statistics(hypervolume_set(completebirds_gaussian,completereptiles_gaussian,check.memory = FALSE))
#Birds and amphibians:
hypervolume_overlap_statistics(hypervolume_set(completebirds_gaussian,completeamph_gaussian,check.memory = FALSE))
#Mammals and reptiles:
hypervolume_overlap_statistics(hypervolume_set(completemammals_gaussian,completereptiles_gaussian,check.memory = FALSE))
#Mammals and amphibians:
hypervolume_overlap_statistics(hypervolume_set(completemammals_gaussian,completeamph_gaussian,check.memory = FALSE))
#Reptiles and amphibians:
hypervolume_overlap_statistics(hypervolume_set(completereptiles_gaussian,completeamph_gaussian,check.memory = FALSE))




#Bat hypervolumes
completebats_gaussian<-hypervolume_gaussian(data = completecase_species[completecase_am$order=="Chiroptera",c(18:20,17)],
                                                name = "completebats_gaussian")
completebats_gaussian@Volume

plot(completebats_gaussian,point.dark.factor=1,color=gg_color_hue(4)[4])
#Plot bats with birds and mammals
plot(hypervolume_join(completebirds_gaussian,completemammals_gaussian,completebats_gaussian),
     num.points.max.random=6000,contour.lwd=1.5,colors = c(brewer.pal(n=3,"Set1")[1],brewer.pal(n=3,"Set1")[2],brewer.pal(n=5, "Set1")[5]),
     names=c("log(LRE)","log(ROS)", "log(RRL)", "log(Body Mass)"),show.legend=FALSE)

complete_data<-completecase_am[,c(18:20,17,1:5)]
complete_data<-as.data.frame(complete_data)
#Add bat points to mammal hypervolume
plot(hypervolume_join(completebirds_gaussian,completemammals_gaussian),num.points.max.random=6000,contour.lwd=1.5,colors=c(brewer.pal(n=3,"Set1")[1],brewer.pal(n=3,"Set1")[2]),
     names=c("log(LRE)","log(ROS)", "log(RRL)", "log(Body Mass)"),show.legend=FALSE,
     plot.function.additional=function(i,j) {   
       points(x=complete_data[complete_data$order=="Chiroptera",i],y=complete_data[complete_data$order=="Chiroptera",j],col=brewer.pal(n=5,"Set1")[5],pch=19)
       })
legend("bottomleft",legend = c("Bats","Birds","Mammals"),text.col=c(brewer.pal(n=6,"Set1")[5],brewer.pal(n=6,"Set1")[1],brewer.pal(n=6,"Set1")[2]),bty="n",cex=1.1,text.font=2)



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



# Estimating A for Mammals ------------------------------------------------

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



# Importing and Pruning Trees ---------------------------------------------------

#Mammals
mammaltrees<-read.nexus("C:/Users/cecin/Desktop/HonorsPhylogenies/fritztree2009.txt")
#Pick just the tree with the best date estimate
mammaltree_best<-mammaltrees$mammalST_MSW05_bestDates
#want to prune to just the mammals with trait data
#named vector including all the mammal species with complete trait data
bmvec_mammal<-completecase_am$adult_body_mass_g[completecase_am$class=="Mammalia"]
names(bmvec_mammal)<-completecase_am$taxaname[completecase_am$class=="Mammalia"]
pruned_mammaltree_best<-prune.missing(x=bmvec_mammal, phylo=mammaltree_best)
pruned_mammaltree_best<-pruned_mammaltree_best$tree

#make mammal tree dichotomous
pruned_mammaltree_di<-multi2di(pruned_mammaltree_best,random=FALSE)

#Create a table of the node labels for each order
mammal_ordernodes<-data.frame(Order=as.character(unique(completecase_am$order[completecase_am$class=="Mammalia"])),num.species=as.numeric(0),node.num=as.numeric(0))
#add number of species per order
for(i in 1:nrow(mammal_ordernodes)){
  mammal_ordernodes$num.species[i]<-sum(pruned_mammaltree_best$tip.label%in%completecase_am$taxaname[as.character(completecase_am$order)==as.character(mammal_ordernodes$Order[i])])
  if(mammal_ordernodes$num.species[i]>1)
    mammal_ordernodes$node.num[i]<-getMRCA(pruned_mammaltree_best,pruned_mammaltree_best$tip.label[pruned_mammaltree_best$tip.label%in%completecase_am$taxaname[as.character(completecase_am$order)==as.character(mammal_ordernodes$Order[i])]])
  else
    mammal_ordernodes$node.num[i]<-NA
}

mammal_ordernodes<-as.data.frame(mammal_ordernodes)
mammal_ordernodes$Order<-as.character(mammal_ordernodes$Order)

#plot tree with clades labeled
plot(pruned_mammaltree_best,type="fan",show.tip.label = FALSE,no.margin = TRUE)
mammal_ordernodenumbers<-mammal_ordernodes$node.num[!is.na(mammal_ordernodes$node.num)]
for(i in 1:length(mammal_ordernodenumbers)){
  arc.cladelabels(tree=pruned_mammaltree_best,text=mammal_ordernodes$Order[!is.na(mammal_ordernodes$node.num)][i],
                  mammal_ordernodenumbers[i])
}

#plot orders with greater than 50 species
plot(pruned_mammaltree_best,type="fan",show.tip.label = FALSE,no.margin = TRUE)
mammal_orderover50<-mammal_ordernodes$node.num[mammal_ordernodes$num.species>50]
for(i in 1:length(mammal_orderover50)){
  arc.cladelabels(tree=pruned_mammaltree_best,text=mammal_ordernodes$Order[mammal_ordernodes$num.species>50][i],
                  mammal_orderover50[i])
}

#plot orders with greater than 40 species
plot(pruned_mammaltree_best,type="fan",show.tip.label = FALSE,no.margin = TRUE)
mammal_orderover40<-mammal_ordernodes$node.num[mammal_ordernodes$num.species>40]
for(i in 1:length(mammal_orderover40)){
  arc.cladelabels(tree=pruned_mammaltree_best,text=mammal_ordernodes$Order[mammal_ordernodes$num.species>40][i],
                  mammal_orderover40[i])
}

#plotting clades with images of orders
par(mar=c(3,0,2,0))
plot(pruned_mammaltree_best,type="fan",show.tip.label = FALSE,no.margin = FALSE)
for(i in 1:length(mammal_orderover40)){
     arc.cladelabels(tree=pruned_mammaltree_best,text=mammal_ordernodes$Order[mammal_ordernodes$num.species>40][i],mammal_orderover40[i],cex=0.85)
}

#primate<-readPNG("primate.png")
rasterImage(primate,-210,60,-170,100)

#artiodactyla<-readPNG("artiodactyla.png")
rasterImage(artiodactyla,-220,-70,-180,-45)

#soricomorpha<-readPNG("soricomorpha.png")
rasterImage(soricomorpha,60,-185,100,-164)

#carnivora<-readPNG("carnivora.png")
rasterImage(carnivora,-140,-165,-100,-146)

#chiroptera<-readPNG("chiroptera.png")
#rasterImage(chiroptera,-60,-210,0,-174)

#diprotodontia<-readPNG("diprotodontia.png")
rasterImage(diprotodontia,150,-120,190,-74)

#rodentia<-readPNG("rodentia.png")
rasterImage(rodentia,110,140,150,180)


#Birdtree.org tree
birdtree1<-read.newick("C:/Users/cecin/Desktop/HonorsPhylogenies/Hacketttree1.txt")
plot(birdtree1)
birdtree1$tip.label[1017]<-"Antrostomus_vociferus"
birdtree1$tip.label[1107]<-"Hydroprogne_caspia"
birdtree1$tip.label[1096]<-"Onychoprion_fuscatus"
birdtree1$tip.label[1216]<-"Stercorarius_skua"
birdtree1$tip.label[6814]<-"Melozone_aberti"
birdtree1$tip.label[6684]<-"Acanthis_flammea"
birdtree1$tip.label[6685]<-"Acanthis_hornemanni"
birdtree1$tip.label[6640]<-"Haemorhous_mexicanus"
birdtree1$tip.label[6747]<-"Spinus_pinus"
birdtree1$tip.label[4298]<-"Periparus_ater"
birdtree1$tip.label[4291]<-"Poecile_atricapillus"
birdtree1$tip.label[7041]<-"Geothlypis_formosa"
birdtree1$tip.label[7142]<-"Leiothlypis_peregrina"
birdtree1$tip.label[7036]<-"Parkesia_motacilla"
birdtree1$tip.label[7059]<-"Setophaga_aestiva"
birdtree1$tip.label[7073]<-"Setophaga_discolor"
birdtree1$tip.label[7078]<-"Setophaga_kirtlandii"
birdtree1$tip.label[7081]<-"Setophaga_petechia"
birdtree1$tip.label[7084]<-"Setophaga_striata"
birdtree1$tip.label[7070]<-"Setophaga_virens"
birdtree1$tip.label[9332]<-"Thalassarche_melanophris"

#pruning birds
bmvec_bird<-completecase_am$adult_body_mass_g[completecase_am$class=="Aves"]
names(bmvec_bird)<-completecase_am$taxaname[completecase_am$class=="Aves"]
pruned_birdtree1<-prune.missing(x=bmvec_bird, phylo=birdtree1)
pruned_birdtree1<-pruned_birdtree1$tree

#Create a table of the node labels for each order
bird_ordernodes<-data.frame(Order=as.character(unique(completecase_am$order[completecase_am$class=="Aves"])),num.species=as.numeric(0),node.num=as.numeric(0))
#add number of species per order
for(i in 1:nrow(bird_ordernodes)){
  bird_ordernodes$num.species[i]<-sum(pruned_birdtree1$tip.label%in%completecase_am$taxaname[as.character(completecase_am$order)==as.character(bird_ordernodes$Order[i])])
  if(bird_ordernodes$num.species[i]>1)
    bird_ordernodes$node.num[i]<-getMRCA(pruned_birdtree1,pruned_birdtree1$tip.label[pruned_birdtree1$tip.label%in%completecase_am$taxaname[as.character(completecase_am$order)==as.character(bird_ordernodes$Order[i])]])
  else
    bird_ordernodes$node.num[i]<-NA
}

bird_ordernodes<-as.data.frame(bird_ordernodes)
bird_ordernodes$Order<-as.character(bird_ordernodes$Order)

#plot tree with clades labeled
plot(pruned_birdtree1,type="fan",show.tip.label = FALSE,no.margin = TRUE)
bird_ordernodenumbers<-bird_ordernodes$node.num[!is.na(bird_ordernodes$node.num)]
for(i in 1:length(bird_ordernodenumbers)){
  arc.cladelabels(tree=pruned_birdtree1,text=bird_ordernodes$Order[!is.na(bird_ordernodes$node.num)][i],bird_ordernodenumbers[i])
}



#Reptiles
#Zheng and Wiens tree
squamatetree<-read.newick("C:/Users/cecin/Desktop/HonorsPhylogenies/zhengwienstree.txt")
#taxonomic resolution
squamatetree$tip.label[squamatetree$tip.label=="Agama_sankaranica"]<-"Agama_boensis"
squamatetree$tip.label[squamatetree$tip.label=="Gallotia_gomerana"]<-"Gallotia_bravoana"
squamatetree$tip.label[squamatetree$tip.label=="Phrynosoma_douglassii"]<-"Phrynosoma_douglasii"

#pruning squamates
bmvec_reptile<-completecase_am$adult_body_mass_g[completecase_am$class=="Reptilia"]
names(bmvec_reptile)<-completecase_am$taxaname[completecase_am$class=="Reptilia"]
pruned_squamatetree<-prune.missing(x=bmvec_reptile, phylo=squamatetree)
pruned_squamatetree<-pruned_squamatetree$tree
pruned_squamatetree<-drop.tip(pruned_squamatetree,c("Crocodylus_porosus","Alligator_mississippiensis","Chelydra_serpentina"))


#make Zheng and Wiens tree ultrametric
#function from Liam Revell's page
force.ultrametric<-function(tree,method=c("nnls","extend")){
  method<-method[1]
  if(method=="nnls") tree<-nnls.tree(cophenetic(tree),tree,
                                     rooted=TRUE,trace=0)
  else if(method=="extend"){
    h<-diag(vcv(tree))
    d<-max(h)-h
    ii<-sapply(1:Ntip(tree),function(x,y) which(y==x),
               y=tree$edge[,2])
    tree$edge.length[ii]<-tree$edge.length[ii]+d
  } else 
    cat("method not recognized: returning input tree\n\n")
  tree
}

ult_pruned_squamatetree<-force.ultrametric(pruned_squamatetree,method = "nnls")

#Amphibians
data("PyronWiens2011")
amphibiantree<-PyronWiens2011
#taxonomic resolution
amphibiantree$tip.label[amphibiantree$tip.label=="Bufo_americanus"]<-"Anaxyrus_americanus"
amphibiantree$tip.label[amphibiantree$tip.label=="Bufo_baxteri"]<-"Anaxyrus_baxteri"
amphibiantree$tip.label[amphibiantree$tip.label=="Bufo_californicus"]<-"Anaxyrus_californicus"
amphibiantree$tip.label[amphibiantree$tip.label=="Bufo_cognatus"]<-"Anaxyrus_cognatus"
amphibiantree$tip.label[amphibiantree$tip.label=="Bufo_fowleri"]<-"Anaxyrus_fowleri"
amphibiantree$tip.label[amphibiantree$tip.label=="Bufo_punctatus"]<-"Anaxyrus_punctatus"
amphibiantree$tip.label[amphibiantree$tip.label=="Bufo_woodhousii"]<-"Anaxyrus_woodhousii"
amphibiantree$tip.label[amphibiantree$tip.label=="Bufo_calamita"]<-"Epidalea_calamita"
amphibiantree$tip.label[amphibiantree$tip.label=="Bufo_calamita"]<-"Epidalea_calamita"
amphibiantree$tip.label[amphibiantree$tip.label=="Bufo_alvarius"]<-"Incilius_alvarius"
amphibiantree$tip.label[amphibiantree$tip.label=="Bufo_nebulifer"]<-"Incilius_nebulifer"
amphibiantree$tip.label[amphibiantree$tip.label=="Bufo_viridis"]<-"Pseudepidalea_viridis"
amphibiantree$tip.label[amphibiantree$tip.label=="Bufo_marinus"]<-"Rhinella_marina"
amphibiantree$tip.label[amphibiantree$tip.label=="Bufo_viridis"]<-"Pseudepidalea_viridis"
amphibiantree$tip.label[amphibiantree$tip.label=="Rana_pleuraden"]<-"Babina_pleuraden"
amphibiantree$tip.label[amphibiantree$tip.label=="Rana_rugosa"]<-"Glandirana_rugosa"
amphibiantree$tip.label[amphibiantree$tip.label=="Rana_guentheri"]<-"Hylarana_guentheri"
amphibiantree$tip.label[amphibiantree$tip.label=="Rana_nigrovittata"]<-"Hylarana_nigrovittata"
amphibiantree$tip.label[amphibiantree$tip.label=="Rana_taipehensis"]<-"Hylarana_taipehensis"
amphibiantree$tip.label[amphibiantree$tip.label=="Rana_guentheri"]<-"Hylarana_guentheri"
amphibiantree$tip.label[amphibiantree$tip.label=="Rana_catesbeiana"]<-"Lithobates_catesbeianus"
amphibiantree$tip.label[amphibiantree$tip.label=="Rana_clamitans"]<-"Lithobates_clamitans"
amphibiantree$tip.label[amphibiantree$tip.label=="Rana_pipiens"]<-"Lithobates_pipiens"
amphibiantree$tip.label[amphibiantree$tip.label=="Rana_septentrionalis"]<-"Lithobates_septentrionalis"
amphibiantree$tip.label[amphibiantree$tip.label=="Rana_sylvatica"]<-"Lithobates_sylvaticus"
amphibiantree$tip.label[amphibiantree$tip.label=="Rana_pipiens"]<-"Lithobates_pipiens"
amphibiantree$tip.label[amphibiantree$tip.label=="Rana_grahami"]<-"Odorrana_grahami"
amphibiantree$tip.label[amphibiantree$tip.label=="Rana_epeirotica"]<-"Pelophylax_epeiroticus"
amphibiantree$tip.label[amphibiantree$tip.label=="Rana_lessonae"]<-"Pelophylax_lessonae"
amphibiantree$tip.label[amphibiantree$tip.label=="Rana_nigromaculata"]<-"Pelophylax_nigromaculatus"
amphibiantree$tip.label[amphibiantree$tip.label=="Rana_perezi"]<-"Pelophylax_perezi"
amphibiantree$tip.label[amphibiantree$tip.label=="Rana_ridibunda"]<-"Pelophylax_ridibundus"

#pruning amphibians
bmvec_amphibian<-completecase_am$adult_body_mass_g[completecase_am$class=="Amphibia"]
names(bmvec_amphibian)<-completecase_am$taxaname[completecase_am$class=="Amphibia"]
pruned_amphibiantree<-prune.missing(x=bmvec_amphibian, phylo=amphibiantree)
pruned_amphibiantree<-pruned_amphibiantree$tree

ult_pruned_amphibiantree<-force.ultrametric(pruned_amphibiantree,method = "nnls")



# All tetrapods

tetrapod_tree <- read.newick("C:/Users/cecin/Desktop/HonorsPhylogenies/tetrapod_tree.tre")
bmvec_tetrapod <- completecase_am$adult_body_mass_g
names(bmvec_tetrapod) <- completecase_am$taxaname
pruned_tetrapodtree <- prune.missing(x = bmvec_tetrapod, phylo = tetrapod_tree)

sum(pruned_tetrapodtree$tree$tip.label %in% completecase_am$taxaname[completecase_am$class == "Aves"])
sum(pruned_tetrapodtree$tree$tip.label %in% completecase_am$taxaname[completecase_am$class == "Mammalia"])
sum(pruned_tetrapodtree$tree$tip.label %in% completecase_am$taxaname[completecase_am$class == "Reptilia"])
sum(pruned_tetrapodtree$tree$tip.label %in% completecase_am$taxaname[completecase_am$class == "Amphibia"])

pruned_tetrapodtree <- pruned_tetrapodtree$tree

ult_pruned_tetrapodtree<-force.ultrametric(pruned_tetrapodtree,method = "nnls")
plot(ult_pruned_tetrapodtree, type = "fan")


# Trait Datasets by Class for Species in Phylogeny -------------------------------------------------

#Mammals
mammaltraits<-completecase_am[completecase_am$taxaname%in%pruned_mammaltree_best$tip.label,c(15,17:20,11,10,8,14,9)]
mammaltraitmatrix<-as.matrix(mammaltraits[,1:5])
#order based on the phylogeny tip labels
mammaltraitmatrix<-mammaltraitmatrix[match(pruned_mammaltree_best$tip.label,mammaltraitmatrix[,1]),]
rownames(mammaltraitmatrix)<-mammaltraitmatrix[,1]
mammaltraitmatrix<-mammaltraitmatrix[,-1]

#matrix with components of invariants
mammalcompmatrix<-as.matrix(mammaltraits[,c(1,6:10)])
#order based on the phylogeny tip labels
mammalcompmatrix<-mammalcompmatrix[match(pruned_mammaltree_best$tip.label,mammalcompmatrix[,1]),]
rownames(mammaltraitmatrix)<-mammaltraitmatrix[,1]
mammaltraitmatrix<-mammaltraitmatrix[,-1]


#Birds
birdtraits<-completecase_am[completecase_am$taxaname%in%pruned_birdtree1$tip.label,c(15,17:20,11,10,8,14,9)]
birdtraitmatrix<-as.matrix(birdtraits[,1:5])

rownames(birdtraitmatrix)<-birdtraits$taxaname

#matrix with components of invariants
birdcompmatrix<-as.matrix(birdtraits[,6:11])
rownames(birdcompmatrix)<-birdtraits$taxaname

##Reptiles
reptiletraits<-completecase_am[completecase_am$class=="Reptilia",c(15,17:20,11,10,8,14,9)]
reptiletraitmatrix<-as.matrix(reptiletraits[,2:5])
rownames(reptiletraitmatrix)<-reptiletraits$taxaname

reptilecompmatrix<-as.matrix(reptiletraits[,6:11])
rownames(reptilecompmatrix)<-reptiletraits$taxaname

##Amphibian
amphibiantraits<-completecase_am[completecase_am$taxaname%in%pruned_amphibiantree$tip.label,c(15,17:20,11,10,8,14,9)]
amphibiantraitmatrix<-as.matrix(amphibiantraits[,2:5])
rownames(amphibiantraitmatrix)<-amphibiantraits$taxaname

# Adding Traits to Trees --------------------------------------------------

#adding traits to mammal tree
#Body mass
mammal_log_bodymass<-completecase_am$log_bodymass[completecase_am$class=="Mammalia"]
names(mammal_log_bodymass)<-completecase_am$taxaname[completecase_am$class=="Mammalia"]
mammal_log_bodymass_tiporder<-mammal_log_bodymass[pruned_mammaltree_best$tip.label]

plot(pruned_mammaltree_best,no.margin = TRUE,type="fan",show.tip.label = FALSE)
tiplabels(pch=19,col=color.scale(mammal_log_bodymass_tiporder,extremes=c("blue","red")))
color.legend(-255,-125,-155,-115,legend=c(0.85,18.82),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")
for(i in 1:length(mammal_orderover50)){
     arc.cladelabels(tree=pruned_mammaltree_best,text=mammal_ordernodes$Order[mammal_ordernodes$num.species>50][i],
                      mammal_orderover50[i],ln.offset = 1.03,lab.offset = 1.07)
   }

#C*E
mammal_log_C_E<-completecase_am$log_C_E[completecase_am$class=="Mammalia"]
names(mammal_log_C_E)<-completecase_am$taxaname[completecase_am$class=="Mammalia"]
mammal_log_C_E_tiporder<-mammal_log_C_E[pruned_mammaltree_best$tip.label]

plot(pruned_mammaltree_best,no.margin = TRUE,type="fan",show.tip.label = FALSE)
tiplabels(pch=19,col=color.scale(mammal_log_C_E_tiporder,extremes=c("blue","red")))
color.legend(-255,-125,-155,-115,legend=c(-2.23,5.76),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")
for(i in 1:length(mammal_orderover50)){
  arc.cladelabels(tree=pruned_mammaltree_best,text=as.character(mammal_ordernodes$Order[mammal_ordernodes$num.species>50][i]),
                  mammal_orderover50[i],ln.offset = 1.03,lab.offset = 1.07)
}

#I/m
mammal_log_I_m<-completecase_am$log_I_m[completecase_am$class=="Mammalia"]
names(mammal_log_I_m)<-completecase_am$taxaname[completecase_am$class=="Mammalia"]
mammal_log_I_m_tiporder<-mammal_log_I_m[pruned_mammaltree_best$tip.label]

plot(pruned_mammaltree_best,no.margin = TRUE,type="fan",show.tip.label = FALSE)
tiplabels(pch=19,col=color.scale(mammal_log_I_m_tiporder,extremes=c("blue","red")))
color.legend(-255,-125,-155,-115,legend=c(-3.77,0.68),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")
for(i in 1:length(mammal_orderover50)){
  arc.cladelabels(tree=pruned_mammaltree_best,text=mammal_ordernodes$Order[mammal_ordernodes$num.species>50][i],
                  mammal_orderover50[i],ln.offset = 1.03,lab.offset = 1.07)
}

#E/alpha
mammal_log_E_alpha<-completecase_am$log_E_alpha[completecase_am$class=="Mammalia"]
names(mammal_log_E_alpha)<-completecase_am$taxaname[completecase_am$class=="Mammalia"]
mammal_log_E_alpha_tiporder<-mammal_log_E_alpha[pruned_mammaltree_best$tip.label]

plot(pruned_mammaltree_best,no.margin = TRUE,type="fan",show.tip.label = FALSE)
tiplabels(pch=19,col=color.scale(mammal_log_E_alpha_tiporder,extremes=c("blue","red")))
color.legend(-255,-125,-155,-115,legend=c(-1.80,4.20),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")
for(i in 1:length(mammal_orderover50)){
  arc.cladelabels(tree=pruned_mammaltree_best,text=mammal_ordernodes$Order[mammal_ordernodes$num.species>50][i],
                  mammal_orderover50[i],ln.offset = 1.03,lab.offset = 1.07)
}



#adding traits to squamate tree
#Body mass
squamate_log_bodymass<-completecase_am$log_bodymass[completecase_am$order=="Squamata"]
names(squamate_log_bodymass)<-completecase_am$taxaname[completecase_am$order=="Squamata"]
squamate_log_bodymass_tiporder<-squamate_log_bodymass[pruned_squamatetree$tip.label]

plot(pruned_squamatetree,cex=0.65,no.margin=TRUE)
tiplabels(pch=19,col=color.scale(squamate_log_bodymass_tiporder,extremes=c("blue","red")))
color.legend(0,60,40,61,legend=c(0.90,10.62),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")


#C*E
squamate_log_C_E<-completecase_am$log_C_E[completecase_am$order=="Squamata"]
names(squamate_log_C_E)<-completecase_am$taxaname[completecase_am$order=="Squamata"]
squamate_log_C_E_tiporder<-squamate_log_C_E[pruned_squamatetree$tip.label]

plot(pruned_squamatetree,cex=0.65)
tiplabels(pch=19,col=color.scale(squamate_log_C_E_tiporder,extremes=c("blue","red")))
color.legend(0,60,40,61,legend=c(-2.44,2.42),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")


#I/m
squamate_log_I_m<-completecase_am$log_I_m[completecase_am$order=="Squamata"]
names(squamate_log_I_m)<-completecase_am$taxaname[completecase_am$order=="Squamata"]
squamate_log_I_m_tiporder<-squamate_log_I_m[pruned_squamatetree$tip.label]

plot(pruned_squamatetree,cex=0.65)
tiplabels(pch=19,col=color.scale(squamate_log_I_m_tiporder,extremes=c("blue","red")))
color.legend(0,60,40,61,legend=c(-6.30,-1.95),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

#E/alpha
squamate_log_E_alpha<-completecase_am$log_E_alpha[completecase_am$order=="Squamata"]
names(squamate_log_E_alpha)<-completecase_am$taxaname[completecase_am$order=="Squamata"]
squamate_log_E_alpha_tiporder<-squamate_log_E_alpha[pruned_squamatetree$tip.label]

plot(pruned_squamatetree,cex=0.65)
tiplabels(pch=19,col=color.scale(squamate_log_E_alpha_tiporder,extremes=c("blue","red")))
color.legend(0,60,40,61,legend=c(-0.32,3.26),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

#adding traits to amphibian tree
#Body mass
amphibian_log_bodymass<-completecase_am$log_bodymass[completecase_am$class=="Amphibia"]
names(amphibian_log_bodymass)<-completecase_am$taxaname[completecase_am$class=="Amphibia"]
amphibian_log_bodymass_tiporder<-amphibian_log_bodymass[pruned_amphibiantree$tip.label]

plot(pruned_amphibiantree,cex=0.65,no.margin=TRUE)
tiplabels(pch=19,col=color.scale(amphibian_log_bodymass_tiporder,extremes=c("blue","red")))
#color.legend(0,60,40,61,legend=c(0.90,10.62),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")


#C*E
amphibian_log_C_E<-completecase_am$log_C_E[completecase_am$class=="Amphibia"]
names(amphibian_log_C_E)<-completecase_am$taxaname[completecase_am$class=="Amphibia"]
amphibian_log_C_E_tiporder<-amphibian_log_C_E[pruned_amphibiantree$tip.label]

plot(pruned_amphibiantree,cex=0.65)
tiplabels(pch=19,col=color.scale(amphibian_log_C_E_tiporder,extremes=c("blue","red")))
color.legend(0,60,40,61,legend=c(-2.44,2.42),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")


#I/m
amphibian_log_I_m<-completecase_am$log_I_m[completecase_am$class=="Amphibia"]
names(amphibian_log_I_m)<-completecase_am$taxaname[completecase_am$class=="Amphibia"]
amphibian_log_I_m_tiporder<-amphibian_log_I_m[pruned_amphibiantree$tip.label]

plot(pruned_amphibiantree,cex=0.65)
tiplabels(pch=19,col=color.scale(amphibian_log_I_m_tiporder,extremes=c("blue","red")))
color.legend(0,60,40,61,legend=c(-6.30,-1.95),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

#E/alpha
amphibian_log_E_alpha<-completecase_am$log_E_alpha[completecase_am$class=="Amphibia"]
names(amphibian_log_E_alpha)<-completecase_am$taxaname[completecase_am$class=="Amphibia"]
amphibian_log_E_alpha_tiporder<-amphibian_log_E_alpha[pruned_amphibiantree$tip.label]

plot(pruned_amphibiantree,cex=0.65)
tiplabels(pch=19,col=color.scale(amphibian_log_E_alpha_tiporder,extremes=c("blue","red")))
color.legend(0,60,40,61,legend=c(-0.32,3.26),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")


#Adding traits to bird tree
#Body mass
bird_log_bodymass<-completecase_am$log_bodymass[completecase_am$class=="Aves"]
names(bird_log_bodymass)<-completecase_am$taxaname[completecase_am$class=="Aves"]
bird_log_bodymass_tiporder<-bird_log_bodymass[pruned_birdtree1$tip.label]

plot(pruned_birdtree1,no.margin = TRUE,type="fan",show.tip.label = FALSE)
tiplabels(pch=19,col=color.scale(bird_log_bodymass_tiporder,extremes=c("blue","red")))
color.legend(-150,-100,-100,-90,legend=c(1.15,8.99),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

#C*E
bird_log_C_E<-completecase_am$log_C_E[completecase_am$class=="Aves"]
names(bird_log_C_E)<-completecase_am$taxaname[completecase_am$class=="Aves"]
bird_log_C_E_tiporder<-bird_log_C_E[pruned_birdtree1$tip.label]

plot(pruned_birdtree1,no.margin = TRUE,show.tip.label = FALSE,type="fan")
tiplabels(pch=19,col=color.scale(bird_log_C_E_tiporder,extremes=c("blue","red")))
color.legend(-150,-100,-100,-90,legend=c(1.51,5.36),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

#I/m
bird_log_I_m<-completecase_am$log_I_m[completecase_am$class=="Aves"]
names(bird_log_I_m)<-completecase_am$taxaname[completecase_am$class=="Aves"]
bird_log_I_m_tiporder<-bird_log_I_m[pruned_birdtree1$tip.label]

plot(pruned_birdtree1,no.margin = TRUE,show.tip.label = FALSE,type="fan")
tiplabels(pch=19,col=color.scale(bird_log_I_m_tiporder,extremes=c("blue","red")))
color.legend(-150,-100,-100,-90,legend=c(-1.94,0.43),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

#E/alpha
bird_log_E_alpha<-completecase_am$log_E_alpha[completecase_am$class=="Aves"]
names(bird_log_E_alpha)<-completecase_am$taxaname[completecase_am$class=="Aves"]
bird_log_E_alpha_tiporder<-bird_log_E_alpha[pruned_birdtree1$tip.label]

plot(pruned_birdtree1,type="fan",no.margin = TRUE,show.tip.label = FALSE)
tiplabels(pch=19,col=color.scale(bird_log_E_alpha_tiporder,extremes=c("blue","red")))
color.legend(-150,-100,-100,-90,legend=c(0.54,3.57),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")


#Adding traits to tetrapod tree
#Body mass
log_bodymass<-completecase_am$log_bodymass
names(log_bodymass)<-completecase_am$taxaname
log_bodymass_tiporder<-log_bodymass[pruned_tetrapodtree$tip.label]

plot(pruned_tetrapodtree, no.margin = TRUE,type="fan",show.tip.label = FALSE)
tiplabels(pch=19,col=color.scale(log_bodymass_tiporder,extremes=c("blue","red")))
color.legend(-150,-100,-100,-90,legend=c(-0.545,18.82),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

#C*E
log_C_E<-completecase_am$log_C_E
names(log_C_E)<-completecase_am$taxaname
log_C_E_tiporder<-log_C_E[pruned_tetrapodtree$tip.label]

plot(pruned_tetrapodtree,no.margin = TRUE,show.tip.label = FALSE,type="fan")
tiplabels(pch=19,col=color.scale(log_C_E_tiporder,extremes=c("blue","red")))
color.legend(-150,-100,-100,-90,legend=c(-7.338,5.762),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

#I/m
log_I_m<-completecase_am$log_I_m
names(log_I_m)<-completecase_am$taxaname
log_I_m_tiporder<-log_I_m[pruned_tetrapodtree$tip.label]

plot(pruned_tetrapodtree,no.margin = TRUE,show.tip.label = FALSE,type="fan")
tiplabels(pch=19,col=color.scale(log_I_m_tiporder,extremes=c("blue","red")))
color.legend(-150,-100,-100,-90,legend=c(-15.66,0.676),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

#E/alpha
log_E_alpha<-completecase_am$log_E_alpha
names(log_E_alpha)<-completecase_am$taxaname
log_E_alpha_tiporder<-log_E_alpha[pruned_tetrapodtree$tip.label]

plot(pruned_tetrapodtree,type="fan",no.margin = TRUE,show.tip.label = FALSE)
tiplabels(pch=19,col=color.scale(log_E_alpha_tiporder,extremes=c("blue","red")))
color.legend(-150,-100,-100,-90,legend=c(-3.045,4.650),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")



# Map traits on tetrapod tree ---------------------------------------------

#Body mass
log_bodymass_contMap <- contMap(ult_pruned_tetrapodtree, log_bodymass_tiporder, plot = FALSE)
plot(log_bodymass_contMap, ftype = "off", outline = FALSE, leg.text = "log(Body Mass)", lwd = 1)
axis(1)
title(xlab="Time from root (million years)")

#C*E
log_C_E_contMap <- contMap(ult_pruned_tetrapodtree, log_C_E_tiporder, plot = FALSE)
plot(log_C_E_contMap, ftype = "off", outline = FALSE, leg.text = "log(LRE)", lwd = 1)

#I/m
log_I_m_contMap <- contMap(ult_pruned_tetrapodtree, log_I_m_tiporder, plot = FALSE)
plot(log_I_m_contMap, ftype = "off", outline = FALSE, leg.text = "log(ROS)", lwd = 1)

#E/alpha
log_E_alpha_contMap <- contMap(ult_pruned_tetrapodtree, log_E_alpha_tiporder, plot = FALSE)
plot(log_E_alpha_contMap, ftype = "off", outline = FALSE, leg.text = "log(RRL)", lwd = 1)



# Making Phylomorphospace Plots -------------------------------------------

#Create a dataframe of body mass and the three dimensionless metrics in tip order
mammal_phylo_order_traits<-cbind(mammal_log_bodymass_tiporder,mammal_log_C_E_tiporder,mammal_log_I_m_tiporder, mammal_log_E_alpha_tiporder)
mammal_phylo_order_traits<-as.data.frame(mammal_phylo_order_traits)
mammal_phylo_order_traits<-add_rownames(mammal_phylo_order_traits,"taxaname")

#Make phylomorphospace 3d plots
#C*E:
mammal_bodymass_C_E_matrix<-as.matrix(mammal_phylo_order_traits[,2:3])
colnames(mammal_bodymass_C_E_matrix)<-NULL
rownames(mammal_bodymass_C_E_matrix)<-mammal_phylo_order_traits$taxaname
View(mammal_bodymass_C_E_matrix)
#E/alpha:
mammal_bodymass_E_alpha_matrix<-as.matrix(mammal_phylo_order_traits[,c(2,5)])
colnames(mammal_bodymass_E_alpha_matrix)<-NULL
rownames(mammal_bodymass_E_alpha_matrix)<-mammal_phylo_order_traits$taxaname
#View(mammal_bodymass_E_alpha_matrix)
#I/m:
mammal_bodymass_I_m_matrix<-as.matrix(mammal_phylo_order_traits[,c(2,4)])
colnames(mammal_bodymass_I_m_matrix)<-NULL
rownames(mammal_bodymass_I_m_matrix)<-mammal_phylo_order_traits$taxaname
#View(mammal_bodymass_I_m_matrix)

fancyTree(pruned_mammaltree_best,type="traitgram3d",X=mammal_bodymass_C_E_matrix,control=list(ftype="off",col.edge=rep(brewer.pal(4,"Set1")[2],nrow(pruned_mammaltree_best$edge))))
fancyTree(pruned_mammaltree_best,type="traitgram3d",X=mammal_bodymass_E_alpha_matrix,control=list(ftype="off",col.edge=rep(brewer.pal(4,"Set1")[2],nrow(pruned_mammaltree_best$edge))))
fancyTree(pruned_mammaltree_best,type="traitgram3d",X=mammal_bodymass_I_m_matrix,control=list(ftype="off",col.edge=rep(brewer.pal(4,"Set1")[2],nrow(pruned_mammaltree_best$edge))))



#Create a dataframe of body mass and the three dimensionless metrics in tip order
squamate_phylo_order_traits<-cbind(squamate_log_bodymass_tiporder,squamate_log_C_E_tiporder,squamate_log_I_m_tiporder, squamate_log_E_alpha_tiporder)
squamate_phylo_order_traits<-as.data.frame(squamate_phylo_order_traits)
squamate_phylo_order_traits<-add_rownames(squamate_phylo_order_traits,"taxaname")

#Make phylomorphospace 3d plot
#C*E
squamate_bodymass_C_E_matrix<-as.matrix(squamate_phylo_order_traits[,2:3])
colnames(squamate_bodymass_C_E_matrix)<-NULL
rownames(squamate_bodymass_C_E_matrix)<-squamate_phylo_order_traits$taxaname
View(squamate_bodymass_C_E_matrix)
#E/alpha:
squamate_bodymass_E_alpha_matrix<-as.matrix(squamate_phylo_order_traits[,c(2,5)])
colnames(squamate_bodymass_E_alpha_matrix)<-NULL
rownames(squamate_bodymass_E_alpha_matrix)<-squamate_phylo_order_traits$taxaname
#View(squamate_bodymass_E_alpha_matrix)
#I/m:
squamate_bodymass_I_m_matrix<-as.matrix(squamate_phylo_order_traits[,c(2,4)])
colnames(squamate_bodymass_I_m_matrix)<-NULL
rownames(squamate_bodymass_I_m_matrix)<-squamate_phylo_order_traits$taxaname
#View(squamate_bodymass_I_m_matrix)

#fancyTree(pruned_squamatetree,type="traitgram3d",X=squamate_bodymass_C_E_matrix,control=list(ftype="off",col.edge=rep(brewer.pal(3,"Set1")[3],nrow(pruned_squamatetree$edge))))
#fancyTree(pruned_squamatetree,type="traitgram3d",X=squamate_bodymass_C_E_matrix,method="static",control=list(ftype="off"))

fancyTree(pruned_squamatetree,type="traitgram3d",X=squamate_bodymass_C_E_matrix,control=list(ftype="off",col.edge=rep(brewer.pal(4,"Set1")[3],nrow(pruned_squamatetree$edge))))
fancyTree(pruned_squamatetree,type="traitgram3d",X=squamate_bodymass_E_alpha_matrix,control=list(spin=F,ftype="off",col.edge=rep(brewer.pal(4,"Set1")[3],nrow(pruned_squamatetree$edge))))
fancyTree(pruned_squamatetree,type="traitgram3d",X=squamate_bodymass_I_m_matrix,control=list(spin=F,ftype="off",col.edge=rep(brewer.pal(4,"Set1")[3],nrow(pruned_squamatetree$edge))))


matrix_squamate_phylo_order_traits<-as.matrix(squamate_phylo_order_traits[,2:5])
rownames(matrix_squamate_phylo_order_traits)<-squamate_phylo_order_traits$taxaname
phylomorphospace3d(pruned_squamatetree,X=as.matrix(matrix_squamate_phylo_order_traits[,2:4]),method="static",control=list(ftype="off"))

#Create a dataframe of body mass and the three dimensionless metrics in tip order
amphibian_phylo_order_traits<-cbind(amphibian_log_bodymass_tiporder,amphibian_log_C_E_tiporder,amphibian_log_I_m_tiporder, amphibian_log_E_alpha_tiporder)
amphibian_phylo_order_traits<-as.data.frame(amphibian_phylo_order_traits)
amphibian_phylo_order_traits<-add_rownames(amphibian_phylo_order_traits,"taxaname")

#Make phylomorphospace 3d plot
#C*E:
amphibian_bodymass_C_E_matrix<-as.matrix(amphibian_phylo_order_traits[,2:3])
colnames(amphibian_bodymass_C_E_matrix)<-NULL
rownames(amphibian_bodymass_C_E_matrix)<-amphibian_phylo_order_traits$taxaname
View(amphibian_bodymass_C_E_matrix)
#E/alpha:
amphibian_bodymass_E_alpha_matrix<-as.matrix(amphibian_phylo_order_traits[,c(2,5)])
colnames(amphibian_bodymass_E_alpha_matrix)<-NULL
rownames(amphibian_bodymass_E_alpha_matrix)<-amphibian_phylo_order_traits$taxaname
#View(amphibian_bodymass_E_alpha_matrix)
#I/m:
amphibian_bodymass_I_m_matrix<-as.matrix(amphibian_phylo_order_traits[,c(2,4)])
colnames(amphibian_bodymass_I_m_matrix)<-NULL
rownames(amphibian_bodymass_I_m_matrix)<-amphibian_phylo_order_traits$taxaname
#View(amphibian_bodymass_I_m_matrix)

fancyTree(pruned_amphibiantree,type="traitgram3d",X=amphibian_bodymass_C_E_matrix,control=list(spin=F,ftype="off",col.edge=rep(brewer.pal(4,"Set1")[4],nrow(pruned_amphibiantree$edge))))
fancyTree(pruned_amphibiantree,type="traitgram3d",X=amphibian_bodymass_E_alpha_matrix,control=list(spin=F,ftype="off",col.edge=rep(brewer.pal(4,"Set1")[4],nrow(pruned_amphibiantree$edge))))
fancyTree(pruned_amphibiantree,type="traitgram3d",X=amphibian_bodymass_I_m_matrix,control=list(spin=F,ftype="off",col.edge=rep(brewer.pal(4,"Set1")[4],nrow(pruned_amphibiantree$edge))))


# fancyTree(pruned_amphibiantree,type="traitgram3d",X=amphibian_bodymass_C_E_matrix,control=list(ftype="off",col.edge=rep(brewer.pal(4,"Set1")[4],nrow(pruned_amphibiantree$edge))))
# fancyTree(pruned_amphibiantree,type="traitgram3d",X=amphibian_bodymass_C_E_matrix,method="static",control=list(ftype="off",col.edge=rep(brewer.pal(4,"Set1")[4],nrow(pruned_amphibiantree$edge))))

matrix_amphibian_phylo_order_traits<-as.matrix(amphibian_phylo_order_traits[,2:5])
rownames(matrix_amphibian_phylo_order_traits)<-amphibian_phylo_order_traits$taxaname
phylomorphospace3d(pruned_amphibiantree,X=as.matrix(matrix_amphibian_phylo_order_traits[,2:4]),method="static",control=list(ftype="off"))



#Create a dataframe of body mass and the three dimensionless metrics in tip order
bird_phylo_order_traits<-cbind(bird_log_bodymass_tiporder,bird_log_C_E_tiporder,bird_log_I_m_tiporder, bird_log_E_alpha_tiporder)
bird_phylo_order_traits<-as.data.frame(bird_phylo_order_traits)
bird_phylo_order_traits<-add_rownames(bird_phylo_order_traits,"taxaname")

#Make phylomorphospace 3d plot
#C*E
bird_bodymass_C_E_matrix<-as.matrix(bird_phylo_order_traits[,2:3])
colnames(bird_bodymass_C_E_matrix)<-NULL
rownames(bird_bodymass_C_E_matrix)<-bird_phylo_order_traits$taxaname
View(bird_bodymass_C_E_matrix)
#E/alpha:
bird_bodymass_E_alpha_matrix<-as.matrix(bird_phylo_order_traits[,c(2,5)])
colnames(bird_bodymass_E_alpha_matrix)<-NULL
rownames(bird_bodymass_E_alpha_matrix)<-bird_phylo_order_traits$taxaname
#View(bird_bodymass_E_alpha_matrix)
#I/m:
bird_bodymass_I_m_matrix<-as.matrix(bird_phylo_order_traits[,c(2,4)])
colnames(bird_bodymass_I_m_matrix)<-NULL
rownames(bird_bodymass_I_m_matrix)<-bird_phylo_order_traits$taxaname
#View(bird_bodymass_I_m_matrix)

fancyTree(pruned_birdtree1,type="traitgram3d",X=bird_bodymass_C_E_matrix,control=list(spin=F,ftype="off",col.edge=rep(brewer.pal(4,"Set1")[1],nrow(pruned_birdtree1$edge))))
fancyTree(pruned_birdtree1,type="traitgram3d",X=bird_bodymass_E_alpha_matrix,control=list(spin=F,ftype="off",col.edge=rep(brewer.pal(4,"Set1")[1],nrow(pruned_birdtree1$edge))))
fancyTree(pruned_birdtree1,type="traitgram3d",X=bird_bodymass_I_m_matrix,control=list(spin=F,ftype="off",col.edge=rep(brewer.pal(4,"Set1")[1],nrow(pruned_birdtree1$edge))))


fancyTree(pruned_birdtree1,type="traitgram3d",X=bird_bodymass_C_E_matrix,control=list(ftype="off",col.edge=rep(brewer.pal(4,"Set1")[1],nrow(pruned_birdtree1$edge))))
fancyTree(pruned_birdtree1,type="traitgram3d",X=bird_bodymass_C_E_matrix,method="static",control=list(ftype="off"))


# Ancestral Reconstruction Models -----------------------------------------


#Create Brownian motion, OU, etc. models
#For body mass
bird_bodymass_fit.ou<-fitContinuous(pruned_birdtree1,bird_log_bodymass_tiporder,model="OU")
bird_bodymass_fit.ou
bird_bodymass_fit.bm<-fitContinuous(pruned_birdtree1,bird_log_bodymass_tiporder,model="BM")
bird_bodymass_fit.bm
bird_bodymass_fit.lambda<-fitContinuous(pruned_birdtree1,bird_log_bodymass_tiporder,model="lambda")
bird_bodymass_fit.lambda #basically brownian
bird_bodymass_fit.kappa<-fitContinuous(pruned_birdtree1,bird_log_bodymass_tiporder,model="kappa")
bird_bodymass_fit.kappa #basically brownian
bird_bodymass_fit.delta<-fitContinuous(pruned_birdtree1,bird_log_bodymass_tiporder,model="delta")
bird_bodymass_fit.delta #basically brownian

#For C*E
bird_C_E_fit.ou<-fitContinuous(pruned_birdtree1,bird_log_C_E_tiporder,model="OU")
bird_C_E_fit.ou
bird_C_E_fit.bm<-fitContinuous(pruned_birdtree1,bird_log_C_E_tiporder,model="BM")
bird_C_E_fit.bm
bird_C_E_fit.lambda<-fitContinuous(pruned_birdtree1,bird_log_C_E_tiporder,model="lambda")
bird_C_E_fit.lambda
bird_C_E_fit.white<-fitContinuous(pruned_birdtree1,bird_log_C_E_tiporder,model="white")
bird_C_E_fit.white
#For I/m
bird_I_m_fit.ou<-fitContinuous(pruned_birdtree1,bird_log_I_m_tiporder,model="OU")
bird_I_m_fit.ou
bird_I_m_fit.bm<-fitContinuous(pruned_birdtree1,bird_log_I_m_tiporder,model="BM")
bird_I_m_fit.bm
bird_I_m_fit.lambda<-fitContinuous(pruned_birdtree1,bird_log_I_m_tiporder,model="lambda")
bird_I_m_fit.lambda
bird_I_m_fit.white<-fitContinuous(pruned_birdtree1,bird_log_I_m_tiporder,model="white")
bird_I_m_fit.white
#For E/alpha
bird_E_alpha_fit.ou<-fitContinuous(pruned_birdtree1,bird_log_E_alpha_tiporder,model="OU")
bird_E_alpha_fit.ou
bird_E_alpha_fit.bm<-fitContinuous(pruned_birdtree1,bird_log_E_alpha_tiporder,model="BM")
bird_E_alpha_fit.bm
bird_E_alpha_fit.lambda<-fitContinuous(pruned_birdtree1,bird_log_E_alpha_tiporder,model="lambda")
bird_E_alpha_fit.lambda
bird_E_alpha_fit.white<-fitContinuous(pruned_birdtree1,bird_log_E_alpha_tiporder,model="white")
bird_E_alpha_fit.white

#fast anc reconstructions for birds
#Body mass
bird_bodymass_bm_tree<-geiger::rescale(pruned_birdtree1,model="BM", bird_bodymass_fit.bm$opt$sigsq)
bird_bodymass_bm_fastAnc<-fastAnc(bird_bodymass_bm_tree, bird_log_bodymass_tiporder)

#Color node labels based on lambda model
plot(pruned_birdtree1,no.margin=TRUE,show.tip.label=FALSE,type="fan")
nodelabels(pch=19,col=color.scale(bird_bodymass_bm_fastAnc,extremes=c("blue","red"),xrange = c(min(bird_log_bodymass_tiporder),max(bird_log_bodymass_tiporder))))
tiplabels(pch=19,col=color.scale(bird_log_bodymass_tiporder,extremes=c("blue","red")))
color.legend(-150,-100,-100,-90,legend=c(1.15,8.99),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")


#C*E
bird_C_E_lam_tree<-geiger::rescale(pruned_birdtree1,model="lambda", bird_C_E_fit.lambda$opt$lambda)
bird_C_E_lam_fastAnc<-fastAnc(bird_C_E_lam_tree, bird_log_C_E_tiporder)

#Color node labels based on lambda model
plot(pruned_birdtree1,no.margin=TRUE,show.tip.label=FALSE,type="fan")
nodelabels(pch=19,col=color.scale(bird_C_E_lam_fastAnc,extremes=c("blue","red"),xrange=c(min(bird_log_C_E_tiporder),max(bird_log_C_E_tiporder))))
tiplabels(pch=19,col=color.scale(bird_log_C_E_tiporder,extremes=c("blue","red")))
color.legend(-150,-100,-100,-90,legend=c(-1.51,5.36),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

#E/alpha
bird_E_alpha_lam_tree<-geiger::rescale(pruned_birdtree1,model="lambda", bird_E_alpha_fit.lambda$opt$lambda)
bird_E_alpha_lam_fastAnc<-fastAnc(bird_E_alpha_lam_tree, bird_log_E_alpha_tiporder)

#Color node labels based on lambda model
plot(pruned_birdtree1,no.margin=TRUE,show.tip.label=FALSE,type="fan")
nodelabels(pch=19,col=color.scale(bird_E_alpha_lam_fastAnc,extremes=c("blue","red"),xrange=c(min(bird_log_E_alpha_tiporder),max(bird_log_E_alpha_tiporder))))
tiplabels(pch=19,col=color.scale(bird_log_E_alpha_tiporder,extremes=c("blue","red")))
color.legend(-150,-100,-100,-90,legend=c(0.54,3.57),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")


#I/m
bird_I_m_bm_tree<-geiger::rescale(pruned_birdtree1,model="BM", mammal_I_m_fit.bm$opt$sigsq)
bird_I_m_bm_fastAnc<-fastAnc(bird_I_m_bm_tree, bird_log_I_m_tiporder)

#Color node labels based on lambda model
plot(pruned_birdtree1,no.margin=TRUE,show.tip.label=FALSE,type="fan")
nodelabels(pch=19,col=color.scale(bird_I_m_bm_fastAnc,extremes=c("blue","red"),xrange = c(min(bird_log_I_m_tiporder),max(bird_log_I_m_tiporder))))
tiplabels(pch=19,col=color.scale(bird_log_I_m_tiporder,extremes=c("blue","red")))
color.legend(-150,-100,-100,-90,legend=c(-1.94,0.43),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")


#Create Brownian motion, OU, etc. models
#For body mass
mammal_bodymass_fit.ou<-fitContinuous(pruned_mammaltree_di,mammal_log_bodymass_tiporder,model="OU")
mammal_bodymass_fit.ou
mammal_bodymass_fit.bm<-fitContinuous(pruned_mammaltree_di,mammal_log_bodymass_tiporder,model="BM")
mammal_bodymass_fit.bm
mammal_bodymass_fit.lambda<-fitContinuous(pruned_mammaltree_di,mammal_log_bodymass_tiporder,model="lambda")
mammal_bodymass_fit.lambda
mammal_bodymass_fit.kappa<-fitContinuous(pruned_mammaltree_di,mammal_log_bodymass_tiporder,model="kappa")
mammal_bodymass_fit.kappa
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

#Create Brownian motion, OU, etc. models for Squamata
#For body mass
squamate_bodymass_fit.bm<-fitContinuous(ult_pruned_squamatetree,squamate_log_bodymass_tiporder,model="BM")
squamate_bodymass_fit.bm
squamate_bodymass_fit.ou<-fitContinuous(ult_pruned_squamatetree,squamate_log_bodymass_tiporder,model="OU")
squamate_bodymass_fit.ou
squamate_bodymass_fit.lambda<-fitContinuous(ult_pruned_squamatetree,squamate_log_bodymass_tiporder,model="lambda")
squamate_bodymass_fit.lambda
squamate_bodymass_fit.kappa<-fitContinuous(ult_pruned_squamatetree,squamate_log_bodymass_tiporder,model="kappa")
squamate_bodymass_fit.kappa
#For C*E
squamate_C_E_fit.ou<-fitContinuous(ult_pruned_squamatetree,squamate_log_C_E_tiporder,model="OU")
squamate_C_E_fit.ou
squamate_C_E_fit.bm<-fitContinuous(ult_pruned_squamatetree,squamate_log_C_E_tiporder,model="BM")
squamate_C_E_fit.bm
squamate_C_E_fit.lambda<-fitContinuous(ult_pruned_squamatetree,squamate_log_C_E_tiporder,model="lambda")
squamate_C_E_fit.lambda
squamate_C_E_fit.white<-fitContinuous(ult_pruned_squamatetree,squamate_log_C_E_tiporder,model="white")
squamate_C_E_fit.white
#For I/m
squamate_I_m_fit.ou<-fitContinuous(ult_pruned_squamatetree,squamate_log_I_m_tiporder,model="OU")
squamate_I_m_fit.ou
squamate_I_m_fit.bm<-fitContinuous(ult_pruned_squamatetree,squamate_log_I_m_tiporder,model="BM")
squamate_I_m_fit.bm
squamate_I_m_fit.lambda<-fitContinuous(ult_pruned_squamatetree,squamate_log_I_m_tiporder,model="lambda")
squamate_I_m_fit.lambda
squamate_I_m_fit.white<-fitContinuous(ult_pruned_squamatetree,squamate_log_I_m_tiporder,model="white")
squamate_I_m_fit.white
#For E/alpha
squamate_E_alpha_fit.ou<-fitContinuous(ult_pruned_squamatetree,squamate_log_E_alpha_tiporder,model="OU")
squamate_E_alpha_fit.ou
squamate_E_alpha_fit.bm<-fitContinuous(ult_pruned_squamatetree,squamate_log_E_alpha_tiporder,model="BM")
squamate_E_alpha_fit.bm
squamate_E_alpha_fit.lambda<-fitContinuous(ult_pruned_squamatetree,squamate_log_E_alpha_tiporder,model="lambda")
squamate_E_alpha_fit.lambda
squamate_E_alpha_fit.white<-fitContinuous(ult_pruned_squamatetree,squamate_log_E_alpha_tiporder,model="white")
squamate_E_alpha_fit.white

#Create Brownian motion, OU, etc. models for Amphibia
#For body mass
amph_bodymass_fit.bm<-fitContinuous(ult_pruned_amphibiantree,amphibian_log_bodymass_tiporder,model="BM")
amph_bodymass_fit.bm
amph_bodymass_fit.ou<-fitContinuous(ult_pruned_amphibiantree,amphibian_log_bodymass_tiporder,model="OU")
amph_bodymass_fit.ou
amph_bodymass_fit.lambda<-fitContinuous(ult_pruned_amphibiantree,amphibian_log_bodymass_tiporder,model="lambda")
amph_bodymass_fit.lambda
amph_bodymass_fit.kappa<-fitContinuous(ult_pruned_amphibiantree,amphibian_log_bodymass_tiporder,model="kappa")
amph_bodymass_fit.kappa



#fastAnc ancestral reconstructions for mammals

#Body mass
mammal_bodymass_lam_tree<-rescale(pruned_mammaltree_best,model="lambda", mammal_bodymass_fit.lambda$opt$lambda)
mammal_bodymass_lam_fastAnc<-fastAnc(mammal_bodymass_lam_tree, mammal_log_bodymass_tiporder)

#Color node labels based on lambda model
plot(pruned_mammaltree_best,no.margin=TRUE,show.tip.label=FALSE,type="fan")
nodelabels(pch=19,col=color.scale(mammal_bodymass_lam_fastAnc,extremes=c("blue","red"),xrange = c(min(mammal_log_bodymass_tiporder),max(mammal_log_bodymass_tiporder))))
tiplabels(pch=19,col=color.scale(mammal_log_bodymass_tiporder,extremes=c("blue","red")))
color.legend(-265,-125,-165,-115,legend=c(0.85,18.82),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")
for(i in 1:length(mammal_orderover40)){
  arc.cladelabels(tree=pruned_mammaltree_best,text=mammal_ordernodes$Order[mammal_ordernodes$num.species>40][i],
                  mammal_orderover40[i],ln.offset = 1.03,lab.offset = 1.07,mark.node=FALSE)
}

#C*E
mammal_C_E_lam_tree<-rescale(pruned_mammaltree_best,model="lambda", mammal_C_E_fit.lambda$opt$lambda)
mammal_C_E_lam_fastAnc<-fastAnc(mammal_C_E_lam_tree, mammal_log_C_E_tiporder)

#Color node labels based on lambda model
plot(pruned_mammaltree_best,no.margin=TRUE,show.tip.label=FALSE,type="fan")
nodelabels(pch=19,col=color.scale(mammal_C_E_lam_fastAnc,extremes=c("blue","red"),xrange=c(min(mammal_log_C_E_tiporder),max(mammal_log_C_E_tiporder))))
tiplabels(pch=19,col=color.scale(mammal_log_C_E_tiporder,extremes=c("blue","red")))
color.legend(-265,-125,-165,-115,legend=c(-2.76,5.38),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")
for(i in 1:length(mammal_orderover40)){  arc.cladelabels(tree=pruned_mammaltree_best,text=mammal_ordernodes$Order[mammal_ordernodes$num.species>40][i],
                  mammal_orderover40[i],ln.offset = 1.03,lab.offset = 1.07,mark.node=FALSE)
}

#E/alpha
mammal_E_alpha_ou_tree<-rescale(pruned_mammaltree_best,model="OU", mammal_E_alpha_fit.ou$opt$alpha)
mammal_E_alpha_ou_fastAnc<-fastAnc(mammal_E_alpha_ou_tree, mammal_log_E_alpha_tiporder)

#Color node labels based on OU model
plot(pruned_mammaltree_best,no.margin=TRUE,show.tip.label=FALSE,type="fan")
nodelabels(pch=19,col=color.scale(mammal_E_alpha_ou_fastAnc,extremes=c("blue","red"),xrange=c(min(mammal_log_E_alpha_tiporder),max(mammal_log_E_alpha_tiporder))))
tiplabels(pch=19,col=color.scale(mammal_log_E_alpha_tiporder,extremes=c("blue","red")))
color.legend(-255,-125,-155,-115,legend=c(-1.80,4.20),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")
for(i in 1:length(mammal_orderover40)){  arc.cladelabels(tree=pruned_mammaltree_best,text=mammal_ordernodes$Order[mammal_ordernodes$num.species>40][i],
                                                         mammal_orderover40[i],ln.offset = 1.03,lab.offset = 1.07,mark.node=FALSE)
}

#I/m
mammal_I_m_lam_tree<-rescale(pruned_mammaltree_best,model="lambda", mammal_I_m_fit.lambda$opt$lambda)
mammal_I_m_lam_fastAnc<-fastAnc(mammal_I_m_lam_tree, mammal_log_I_m_tiporder)

#Color node labels based on lambda model
plot(pruned_mammaltree_best,no.margin=TRUE,show.tip.label=FALSE,type="fan")
nodelabels(pch=19,col=color.scale(mammal_I_m_lam_fastAnc,extremes=c("blue","red"),xrange=c(min(mammal_log_I_m_tiporder),max(mammal_log_I_m_tiporder))))
tiplabels(pch=19,col=color.scale(mammal_log_I_m_tiporder,extremes=c("blue","red")))
color.legend(-255,-125,-155,-115,legend=c(-3.77,0.68),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")
for(i in 1:length(mammal_orderover50)){  arc.cladelabels(tree=pruned_mammaltree_best,text=mammal_ordernodes$Order[mammal_ordernodes$num.species>50][i],
                                                         mammal_orderover50[i],ln.offset = 1.03,lab.offset = 1.07,mark.node=FALSE)
}

plot(pruned_mammaltree_best,show.tip.label=FALSE,type="fan")
nodelabels(pch=19,col=color.scale(mammal_I_m_lam_fastAnc,extremes=c("blue","red"),xrange=c(min(mammal_log_I_m_tiporder),max(mammal_log_I_m_tiporder))))
tiplabels(pch=19,col=color.scale(mammal_log_I_m_tiporder,extremes=c("blue","red")))
color.legend(-255,-125,-155,-115,legend=c(-3.77,0.68),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")
for(i in 1:length(mammal_orderover40)){  arc.cladelabels(tree=pruned_mammaltree_best,text=mammal_ordernodes$Order[mammal_ordernodes$num.species>40][i],
                                                         mammal_orderover40[i],ln.offset = 1.03,lab.offset = 1.07,mark.node=FALSE,cex=0.85)
}
rasterImage(primate,-210,60,-170,100)
rasterImage(artiodactyla,-220,-70,-180,-45)
rasterImage(soricomorpha,60,-185,100,-164)
rasterImage(carnivora,-140,-165,-100,-146)
rasterImage(diprotodontia,150,-120,190,-74)
rasterImage(rodentia,110,140,150,180)

#fastAnc ancestral reconstructions for squamates

# #Body mass
# squamate_bodymass_lam_tree<-geiger::rescale(pruned_squamatetree,model="lambda", squamate_bodymass_fit.lambda$opt$lambda)
# mammal_bodymass_lam_fastAnc<-fastAnc(mammal_bodymass_lam_tree, mammal_log_bodymass_tiporder)
# 
# #Color node labels based on lambda model
# plot(pruned_mammaltree_best,no.margin=TRUE,show.tip.label=FALSE,type="fan")
# nodelabels(pch=19,col=color.scale(mammal_bodymass_lam_fastAnc,extremes=c("blue","red"),xrange = c(min(mammal_log_bodymass_tiporder),max(mammal_log_bodymass_tiporder))))
# tiplabels(pch=19,col=color.scale(mammal_log_bodymass_tiporder,extremes=c("blue","red")))
# color.legend(-265,-125,-165,-115,legend=c(0.85,18.82),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")
# for(i in 1:length(mammal_orderover40)){
#   arc.cladelabels(tree=pruned_mammaltree_best,text=mammal_ordernodes$Order[mammal_ordernodes$num.species>40][i],
#                   mammal_orderover40[i],ln.offset = 1.03,lab.offset = 1.07,mark.node=FALSE)
# }

#C*E
squamate_C_E_lam_tree<-geiger::rescale(ult_pruned_squamatetree,model="lambda", squamate_C_E_fit.lambda$opt$lambda)
squamate_C_E_lam_fastAnc<-fastAnc(squamate_C_E_lam_tree, squamate_log_C_E_tiporder)

#Color node labels based on lambda model
plot(pruned_squamatetree,no.margin=TRUE,show.tip.label=FALSE)
nodelabels(pch=19,col=color.scale(squamate_C_E_lam_fastAnc,extremes=c("blue","red"),xrange=c(min(squamate_log_C_E_tiporder),max(squamate_log_C_E_tiporder))))
tiplabels(pch=19,col=color.scale(squamate_log_C_E_tiporder,extremes=c("blue","red")))
color.legend(-0,60,40,61,legend=c(-2.44,2.42),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")


#E/alpha
squamate_E_alpha_lam_tree<-geiger::rescale(ult_pruned_squamatetree,model="lambda", squamate_E_alpha_fit.lambda$opt$lambda)
squamate_E_alpha_lam_fastAnc<-fastAnc(squamate_E_alpha_lam_tree, squamate_log_E_alpha_tiporder)

#Color node labels based on lambda model
plot(pruned_squamatetree,no.margin=TRUE,show.tip.label=FALSE)
nodelabels(pch=19,col=color.scale(squamate_E_alpha_lam_fastAnc,extremes=c("blue","red"),xrange=c(min(squamate_log_E_alpha_tiporder),max(squamate_log_E_alpha_tiporder))))
tiplabels(pch=19,col=color.scale(squamate_log_E_alpha_tiporder,extremes=c("blue","red")))
color.legend(0,60,40,61,legend=c(-0.32,3.26),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")


#I/m
mammal_I_m_lam_tree<-rescale(pruned_mammaltree_best,model="lambda", mammal_I_m_fit.lambda$opt$lambda)
mammal_I_m_lam_fastAnc<-fastAnc(mammal_I_m_lam_tree, mammal_log_I_m_tiporder)

#Color node labels based on lambda model
plot(pruned_mammaltree_best,no.margin=TRUE,show.tip.label=FALSE,type="fan")
nodelabels(pch=19,col=color.scale(mammal_I_m_lam_fastAnc,extremes=c("blue","red"),xrange=c(min(mammal_log_I_m_tiporder),max(mammal_log_I_m_tiporder))))
tiplabels(pch=19,col=color.scale(mammal_log_I_m_tiporder,extremes=c("blue","red")))
color.legend(-255,-125,-155,-115,legend=c(-3.77,0.68),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")
for(i in 1:length(mammal_orderover50)){  arc.cladelabels(tree=pruned_mammaltree_best,text=mammal_ordernodes$Order[mammal_ordernodes$num.species>50][i],
                                                         mammal_orderover50[i],ln.offset = 1.03,lab.offset = 1.07,mark.node=FALSE)
}



# Invariance Assessment and PGLS ------------------------------------------


#C*E and body mass

#Mammals
#linear model
summary(lm(log_C_E~log_bodymass,data=completecase_am))
summary(lm(as.numeric(log_C_E)~as.numeric(log_bodymass),data = as.data.frame(mammaltraitmatrix)))
#PGLS
#Use Pagel's lambda model
summary(gls(mammal_log_C_E_tiporder~mammal_log_bodymass_tiporder,
            correlation = corPagel(value=0.95,phy=pruned_mammaltree_di),data = mammal_phylo_order_traits,method = "ML"))
#summary(gls(mammal_log_C_E_tiporder~mammal_log_bodymass_tiporder,correlation = corBrownian(phy=pruned_mammaltree_di),data = mammal_phylo_order_traits,method = "ML"))

#Birds
#linear model
summary(lm(as.numeric(log_C_E)~as.numeric(log_bodymass),data = as.data.frame(birdtraitmatrix)))
#PGLS
#Use Brownian motion
summary(gls(bird_log_C_E_tiporder~bird_log_bodymass_tiporder,correlation = corBrownian(phy=pruned_birdtree1),data=bird_phylo_order_traits,method="ML"))

#Reptiles
#linear model
summary(lm(log_C_E~log_bodymass,data = as.data.frame(reptiletraitmatrix)))

#Squamates
#linear model
summary(lm(log_C_E~log_bodymass,data = as.data.frame(reptiletraitmatrix[rownames(reptiletraitmatrix)%in%pruned_squamatetree$tip.label,])))
#PGLS
#Use Kappa model
rownames(squamate_phylo_order_traits)<-squamate_phylo_order_traits$taxaname
squamate_phylo_order_traits_compdata<-comparative.data(pruned_squamatetree,squamate_phylo_order_traits,names.col = 'taxaname')
summary(pgls(squamate_log_C_E_tiporder~squamate_log_bodymass_tiporder, data = squamate_phylo_order_traits_compdata,kappa='ML'))

#summary(gls(squamate_log_C_E_tiporder~squamate_log_bodymass_tiporder,correlation = corBrownian(phy=pruned_squamatetree),data=squamate_phylo_order_traits,method="ML"))

#Amphibians
#linear model
summary(lm(log_C_E~log_bodymass,data = as.data.frame(amphibiantraitmatrix)))
#PGLS
#Use OU model
summary(gls(amphibian_log_C_E_tiporder~amphibian_log_bodymass_tiporder,
            correlation = corMartins(value = 0.5,phy=pruned_amphibiantree),data=amphibian_phylo_order_traits,method="ML"))

#summary(gls(amphibian_log_C_E_tiporder~amphibian_log_bodymass_tiporder,correlation = corBrownian(phy=pruned_amphibiantree),data=amphibian_phylo_order_traits,method="ML"))


plot(log_C_E~log_bodymass,data = completecase_am[completecase_am$class=="Mammalia",],col=alpha(brewer.pal(n=3,"Set1")[2],0.7),pch=19,
     xlab="Log(Body Mass)",ylab="Log(LRE)",xlim=c(-0.5,20),ylim=c(-8,6))
points(log_C_E~log_bodymass,data = completecase_am[completecase_am$class=="Aves",],col=alpha(brewer.pal(n=3,"Set1")[1],0.7),pch=19)
points(log_C_E~log_bodymass,data = completecase_am[completecase_am$class=="Reptilia",],col=alpha(brewer.pal(n=3,"Set1")[3],0.7),pch=19)
points(log_C_E~log_bodymass,data = completecase_am[completecase_am$class=="Amphibia",],col=alpha(brewer.pal(n=4, "Set1")[4],0.7),pch=19)

abline(lm(log_C_E~log_bodymass,data = completecase_am[completecase_am$class=="Mammalia",]),col=brewer.pal(n=3,"Set1")[2])
abline(lm(log_C_E~log_bodymass,data = completecase_am[completecase_am$class=="Aves",]),col=brewer.pal(n=3,"Set1")[1])
abline(lm(log_C_E~log_bodymass,data = completecase_am[completecase_am$class=="Reptilia",]),col=brewer.pal(n=3,"Set1")[3])
abline(lm(log_C_E~log_bodymass,data = completecase_am[completecase_am$class=="Amphibia",]),col=brewer.pal(n=4,"Set1")[4])

legend(legend = c("Aves","Mammalia","Reptilia","Amphibia"),col = brewer.pal(n=4,"Set1"),pch=19,"bottomright")


#E/alpha and body mass

#Mammals
#linear model
summary(lm(log_E_alpha~log_bodymass,data = as.data.frame(mammaltraitmatrix)))
#PGLS
#Use Pagel's lambda model
summary(gls(mammal_log_E_alpha_tiporder~mammal_log_bodymass_tiporder,
            correlation = corPagel(value=0.95,phy=pruned_mammaltree_di),data = mammal_phylo_order_traits,method = "ML"))

#summary(gls(mammal_log_E_alpha_tiporder~mammal_log_bodymass_tiporder,correlation = corBrownian(phy=pruned_mammaltree_di),data = mammal_phylo_order_traits,method = "ML"))

#Birds
#linear model
summary(lm(log_E_alpha~log_bodymass,data = as.data.frame(birdtraitmatrix)))
#PGLS
summary(gls(bird_log_E_alpha_tiporder~bird_log_bodymass_tiporder,correlation = corBrownian(phy=pruned_birdtree1),data = bird_phylo_order_traits,method = "ML"))


#Reptiles
#linear model
summary(lm(log_E_alpha~log_bodymass,data = as.data.frame(reptiletraitmatrix)))
#Squamates
#linear model
summary(lm(log_E_alpha~log_bodymass,data = as.data.frame(reptiletraitmatrix[rownames(reptiletraitmatrix)%in%pruned_squamatetree$tip.label,])))
#PGLS
summary(gls(squamate_log_E_alpha_tiporder~squamate_log_bodymass_tiporder,correlation = corBrownian(phy=pruned_squamatetree),data = squamate_phylo_order_traits,method = "ML"))

#Amphibians
summary(gls(amphibian_log_E_alpha_tiporder~amphibian_log_bodymass_tiporder,correlation = corBrownian(phy=pruned_amphibiantree),data=amphibian_phylo_order_traits,method="ML"))



plot(log_E_alpha~log_bodymass,data = completecase_am[completecase_am$class=="Mammalia",],col=alpha(brewer.pal(n=3,"Set1")[2],0.7),pch=19,
     xlab="Log(Body Mass)",ylab="Log(RRL)",xlim=c(-0.5,20),ylim=c(-3,4.5))
points(log_E_alpha~log_bodymass,data = completecase_am[completecase_am$class=="Aves",],col=alpha(brewer.pal(n=3,"Set1")[1],0.7),pch=19)
points(log_E_alpha~log_bodymass,data = completecase_am[completecase_am$class=="Reptilia",],col=alpha(brewer.pal(n=3,"Set1")[3],0.7),pch=19)
points(log_E_alpha~log_bodymass,data = completecase_am[completecase_am$class=="Amphibia",],col=alpha(brewer.pal(n=4, "Set1")[4],0.7),pch=19)

abline(lm(log_E_alpha~log_bodymass,data = completecase_am[completecase_am$class=="Mammalia",]),col=brewer.pal(n=3,"Set1")[2])
abline(lm(log_E_alpha~log_bodymass,data = completecase_am[completecase_am$class=="Aves",]),col=brewer.pal(n=3,"Set1")[1])
abline(lm(log_E_alpha~log_bodymass,data = completecase_am[completecase_am$class=="Reptilia",]),col=brewer.pal(n=3,"Set1")[3])
abline(lm(log_E_alpha~log_bodymass,data = completecase_am[completecase_am$class=="Amphibia",]),col=brewer.pal(n=4,"Set1")[4])

legend(legend = c("Aves","Mammalia","Reptilia","Amphibia"),col = brewer.pal(n=4,"Set1"),pch=19,"bottomright")



#I/m and body mass
#Mammals
#linear model
summary(lm(log_I_m~log_bodymass,data = as.data.frame(mammaltraitmatrix)))
#PGLS
summary(gls(mammal_log_I_m_tiporder~mammal_log_bodymass_tiporder,correlation = corBrownian(phy=pruned_mammaltree_di),data = mammal_phylo_order_traits,method = "ML"))


#Birds
#linear model
summary(lm(log_I_m~log_bodymass,data = as.data.frame(birdtraitmatrix)))
#PGLS
summary(gls(bird_log_I_m_tiporder~bird_log_bodymass_tiporder,correlation = corBrownian(phy=pruned_birdtree1),data = bird_phylo_order_traits,method = "ML"))


#Reptiles
#linear model
summary(lm(log_I_m~log_bodymass,data = as.data.frame(reptiletraitmatrix)))
#Squamates
#linear model
summary(lm(log_I_m~log_bodymass,data = as.data.frame(reptiletraitmatrix[rownames(reptiletraitmatrix)%in%pruned_squamatetree$tip.label,])))
#PGLS
summary(gls(squamate_log_I_m_tiporder~squamate_log_bodymass_tiporder,correlation = corBrownian(phy=pruned_squamatetree),data = squamate_phylo_order_traits,method = "ML"))

#Amphibians
summary(gls(amphibian_log_I_m_tiporder~amphibian_log_bodymass_tiporder,correlation = corBrownian(phy=pruned_amphibiantree),data=amphibian_phylo_order_traits,method="ML"))



plot(log_I_m~log_bodymass,data = completecase_am[completecase_am$class=="Mammalia",],col=alpha(brewer.pal(n=3,"Set1")[2],0.7),pch=19,
     xlab="Log(Body Mass)",ylab="Log(ROS)",xlim=c(-0.5,20),ylim=c(-16,0.5))
points(log_I_m~log_bodymass,data = completecase_am[completecase_am$class=="Aves",],col=alpha(brewer.pal(n=3,"Set1")[1],0.7),pch=19)
points(log_I_m~log_bodymass,data = completecase_am[completecase_am$class=="Reptilia",],col=alpha(brewer.pal(n=3,"Set1")[3],0.7),pch=19)
points(log_I_m~log_bodymass,data = completecase_am[completecase_am$class=="Amphibia",],col=alpha(brewer.pal(n=4, "Set1")[4],0.7),pch=19)

abline(lm(log_I_m~log_bodymass,data = completecase_am[completecase_am$class=="Mammalia",]),col=brewer.pal(n=3,"Set1")[2])
abline(lm(log_I_m~log_bodymass,data = completecase_am[completecase_am$class=="Aves",]),col=brewer.pal(n=3,"Set1")[1])
abline(lm(log_I_m~log_bodymass,data = completecase_am[completecase_am$class=="Reptilia",]),col=brewer.pal(n=3,"Set1")[3])
abline(lm(log_I_m~log_bodymass,data = completecase_am[completecase_am$class=="Amphibia",]),col=brewer.pal(n=4,"Set1")[4])

legend(legend = c("Aves","Mammalia","Reptilia","Amphibia"),col = brewer.pal(n=4,"Set1"),pch=19,"bottomright")


# Wright PGLS Code --------------------------------------------------------

##Amphibians

#create correlation matrices for each model of trait evolution 
amphibian.bm <- corBrownian(phy = pruned_amphibiantree)
amphibian.ou <- corMartins(0, phy = pruned_amphibiantree)
amphibian.pa <- corPagel(1, phy = pruned_amphibiantree)

# create a function that runs a null model of the trait of interest evolving along the tree
f <- function(cs) gls(amphibian_log_bodymass_tiporder ~ 1, data = amphibian_phylo_order_traits, correlation = cs) 

# run the above function on each different correlation structure/model of trait evolution (plus a null model with no correlation structure)
amphibian.fit <- lapply(list(NULL, amphibian.bm, amphibian.pa, amphibian.ou), f)

# extract the AIC values for each 
sapply(amphibian.fit, AIC) #OU model fits best

summary(gls(amphibian_log_C_E_tiporder ~ amphibian_log_bodymass_tiporder, correlation=amphibian.ou, data=amphibian_phylo_order_traits))
summary(gls(amphibian_log_E_alpha_tiporder ~ amphibian_log_bodymass_tiporder, correlation=amphibian.ou, data=amphibian_phylo_order_traits))
summary(gls(amphibian_log_I_m_tiporder ~ amphibian_log_bodymass_tiporder, correlation=amphibian.ou, data=amphibian_phylo_order_traits))

##Squamates

#create correlation matrices for each model of trait evolution 
squamate.bm <- corBrownian(phy = pruned_squamatetree)
squamate.ou <- corMartins(0, phy = pruned_squamatetree)
squamate.pa <- corPagel(1, phy = pruned_squamatetree)

# create a function that runs a null model of the trait of interest evolving along the tree
f <- function(cs) gls(squamate_log_bodymass_tiporder ~ 1, data = squamate_phylo_order_traits, correlation = cs) 

# run the above function on each different correlation structure/model of trait evolution (plus a null model with no correlation structure)
squamate.fit <- lapply(list(NULL, squamate.bm, squamate.pa, squamate.ou), f)

# extract the AIC values for each 
sapply(squamate.fit, AIC) #OU model fits best

summary(gls(squamate_log_C_E_tiporder ~ squamate_log_bodymass_tiporder, correlation=squamate.ou, data=squamate_phylo_order_traits))
summary(gls(squamate_log_E_alpha_tiporder ~ squamate_log_bodymass_tiporder, correlation=squamate.ou, data=squamate_phylo_order_traits))
summary(gls(squamate_log_I_m_tiporder ~ squamate_log_bodymass_tiporder, correlation=squamate.ou, data=squamate_phylo_order_traits))

##Mammals

#create correlation matrices for each model of trait evolution 
mammal.bm <- corBrownian(phy = pruned_mammaltree_best)
mammal.ou <- corMartins(0, phy = pruned_mammaltree_best)
mammal.pa <- corPagel(1, phy = pruned_mammaltree_best)

# create a function that runs a null model of the trait of interest evolving along the tree
f <- function(cs) gls(mammal_log_bodymass_tiporder ~ 1, data = mammal_phylo_order_traits, correlation = cs) 

# run the above function on each different correlation structure/model of trait evolution (plus a null model with no correlation structure)
mammal.fit <- lapply(list(NULL, mammal.bm, mammal.pa, mammal.ou), f)

# extract the AIC values for each 
sapply(mammal.fit, AIC) #Pagel model fits best

summary(gls(mammal_log_C_E_tiporder ~ mammal_log_bodymass_tiporder, correlation=mammal.pa, data=mammal_phylo_order_traits))
summary(gls(mammal_log_E_alpha_tiporder ~ mammal_log_bodymass_tiporder, correlation=mammal.pa, data=mammal_phylo_order_traits))
summary(gls(mammal_log_I_m_tiporder ~ mammal_log_bodymass_tiporder, correlation=mammal.pa, data=mammal_phylo_order_traits))

##Birds

#create correlation matrices for each model of trait evolution 
bird.bm <- corBrownian(phy = pruned_birdtree1)
bird.ou <- corMartins(0, phy = pruned_birdtree1)
bird.pa <- corPagel(1, phy = pruned_birdtree1)

# create a function that runs a null model of the trait of interest evolving along the tree
f <- function(cs) gls(bird_log_bodymass_tiporder ~ 1, data = bird_phylo_order_traits, correlation = cs) 

# run the above function on each different correlation structure/model of trait evolution (plus a null model with no correlation structure)
bird.fit <- lapply(list(NULL, bird.bm, bird.pa, bird.ou), f)

# extract the AIC values for each 
sapply(bird.fit, AIC) #Pagel model fits best

summary(gls(bird_log_C_E_tiporder ~ bird_log_bodymass_tiporder, correlation=bird.pa, data=bird_phylo_order_traits))
summary(gls(bird_log_E_alpha_tiporder ~ bird_log_bodymass_tiporder, correlation=bird.pa, data=bird_phylo_order_traits))
summary(gls(bird_log_I_m_tiporder ~ bird_log_bodymass_tiporder, correlation=bird.pa, data=bird_phylo_order_traits))


# Invariance Calculations -------------------------------------------------

#C*E:
#Linear model
summary(lm(log_C_E~log_bodymass, data = completecase_am))
summary(lm(log_C_E~log_bodymass,data=completecase_am[completecase_am$class=="Amphibia",]))
summary(lm(log_C_E~log_bodymass,data=completecase_am[completecase_am$class=="Reptilia",]))
summary(lm(log_C_E~log_bodymass,data=completecase_am[completecase_am$class=="Mammalia",]))
summary(lm(log_C_E~log_bodymass,data=completecase_am[completecase_am$class=="Aves",]))
#Ratio of variances
var(completecase_am$log_C_E)/var(completecase_am$log_bodymass)
var(completecase_am[completecase_am$class=="Amphibia",]$log_C_E)/var(completecase_am[completecase_am$class=="Amphibia",]$log_bodymass)
var(completecase_am[completecase_am$class=="Reptilia",]$log_C_E)/var(completecase_am[completecase_am$class=="Reptilia",]$log_bodymass)
var(completecase_am[completecase_am$class=="Mammalia",]$log_C_E)/var(completecase_am[completecase_am$class=="Mammalia",]$log_bodymass)
var(completecase_am[completecase_am$class=="Aves",]$log_C_E)/var(completecase_am[completecase_am$class=="Aves",]$log_bodymass)
#Isometric Variation
plot(log(C)~log(1/maximum_longevity_y),data=completecase_am)
summary(lm(log(C)~log(1/maximum_longevity_y),data=completecase_am))
confint(lm(log(C)~log(1/maximum_longevity_y),data=completecase_am))
summary(lm(log(C)~log(1/maximum_longevity_y),data=completecase_am[completecase_am$class=="Amphibia",]))
confint(lm(log(C)~log(1/maximum_longevity_y),data=completecase_am[completecase_am$class=="Amphibia",]))
summary(lm(log(C)~log(1/maximum_longevity_y),data=completecase_am[completecase_am$class=="Reptilia",]))
confint(lm(log(C)~log(1/maximum_longevity_y),data=completecase_am[completecase_am$class=="Reptilia",]))
summary(lm(log(C)~log(1/maximum_longevity_y),data=completecase_am[completecase_am$class=="Mammalia",]))
confint(lm(log(C)~log(1/maximum_longevity_y),data=completecase_am[completecase_am$class=="Mammalia",]))
summary(lm(log(C)~log(1/maximum_longevity_y),data=completecase_am[completecase_am$class=="Aves",]))
confint(lm(log(C)~log(1/maximum_longevity_y),data=completecase_am[completecase_am$class=="Aves",]))

#E/alpha:
#Linear model
summary(lm(log_E_alpha~log_bodymass,data=completecase_am))
summary(lm(log_E_alpha~log_bodymass,data=completecase_am[completecase_am$class=="Amphibia",]))
summary(lm(log_E_alpha~log_bodymass,data=completecase_am[completecase_am$class=="Reptilia",]))
summary(lm(log_E_alpha~log_bodymass,data=completecase_am[completecase_am$class=="Mammalia",]))
summary(lm(log_E_alpha~log_bodymass,data=completecase_am[completecase_am$class=="Aves",]))
#Ratio of variances
var(completecase_am$log_E_alpha)/var(completecase_am$log_bodymass)
var(completecase_am[completecase_am$class=="Amphibia",]$log_E_alpha)/var(completecase_am[completecase_am$class=="Amphibia",]$log_bodymass)
var(completecase_am[completecase_am$class=="Reptilia",]$log_E_alpha)/var(completecase_am[completecase_am$class=="Reptilia",]$log_bodymass)
var(completecase_am[completecase_am$class=="Mammalia",]$log_E_alpha)/var(completecase_am[completecase_am$class=="Mammalia",]$log_bodymass)
var(completecase_am[completecase_am$class=="Aves",]$log_E_alpha)/var(completecase_am[completecase_am$class=="Aves",]$log_bodymass)
#Isometric variation
plot(log(maximum_longevity_y*365)~log(female_maturity_d),data=completecase_am)
summary(lm(log(maximum_longevity_y*365)~log(female_maturity_d),data=completecase_am))
confint(lm(log(maximum_longevity_y*365)~log(female_maturity_d),data=completecase_am))
summary(lm(log(maximum_longevity_y*365)~log(female_maturity_d),data=completecase_am[completecase_am$class=="Amphibia",]))
confint(lm(log(maximum_longevity_y*365)~log(female_maturity_d),data=completecase_am[completecase_am$class=="Amphibia",]))
summary(lm(log(maximum_longevity_y*365)~log(female_maturity_d),data=completecase_am[completecase_am$class=="Reptilia",]))
confint(lm(log(maximum_longevity_y*365)~log(female_maturity_d),data=completecase_am[completecase_am$class=="Reptilia",]))
summary(lm(log(maximum_longevity_y*365)~log(female_maturity_d),data=completecase_am[completecase_am$class=="Mammalia",]))
confint(lm(log(maximum_longevity_y*365)~log(female_maturity_d),data=completecase_am[completecase_am$class=="Mammalia",]))
summary(lm(log(maximum_longevity_y*365)~log(female_maturity_d),data=completecase_am[completecase_am$class=="Aves",]))
confint(lm(log(maximum_longevity_y*365)~log(female_maturity_d),data=completecase_am[completecase_am$class=="Aves",]))

#I/m:
#Linear model
summary(lm(log_I_m~log_bodymass,data=completecase_am))
summary(lm(log_I_m~log_bodymass,data=completecase_am[completecase_am$class=="Amphibia",]))
summary(lm(log_I_m~log_bodymass,data=completecase_am[completecase_am$class=="Reptilia",]))
summary(lm(log_I_m~log_bodymass,data=completecase_am[completecase_am$class=="Mammalia",]))
summary(lm(log_I_m~log_bodymass,data=completecase_am[completecase_am$class=="Aves",]))
#Ratio of variances
var(completecase_am$log_I_m)/var(completecase_am$log_bodymass)
var(completecase_am[completecase_am$class=="Amphibia",]$log_I_m)/var(completecase_am[completecase_am$class=="Amphibia",]$log_bodymass)
var(completecase_am[completecase_am$class=="Reptilia",]$log_I_m)/var(completecase_am[completecase_am$class=="Reptilia",]$log_bodymass)
var(completecase_am[completecase_am$class=="Mammalia",]$log_I_m)/var(completecase_am[completecase_am$class=="Mammalia",]$log_bodymass)
var(completecase_am[completecase_am$class=="Aves",]$log_I_m)/var(completecase_am[completecase_am$class=="Aves",]$log_bodymass)
#Isometric variation
plot(log(I)~log_bodymass,data=completecase_am)
points(log(I)~log_bodymass,data=completecase_am[completecase_am$class=="Reptilia",],col="red")
points(log(I)~log_bodymass,data=completecase_am[completecase_am$class=="Amphibia",],col="green")
summary(lm(log(I)~log_bodymass,data=completecase_am))
confint(lm(log(I)~log_bodymass,data=completecase_am))
summary(lm(log(I)~log_bodymass,data=completecase_am[completecase_am$class=="Amphibia",]))
confint(lm(log(I)~log_bodymass,data=completecase_am[completecase_am$class=="Amphibia",]))
summary(lm(log(I)~log_bodymass,data=completecase_am[completecase_am$class=="Reptilia",]))
confint(lm(log(I)~log_bodymass,data=completecase_am[completecase_am$class=="Reptilia",]))
summary(lm(log(I)~log_bodymass,data=completecase_am[completecase_am$class=="Mammalia",]))
confint(lm(log(I)~log_bodymass,data=completecase_am[completecase_am$class=="Mammalia",]))
summary(lm(log(I)~log_bodymass,data=completecase_am[completecase_am$class=="Aves",]))
confint(lm(log(I)~log_bodymass,data=completecase_am[completecase_am$class=="Aves",]))


# C*E vs. I/m -------------------------------------------------------------

xyplot(log_C_E~log_I_m,data = completecase_am, type = c("p","r"))
summary(lm(log_C_E~log_I_m,data = completecase_am))

xyplot(C_E~I_m,data = completecase_am, type = c("p"))
summary(lm(log_C_E~log_I_m,data = completecase_am))

#Mammals
#linear model
summary(lm(log_C_E~log_I_m,data = as.data.frame(mammaltraitmatrix)))
#PGLS
summary(gls(mammal_log_C_E_tiporder~mammal_log_bodymass_tiporder,correlation = corBrownian(phy=pruned_mammaltree_di),data = mammal_phylo_order_traits,method = "ML"))


#95% confidence intervals for linear models
confint(lm(log_C_E~log_I_m,data = completecase_am[completecase_am$class=="Aves",]))
confint(lm(log_C_E~log_I_m,data = completecase_am[completecase_am$class=="Mammalia",]))
confint(lm(log_C_E~log_I_m,data = completecase_am[completecase_am$class=="Reptilia",]))
confint(lm(log_C_E~log_I_m,data = completecase_am[completecase_am$class=="Amphibia",]))
#for endotherm and ectotherm models
confint(lm(log_C_E~log_I_m, data = completecase_am[completecase_am$class=="Amphibia" | completecase_am$class=="Reptilia",]),col="green")
confint(lm(log_C_E~log_I_m, data = completecase_am[completecase_am$class=="Mammalia" | completecase_am$class=="Aves",]),col="red")



plot(log_C_E~log_I_m,data = completecase_am[completecase_am$class=="Mammalia",],col=alpha(brewer.pal(n=3,"Set1")[2],0.7),pch=19,
     xlab="Log(I/m)",ylab="Log(C*E)",xlim=c(-16,1),ylim=c(-7,7))
points(log_C_E~log_I_m,data = completecase_am[completecase_am$class=="Aves",],col=alpha(brewer.pal(n=3,"Set1")[1],0.7),pch=19)
points(log_C_E~log_I_m,data = completecase_am[completecase_am$class=="Reptilia",],col=alpha(brewer.pal(n=3,"Set1")[3],0.7),pch=19)
points(log_C_E~log_I_m,data = completecase_am[completecase_am$class=="Amphibia",],col=alpha(brewer.pal(n=4, "Set1")[4],0.7),pch=19)

abline(lm(log_C_E~log_I_m,data = completecase_am[completecase_am$class=="Mammalia",]),col=brewer.pal(n=3,"Set1")[2])
abline(lm(log_C_E~log_I_m,data = completecase_am[completecase_am$class=="Aves",]),col=brewer.pal(n=3,"Set1")[1])
abline(lm(log_C_E~log_I_m,data = completecase_am[completecase_am$class=="Reptilia",]),col=brewer.pal(n=3,"Set1")[3])
abline(lm(log_C_E~log_I_m,data = completecase_am[completecase_am$class=="Amphibia",]),col=brewer.pal(n=4,"Set1")[4])

abline(lm(log_C_E~log_I_m, data = completecase_am[completecase_am$class=="Amphibia" | completecase_am$class=="Reptilia",]),col="green")
abline(lm(log_C_E~log_I_m, data = completecase_am[completecase_am$class=="Mammalia" | completecase_am$class=="Aves",]),col="red")



# Pairwise Components of Invariants ---------------------------------------

#C and E

#Mammals
#linear model
summary(lm(log(C)~log(E),data = as.data.frame(mammalcompmatrix)))
#PGLS
summary(gls(log(C)~log(E),correlation = corBrownian(phy=pruned_mammaltree_di),data = as.data.frame(mammalcompmatrix),method = "ML"))

#Birds
#linear model
summary(lm(log(C)~log(E),data = as.data.frame(birdcompmatrix)))
#PGLS
summary(gls(log(C)~log(E),correlation = corBrownian(phy=pruned_birdtree1),data = as.data.frame(birdcompmatrix),method = "ML"))


#Reptiles
#linear model
summary(lm(log(C)~log(E),data = as.data.frame(reptilecompmatrix)))
#Squamates
#linear model
summary(lm(log(C)~log(E),data = as.data.frame(reptilecompmatrix[rownames(reptilecompmatrix)%in%pruned_squamatetree$tip.label,])))
#PGLS
summary(gls(log(C)~log(E),correlation = corBrownian(phy=pruned_squamatetree),data=as.data.frame(reptilecompmatrix[rownames(reptilecompmatrix)%in%pruned_squamatetree$tip.label,]),method="ML"))


plot(log(C)~log(E),data = as.data.frame(mammalcompmatrix),col=brewer.pal(n=3,"Set1")[2])
points(log(C)~log(E),data = as.data.frame(birdcompmatrix),col=brewer.pal(n=3,"Set1")[1])
points(log(C)~log(E),data = as.data.frame(reptilecompmatrix),col=brewer.pal(n=3,"Set1")[3])


abline(lm(log(C)~log(E),data = as.data.frame(mammalcompmatrix)),col=brewer.pal(n=3,"Set1")[2])
abline(lm(log(C)~log(E),data = as.data.frame(birdcompmatrix)),col=brewer.pal(n=3,"Set1")[1])
abline(lm(log(C)~log(E),data = as.data.frame(reptilecompmatrix)),col=brewer.pal(n=3,"Set1")[3])



#E and alpha

#Mammals
#linear model
summary(lm(log(E)~log(alpha),data = as.data.frame(mammalcompmatrix)))
#PGLS
summary(gls(log(E)~log(alpha),correlation = corBrownian(phy=pruned_mammaltree_di),data = as.data.frame(mammalcompmatrix),method = "ML"))

#Birds
#linear model
summary(lm(log(E)~log(alpha),data = as.data.frame(birdcompmatrix)))
#PGLS
summary(gls(log(E)~log(alpha),correlation = corBrownian(phy=pruned_birdtree1),data = as.data.frame(birdcompmatrix),method = "ML"))


#Reptiles
#linear model
summary(lm(log(E)~log(alpha),data = as.data.frame(reptilecompmatrix)))
#Squamates
#linear model
summary(lm(log(E)~log(alpha),data = as.data.frame(reptilecompmatrix[rownames(reptilecompmatrix)%in%pruned_squamatetree$tip.label,])))
#PGLS
summary(gls(log(E)~log(alpha),correlation = corBrownian(phy=pruned_squamatetree),data=as.data.frame(reptilecompmatrix[rownames(reptilecompmatrix)%in%pruned_squamatetree$tip.label,]),method="ML"))


plot(log(E)~log(alpha),data = as.data.frame(mammalcompmatrix),xlim=c(3,9.5),col=brewer.pal(n=3,"Set1")[2])
points(log(E)~log(alpha),data = as.data.frame(birdcompmatrix),col=brewer.pal(n=3,"Set1")[1])
points(log(E)~log(alpha),data = as.data.frame(reptilecompmatrix),col=brewer.pal(n=3,"Set1")[3])


abline(lm(log(E)~log(alpha),data = as.data.frame(mammalcompmatrix)),col=brewer.pal(n=3,"Set1")[2])
abline(lm(log(E)~log(alpha),data = as.data.frame(birdcompmatrix)),col=brewer.pal(n=3,"Set1")[1])
abline(lm(log(E)~log(alpha),data = as.data.frame(reptilecompmatrix)),col=brewer.pal(n=3,"Set1")[3])


#I and m

#Mammals
#linear model
summary(lm(log(I)~log(m),data = as.data.frame(mammalcompmatrix)))
#PGLS
summary(gls(log(I)~log(m),correlation = corBrownian(phy=pruned_mammaltree_di),data = as.data.frame(mammalcompmatrix),method = "ML"))

#Birds
#linear model
summary(lm(log(I)~log(m),data = as.data.frame(birdcompmatrix)))
#PGLS
summary(gls(log(I)~log(m),correlation = corBrownian(phy=pruned_birdtree1),data = as.data.frame(birdcompmatrix),method = "ML"))

#Reptiles
#linear model
summary(lm(log(I)~log(m),data = as.data.frame(reptilecompmatrix)))
#Squamates
#linear model
summary(lm(log(I)~log(m),data = as.data.frame(reptilecompmatrix[rownames(reptilecompmatrix)%in%pruned_squamatetree$tip.label,])))
#PGLS
summary(gls(log(I)~log(m),correlation = corBrownian(phy=pruned_squamatetree),data=as.data.frame(reptilecompmatrix[rownames(reptilecompmatrix)%in%pruned_squamatetree$tip.label,]),method="ML"))


plot(log(I)~log(m),data = as.data.frame(mammalcompmatrix),ylim=c(-2.5,16),col=brewer.pal(n=3,"Set1")[2])
points(log(I)~log(m),data = as.data.frame(birdcompmatrix),col=brewer.pal(n=3,"Set1")[1])
points(log(I)~log(m),data = as.data.frame(reptilecompmatrix),col=brewer.pal(n=3,"Set1")[3])


abline(lm(log(I)~log(m),data = as.data.frame(mammalcompmatrix)),col=brewer.pal(n=3,"Set1")[2])
abline(lm(log(I)~log(m),data = as.data.frame(birdcompmatrix)),col=brewer.pal(n=3,"Set1")[1])
abline(lm(log(I)~log(m),data = as.data.frame(reptilecompmatrix)),col=brewer.pal(n=3,"Set1")[3])


# Components of Invariance vs. Mass ---------------------------------------

#C and m

#Mammals
#linear model
summary(lm(log(C)~log(m),data = as.data.frame(mammalcompmatrix)))
#PGLS
summary(gls(log(C)~log(m),correlation = corBrownian(phy=pruned_mammaltree_di),data = as.data.frame(mammalcompmatrix),method = "ML"))

#Birds
#linear model
summary(lm(log(C)~log(m),data = as.data.frame(birdcompmatrix)))
#PGLS
summary(gls(log(C)~log(m),correlation = corBrownian(phy=pruned_birdtree1),data = as.data.frame(birdcompmatrix),method = "ML"))


#Reptiles
#linear model
summary(lm(log(C)~log(m),data = as.data.frame(reptilecompmatrix)))
#Squamates
#linear model
summary(lm(log(C)~log(m),data = as.data.frame(reptilecompmatrix[rownames(reptilecompmatrix)%in%pruned_squamatetree$tip.label,])))
#PGLS
summary(gls(log(C)~log(m),correlation = corBrownian(phy=pruned_squamatetree),data=as.data.frame(reptilecompmatrix[rownames(reptilecompmatrix)%in%pruned_squamatetree$tip.label,]),method="ML"))


plot(log(C)~log(m),data = as.data.frame(mammalcompmatrix),ylim=c(-4.5,3),col=brewer.pal(n=3,"Set1")[2])
points(log(C)~log(m),data = as.data.frame(birdcompmatrix),col=brewer.pal(n=3,"Set1")[1])
points(log(C)~log(m),data = as.data.frame(reptilecompmatrix),col=brewer.pal(n=3,"Set1")[3])


abline(lm(log(C)~log(m),data = as.data.frame(mammalcompmatrix)),col=brewer.pal(n=3,"Set1")[2])
abline(lm(log(C)~log(m),data = as.data.frame(birdcompmatrix)),col=brewer.pal(n=3,"Set1")[1])
abline(lm(log(C)~log(m),data = as.data.frame(reptilecompmatrix)),col=brewer.pal(n=3,"Set1")[3])


#alpha and m

#Mammals
#linear model
summary(lm(log(alpha)~log(m),data = as.data.frame(mammalcompmatrix)))
#PGLS
summary(gls(log(alpha)~log(m),correlation = corBrownian(phy=pruned_mammaltree_di),data = as.data.frame(mammalcompmatrix),method = "ML"))

#Birds
#linear model
summary(lm(log(alpha)~log(m),data = as.data.frame(birdcompmatrix)))
#PGLS
summary(gls(log(alpha)~log(m),correlation = corBrownian(phy=pruned_birdtree1),data = as.data.frame(birdcompmatrix),method = "ML"))

#Reptiles
#linear model
summary(lm(log(alpha)~log(m),data = as.data.frame(reptilecompmatrix)))
#Squamates
#linear model
summary(lm(log(alpha)~log(m),data = as.data.frame(reptilecompmatrix[rownames(reptilecompmatrix)%in%pruned_squamatetree$tip.label,])))
#PGLS
summary(gls(log(alpha)~log(m),correlation = corBrownian(phy=pruned_squamatetree),data=as.data.frame(reptilecompmatrix[rownames(reptilecompmatrix)%in%pruned_squamatetree$tip.label,]),method="ML"))


plot(log(alpha)~log(m),data = as.data.frame(mammalcompmatrix),ylim=c(3,9),col=brewer.pal(n=3,"Set1")[2])
points(log(alpha)~log(m),data = as.data.frame(birdcompmatrix),col=brewer.pal(n=3,"Set1")[1])
points(log(alpha)~log(m),data = as.data.frame(reptilecompmatrix),col=brewer.pal(n=3,"Set1")[3])


abline(lm(log(alpha)~log(m),data = as.data.frame(mammalcompmatrix)),col=brewer.pal(n=3,"Set1")[2])
abline(lm(log(alpha)~log(m),data = as.data.frame(birdcompmatrix)),col=brewer.pal(n=3,"Set1")[1])
abline(lm(log(alpha)~log(m),data = as.data.frame(reptilecompmatrix)),col=brewer.pal(n=3,"Set1")[3])



#E and m

#Mammals
#linear model
summary(lm(log(E)~log(m),data = as.data.frame(mammalcompmatrix)))
#PGLS
summary(gls(log(E)~log(m),correlation = corBrownian(phy=pruned_mammaltree_di),data = as.data.frame(mammalcompmatrix),method = "ML"))

#Birds
#linear model
summary(lm(log(E)~log(m),data = as.data.frame(birdcompmatrix)))
#PGLS
summary(gls(log(E)~log(m),correlation = corBrownian(phy=pruned_birdtree1),data = as.data.frame(birdcompmatrix),method = "ML"))

#Reptiles
#linear model
summary(lm(log(E)~log(m),data = as.data.frame(reptilecompmatrix)))
#linear model
summary(lm(log(E)~log(m),data = as.data.frame(reptilecompmatrix[rownames(reptilecompmatrix)%in%pruned_squamatetree$tip.label,])))
#PGLS
summary(gls(log(E)~log(m),correlation = corBrownian(phy=pruned_squamatetree),data=as.data.frame(reptilecompmatrix[rownames(reptilecompmatrix)%in%pruned_squamatetree$tip.label,]),method="ML"))


plot(log(E)~log(m),data = as.data.frame(mammalcompmatrix),ylim=c(-2.5,5),col=brewer.pal(n=3,"Set1")[2])
points(log(E)~log(m),data = as.data.frame(birdcompmatrix),col=brewer.pal(n=3,"Set1")[1])
points(log(E)~log(m),data = as.data.frame(reptilecompmatrix),col=brewer.pal(n=3,"Set1")[3])


abline(lm(log(E)~log(m),data = as.data.frame(mammalcompmatrix)),col=brewer.pal(n=3,"Set1")[2])
abline(lm(log(E)~log(m),data = as.data.frame(birdcompmatrix)),col=brewer.pal(n=3,"Set1")[1])
abline(lm(log(E)~log(m),data = as.data.frame(reptilecompmatrix)),col=brewer.pal(n=3,"Set1")[3])


# Mammal Trophic Level ----------------------------------------------------

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

