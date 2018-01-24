library(readr)
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

# myColours <- brewer.pal(6,"Set2")
# 
# my.settings <- list(
#   superpose.polygon=list(col=myColours[1:4], border="transparent"),
#   strip.background=list(col=myColours[6]),
#   strip.border=list(col="black")
# )

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
AmphiBIO_v1$C_E<-AmphiBIO_v1$C*AmphiBIO_v1$Longevity_max_y

#Calculate E/alpha
AmphiBIO_v1$E_alpha<-AmphiBIO_v1$Longevity_max_y/AmphiBIO_v1$Age_at_maturity_avg_y

#Calculate I/m
AmphiBIO_v1$I_m<-AmphiBIO_v1$Offspring_size_avg_g/AmphiBIO_v1$Body_mass_g

#How many amphibian complete cases?
sum(complete.cases(AmphiBIO_v1[,46:48]))
#132 complete cases


#Import amniote database
#Downloaded from http://esapubs.org/archive/ecol/E096/269/#data
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

#Add taxaname column
Amniote_Database_Aug_2015$taxaname<-paste(Amniote_Database_Aug_2015$genus,Amniote_Database_Aug_2015$species,sep="_")


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

#Import bird data from Sol et al. 2012
Sol_etal_birddata <- read_csv("C:/Users/Cecina/OneDrive/Documents/Kenyon College/Kerkhoff Lab/Summer Science 2017/bodymasspatterns/Sol_etal_birddata.csv")

#Data cleaning
#Alopochen_aegyptiaca
Sol_etal_birddata$Species[Sol_etal_birddata$Species=="Alopochen_aegyptiacus"]<-"Alopochen_aegyptiaca"
#Chen_caerulescens
Sol_etal_birddata$Species[Sol_etal_birddata$Species=="Anser_caerulescens"]<-"Chen_caerulescens"
#Collocalia_vanikorensis
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Aerodramus_vanikorensis"]<-"Collocalia"
#Columbina_inca
Sol_etal_birddata$Species[Sol_etal_birddata$Species=="Scardafella_inca"]<-"Columbina_inca"
#Spilopelia_chinensis
Sol_etal_birddata$Species[Sol_etal_birddata$Species=="Streptopelia_chinensis"]<-"Spilopelia_chinensis"
#Spilopelia_senegalensis
Sol_etal_birddata$Species[Sol_etal_birddata$Species=="Streptopelia_senegalensis"]<-"Spilopelia_senegalensis"
#Bambusicola_thoracicus
Sol_etal_birddata$Species[Sol_etal_birddata$Species=="Bambusicola_thoracica"]<-"Bambusicola_thoracicus"
#Tetrastes_bonasia
Sol_etal_birddata$Species[Sol_etal_birddata$Species=="Bonasa_bonasia"]<-"Tetrastes_bonasia"
birdtree1$tip.label[birdtree1$tip.label=="Bonasa_bonasia"]<-"Tetrastes_bonasia"
#Coturnix_chinensis
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Excalfactoria_chinensis"]<-"Coturnix"
#Dendragapus_canadensis
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Falcipennis_canadensis"]<-"Dendragapus"
#Francolinus_erckelii
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Pternistis_erckelii"]<-"Francolinus"
#Francolinus_icterorhynchus
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Pternistis_icterorhynchus"]<-"Francolinus"
#Francolinus_leucoscepus
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Pternistis_leucoscepus"]<-"Francolinus"
#Francolinus_afer
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Pternistis_afer"]<-"Francolinus"
#Francolinus_capensis
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Pternistis_capensis"]<-"Francolinus"
#Francolinus_adspersus
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Pternistis_adspersus"]<-"Francolinus"
#Francolinus_clappertoni
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Pternistis_clappertoni"]<-"Francolinus"
#Lagopus_muta
Sol_etal_birddata$Species[Sol_etal_birddata$Species=="Lagopus_mutus"]<-"Lagopus_muta"
#Lagopus_leucura
Sol_etal_birddata$Species[Sol_etal_birddata$Species=="Lagopus_leucurus"]<-"Lagopus_leucura"
#Tetrao_tetrix
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Lyrurus_tetrix"]<-"Tetrao"
#Cyanopica_cyanus
Sol_etal_birddata$Species[Sol_etal_birddata$Species=="Cyanopica_cyana"]<-"Cyanopica_cyanus"
#Carduelis_tristis
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Spinus_tristis"]<-"Carduelis"
#Carduelis_cannabina
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Linaria_cannabina"]<-"Carduelis"
#Carduelis_chloris
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Chloris_chloris"]<-"Carduelis"
#Carduelis_cucullata
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Spinus_cucullata"]<-"Carduelis"
#Acanthis_flammea
Sol_etal_birddata$Species[Sol_etal_birddata$Species=="Carduelis_flammea"]<-"Acanthis_flammea"
#Carduelis_flavirostris
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Linaria_flavirostris"]<-"Carduelis"
#Carduelis_spinus
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Spinus_spinus"]<-"Carduelis"
#Carduelis_psaltria
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Spinus_psaltria"]<-"Carduelis"
#Haemorhous_mexicanus
Sol_etal_birddata$Species[Sol_etal_birddata$Species=="Carpodacus_mexicanus"]<-"Haemorhous_mexicanus"
#Serinus_atrogularis
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Crithagra_atrogularis"]<-"Serinus"
#Serinus_mozambicus
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Crithagra_mozambica"]<-"Serinus"
Amniote_Database_Aug_2015$species[Amniote_Database_Aug_2015$taxaname=="Crithagra_mozambica"]<-"mozambicus"
#Serinus_leucopygius
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Crithagra_leucopygia"]<-"Serinus"
Amniote_Database_Aug_2015$species[Amniote_Database_Aug_2015$taxaname=="Crithagra_leucopygia"]<-"leucopygius"
#Serinus_flaviventris
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Crithagra_flaviventris"]<-"Serinus"
#Tiaris_canorus
Sol_etal_birddata$Species[Sol_etal_birddata$Species=="Tiaris_canora"]<-"Tiaris_canorus"
#Tiaris_olivaceus
Sol_etal_birddata$Species[Sol_etal_birddata$Species=="Tiaris_olivacea"]<-"Tiaris_olivaceus"
#Erithacus_akahige
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Larvivora_akahige"]<-"Erithacus"
#Erithacus_komadori
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Larvivora_komadori"]<-"Erithacus"
#Parus_caeruleus
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Cyanistes_caeruleus"]<-"Parus"
#Parus_varius
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Sittiparus_varius"]<-"Parus"
#Erythrura_gouldiae
Sol_etal_birddata$Species[Sol_etal_birddata$Species=="Chloebia_gouldiae"]<-"Erythrura_gouldiae"
#Estrilda_melanotis
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Coccopygia_melanotis"]<-"Estrilda"
#Lagonosticta_rubricata
Sol_etal_birddata$Species[Sol_etal_birddata$Species=="Lagonistica_rubricata"]<-"Lagonosticta_rubricata"
#Lonchura_malabarica
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Euodice_malabarica"]<-"Lonchura"
#Lonchura_cantans
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Euodice_cantans"]<-"Lonchura"
#Padda_oryzivora
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Lonchura_oryzivora"]<-"Padda"
#Uraeginthus_granatinus
Sol_etal_birddata$Species[Sol_etal_birddata$Species=="Uraeginthus_granatina"]<-"Uraeginthus_granatinus"
#Vidua_paradisaea
Sol_etal_birddata$Species[Sol_etal_birddata$Species=="Vidua_paradisea"]<-"Vidua_paradisaea"
#Sturnus_melanopterus
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Acridotheres_melanopterus"]<-"Sturnus"
#Sturnus_contra
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Gracupica_contra"]<-"Sturnus"
#Cettia_diphone
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Horornis_diphone"]<-"Cettia"
#Ara_severa
Sol_etal_birddata$Species[Sol_etal_birddata$Species=="Ara_severa"]<-"Ara_severus"
#Aratinga_canicularis
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Eupsittula_canicularis"]<-"Aratinga"
#Aratinga_pertinax
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Eupsittula_pertinax"]<-"Aratinga"
#Aratinga_mitrata
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Psittacara_mitratus"]<-"Aratinga"
Amniote_Database_Aug_2015$species[Amniote_Database_Aug_2015$taxaname=="Psittacara_mitratus"]<-"mitrata"
#Aratinga_holochlora
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Psittacara_holochlorus"]<-"Aratinga"
Amniote_Database_Aug_2015$species[Amniote_Database_Aug_2015$taxaname=="Psittacara_holochlorus"]<-"holochlora"
#Aratinga_acuticaudata
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Thectocercus_acuticaudatus"]<-"Aratinga"
Amniote_Database_Aug_2015$species[Amniote_Database_Aug_2015$taxaname=="Thectocercus_acuticaudatus"]<-"acuticaudata"
#Aratinga_chloroptera
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Psittacara_chloropterus"]<-"Aratinga"
Amniote_Database_Aug_2015$species[Amniote_Database_Aug_2015$taxaname=="Psittacara_chloropterus"]<-"chloroptera"
#Aratinga_erythrogenys
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Psittacara_erythrogenys"]<-"Aratinga"
#Cacatua_goffini
Sol_etal_birddata$Species[Sol_etal_birddata$Species=="Cacatua_goffini"]<-"Cacatua_goffiniana"
#Eolophus_roseicapilla
Sol_etal_birddata$Species[Sol_etal_birddata$Species=="Eolophus_roseicapillus"]<-"Eolophus_roseicapilla"
#Nandayus_nenday
Amniote_Database_Aug_2015$genus[Amniote_Database_Aug_2015$taxaname=="Aratinga_nenday"]<-"Nandayus"
#Turnix_varius
Sol_etal_birddata$Species[Sol_etal_birddata$Species=="Turnix_varia"]<-"Turnix_varius"

#redo taxaname column for amniote
Amniote_Database_Aug_2015$taxaname<-paste(Amniote_Database_Aug_2015$genus,Amniote_Database_Aug_2015$species,sep="_")


#create dataframe of just the birds from the amniote database
amniote_birddata<-Amniote_Database_Aug_2015[Amniote_Database_Aug_2015$class=="Aves",]

#Convert age at first breeding to years
Sol_etal_birddata$AgeFirstBreeding<-Sol_etal_birddata$AgeFirstBreeding/12
#Convert age at first breeding to days
Sol_etal_birddata$AgeFirstBreeding<-Sol_etal_birddata$AgeFirstBreeding*365

#Calculate longevity from Sol et al data
Sol_etal_birddata$longevity_y<-Sol_etal_birddata$Lifespan+(Sol_etal_birddata$AgeFirstBreeding/365)

#remove duplicate observations in Sol et al data
Sol_etal_birddata<-distinct(Sol_etal_birddata,Species, .keep_all=TRUE)

#combined bird data
combined_birddata<-amniote_birddata[,-c(37:42)]

#fill in possible missing trait values for the species in the amniote database
for(i in 1:nrow(combined_birddata)){
  species_i<-combined_birddata$taxaname[i]
  if(species_i%in%Sol_etal_birddata$Species){
    if(is.na(combined_birddata$female_maturity_d[i])){
      combined_birddata$female_maturity_d[i]<-Sol_etal_birddata$AgeFirstBreeding[Sol_etal_birddata$Species==species_i]
    }
    if(is.na(combined_birddata$litter_or_clutch_size_n[i])){
      combined_birddata$litter_or_clutch_size_n[i]<-Sol_etal_birddata$Clutch_size[Sol_etal_birddata$Species==species_i]
    }
    if(is.na(combined_birddata$litters_or_clutches_per_y[i])){
      combined_birddata$litters_or_clutches_per_y[i]<-Sol_etal_birddata$Broods[Sol_etal_birddata$Species==species_i]
    }
    if(is.na(combined_birddata$adult_body_mass_g[i])){
      combined_birddata$adult_body_mass_g[i]<-Sol_etal_birddata$Body_mass[Sol_etal_birddata$Species==species_i]
    }
    if(is.na(combined_birddata$longevity_y[i])){
      combined_birddata$longevity_y[i]<-Sol_etal_birddata$longevity_y[Sol_etal_birddata$Species==species_i]
    }
  }
}



#Import reptile data from Allen et al. 2017
Allen_etal_reptiledata <- read_csv("C:/Users/Cecina/OneDrive/Documents/Kenyon College/Kerkhoff Lab/Summer Science 2017/bodymasspatterns/Allen_etal_reptiledata.csv")

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
combined_reptiledata<-amniote_reptiledata[,-c(37:42)]

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
    if(is.na(combined_reptiledata$longevity_y[i])){
      combined_reptiledata$longevity_y[i]<-Allen_etal_reptiledata$LG[Allen_etal_reptiledata$Species==species_i]
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
Allen_uniquespecies$maximum_longevity_y<-NA
Allen_uniquespecies$gestation_d<-NA
Allen_uniquespecies$weaning_d<-NA
colnames(Allen_uniquespecies)[10]<-"birth_or_hatching_weight_g"
Allen_uniquespecies$weaning_weight_g<-NA
Allen_uniquespecies$egg_mass_g<-NA
Allen_uniquespecies$incubation_d<-NA
Allen_uniquespecies$fledging_age_d<-NA
colnames(Allen_uniquespecies)[14]<-"longevity_y"
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
combined_reptiledata$C_E<-combined_reptiledata$C*combined_reptiledata$longevity_y

#E/alpha
combined_reptiledata$E_alpha<-combined_reptiledata$longevity_y*365/combined_reptiledata$female_maturity_d

#I
combined_reptiledata$I<-combined_reptiledata$birth_or_hatching_weight_g
#I/m
combined_reptiledata$I_m<-combined_reptiledata$I/combined_reptiledata$adult_body_mass_g

#create augmented Amniote database for use in following calculations
augmented_amniote_database<-Amniote_Database_Aug_2015[Amniote_Database_Aug_2015$class!="Reptilia",]
augmented_amniote_database<-rbind(augmented_amniote_database,combined_reptiledata)

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
desiredcolumns<-c(1:7,11,39,40,42,43)
completecase_species<-augmented_amniote_database[complete.cases(augmented_amniote_database$adult_body_mass_g,augmented_amniote_database$C_E,augmented_amniote_database$I_m,augmented_amniote_database$E_alpha),desiredcolumns]
#remove otter
completecase_species<-completecase_species[completecase_species$taxaname!="Enhydra_lutris",]
#remove Acanthis hornemani
completecase_species<-completecase_species[completecase_species$taxaname!="Acanthis_hornemanni",]
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

#complete case amphibians
desiredcolumns_amph<-c(49,2:4,50:52,23,46:48,53)
completecase_amph<-AmphiBIO_v1[complete.cases(AmphiBIO_v1$Body_mass_g,AmphiBIO_v1$C_E,AmphiBIO_v1$I_m,AmphiBIO_v1$E_alpha),desiredcolumns_amph]
colnames(completecase_amph)[8]<-"adult_body_mass_g"
completecase_amph$log_body_mass<-log(completecase_amph$adult_body_mass_g)
completecase_amph$log_C_E<-log(completecase_amph$C_E)
completecase_amph$log_I_m<-log(completecase_amph$I_m)
completecase_amph$log_E_alpha<-log(completecase_amph$E_alpha)

#complete cases of amniotes and amphibians
completecase_am<-rbind(completecase_species,completecase_amph)

#Create hypervolumes for each class of amniotes


#Bird Gaussian hypervolume

#Log transform bird hypervolume
completebirds_gaussian<-hypervolume_gaussian(data = completecase_species[completecase_species$class=="Aves",13:16],
                                             name = "completebirds_gaussian")
completebirds_gaussian@Volume

#Plot bird hypervolume
#Log transform bird hypervolume
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
completemammals_gaussian<-hypervolume_gaussian(data = completecase_species[completecase_species$class=="Mammalia",13:16],
                                             name = "completemammals_gaussian")
completemammals_gaussian@Volume


#Plot mammal hypervolume
#Log transform mammal hypervolume
plot(completemammals_gaussian,point.dark.factor=1,color=gg_color_hue(3)[2])
plot(completemammals_gaussian,show.3d=TRUE,plot.3d.axes.id=2:4,cex.random=3,cex.data=6,
     show.legend=TRUE,point.alpha.min=0.5,point.dark.factor=1)


#Reptile Gaussian hypervolume

#Log transform reptile hypervolume
completereptiles_gaussian<-hypervolume_gaussian(data = completecase_species[completecase_species$class=="Reptilia",13:16],
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
completeamph_gaussian<-hypervolume_gaussian(data = completecase_am[completecase_am$class=="Amphibia",13:16],
                                             name = "completeamphib_gaussian")
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
plot(hypervolume_join(completebirds_gaussian,completemammals_gaussian,completereptiles_gaussian,completeamph_gaussian),num.points.max.random=6000,contour.lwd=1.5,colors=c(brewer.pal(n=3,"Set1")[1],brewer.pal(n=3,"Set1")[2],brewer.pal(n=3,"Set1")[3],brewer.pal(n=4,"Set1")[4]),show.legend=FALSE)
legend("bottomleft",legend = c("Birds","Mammals","Reptiles"),text.col=c(brewer.pal(n=3,"Set1")[1],brewer.pal(n=3,"Set1")[2],brewer.pal(n=3,"Set1")[3]),bty="n",cex=1.1,text.font=2)
plot(hypervolume_join(completebirds_gaussian,completemammals_gaussian,completereptiles_gaussian,completeamph_gaussian),
     show.3d=TRUE,plot.3d.axes.id=2:4,
     colors = c(brewer.pal(n=3,"Set1")[1],brewer.pal(n=3,"Set1")[2],brewer.pal(n=3,"Set1")[3],brewer.pal(n=4,"Set1")[4]),point.alpha.min = 0.5,cex.random=3,cex.data=6)


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

complete_data<-completecase_species[,c(13:16,1:5)]
complete_data<-as.data.frame(complete_data)
#Add bat points to mammal hypervolume
#with rodents too
plot(hypervolume_join(completebirds_gaussian,completemammals_gaussian),num.points.max.random=6000,contour.lwd=1.5,colors=c(brewer.pal(n=3,"Set1")[1],brewer.pal(n=3,"Set1")[2]),show.legend=FALSE,
     plot.function.additional=function(i,j) {   
       points(x=complete_data[complete_data$order=="Rodentia",i],y=complete_data[complete_data$order=="Rodentia",j],col=brewer.pal(n=6,"Set1")[6])
       points(x=complete_data[complete_data$order=="Chiroptera",i],y=complete_data[complete_data$order=="Chiroptera",j],col=brewer.pal(n=5,"Set1")[5],pch=19)
       })
legend("bottomleft",legend = c("Bats","Rodents","Birds","Mammals"),text.col=c(brewer.pal(n=6,"Set1")[5],brewer.pal(n=6,"Set1")[6],brewer.pal(n=6,"Set1")[1],brewer.pal(n=6,"Set1")[2]),bty="n",cex=1.1,text.font=2)



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
#bmvec_mammal<-completecase_species$adult_body_mass_g[completecase_species$class=="Mammalia"]
#removing the sea otter
bmvec_mammal<-completecase_species$adult_body_mass_g[completecase_species$class=="Mammalia" & completecase_species$taxaname!="Enhydra_lutris"]

names(bmvec_mammal)<-completecase_species$taxaname[completecase_species$class=="Mammalia" & completecase_species$taxaname!="Enhydra_lutris"]
pruned_mammaltree_best<-prune.missing(x=bmvec_mammal, phylo=mammaltree_best)
pruned_mammaltree_best<-pruned_mammaltree_best$tree

#Create a table of the node labels for each order
mammal_ordernodes<-data.frame(Order=as.character(unique(completecase_species$order[completecase_species$class=="Mammalia"])),num.species=as.numeric(0),node.num=as.numeric(0))
#add number of species per order
for(i in 1:nrow(mammal_ordernodes)){
  mammal_ordernodes$num.species[i]<-sum(pruned_mammaltree_best$tip.label%in%completecase_species$taxaname[completecase_species$order==mammal_ordernodes$Order[i]])
  if(mammal_ordernodes$num.species[i]>1)
    mammal_ordernodes$node.num[i]<-getMRCA(pruned_mammaltree_best,pruned_mammaltree_best$tip.label[pruned_mammaltree_best$tip.label%in%completecase_species$taxaname[completecase_species$order==mammal_ordernodes$Order[i]]])
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
multbirdtree<-read.newick("C:/Users/Cecina/Desktop/HackettStage2_0001_1000/mnt/data/projects/birdphylo/Tree_sets/Stage2_full_data/CombinedTrees/AllBirdsHackett1.tre")
birdtree1<-read.newick("C:/Users/Cecina/OneDrive/Documents/Kenyon College/Kerkhoff Lab/Summer Science 2017/bodymasspatterns/Hacketttree1.txt")
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
#removing Acanthis hornemanni
bmvec_bird<-completecase_species$adult_body_mass_g[completecase_species$class=="Aves" & completecase_species$taxaname!="Acanthis_hornemanni"]

names(bmvec_bird)<-completecase_species$taxaname[completecase_species$class=="Aves" & completecase_species$taxaname!="Acanthis_hornemanni"]
pruned_birdtree1<-prune.missing(x=bmvec_bird, phylo=birdtree1)
pruned_birdtree1<-pruned_birdtree1$tree


#Reptiles
#Zheng and Wiens tree
squamatetree<-read.newick("C:/Users/Cecina/OneDrive/Documents/Kenyon College/Kerkhoff Lab/Summer Science 2017/bodymasspatterns/zhengwienstree.txt")
#taxonomic resolution
squamatetree$tip.label[squamatetree$tip.label=="Agama_sankaranica"]<-"Agama_boensis"
squamatetree$tip.label[squamatetree$tip.label=="Gallotia_gomerana"]<-"Gallotia_bravoana"
squamatetree$tip.label[squamatetree$tip.label=="Phrynosoma_douglassii"]<-"Phrynosoma_douglasii"

#pruning squamates
bmvec_reptile<-completecase_species$adult_body_mass_g[completecase_species$class=="Reptilia"]
names(bmvec_reptile)<-completecase_species$taxaname[completecase_species$class=="Reptilia"]
pruned_squamatetree<-prune.missing(x=bmvec_reptile, phylo=squamatetree)
pruned_squamatetree<-pruned_squamatetree$tree
pruned_squamatetree<-drop.tip(pruned_squamatetree,c("Crocodylus_porosus","Alligator_mississippiensis"))


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





data(JaffeEtAl2011)
turtletree<-JaffeEtAl2011
#fix spelling of Texas tortoise
turtletree$tip.label[81]<-"Gopherus_berlandieri"
#Get rid of Greek tortoise subspecies
turtletree$tip.label[26]<-"Testudo_graeca"
#fix red-footed tortoise
turtletree$tip.label[60]<-"Chelonoidis_carbonarius"
#get rid of common box turtle subspecies
turtletree$tip.label[turtletree$tip.label=="Terrapene_carolina_triunguis"]<-"Terrapene_carolina"
#picking a subspecies for yellow-bellied slider
turtletree$tip.label[turtletree$tip.label=="Trachemys_scripta_elegans"]<-"Trachemys_scripta"

#pruning turtles
pruned_turtletree<-prune.missing(x=bmvec_reptile, phylo=turtletree)
pruned_turtletree<-pruned_turtletree$tree

data("Oaks2011")
croctree<-Oaks2011
#rename freshwater croc to match amniote taxonomy
croctree$tip.label[11]<-"Crocodylus_johnsoni"
#rename one of Nile crocodiles
croctree$tip.label[13]<-"Crocodylus_niloticus"
#rename one of the dwarf crocodiles
croctree$tip.label[croctree$tip.label=="Osteolaemus_tetraspis_1"]<-"Osteolaemus_tetraspis"
#pruning crocs
pruned_croctree<-prune.missing(x=bmvec_reptile,phylo = croctree)
pruned_croctree<-pruned_croctree$tree

#binding reptile tree


#adding traits to mammal tree
#Body mass
mammal_log_bodymass<-completecase_species$log_bodymass[completecase_species$class=="Mammalia"]
names(mammal_log_bodymass)<-completecase_species$taxaname[completecase_species$class=="Mammalia"]
mammal_log_bodymass_tiporder<-mammal_log_bodymass[pruned_mammaltree_best$tip.label]

plot(pruned_mammaltree_best,no.margin = TRUE,type="fan",show.tip.label = FALSE)
tiplabels(pch=19,col=color.scale(mammal_log_bodymass_tiporder,extremes=c("blue","red")))
color.legend(-255,-125,-155,-115,legend=c(0.85,18.82),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")
for(i in 1:length(mammal_orderover50)){
     arc.cladelabels(tree=pruned_mammaltree_best,text=mammal_ordernodes$Order[mammal_ordernodes$num.species>50][i],
                      mammal_orderover50[i],ln.offset = 1.03,lab.offset = 1.07)
   }

#C*E
mammal_log_C_E<-completecase_species$log_C_E[completecase_species$class=="Mammalia"]
names(mammal_log_C_E)<-completecase_species$taxaname[completecase_species$class=="Mammalia"]
mammal_log_C_E_tiporder<-mammal_log_C_E[pruned_mammaltree_best$tip.label]

plot(pruned_mammaltree_best,no.margin = TRUE,type="fan",show.tip.label = FALSE)
tiplabels(pch=19,col=color.scale(mammal_log_C_E_tiporder,extremes=c("blue","red"),xrange=c(-2.760842,5.378637)))
color.legend(-255,-125,-155,-115,legend=c(-2.76,5.38),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")
for(i in 1:length(mammal_orderover50)){
  arc.cladelabels(tree=pruned_mammaltree_best,text=mammal_ordernodes$Order[mammal_ordernodes$num.species>50][i],
                  mammal_orderover50[i],ln.offset = 1.03,lab.offset = 1.07)
}

#I/m
mammal_log_I_m<-completecase_species$log_I_m[completecase_species$class=="Mammalia"]
names(mammal_log_I_m)<-completecase_species$taxaname[completecase_species$class=="Mammalia"]
mammal_log_I_m_tiporder<-mammal_log_I_m[pruned_mammaltree_best$tip.label]

plot(pruned_mammaltree_best,no.margin = TRUE,type="fan",show.tip.label = FALSE)
tiplabels(pch=19,col=color.scale(mammal_log_I_m_tiporder,extremes=c("blue","red")))
color.legend(-255,-125,-155,-115,legend=c(-3.77,0.68),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")
for(i in 1:length(mammal_orderover50)){
  arc.cladelabels(tree=pruned_mammaltree_best,text=mammal_ordernodes$Order[mammal_ordernodes$num.species>50][i],
                  mammal_orderover50[i],ln.offset = 1.03,lab.offset = 1.07)
}

#E/alpha
mammal_log_E_alpha<-completecase_species$log_E_alpha[completecase_species$class=="Mammalia"]
names(mammal_log_E_alpha)<-completecase_species$taxaname[completecase_species$class=="Mammalia"]
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
squamate_log_bodymass<-completecase_species$log_bodymass[completecase_species$order=="Squamata"]
names(squamate_log_bodymass)<-completecase_species$taxaname[completecase_species$order=="Squamata"]
squamate_log_bodymass_tiporder<-squamate_log_bodymass[pruned_squamatetree$tip.label]

plot(pruned_squamatetree,cex=0.65,no.margin=TRUE)
tiplabels(pch=19,col=color.scale(squamate_log_bodymass_tiporder,extremes=c("blue","red")))
color.legend(0,60,40,61,legend=c(0.90,10.62),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")


#C*E
squamate_log_C_E<-completecase_species$log_C_E[completecase_species$order=="Squamata"]
names(squamate_log_C_E)<-completecase_species$taxaname[completecase_species$order=="Squamata"]
squamate_log_C_E_tiporder<-squamate_log_C_E[pruned_squamatetree$tip.label]

plot(pruned_squamatetree,cex=0.65)
tiplabels(pch=19,col=color.scale(squamate_log_C_E_tiporder,extremes=c("blue","red")))
color.legend(0,60,40,61,legend=c(-2.44,2.42),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")


#I/m
squamate_log_I_m<-completecase_species$log_I_m[completecase_species$order=="Squamata"]
names(squamate_log_I_m)<-completecase_species$taxaname[completecase_species$order=="Squamata"]
squamate_log_I_m_tiporder<-squamate_log_I_m[pruned_squamatetree$tip.label]

plot(pruned_squamatetree,cex=0.65)
tiplabels(pch=19,col=color.scale(squamate_log_I_m_tiporder,extremes=c("blue","red")))
color.legend(0,60,40,61,legend=c(-6.30,-1.95),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

#E/alpha
squamate_log_E_alpha<-completecase_species$log_E_alpha[completecase_species$order=="Squamata"]
names(squamate_log_E_alpha)<-completecase_species$taxaname[completecase_species$order=="Squamata"]
squamate_log_E_alpha_tiporder<-squamate_log_E_alpha[pruned_squamatetree$tip.label]

plot(pruned_squamatetree,cex=0.65)
tiplabels(pch=19,col=color.scale(squamate_log_E_alpha_tiporder,extremes=c("blue","red")))
color.legend(0,60,40,61,legend=c(-0.32,3.26),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")




#adding traits to croc tree
#Body mass
croc_log_bodymass<-completecase_species$log_bodymass[completecase_species$order=="Crocodilia"]
names(croc_log_bodymass)<-completecase_species$taxaname[completecase_species$order=="Crocodilia"]
croc_log_bodymass_tiporder<-croc_log_bodymass[pruned_croctree$tip.label]

plot(pruned_croctree)
tiplabels(pch=19,col=color.scale(croc_log_bodymass_tiporder,extremes=c("blue","red")))
color.legend(0,0,20,0.4,legend=c(9.30,12.20),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")


#C*E
croc_log_C_E<-completecase_species$log_C_E[completecase_species$order=="Crocodilia"]
names(croc_log_C_E)<-completecase_species$taxaname[completecase_species$order=="Crocodilia"]
croc_log_C_E_tiporder<-croc_log_C_E[pruned_croctree$tip.label]

plot(pruned_croctree)
tiplabels(pch=19,col=color.scale(croc_log_C_E_tiporder,extremes=c("blue","red")))
color.legend(0,0,20,0.4,legend=c(-0.60,0.16),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")


#I/m
croc_log_I_m<-completecase_species$log_I_m[completecase_species$order=="Crocodilia"]
names(croc_log_I_m)<-completecase_species$taxaname[completecase_species$order=="Crocodilia"]
croc_log_I_m_tiporder<-croc_log_I_m[pruned_croctree$tip.label]

plot(pruned_croctree)
tiplabels(pch=19,col=color.scale(croc_log_I_m_tiporder,extremes=c("blue","red")))
color.legend(0,0,20,0.4,legend=c(-7.90,-5.63),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

#E/alpha
croc_log_E_alpha<-completecase_species$log_E_alpha[completecase_species$order=="Crocodilia"]
names(croc_log_E_alpha)<-completecase_species$taxaname[completecase_species$order=="Crocodilia"]
croc_log_E_alpha_tiporder<-croc_log_E_alpha[pruned_croctree$tip.label]

plot(pruned_croctree)
tiplabels(pch=19,col=color.scale(croc_log_E_alpha_tiporder,extremes=c("blue","red")))
color.legend(0,0,20,0.4,legend=c(0.47,2.31),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")



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

#Create Brownian motion, OU, etc. models for Squamata
#For body mass
squamate_bodymass_fit.ou<-fitContinuous(ult_pruned_squamatetree,squamate_log_bodymass_tiporder,model="OU")
squamate_bodymass_fit.ou
squamate_bodymass_fit.bm<-fitContinuous(ult_pruned_squamatetree,squamate_log_bodymass_tiporder,model="BM")
squamate_bodymass_fit.bm
squamate_bodymass_fit.lambda<-fitContinuous(ult_pruned_squamatetree,squamate_log_bodymass_tiporder,model="lambda")
squamate_bodymass_fit.lambda
squamate_bodymass_fit.white<-fitContinuous(ult_pruned_squamatetree,squamate_log_bodymass_tiporder,model="white")
squamate_bodymass_fit.white
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

#Create Brownian motion, OU, etc. models for Crocodilia
#For body mass
croc_bodymass_fit.ou<-fitContinuous(pruned_croctree,croc_log_bodymass_tiporder,model="OU")
croc_bodymass_fit.ou
croc_bodymass_fit.bm<-fitContinuous(pruned_croctree,croc_log_bodymass_tiporder,model="BM")
croc_bodymass_fit.bm
croc_bodymass_fit.lambda<-fitContinuous(pruned_croctree,croc_log_bodymass_tiporder,model="lambda")
croc_bodymass_fit.lambda
croc_bodymass_fit.white<-fitContinuous(pruned_croctree,croc_log_bodymass_tiporder,model="white")
croc_bodymass_fit.white
#For C*E
croc_C_E_fit.ou<-fitContinuous(pruned_croctree,croc_log_C_E_tiporder,model="OU")
croc_C_E_fit.ou
croc_C_E_fit.bm<-fitContinuous(pruned_croctree,croc_log_C_E_tiporder,model="BM")
croc_C_E_fit.bm
croc_C_E_fit.lambda<-fitContinuous(pruned_croctree,croc_log_C_E_tiporder,model="lambda")
croc_C_E_fit.lambda
croc_C_E_fit.white<-fitContinuous(pruned_croctree,croc_log_C_E_tiporder,model="white")
croc_C_E_fit.white
#For I/m
croc_I_m_fit.ou<-fitContinuous(pruned_croctree,croc_log_I_m_tiporder,model="OU")
croc_I_m_fit.ou
croc_I_m_fit.bm<-fitContinuous(pruned_croctree,croc_log_I_m_tiporder,model="BM")
croc_I_m_fit.bm
croc_I_m_fit.lambda<-fitContinuous(pruned_croctree,croc_log_I_m_tiporder,model="lambda")
croc_I_m_fit.lambda
croc_I_m_fit.white<-fitContinuous(pruned_croctree,croc_log_I_m_tiporder,model="white")
croc_I_m_fit.white
#For E/alpha
croc_E_alpha_fit.ou<-fitContinuous(pruned_croctree,croc_log_E_alpha_tiporder,model="OU")
croc_E_alpha_fit.ou
croc_E_alpha_fit.bm<-fitContinuous(pruned_croctree,croc_log_E_alpha_tiporder,model="BM")
croc_E_alpha_fit.bm
croc_E_alpha_fit.lambda<-fitContinuous(pruned_croctree,croc_log_E_alpha_tiporder,model="lambda")
croc_E_alpha_fit.lambda
croc_E_alpha_fit.white<-fitContinuous(pruned_croctree,croc_log_E_alpha_tiporder,model="white")
croc_E_alpha_fit.white




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



#C*E and body mass

#Mammals
#linear model
summary(lm(log_C_E~log_bodymass,data = as.data.frame(mammaltraitmatrix)))
#PGLS
summary(gls(log_C_E~log_bodymass,correlation = corBrownian(phy=pruned_mammaltree_di),data = as.data.frame(mammaltraitmatrix),method = "ML"))
summary(gls(log_C_E~log_bodymass,correlation = corPagel(1,phy=pruned_mammaltree_di),data = as.data.frame(mammaltraitmatrix),method = "ML"))

#Birds
#linear model
summary(lm(log_C_E~log_bodymass,data = as.data.frame(birdtraitmatrix)))
#PGLS
summary(gls(log_C_E~log_bodymass,correlation = corBrownian(phy=pruned_birdtree1),data=as.data.frame(birdtraitmatrix),method="ML"))

#Reptiles
#linear model
summary(lm(log_C_E~log_bodymass,data = as.data.frame(reptiletraitmatrix)))

#Squamates
#linear model
summary(lm(log_C_E~log_bodymass,data = as.data.frame(reptiletraitmatrix[rownames(reptiletraitmatrix)%in%pruned_squamatetree$tip.label,])))
#PGLS
summary(gls(log_C_E~log_bodymass,correlation = corBrownian(phy=pruned_squamatetree),data=as.data.frame(reptiletraitmatrix[rownames(reptiletraitmatrix)%in%pruned_squamatetree$tip.label,]),method="ML"))



plot(log_C_E~log_bodymass,data = as.data.frame(mammaltraitmatrix),col=brewer.pal(n=3,"Set1")[2],xlab="Log(Body Mass)",ylab="Log(C*E)")
points(log_C_E~log_bodymass,data = as.data.frame(birdtraitmatrix),col=brewer.pal(n=3,"Set1")[1])
points(log_C_E~log_bodymass,data = as.data.frame(reptiletraitmatrix),col=brewer.pal(n=3,"Set1")[3])


abline(lm(log_C_E~log_bodymass,data = as.data.frame(mammaltraitmatrix)),col=brewer.pal(n=3,"Set1")[2])
abline(lm(log_C_E~log_bodymass,data = as.data.frame(birdtraitmatrix)),col=brewer.pal(n=3,"Set1")[1])
abline(lm(log_C_E~log_bodymass,data = as.data.frame(reptiletraitmatrix)),col=brewer.pal(n=3,"Set1")[3])



#E/alpha and body mass

#Mammals
#linear model
summary(lm(log_E_alpha~log_bodymass,data = as.data.frame(mammaltraitmatrix)))
#PGLS
summary(gls(log_E_alpha~log_bodymass,correlation = corBrownian(phy=pruned_mammaltree_di),data = as.data.frame(mammaltraitmatrix),method = "ML"))

#Birds
#linear model
summary(lm(log_E_alpha~log_bodymass,data = as.data.frame(birdtraitmatrix)))
#PGLS
summary(gls(log_E_alpha~log_bodymass,correlation = corBrownian(phy=pruned_birdtree1),data=as.data.frame(birdtraitmatrix),method="ML"))


#Reptiles
#linear model
summary(lm(log_E_alpha~log_bodymass,data = as.data.frame(reptiletraitmatrix)))
#Squamates
#linear model
summary(lm(log_E_alpha~log_bodymass,data = as.data.frame(reptiletraitmatrix[rownames(reptiletraitmatrix)%in%pruned_squamatetree$tip.label,])))
#PGLS
summary(gls(log_E_alpha~log_bodymass,correlation = corBrownian(phy=pruned_squamatetree),data=as.data.frame(reptiletraitmatrix[rownames(reptiletraitmatrix)%in%pruned_squamatetree$tip.label,]),method="ML"))


plot(log_E_alpha~log_bodymass,data = as.data.frame(mammaltraitmatrix),col=brewer.pal(n=3,"Set1")[2],xlab="Log(Body Mass)",ylab="Log(E/alpha)")
points(log_E_alpha~log_bodymass,data = as.data.frame(birdtraitmatrix),col=brewer.pal(n=3,"Set1")[1])
points(log_E_alpha~log_bodymass,data = as.data.frame(reptiletraitmatrix),col=brewer.pal(n=3,"Set1")[3])


abline(lm(log_E_alpha~log_bodymass,data = as.data.frame(mammaltraitmatrix)),col=brewer.pal(n=3,"Set1")[2])
abline(lm(log_E_alpha~log_bodymass,data = as.data.frame(birdtraitmatrix)),col=brewer.pal(n=3,"Set1")[1])
abline(lm(log_E_alpha~log_bodymass,data = as.data.frame(reptiletraitmatrix)),col=brewer.pal(n=3,"Set1")[3])



#I/m and body mass
#Mammals
#linear model
summary(lm(log_I_m~log_bodymass,data = as.data.frame(mammaltraitmatrix)))
#PGLS
summary(gls(log_I_m~log_bodymass,correlation = corBrownian(phy=pruned_mammaltree_di),data = as.data.frame(mammaltraitmatrix),method = "ML"))

#Birds
#linear model
summary(lm(log_I_m~log_bodymass,data = as.data.frame(birdtraitmatrix)))
#PGLS
summary(gls(log_I_m~log_bodymass,correlation = corBrownian(phy=pruned_birdtree1),data=as.data.frame(birdtraitmatrix),method="ML"))



#Reptiles
#linear model
summary(lm(log_I_m~log_bodymass,data = as.data.frame(reptiletraitmatrix)))
#Squamates
#linear model
summary(lm(log_I_m~log_bodymass,data = as.data.frame(reptiletraitmatrix[rownames(reptiletraitmatrix)%in%pruned_squamatetree$tip.label,])))
#PGLS
summary(gls(log_I_m~log_bodymass,correlation = corBrownian(phy=pruned_squamatetree),data=as.data.frame(reptiletraitmatrix[rownames(reptiletraitmatrix)%in%pruned_squamatetree$tip.label,]),method="ML"))


plot(log_I_m~log_bodymass,data = as.data.frame(mammaltraitmatrix),ylim=c(-9,1),xlim=c(1,18),col=brewer.pal(n=3,"Set1")[2],xlab="Log(Body Mass)",ylab="Log(I/m)")
points(log_I_m~log_bodymass,data = as.data.frame(birdtraitmatrix),col=brewer.pal(n=3,"Set1")[1])
points(log_I_m~log_bodymass,data = as.data.frame(reptiletraitmatrix),col=brewer.pal(n=3,"Set1")[3])


abline(lm(log_I_m~log_bodymass,data = as.data.frame(mammaltraitmatrix)),col=brewer.pal(n=3,"Set1")[2])
abline(lm(log_I_m~log_bodymass,data = as.data.frame(birdtraitmatrix)),col=brewer.pal(n=3,"Set1")[1])
abline(lm(log_I_m~log_bodymass,data = as.data.frame(reptiletraitmatrix)),col=brewer.pal(n=3,"Set1")[3])


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



#tree with mapped continuous character
mammalcont_C_E<-contMap(pruned_mammaltree_best,mammal_log_C_E_tiporder,plot = FALSE)
plot(mammalcont_C_E,type="fan",no.margin=TRUE,show.tip.label=FALSE)

#Trait datasets by class
#Mammals
mammaltraits<-completecase_species[completecase_species$taxaname%in%pruned_mammaltree_best$tip.label,c(20,12:15)]
mammaltraits$C<-augmented_amniote_database$C[augmented_amniote_database$taxaname%in%mammaltraits$taxaname]
mammaltraits$E<-augmented_amniote_database$longevity_y[augmented_amniote_database$taxaname%in%mammaltraits$taxaname]
mammaltraits$E_day<-(augmented_amniote_database$longevity_y[augmented_amniote_database$taxaname%in%mammaltraits$taxaname])*365
mammaltraits$alpha<-augmented_amniote_database$female_maturity_d[augmented_amniote_database$taxaname%in%mammaltraits$taxaname]
mammaltraits$I<-augmented_amniote_database$I[augmented_amniote_database$taxaname%in%mammaltraits$taxaname]
mammaltraits$m<-augmented_amniote_database$adult_body_mass_g[augmented_amniote_database$taxaname%in%mammaltraits$taxaname]


mammaltraitmatrix<-as.matrix(mammaltraits[,2:5])
mammaltraitmatrix<-mammaltraitmatrix[pruned_mammaltree_best$tip.label,]
rownames(mammaltraitmatrix)<-mammaltraits$taxaname


#matrix with components of invariants
mammalcompmatrix<-as.matrix(mammaltraits[,6:11])
rownames(mammalcompmatrix)<-mammaltraits$taxaname
#remove otter
mammalcompmatrix<-mammalcompmatrix[rownames(mammalcompmatrix)!="Enhydra_lutris"]

#Birds
birdtraits<-completecase_species[completecase_species$taxaname%in%pruned_birdtree1$tip.label,c(20,12:15)]
birdtraits$C<-augmented_amniote_database$C[augmented_amniote_database$taxaname%in%birdtraits$taxaname]
birdtraits$E<-augmented_amniote_database$longevity_y[augmented_amniote_database$taxaname%in%birdtraits$taxaname]
birdtraits$E_day<-(augmented_amniote_database$longevity_y[augmented_amniote_database$taxaname%in%birdtraits$taxaname])*365
birdtraits$alpha<-augmented_amniote_database$female_maturity_d[augmented_amniote_database$taxaname%in%birdtraits$taxaname]
birdtraits$I<-augmented_amniote_database$I[augmented_amniote_database$taxaname%in%birdtraits$taxaname]
birdtraits$m<-augmented_amniote_database$adult_body_mass_g[augmented_amniote_database$taxaname%in%birdtraits$taxaname]


birdtraitmatrix<-as.matrix(birdtraits[,2:5])
rownames(birdtraitmatrix)<-birdtraits$taxaname

#Reptiles
reptiletraits<-completecase_species[completecase_species$class=="Reptilia",c(20,12:15)]
reptiletraits$C<-augmented_amniote_database$C[augmented_amniote_database$taxaname%in%reptiletraits$taxaname]
reptiletraits$E<-augmented_amniote_database$longevity_y[augmented_amniote_database$taxaname%in%reptiletraits$taxaname]
reptiletraits$E_day<-(augmented_amniote_database$longevity_y[augmented_amniote_database$taxaname%in%reptiletraits$taxaname])*365
reptiletraits$alpha<-augmented_amniote_database$female_maturity_d[augmented_amniote_database$taxaname%in%reptiletraits$taxaname]
reptiletraits$I<-augmented_amniote_database$I[augmented_amniote_database$taxaname%in%reptiletraits$taxaname]
reptiletraits$m<-augmented_amniote_database$adult_body_mass_g[augmented_amniote_database$taxaname%in%reptiletraits$taxaname]


reptiletraitmatrix<-as.matrix(reptiletraits[,2:5])
rownames(reptiletraitmatrix)<-reptiletraits$taxaname


#matrix with components of invariants
birdcompmatrix<-as.matrix(birdtraits[,6:11])
rownames(birdcompmatrix)<-birdtraits$taxaname

reptilecompmatrix<-as.matrix(reptiletraits[,6:11])
rownames(reptilecompmatrix)<-reptiletraits$taxaname


#Adding traits to bird tree
#Body mass
bird_log_bodymass<-completecase_species$log_bodymass[completecase_species$class=="Aves"]
names(bird_log_bodymass)<-completecase_species$taxaname[completecase_species$class=="Aves"]
bird_log_bodymass_tiporder<-bird_log_bodymass[pruned_birdtree1$tip.label]

plot(pruned_birdtree1,no.margin = TRUE,type="fan",show.tip.label = FALSE)
tiplabels(pch=19,col=color.scale(bird_log_bodymass_tiporder,extremes=c("blue","red")))
color.legend(-150,-100,-100,-90,legend=c(1.15,8.99),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

#C*E
bird_log_C_E<-completecase_species$log_C_E[completecase_species$class=="Aves"]
names(bird_log_C_E)<-completecase_species$taxaname[completecase_species$class=="Aves"]
bird_log_C_E_tiporder<-bird_log_C_E[pruned_birdtree1$tip.label]

plot(pruned_birdtree1,no.margin = TRUE,show.tip.label = FALSE,type="fan")
tiplabels(pch=19,col=color.scale(bird_log_C_E_tiporder,extremes=c("blue","red")))
color.legend(-150,-100,-100,-90,legend=c(1.51,5.36),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

#I/m
bird_log_I_m<-completecase_species$log_I_m[completecase_species$class=="Aves"]
names(bird_log_I_m)<-completecase_species$taxaname[completecase_species$class=="Aves"]
bird_log_I_m_tiporder<-bird_log_I_m[pruned_birdtree1$tip.label]

plot(pruned_birdtree1,no.margin = TRUE,show.tip.label = FALSE,type="fan")
tiplabels(pch=19,col=color.scale(bird_log_I_m_tiporder,extremes=c("blue","red")))
color.legend(-150,-100,-100,-90,legend=c(-1.94,0.43),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

#E/alpha
bird_log_E_alpha<-completecase_species$log_E_alpha[completecase_species$class=="Aves"]
names(bird_log_E_alpha)<-completecase_species$taxaname[completecase_species$class=="Aves"]
bird_log_E_alpha_tiporder<-bird_log_E_alpha[pruned_birdtree1$tip.label]

plot(pruned_birdtree1,type="fan",no.margin = TRUE,show.tip.label = FALSE)
tiplabels(pch=19,col=color.scale(bird_log_E_alpha_tiporder,extremes=c("blue","red")))
color.legend(-150,-100,-100,-90,legend=c(0.54,3.57),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")


#Create Brownian motion, OU, etc. models
#For body mass
bird_bodymass_fit.ou<-fitContinuous(pruned_birdtree1,bird_log_bodymass_tiporder,model="OU")
bird_bodymass_fit.ou
bird_bodymass_fit.bm<-fitContinuous(pruned_birdtree1,bird_log_bodymass_tiporder,model="BM")
bird_bodymass_fit.bm
bird_bodymass_fit.lambda<-fitContinuous(pruned_birdtree1,bird_log_bodymass_tiporder,model="lambda")
bird_bodymass_fit.lambda
bird_bodymass_fit.white<-fitContinuous(pruned_birdtree1,bird_log_bodymass_tiporder,model="white")
bird_bodymass_fit.white
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

