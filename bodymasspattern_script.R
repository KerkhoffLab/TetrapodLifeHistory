library(readr)


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
    Amniote_Database_Aug_2015[i, 'R'] <- Amniote_Database_Aug_2015[i,"litter_or_clutch_size_n"]*Amniote_Database_Aug_2015[i,"litters_or_clutches_per_y"]*Amniote_Database_Aug_2015[i,"egg_mass_g"]
  }
  else if(Amniote_Database_Aug_2015$class[i]=='Aves') {
    Amniote_Database_Aug_2015[i, 'R'] <- Amniote_Database_Aug_2015[i,"litter_or_clutch_size_n"]*Amniote_Database_Aug_2015[i,"litters_or_clutches_per_y"]*Amniote_Database_Aug_2015[i,"fledging_mass_g"]
  }
}

#C=reproductive effort
#C

#How many non-NA values for R?
length(Amniote_Database_Aug_2015$R[!is.na(Amniote_Database_Aug_2015$R)])
#How many birds?
length(Amniote_Database_Aug_2015$R[!is.na(Amniote_Database_Aug_2015$R) & Amniote_Database_Aug_2015$class=="Aves"])
#How many mammals?
length(Amniote_Database_Aug_2015$R[!is.na(Amniote_Database_Aug_2015$R) & Amniote_Database_Aug_2015$class=="Mammalia"])
#How many reptiles?
length(Amniote_Database_Aug_2015$R[!is.na(Amniote_Database_Aug_2015$R) & Amniote_Database_Aug_2015$class=="Reptilia"])



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


