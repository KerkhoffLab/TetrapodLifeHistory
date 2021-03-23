# Libraries ---------------------------------------------------------------
library(tidyverse)
library(janitor)
library(lattice)
library(RColorBrewer)
library(hypervolume)
library(phylosignal)
library(taxize)
library(brranching)
library(ape)
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
# install.packages("PhyloOrchard", repos = "http://R-Forge.R-project.org")
library(PhyloOrchard)
library(phangorn)
library(magick)
library(viridis)

palette1 <- c("#EA9010", "#013677", "#91C16C", "#BE55C1")
palette2 <- c( "#E69F00", "#F0E442", "#009E73", "#CC79A7")
batpalette <- c("#EA9010", "#013677", "#EDD161")

# Amphibian Data ----------------------------------------------------------

#Import AmphiBIO database
#Downloaded from https://doi.org/10.6084/m9.figshare.4644424
AmphiBIO_v1 <- read_csv("./data/AmphiBIO_v1.csv")
AmphiBIO_v1 <- AmphiBIO_v1 %>% 
  clean_names() %>% 
  mutate(class = "Amphibia",
         taxaname = gsub(" ", "_", species)) %>% 
  rowwise() %>% 
  # Calculate average clutch size
  mutate(litter_size_avg_n = mean(c(litter_size_max_n, litter_size_min_n))) %>% 
  # Calculate average age at maturity
  mutate(age_at_maturity_avg_y = mean(c(age_at_maturity_max_y, age_at_maturity_min_y))) %>% 
  #Use allometry equations to calculate mass at independence
  mutate(offspring_size_min_g = case_when(
    order == "Anura" ~ 10^-4.324*(offspring_size_min_mm)^3.189,
    order == "Caudata" ~ 10^-3.98*(offspring_size_min_mm)^2.644),
    offspring_size_max_g = case_when(
      order == "Anura" ~ 10^-4.324*(offspring_size_max_mm)^3.189,
      order == "Caudata" ~ 10^-3.98*(offspring_size_max_mm)^2.644)) %>% 
  # Use allometry equations to convert adult SVL to adult bodymass
  mutate(adult_size_min_g = case_when(order == "Anura" ~ 10^-4.324*(size_at_maturity_min_mm)^3.189,
                                      order == "Caudata" ~ 10^-3.98*(size_at_maturity_min_mm)^2.644),
         adult_size_max_g = case_when(order == "Anura" ~ 10^-4.324*(size_at_maturity_max_mm)^3.189,
                                      order == "Caudata" ~ 10^-3.98*(size_at_maturity_max_mm)^2.644)) %>% 
  rowwise() %>% 
  mutate(avg_adult_size_g = mean(c(adult_size_min_g, adult_size_max_g))) %>% 
  mutate(max_minus_avg_g = body_mass_g - avg_adult_size_g) %>% 
  # Calculate average mass at independence
  mutate(offspring_size_avg_g = mean(c(offspring_size_min_g, offspring_size_max_g))) %>% 
  # Create columns for invariant traits
  #R=average reproductive allocation per unit time
  #For amphibians:
  #R=Litter_size_avg_n*Reproductive_output_y*Offspring_size_avg_g
  mutate(R = litter_size_avg_n*reproductive_output_y*offspring_size_avg_g) %>% 
  #C=reproductive effort
  #C=R/m
  mutate(C = R/body_mass_g) %>% 
  # Calculate C with the average mass
  mutate(C_avg = R / avg_adult_size_g) %>% 
  #Calculate C*E
  #E=max longevity-avg. age at maturity
  mutate(C_E = C*(longevity_max_y - age_at_maturity_avg_y)) %>% 
  # Calculate C*E with the average mass
  mutate(C_E_avg = C_avg*(longevity_max_y - age_at_maturity_avg_y)) %>% 
  #Calculate E/alpha
  mutate(E_alpha = (longevity_max_y - age_at_maturity_avg_y)/age_at_maturity_avg_y) %>% 
  #Calculate I/m
  mutate(I_m = offspring_size_avg_g / body_mass_g) %>% 
  #Calculate I/m with average mass
  mutate(I_m_avg = offspring_size_avg_g / avg_adult_size_g)

# Amniote Data ------------------------------------------------------------

#Import amniote database
#Downloaded from http://esapubs.org/archive/ecol/E096/269/#data
#Metadata available at http://esapubs.org/archive/ecol/E096/269/metadata.php
#(Replaced -999 with NA in the .csv document itself prior to importing)
Amniote_Database_Aug_2015 <- read_csv("./data/Amniote_Database_Aug_2015.csv")
Amniote_Database_Aug_2015 <- Amniote_Database_Aug_2015 %>% 
  mutate(weaning_weight_g = as.numeric(weaning_weight_g)) %>% 
  #Add taxaname column
  mutate(taxaname = paste(genus, species, sep = "_")) %>% 
  #Create columns for invariant traits
  #R=average reproductive allocation per unit time
  #For mammals:
  ##R=litter_or_clutch_size_n*litters_or_clutches_per_y*weaning_weight_g
  #For reptiles:
  ##R=litter_or_clutch_size_n*litters_or_clutches_per_y*birth_or_hatching_weight_g
  #For birds:
  ##R=litter_or_clutch_size_n*litters_or_clutches_per_y*fledging_mass_g
  mutate(R = case_when(
    class == "Mammalia" ~ litter_or_clutch_size_n*litters_or_clutches_per_y*weaning_weight_g,
    class == "Reptilia" ~ litter_or_clutch_size_n*litters_or_clutches_per_y*birth_or_hatching_weight_g,
    class == "Aves" ~ litter_or_clutch_size_n*litters_or_clutches_per_y*fledging_mass_g
  )) %>% 
  #C=reproductive effort
  #C=R/m
  mutate(C = R / adult_body_mass_g) %>% 
  #Calculate C*E
  #using E=maximum longevity
  mutate(C_E = C * (maximum_longevity_y - (female_maturity_d/365))) %>% 
  #Calculate E/alpha
  mutate(E_alpha = (maximum_longevity_y*365 - female_maturity_d) / female_maturity_d) %>% 
  #Calculate I/m
  #I=size of offspring at independence
  #For mammals:
  ##I=weaning_weight_g
  #For reptiles:
  ##I=birth_or_hatching_weight_g
  #For birds:
  ##I=fledging_mass_g
  mutate(I = case_when(
    class == "Mammalia" ~ weaning_weight_g,
    class == "Reptilia" ~ birth_or_hatching_weight_g,
    class == "Aves" ~ fledging_mass_g
  )) %>% 
  mutate(I_m = I / adult_body_mass_g)

# Allen et al. Reptile Data -----------------------------------------------

#Import reptile data from Allen et al. 2017
#Downloaded from http://datadryad.org/resource/doi:10.5061/dryad.2d7b0
Allen_etal_reptiledata <- read_csv("./data/Allen_etal_reptiledata.csv")
#need to convert age at sexual maturity to days so it corresponds to amniote database
Allen_etal_reptiledata <- Allen_etal_reptiledata %>% 
  mutate(SM = SM*365) %>% 
  rename(order_allen = Order,
         family_allen = Family)

#Data cleaning for reptile species names
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
amniote_reptiledata <- Amniote_Database_Aug_2015 %>% 
  filter(class=="Reptilia")

#combined reptile data
combined_reptiledata <- amniote_reptiledata %>% 
  select(-c(R,
            C,
            C_E,
            E_alpha,
            I,
            I_m)) %>% 
  full_join(select(Allen_etal_reptiledata,
                   Species,
                   order_allen,
                   family_allen,
                   SM,
                   CS,
                   CY,
                   BM,
                   HM,
                   LG),
            by = c("taxaname" = "Species")) %>% 
  mutate(class = "Reptilia") %>% 
  group_by(taxaname) %>% 
  #fill in possible missing trait values for the species in the amniote database
  mutate(female_maturity_d = ifelse(is.na(female_maturity_d),
                                    SM,
                                    female_maturity_d),
         litter_or_clutch_size_n = ifelse(is.na(litter_or_clutch_size_n),
                                          CS,
                                          litter_or_clutch_size_n),
         litters_or_clutches_per_y = ifelse(is.na(litters_or_clutches_per_y),
                                            CY,
                                            litters_or_clutches_per_y),
         adult_body_mass_g = ifelse(is.na(adult_body_mass_g),
                                    BM,
                                    adult_body_mass_g),
         birth_or_hatching_weight_g = ifelse(is.na(birth_or_hatching_weight_g),
                                             HM,
                                             birth_or_hatching_weight_g),
         maximum_longevity_y = ifelse(is.na(maximum_longevity_y),
                                      LG,
                                      maximum_longevity_y),
         order = ifelse(is.na(order),
                        order_allen,
                        order),
         family = ifelse(is.na(family),
                         family_allen,
                         family)
         ) %>% 
  #Create columns for invariant traits
  #R
  mutate(R = litter_or_clutch_size_n * litters_or_clutches_per_y * birth_or_hatching_weight_g) %>% 
  #C*E
  mutate(C = R / adult_body_mass_g) %>% 
  mutate(C_E = C*(maximum_longevity_y - (female_maturity_d / 365))) %>% 
  #E/alpha
  mutate(E_alpha = (maximum_longevity_y * 365 - female_maturity_d) / female_maturity_d) %>% 
  #I
  mutate(I = birth_or_hatching_weight_g) %>% 
  #I/m
  mutate(I_m = I / adult_body_mass_g)

#create augmented Amniote database for use in following calculations
augmented_amniote_database<-Amniote_Database_Aug_2015[Amniote_Database_Aug_2015$class!="Reptilia",]
augmented_amniote_database<-bind_rows(augmented_amniote_database, combined_reptiledata)

# write.csv(augmented_amniote_database, file = "./data/augmented_amniote_database.csv")
# augmented_amniote_database<-read.csv(file = "./data/augmented_amniote_database.csv")


# Complete Case Data ------------------------------------------------------

#Subset of database including only species for all of the invariants
completecase_species <- augmented_amniote_database[complete.cases(augmented_amniote_database$adult_body_mass_g,augmented_amniote_database$C_E,augmented_amniote_database$I_m,augmented_amniote_database$E_alpha),] %>% 
  select(class,
         order,
         family,
         genus,
         species,
         taxaname,
         female_maturity_d,
         adult_body_mass_g,
         maximum_longevity_y,
         R,
         C,
         C_E,
         E_alpha,
         I,
         I_m)
completecase_species <- completecase_species %>% 
  #remove otter & Acanthis hornemani
  filter(taxaname != "Enhydra_lutris",
         taxaname != "Acanthis_hornemanni") %>% 
  #remove species with negative values for C*E
  filter(C_E > 0) %>% 
  #Log transform
  mutate(log_bodymass = log(adult_body_mass_g),
         log_C_E = log(C_E),
         log_I_m = log(I_m),
         log_E_alpha = log(E_alpha))


#complete case amphibians
completecase_amph <- AmphiBIO_v1[complete.cases(AmphiBIO_v1$body_mass_g,AmphiBIO_v1$C_E,AmphiBIO_v1$I_m,AmphiBIO_v1$E_alpha),] %>% 
  select(class,
         order,
         family,
         genus,
         species,
         taxaname,
         age_at_maturity_avg_y,
         adult_body_mass_g = body_mass_g,
         avg_adult_size_g,
         maximum_longevity_y = longevity_max_y,
         C,
         C_avg,
         C_E,
         C_E_avg,
         E_alpha,
         I = offspring_size_avg_g,
         I_m,
         I_m_avg) %>% 
  #removing species with negative values for C*E:
  filter(C_E > 0) %>% 
  mutate(female_maturity_d = age_at_maturity_avg_y*365,
         log_bodymass = log(adult_body_mass_g),
         log_C_E = log(C_E),
         log_I_m = log(I_m),
         log_E_alpha = log(E_alpha))

# Using the average mass
completecase_amph_avg <- AmphiBIO_v1[complete.cases(AmphiBIO_v1$avg_adult_size_g,AmphiBIO_v1$C_E_avg,AmphiBIO_v1$I_m_avg,AmphiBIO_v1$E_alpha),] %>% 
  select(class,
         order,
         family,
         genus,
         species,
         taxaname,
         age_at_maturity_avg_y,
         body_mass_g,
         adult_body_mass_g =avg_adult_size_g,
         maximum_longevity_y = longevity_max_y,
         C,
         C_avg,
         C_E,
         C_E_avg,
         E_alpha,
         I = offspring_size_avg_g,
         I_m,
         I_m_avg) %>% 
  #removing species with negative values for C*E:
  filter(C_E_avg > 0) %>% 
  mutate(female_maturity_d = age_at_maturity_avg_y*365,
         log_bodymass = log(adult_body_mass_g),
         log_C_E = log(C_E),
         log_I_m = log(I_m),
         log_E_alpha = log(E_alpha))

#complete cases of amniotes and amphibians
completecase_am <- bind_rows(completecase_species,completecase_amph) %>% 
  #Make class a factor
  mutate(class = factor(class,
                        levels = c("Aves", "Mammalia", "Reptilia", "Amphibia"),
                        ordered = TRUE))

# write.csv(completecase_am, file = "./data/completecase_am.csv")
# completecase_am<-read.csv(file = "./data/completecase_am.csv")


# Histograms of Traits ----------------------------------------------------

#Make frequency graphs of body mass and three invariants for all 4 classes:
#body mass
ggplot(data=completecase_am,aes(x=log(adult_body_mass_g),colour=class, ..density..))+
  geom_freqpoly(binwidth=0.5,lwd=1.3)+
  scale_color_manual(name="Class", values = palette1)+
  scale_x_continuous(labels = function(breaks) {round(exp(breaks))}) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=12),axis.title = element_text(size=14))+
  theme(legend.text=element_text(size=18), legend.title = element_text(size=18),legend.position = c(0.8,0.5))+
  labs(x = "Body Mass (g)", y="Density")
#C*E
ggplot(data=completecase_am,aes(x=log_C_E,..density..,colour=class))+
  geom_freqpoly(binwidth=0.5,lwd=1.3,show.legend = F)+
  scale_color_manual(values = palette1)+
  scale_x_continuous(labels = function(breaks) {round(exp(breaks), 2)}) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=12),axis.title = element_text(size=14))+
  labs(x = "Lifetime Reproductive Effort", y="Density")
#E/alpha
ggplot(data=completecase_am,aes(x=log_E_alpha,..density..,colour=class))+
  geom_freqpoly(binwidth=0.5,lwd=1.3,show.legend = F)+
  scale_color_manual(values = palette1)+
  scale_x_continuous(labels = function(breaks) {round(exp(breaks), 2)}) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=12),axis.title = element_text(size=14))+
  labs(x = "Relative Reproductive Lifespan",y="Density")
#I/m
ggplot(data=completecase_am,aes(x=log_I_m,..density..,colour=class))+
  geom_freqpoly(binwidth=0.5,lwd=1.3,show.legend = F)+
  scale_color_manual(values = palette1)+
  scale_x_continuous(labels = function(breaks) {round(exp(breaks), 5)}) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=12),axis.title = element_text(size=14))+
  labs(x = "Relative Offspring Size",y="Density")

# Compare metric values across classes
#Body mass
bodymass_class_anova <- aov(log_bodymass~class, data = completecase_am)
summary(bodymass_class_anova)
TukeyHSD(bodymass_class_anova)
favstats(log_bodymass~class, data = completecase_am)
#C*E
log_C_E_class_anova <- aov(log_C_E~class, data = completecase_am)
summary(log_C_E_class_anova)
TukeyHSD(log_C_E_class_anova)
favstats(log_C_E~class, data = completecase_am)
#E/alpha
log_E_alpha_class_anova <- aov(log_E_alpha~class, data = completecase_am)
summary(log_C_E_class_anova)
TukeyHSD(log_E_alpha_class_anova)
favstats(log_E_alpha~class, data = completecase_am)
#I/m
log_I_m_class_anova <- aov(log_I_m~class, data = completecase_am)
summary(log_I_m_class_anova)
TukeyHSD(log_I_m_class_anova)
favstats(log_I_m~class, data = completecase_am)

# Amphibians: comparing the max and calculated average body masses
ggplot() +
  geom_freqpoly(data = completecase_amph, aes(x=log(adult_body_mass_g), ..density.., color = "Maximum mass"),
                binwidth=0.5,lwd=1.3) +
  geom_freqpoly(data = completecase_amph_avg, aes(x=log(adult_body_mass_g), ..density.., color = "Calculated average mass"),
                binwidth=0.5,lwd=1.3) +
  scale_x_continuous(labels = function(breaks) {round(exp(breaks))}) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=12),axis.title = element_text(size=14))+
  theme(legend.text=element_text(size=18), legend.title = element_text(size=18),legend.position = c(0.8,0.5))+
  labs(x = "Body Mass (g)", y="Density", color = "Mass value used")

ggplot() +
  geom_freqpoly(data = completecase_amph, aes(x=log_C_E, ..density.., color = "Calculated with maximum mass"),
                binwidth=0.5,lwd=1.3, show.legend = F) +
  geom_freqpoly(data = completecase_amph_avg, aes(x=log_C_E, ..density.., color = "Calculated with average mass"),
                binwidth=0.5,lwd=1.3, show.legend = F) +
  scale_x_continuous(labels = function(breaks) {round(exp(breaks), 2)}) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=12),axis.title = element_text(size=14))+
  labs(x = "Lifetime Reproductive Effort", y="Density")

ggplot() +
  geom_freqpoly(data = completecase_amph, aes(x=log_I_m, ..density.., color = "Calculated with maximum mass"),
                binwidth=0.5,lwd=1.3, show.legend = F) +
  geom_freqpoly(data = completecase_amph_avg, aes(x=log_I_m, ..density.., color = "Calculated with average mass"),
                binwidth=0.5,lwd=1.3, show.legend = F) +
  scale_x_continuous(labels = function(breaks) {round(exp(breaks), 5)}) +
  # scale_x_continuous(trans = "log") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.text=element_text(size=18), legend.title = element_text(size=18))+
  labs(x = "Relative Reproductive Lifespan", y="Density")


#Histograms with bats
#Body mass
ggplot()+
  geom_freqpoly(data=completecase_am[completecase_am$class=="Aves"|completecase_am$class=="Mammalia",],aes(x=log_bodymass,..density..,colour=class),
                binwidth=0.5, lwd=1.3,show.legend = F)+
  scale_color_manual(values = batpalette[1:2])+
  geom_freqpoly(data=completecase_am[completecase_am$order=="Chiroptera",],aes(x=log_bodymass,..density..),
                binwidth=0.5, lwd=1.3, show.legend = F,colour=batpalette[3])+
  scale_x_continuous(labels = function(breaks) {round(exp(breaks))}) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=12),axis.title = element_text(size=14))+
  labs(x = "Body Mass (g)", y="Density")+
  theme(legend.text=element_text(size=18), legend.title = element_text(size=18),legend.position = c(0.8,0.5))

legend("topright",legend=c("Aves","Mammalia","Chiroptera"),lwd=2.5,col=batpalette)

#C*E
ggplot()+
  geom_freqpoly(data=completecase_am[completecase_am$class=="Aves"|completecase_am$class=="Mammalia",],aes(x=log_C_E,..density..,colour=class),
                binwidth=0.5, lwd=1.3,show.legend = F)+
  scale_x_continuous(labels = function(breaks) {round(exp(breaks), 2)}) +
  scale_color_manual(values = batpalette[1:2])+
  geom_freqpoly(data=completecase_am[completecase_am$order=="Chiroptera",],aes(x=log_C_E,..density..),
                binwidth=0.5, lwd=1.3, show.legend = F,colour=batpalette[3])+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=12),axis.title = element_text(size=14))+
  labs(x = "Lifetime Reproductive Effort", y="Density")
  
#E/alpha
ggplot()+
  geom_freqpoly(data=completecase_am[completecase_am$class=="Aves"|completecase_am$class=="Mammalia",],aes(x=log_E_alpha,..density..,colour=class),
              binwidth=0.5, lwd=1.3,show.legend = F)+
  scale_color_manual(values = batpalette[1:2])+
  geom_freqpoly(data=completecase_am[completecase_am$order=="Chiroptera",],aes(x=log_E_alpha,..density..),
                binwidth=0.5, lwd=1.3, show.legend = F,colour=batpalette[3])+
  scale_x_continuous(labels = function(breaks) {round(exp(breaks), 2)}) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=12),axis.title = element_text(size=14))+
  labs(x = "Relative Reproductive Lifespan", y="Density")

#I/m
ggplot()+
  geom_freqpoly(data=completecase_am[completecase_am$class=="Aves"|completecase_am$class=="Mammalia",],aes(x=log_I_m,..density..,colour=class),
                binwidth=0.5, lwd=1.3,show.legend = F)+
  scale_color_manual(values = batpalette[1:2])+
  geom_freqpoly(data=completecase_am[completecase_am$order=="Chiroptera",],aes(x=log_I_m,..density..),
                binwidth=0.5, lwd=1.3, show.legend = F,colour=batpalette[3])+
  scale_x_continuous(labels = function(breaks) {round(exp(breaks), 2)}) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=12),axis.title = element_text(size=14))+
  labs(x = "Relative Offspring Size", y="Density")

# Hypervolumes ------------------------------------------------------------

#Bird Gaussian hypervolume

#Log transform bird hypervolume
completebirds_gaussian<-hypervolume_gaussian(data = completecase_am %>% 
                                               filter(class == "Aves") %>% 
                                               select(log_C_E,
                                                      log_E_alpha,
                                                      log_I_m,
                                                      log_bodymass),
                                             name = "completebirds_gaussian")
completebirds_gaussian@Volume
#Plot bird hypervolume
plot(completebirds_gaussian,point.dark.factor=1,color=gg_color_hue(3)[1])
plot(completebirds_gaussian,show.3d=TRUE,plot.3d.axes.id=2:4,cex.random=3,cex.data=6,
     show.legend=TRUE,point.alpha.min=0.5,point.dark.factor=1)

#Mammal Gaussian hypervolume

#Log transform mammal hypervolume
completemammals_gaussian<-hypervolume_gaussian(data = completecase_am %>% 
                                                 filter(class == "Mammalia") %>% 
                                                 select(log_C_E,
                                                        log_E_alpha,
                                                        log_I_m,
                                                        log_bodymass),
                                             name = "completemammals_gaussian")
completemammals_gaussian@Volume


#Plot mammal hypervolume
#Log transform mammal hypervolume
plot(completemammals_gaussian,point.dark.factor=1,color=gg_color_hue(3)[2])
plot(completemammals_gaussian,show.3d=TRUE,plot.3d.axes.id=2:4,cex.random=3,cex.data=6,
     show.legend=TRUE,point.alpha.min=0.5,point.dark.factor=1)


#Reptile Gaussian hypervolume

#Log transform reptile hypervolume
completereptiles_gaussian<-hypervolume_gaussian(data = completecase_am %>% 
                                                  filter(class == "Reptilia") %>% 
                                                  select(log_C_E,
                                                         log_E_alpha,
                                                         log_I_m,
                                                         log_bodymass),
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

#Amphibian hypervolume
completeamph_gaussian<-hypervolume_gaussian(data = completecase_am %>% 
                                              filter(class == "Amphibia") %>% 
                                              select(log_C_E,
                                                     log_E_alpha,
                                                     log_I_m,
                                                     log_bodymass),
                                                name = "completeamph_gaussian")
completeamph_gaussian@Volume

# Using average mass
complete_amph_gaussian_avg <- hypervolume_gaussian(data = completecase_amph_avg[,c(21:23, 20)],
                                                   name = "completeamph_gaussian_avg")
complete_amph_gaussian_avg@Volume

#Plot amphibian hypervolume
#Log transform amphibian hypervolume
plot(completeamph_gaussian,point.dark.factor=1,
     # color=gg_color_hue(4)[4]
     )
plot(completeamph_gaussian,show.3d=TRUE,plot.3d.axes.id=2:4,cex.random=3,cex.data=6,
     show.legend=TRUE,point.alpha.min=0.5,point.dark.factor=1)

# Both amphibian hypervolumes
plot(hypervolume_join(completeamph_gaussian, complete_amph_gaussian_avg),
     num.points.max.random=6000, contour.lwd = 1.5, colors = c("#BE55C1", "#F9B641FF"),
     names=c("log(LRE)","log(ROS)", "log(RRL)", "log(Body Mass(g))"),show.legend=FALSE)
legend("bottomleft", legend = c("With maximum body mass", "With calculated average body mass"), text.col = c("#BE55C1", "#F9B641FF"), bty = "n", cex = 1.1, text.font = 2)


#Plotting all four hypervolumes together
#Log transformed hypervolumes
plot(hypervolume_join(completebirds_gaussian,completemammals_gaussian,completereptiles_gaussian,completeamph_gaussian),
     num.points.max.random=6000,contour.lwd=1.5,colors=palette1,
     names=c("log(LRE)","log(ROS)", "log(RRL)", "log(Body Mass (g))"),show.legend=FALSE)
legend("bottomleft",legend = c("Birds","Mammals","Reptiles", "Amphibians"),text.col=palette1,bty="n",cex=1.1,text.font=2)

plot(hypervolume_join(completebirds_gaussian,completemammals_gaussian,completereptiles_gaussian,completeamph_gaussian),
     show.3d=TRUE,plot.3d.axes.id=1:3,
     colors = palette1,
     names=c("log(LRE)", "log(ROS)", "log(RRL)"),show.legend=FALSE,point.alpha.min = 0.5,cex.random=3,cex.data=6)

open3d()
plot(hypervolume_join(completebirds_gaussian,completemammals_gaussian,completereptiles_gaussian,completeamph_gaussian),
     show.3d=TRUE,plot.3d.axes.id=c(1,3,2),
     colors = palette1,
     names=c("log(LRE)", "log(RRL)", "log(ROS)"),show.legend=FALSE,point.alpha.min = 0.5,cex.random=3,cex.data=6)

play3d(spin3d(axis = c(0, 0, 1), rpm = 10))
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

# Amphibians: max body mass vs. calculated average
hypervolume_overlap_statistics(hypervolume_set(completeamph_gaussian, complete_amph_gaussian_avg, check.memory = FALSE))
hypervolume_overlap_statistics(hypervolume_set(completebirds_gaussian, complete_amph_gaussian_avg, check.memory = FALSE))
hypervolume_overlap_statistics(hypervolume_set(completemammals_gaussian, complete_amph_gaussian_avg, check.memory = FALSE))
hypervolume_overlap_statistics(hypervolume_set(completereptiles_gaussian, complete_amph_gaussian_avg, check.memory = FALSE))

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
plot(hypervolume_join(completebirds_gaussian,completemammals_gaussian),num.points.max.random=6000,contour.lwd=1.5,colors=batpalette[1:2],
     names=c("log(LRE)","log(ROS)", "log(RRL)", "log(Body Mass (g))"),show.legend=FALSE,
     plot.function.additional=function(i,j) {   
       points(x=complete_data[complete_data$order=="Chiroptera",i],y=complete_data[complete_data$order=="Chiroptera",j],col=batpalette[3],pch=19)
       })
legend("bottomleft",legend = c("Birds","Mammals", "Bats"),text.col=batpalette,bty="n",cex=1.1,text.font=2)



plot(completemammals_gaussian,colors=gg_color_hue(3)[2],plot.function.additional=function(i,j) {
     points(x=complete_data[complete_data$order=="Chiroptera",i],y=complete_data[complete_data$order=="Chiroptera",j],col="red",pch=19) 
     })
plot(hypervolume_join(completebirds_gaussian,completemammals_gaussian),colors=c(gg_color_hue(3)[1],gg_color_hue(3)[2]),plot.function.additional=function(i,j) {
  points(x=complete_data[complete_data$order=="Chiroptera",i],y=complete_data[complete_data$order=="Chiroptera",j],col="darkviolet",pch=19) 
})

#Bat overlaps
#Birds and bats:
hypervolume_overlap_statistics(hypervolume_set(completebirds_gaussian,completebats_gaussian,check.memory = FALSE))
#Mammals and bats:
hypervolume_overlap_statistics(hypervolume_set(completemammals_gaussian,completebats_gaussian,check.memory = FALSE))

# Importing and Pruning Trees ---------------------------------------------------

#Mammals
mammaltrees<-read.nexus("./trees/fritztree2009.txt")
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

#Birdtree.org tree
birdtree1<-read.newick("./trees/Hacketttree1.txt")
# plot(birdtree1)
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

#Reptiles
#Zheng and Wiens tree
squamatetree<-read.newick("./trees/zhengwienstree.txt")
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

tetrapod_tree <- read.newick("./trees/tetrapod_tree.tre")
bmvec_tetrapod <- completecase_am$adult_body_mass_g
names(bmvec_tetrapod) <- completecase_am$taxaname
pruned_tetrapodtree <- prune.missing(x = bmvec_tetrapod, phylo = tetrapod_tree)

sum(pruned_tetrapodtree$tree$tip.label %in% completecase_am$taxaname[completecase_am$class == "Aves"])
sum(pruned_tetrapodtree$tree$tip.label %in% completecase_am$taxaname[completecase_am$class == "Mammalia"])
sum(pruned_tetrapodtree$tree$tip.label %in% completecase_am$taxaname[completecase_am$class == "Reptilia"])
sum(pruned_tetrapodtree$tree$tip.label %in% completecase_am$taxaname[completecase_am$class == "Amphibia"])

pruned_tetrapodtree <- pruned_tetrapodtree$tree

ult_pruned_tetrapodtree<-force.ultrametric(pruned_tetrapodtree,method = "nnls")
# plot(ult_pruned_tetrapodtree, type = "fan")

# Trait Datasets by Class for Species in Phylogeny -------------------------------------------------

#Mammals
mammaltraits <- completecase_am %>% 
  filter(taxaname %in% pruned_mammaltree_best$tip.label) %>% 
  select(taxaname,
         log_bodymass,
         log_C_E,
         log_E_alpha,
         log_I_m)

mammaltraitmatrix <- as.matrix(mammaltraits)
#order based on the phylogeny tip labels
mammaltraitmatrix<-mammaltraitmatrix[match(pruned_mammaltree_best$tip.label,mammaltraitmatrix[,1]),]
rownames(mammaltraitmatrix)<-mammaltraitmatrix[,1]
mammaltraitmatrix<-mammaltraitmatrix[,-1]

#Birds
birdtraits <- completecase_am %>% 
  filter(taxaname %in% pruned_birdtree1$tip.label) %>% 
  select(taxaname,
         log_bodymass,
         log_C_E,
         log_E_alpha,
         log_I_m)
birdtraitmatrix<-as.matrix(birdtraits)

rownames(birdtraitmatrix)<-birdtraits$taxaname

##Reptiles
reptiletraits <- completecase_am %>% 
  filter(class == "Reptilia") %>% 
  select(taxaname,
         log_bodymass,
         log_C_E,
         log_E_alpha,
         log_I_m)
reptiletraitmatrix<-as.matrix(reptiletraits[-1])
rownames(reptiletraitmatrix)<-reptiletraits$taxaname

##Amphibian
amphibiantraits <- completecase_am %>% 
  filter(taxaname%in%pruned_amphibiantree$tip.label) %>% 
  select(taxaname,
         log_bodymass,
         log_C_E,
         log_E_alpha,
         log_I_m)
amphibiantraitmatrix <- as.matrix(amphibiantraits[,-1])
rownames(amphibiantraitmatrix) <- amphibiantraits$taxaname

# Adding Traits to Trees --------------------------------------------------

#adding traits to mammal tree
#Body mass
mammal_log_bodymass<-completecase_am$log_bodymass[completecase_am$class=="Mammalia"]
names(mammal_log_bodymass)<-completecase_am$taxaname[completecase_am$class=="Mammalia"]
mammal_log_bodymass_tiporder<-mammal_log_bodymass[pruned_mammaltree_best$tip.label]

#C*E
mammal_log_C_E<-completecase_am$log_C_E[completecase_am$class=="Mammalia"]
names(mammal_log_C_E)<-completecase_am$taxaname[completecase_am$class=="Mammalia"]
mammal_log_C_E_tiporder<-mammal_log_C_E[pruned_mammaltree_best$tip.label]

#I/m
mammal_log_I_m<-completecase_am$log_I_m[completecase_am$class=="Mammalia"]
names(mammal_log_I_m)<-completecase_am$taxaname[completecase_am$class=="Mammalia"]
mammal_log_I_m_tiporder<-mammal_log_I_m[pruned_mammaltree_best$tip.label]

#E/alpha
mammal_log_E_alpha<-completecase_am$log_E_alpha[completecase_am$class=="Mammalia"]
names(mammal_log_E_alpha)<-completecase_am$taxaname[completecase_am$class=="Mammalia"]
mammal_log_E_alpha_tiporder<-mammal_log_E_alpha[pruned_mammaltree_best$tip.label]

#Create a dataframe of body mass and the three dimensionless metrics in tip order
mammal_phylo_order_traits <- cbind(mammal_log_bodymass_tiporder,mammal_log_C_E_tiporder,mammal_log_I_m_tiporder, mammal_log_E_alpha_tiporder)
mammal_phylo_order_traits <- as.data.frame(mammal_phylo_order_traits)
# mammal_phylo_order_traits<-add_rownames(mammal_phylo_order_traits,"taxaname")

#adding traits to squamate tree
#Body mass
squamate_log_bodymass<-completecase_am$log_bodymass[completecase_am$order=="Squamata"]
names(squamate_log_bodymass)<-completecase_am$taxaname[completecase_am$order=="Squamata"]
squamate_log_bodymass_tiporder<-squamate_log_bodymass[pruned_squamatetree$tip.label]

#C*E
squamate_log_C_E<-completecase_am$log_C_E[completecase_am$order=="Squamata"]
names(squamate_log_C_E)<-completecase_am$taxaname[completecase_am$order=="Squamata"]
squamate_log_C_E_tiporder<-squamate_log_C_E[pruned_squamatetree$tip.label]

#I/m
squamate_log_I_m<-completecase_am$log_I_m[completecase_am$order=="Squamata"]
names(squamate_log_I_m)<-completecase_am$taxaname[completecase_am$order=="Squamata"]
squamate_log_I_m_tiporder<-squamate_log_I_m[pruned_squamatetree$tip.label]

#E/alpha
squamate_log_E_alpha<-completecase_am$log_E_alpha[completecase_am$order=="Squamata"]
names(squamate_log_E_alpha)<-completecase_am$taxaname[completecase_am$order=="Squamata"]
squamate_log_E_alpha_tiporder<-squamate_log_E_alpha[pruned_squamatetree$tip.label]

#Create a dataframe of body mass and the three dimensionless metrics in tip order
squamate_phylo_order_traits <- cbind(squamate_log_bodymass_tiporder,squamate_log_C_E_tiporder,squamate_log_I_m_tiporder, squamate_log_E_alpha_tiporder)
squamate_phylo_order_traits <- as.data.frame(squamate_phylo_order_traits)
# squamate_phylo_order_traits <- add_rownames(squamate_phylo_order_traits,"taxaname")

#adding traits to amphibian tree
#Body mass
amphibian_log_bodymass<-completecase_am$log_bodymass[completecase_am$class=="Amphibia"]
names(amphibian_log_bodymass)<-completecase_am$taxaname[completecase_am$class=="Amphibia"]
amphibian_log_bodymass_tiporder<-amphibian_log_bodymass[pruned_amphibiantree$tip.label]

#C*E
amphibian_log_C_E<-completecase_am$log_C_E[completecase_am$class=="Amphibia"]
names(amphibian_log_C_E)<-completecase_am$taxaname[completecase_am$class=="Amphibia"]
amphibian_log_C_E_tiporder<-amphibian_log_C_E[pruned_amphibiantree$tip.label]

#I/m
amphibian_log_I_m<-completecase_am$log_I_m[completecase_am$class=="Amphibia"]
names(amphibian_log_I_m)<-completecase_am$taxaname[completecase_am$class=="Amphibia"]
amphibian_log_I_m_tiporder<-amphibian_log_I_m[pruned_amphibiantree$tip.label]

#E/alpha
amphibian_log_E_alpha<-completecase_am$log_E_alpha[completecase_am$class=="Amphibia"]
names(amphibian_log_E_alpha)<-completecase_am$taxaname[completecase_am$class=="Amphibia"]
amphibian_log_E_alpha_tiporder<-amphibian_log_E_alpha[pruned_amphibiantree$tip.label]

#Create a dataframe of body mass and the three dimensionless metrics in tip order
amphibian_phylo_order_traits<-cbind(amphibian_log_bodymass_tiporder,amphibian_log_C_E_tiporder,amphibian_log_I_m_tiporder, amphibian_log_E_alpha_tiporder)
amphibian_phylo_order_traits<-as.data.frame(amphibian_phylo_order_traits)
# amphibian_phylo_order_traits<-add_rownames(amphibian_phylo_order_traits,"taxaname")

#Adding traits to bird tree
#Body mass
bird_log_bodymass<-completecase_am$log_bodymass[completecase_am$class=="Aves"]
names(bird_log_bodymass)<-completecase_am$taxaname[completecase_am$class=="Aves"]
bird_log_bodymass_tiporder<-bird_log_bodymass[pruned_birdtree1$tip.label]

#C*E
bird_log_C_E<-completecase_am$log_C_E[completecase_am$class=="Aves"]
names(bird_log_C_E)<-completecase_am$taxaname[completecase_am$class=="Aves"]
bird_log_C_E_tiporder<-bird_log_C_E[pruned_birdtree1$tip.label]

#I/m
bird_log_I_m<-completecase_am$log_I_m[completecase_am$class=="Aves"]
names(bird_log_I_m)<-completecase_am$taxaname[completecase_am$class=="Aves"]
bird_log_I_m_tiporder<-bird_log_I_m[pruned_birdtree1$tip.label]

#E/alpha
bird_log_E_alpha<-completecase_am$log_E_alpha[completecase_am$class=="Aves"]
names(bird_log_E_alpha)<-completecase_am$taxaname[completecase_am$class=="Aves"]
bird_log_E_alpha_tiporder<-bird_log_E_alpha[pruned_birdtree1$tip.label]

#Create a dataframe of body mass and the three dimensionless metrics in tip order
bird_phylo_order_traits<-cbind(bird_log_bodymass_tiporder,bird_log_C_E_tiporder,bird_log_I_m_tiporder, bird_log_E_alpha_tiporder)
bird_phylo_order_traits<-as.data.frame(bird_phylo_order_traits)
bird_phylo_order_traits<-add_rownames(bird_phylo_order_traits,"taxaname")


#Adding traits to tetrapod tree
# Vector of classes
tetrapod_classes <- completecase_am$class
names(tetrapod_classes) <- completecase_am$taxaname
classes_tiporder <- tetrapod_classes[pruned_tetrapodtree$tip.label]

#Body mass
log_bodymass<-completecase_am$log_bodymass
names(log_bodymass)<-completecase_am$taxaname
log_bodymass_tiporder<-log_bodymass[pruned_tetrapodtree$tip.label]

#C*E
log_C_E<-completecase_am$log_C_E
names(log_C_E)<-completecase_am$taxaname
log_C_E_tiporder<-log_C_E[pruned_tetrapodtree$tip.label]

#I/m
log_I_m<-completecase_am$log_I_m
names(log_I_m)<-completecase_am$taxaname
log_I_m_tiporder<-log_I_m[pruned_tetrapodtree$tip.label]

#E/alpha
log_E_alpha<-completecase_am$log_E_alpha
names(log_E_alpha)<-completecase_am$taxaname
log_E_alpha_tiporder<-log_E_alpha[pruned_tetrapodtree$tip.label]

# Map traits on tetrapod tree ---------------------------------------------
taxanodes<-data.frame(Taxon=as.character(unique(completecase_am$class)),num.species=as.numeric(0),node.num=as.numeric(0))
taxanodes$Taxon <- as.character(taxanodes$Taxon)
#add number of species per class
for(i in 1:nrow(classnodes)){
  taxanodes$num.species[i]<-sum(ult_pruned_tetrapodtree$tip.label%in%completecase_am$taxaname[as.character(completecase_am$class)==taxanodes$Taxon[i]])
  if(classnodes$num.species[i]>1)
    taxanodes$node.num[i]<-getMRCA(ult_pruned_tetrapodtree,ult_pruned_tetrapodtree$tip.label[ult_pruned_tetrapodtree$tip.label%in%completecase_am$taxaname[as.character(completecase_am$class)==taxanodes$Taxon[i]]])
  else
    taxanodes$node.num[i]<-NA
}
taxanodes$Taxon[3] <- "Squamata"

classnodes <- data.frame(Class = as.character(unique(completecase_am$class)),
                         num.species = as.numeric((0)))
#add in number of species per class
for(i in 1:nrow(classnodes)) {
  classnodes$num.species[i] <- sum(ult_pruned_tetrapodtree$tip.label %in% completecase_am$taxaname[completecase_am$class == as.character(classnodes$Class[i])])
}

#Body mass
log_bodymass_contMap <- contMap(ult_pruned_tetrapodtree, log_bodymass_tiporder, plot = FALSE)
log_bodymass_contMap <- setMap(log_bodymass_contMap, colors = plasma(1449))
plot(log_bodymass_contMap, ftype = "off", outline = FALSE, legend = FALSE, lwd = 1, ylim=c(1-0.09*(Ntip(log_bodymass_contMap$tree)-1),Ntip(log_bodymass_contMap$tree)),
     mar=c(5.1,0.4,0.4,0.4))
for(i in 1:nrow(classnodes)){
  cladelabels(tree=ult_pruned_tetrapodtree,text=taxanodes$Taxon[i],
                  taxanodes$node.num[i], offset = 1.1)
}
add.color.bar(200,log_bodymass_contMap$cols,title="Log(Body Mass)",
              lims=log_bodymass_contMap$lims,digits=3,prompt=FALSE,x=0,
              y=1-0.08*(Ntip(log_bodymass_contMap$tree)-1),lwd=4,fsize=1,subtitle="200 million years")

#C*E
log_C_E_contMap <- contMap(ult_pruned_tetrapodtree, log_C_E_tiporder, plot = FALSE)
log_C_E_contMap <- setMap(log_C_E_contMap, colors = plasma(1449))
plot(log_C_E_contMap, ftype = "off", outline = FALSE, legend = FALSE, lwd = 1, ylim=c(1-0.09*(Ntip(log_C_E_contMap$tree)-1),Ntip(log_C_E_contMap$tree)),
     mar=c(5.1,0.4,0.4,0.4))
for(i in 1:nrow(classnodes)){
  cladelabels(tree=ult_pruned_tetrapodtree,text=taxanodes$Taxon[i],
              taxanodes$node.num[i], offset = 1.1)
}
add.color.bar(200,log_C_E_contMap$cols,title="Log(LRE)",
              lims=log_C_E_contMap$lims,digits=3,prompt=FALSE,x=0,
              y=1-0.08*(Ntip(log_C_E_contMap$tree)-1),lwd=4,fsize=1,subtitle="200 million years")

#E/alpha
log_E_alpha_contMap <- contMap(ult_pruned_tetrapodtree, log_E_alpha_tiporder, plot = FALSE)
log_E_alpha_contMap <- setMap(log_E_alpha_contMap, colors = plasma(1449))
plot(log_E_alpha_contMap, ftype = "off", outline = FALSE, legend = FALSE, lwd = 1, ylim=c(1-0.09*(Ntip(log_E_alpha_contMap$tree)-1),Ntip(log_E_alpha_contMap$tree)),
     mar=c(4,0.4,0.4,2.5))
for(i in 1:nrow(classnodes)){
  cladelabels(tree=ult_pruned_tetrapodtree,text=taxanodes$Taxon[i],
              taxanodes$node.num[i], offset = 1.1)
}
add.color.bar(200,log_E_alpha_contMap$cols,title="Log(RRL)",
              lims=log_E_alpha_contMap$lims,digits=3,prompt=FALSE,x=0,
              y=1-0.08*(Ntip(log_E_alpha_contMap$tree)-1),lwd=4,fsize=1,subtitle="200 million years")


#I/m
log_I_m_contMap <- contMap(ult_pruned_tetrapodtree, log_I_m_tiporder, plot = FALSE)
log_I_m_contMap <- setMap(log_I_m_contMap, colors = plasma(1449))
plot(log_I_m_contMap, ftype = "off", outline = FALSE, legend = FALSE, lwd = 1, ylim=c(1-0.09*(Ntip(log_I_m_contMap$tree)-1),Ntip(log_I_m_contMap$tree)),
     mar=c(5.1,0.4,0.4,0.4))
for(i in 1:nrow(classnodes)){
  cladelabels(tree=ult_pruned_tetrapodtree,text=taxanodes$Taxon[i],
              taxanodes$node.num[i], offset = 1.1)
}
add.color.bar(200,log_I_m_contMap$cols,title="Log(ROS)",
              lims=log_I_m_contMap$lims,digits=3,prompt=FALSE,x=0,
              y=1-0.08*(Ntip(log_I_m_contMap$tree)-1),lwd=4,fsize=1,subtitle="200 million years")

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


# Phylogenetic signal ----------------------------------------------------
# Amphibians
amphibian_phylo4d <- phylo4d(pruned_amphibiantree, tip.data = amphibian_phylo_order_traits)
phyloSignal(amphibian_phylo4d)

squamate_phylo4d <- phylo4d(pruned_squamatetree, tip.data = squamate_phylo_order_traits)
phyloSignal(squamate_phylo4d)

mammal_phylo4d <- phylo4d(pruned_mammaltree_best, tip.data = mammal_phylo_order_traits)
phyloSignal(mammal_phylo4d)

bird_phylo4d <- phylo4d(pruned_birdtree1, tip.data = bird_phylo_order_traits[-1])
phyloSignal(bird_phylo4d)

# Invariance Assessment and PGLS ------------------------------------------

#C*E and body mass
summary(lm(log_C_E~log_bodymass,data=completecase_am))

#Mammals
#linear model
summary(lm(as.numeric(log_C_E)~as.numeric(log_bodymass),data = as.data.frame(mammaltraitmatrix)))
#PGLS
#Use Pagel's lambda model
summary(gls(mammal_log_C_E_tiporder~mammal_log_bodymass_tiporder,
            correlation = corPagel(value=0.95,phy=pruned_mammaltree_di),data = mammal_phylo_order_traits,method = "ML"))

#Birds
#linear model
summary(lm(as.numeric(log_C_E)~as.numeric(log_bodymass),data = as.data.frame(birdtraitmatrix)))
#PGLS
#Use Pagel's lambda model
summary(gls(bird_log_C_E_tiporder~bird_log_bodymass_tiporder,
            correlation = corPagel(value = 0.95,phy=pruned_birdtree1),data=bird_phylo_order_traits,method="ML"))

#Reptiles
#linear model
summary(lm(log_C_E~log_bodymass,data = as.data.frame(reptiletraitmatrix)))

#Squamates
#linear model
summary(lm(log_C_E~log_bodymass,data = as.data.frame(reptiletraitmatrix[rownames(reptiletraitmatrix)%in%pruned_squamatetree$tip.label,])))
#PGLS
# Use OU model
summary(gls(squamate_log_C_E_tiporder~squamate_log_bodymass_tiporder,
            correlation = corMartins(value = 0.5, phy=pruned_squamatetree),data=squamate_phylo_order_traits,method="ML"))

#Amphibians
#linear model
summary(lm(log_C_E~log_bodymass,data = as.data.frame(amphibiantraitmatrix)))
#PGLS
#Use OU model
summary(gls(amphibian_log_C_E_tiporder~amphibian_log_bodymass_tiporder,
            correlation = corMartins(value = 0.5,phy=pruned_amphibiantree),data=amphibian_phylo_order_traits,method="ML"))

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

#Birds
#linear model
summary(lm(log_E_alpha~log_bodymass,data = as.data.frame(birdtraitmatrix)))
#PGLS
summary(gls(bird_log_E_alpha_tiporder~bird_log_bodymass_tiporder,correlation = corBrownian(phy=pruned_birdtree1),data = bird_phylo_order_traits,method = "ML"))
# Use Pagel's lambda model
summary(gls(bird_log_E_alpha_tiporder~bird_log_bodymass_tiporder,
            correlation = corPagel(value = 0.95, phy=pruned_birdtree1),data = bird_phylo_order_traits,method = "ML"))

#Reptiles
#linear model
summary(lm(log_E_alpha~log_bodymass,data = as.data.frame(reptiletraitmatrix)))
#Squamates
#linear model
summary(lm(log_E_alpha~log_bodymass,data = as.data.frame(reptiletraitmatrix[rownames(reptiletraitmatrix)%in%pruned_squamatetree$tip.label,])))
#PGLS
summary(gls(squamate_log_E_alpha_tiporder~squamate_log_bodymass_tiporder,correlation = corBrownian(phy=pruned_squamatetree),data = squamate_phylo_order_traits,method = "ML"))
# Use OU model
summary(gls(squamate_log_E_alpha_tiporder~squamate_log_bodymass_tiporder,
            correlation = corMartins(value = 0.5, phy=pruned_squamatetree),data = squamate_phylo_order_traits,method = "ML"))

#Amphibians
summary(gls(amphibian_log_E_alpha_tiporder~amphibian_log_bodymass_tiporder,
            correlation = corMartins(value = 0.5, phy=pruned_amphibiantree),data=amphibian_phylo_order_traits,method="ML"))



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


# mvMORPH ------------------------------------------

# Amphibians
amphibian_mvmorph <- mvgls(cbind(amphibian_log_C_E_tiporder, amphibian_log_E_alpha_tiporder, amphibian_log_I_m_tiporder) ~ amphibian_log_bodymass_tiporder,
                           data = amphibian_phylo_order_traits, tree = pruned_amphibiantree, model = "lambda", error = TRUE)
summary(amphibian_mvmorph)
amphibian_test <- manova.gls(amphibian_mvmorph)
amphibian_test
cov2cor(amphibian_mvmorph$sigma$Pinv)

# Squamates
squamate_mvmorph <- mvgls(cbind(squamate_log_C_E_tiporder, squamate_log_E_alpha_tiporder, squamate_log_I_m_tiporder) ~ squamate_log_bodymass_tiporder,
                          data = squamate_phylo_order_traits, tree = pruned_squamatetree, model = "lambda", error = TRUE)
summary(squamate_mvmorph)
squamate_test <- manova.gls(squamate_mvmorph)
squamate_test
cov2cor(squamate_mvmorph$sigma$Pinv)

# Mammals
mammal_mvmorph <- mvgls(cbind(mammal_log_C_E_tiporder, mammal_log_E_alpha_tiporder, mammal_log_I_m_tiporder) ~ mammal_log_bodymass_tiporder,
                        data = mammal_phylo_order_traits, tree = pruned_mammaltree_di, model = "lambda", error = TRUE)
summary(mammal_mvmorph)
mammal_test <- manova.gls(mammal_mvmorph)
mammal_test
cov2cor(mammal_mvmorph$sigma$Pinv)

# Birds
bird_mvmorph <- mvgls(cbind(bird_log_C_E_tiporder, bird_log_E_alpha_tiporder, bird_log_I_m_tiporder) ~ bird_log_bodymass_tiporder,
                      data = bird_phylo_order_traits, tree = pruned_birdtree1, model = "lambda", error = TRUE)
summary(bird_mvmorph)
bird_test <- manova.gls(bird_mvmorph)
bird_test
cov2cor(bird_mvmorph$sigma$Pinv)

