#### Plot different orders on bird hypervolumes ####
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

#### Plot different orders on reptile hypervolumes ####
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

#### Visualize different groups of mammals in hypervolume ####
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

##### Pair plots of mammals split up by orders ####
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

#### Showing position of mammal orders on tree ####
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

#### Showing position of bird orders on tree ####
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

#### Plotting traits on tree ####
plot(pruned_mammaltree_best,no.margin = TRUE,type="fan",show.tip.label = FALSE)
tiplabels(pch=19,col=color.scale(mammal_log_bodymass_tiporder,extremes=c("blue","red")))
color.legend(-255,-125,-155,-115,legend=c(0.85,18.82),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")
for(i in 1:length(mammal_orderover50)){
  arc.cladelabels(tree=pruned_mammaltree_best,text=mammal_ordernodes$Order[mammal_ordernodes$num.species>50][i],
                  mammal_orderover50[i],ln.offset = 1.03,lab.offset = 1.07)
}

plot(pruned_mammaltree_best,no.margin = TRUE,type="fan",show.tip.label = FALSE)
tiplabels(pch=19,col=color.scale(mammal_log_C_E_tiporder,extremes=c("blue","red")))
color.legend(-255,-125,-155,-115,legend=c(-2.23,5.76),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")
for(i in 1:length(mammal_orderover50)){
  arc.cladelabels(tree=pruned_mammaltree_best,text=as.character(mammal_ordernodes$Order[mammal_ordernodes$num.species>50][i]),
                  mammal_orderover50[i],ln.offset = 1.03,lab.offset = 1.07)
}

plot(pruned_mammaltree_best,no.margin = TRUE,type="fan",show.tip.label = FALSE)
tiplabels(pch=19,col=color.scale(mammal_log_I_m_tiporder,extremes=c("blue","red")))
color.legend(-255,-125,-155,-115,legend=c(-3.77,0.68),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")
for(i in 1:length(mammal_orderover50)){
  arc.cladelabels(tree=pruned_mammaltree_best,text=mammal_ordernodes$Order[mammal_ordernodes$num.species>50][i],
                  mammal_orderover50[i],ln.offset = 1.03,lab.offset = 1.07)
}

plot(pruned_mammaltree_best,no.margin = TRUE,type="fan",show.tip.label = FALSE)
tiplabels(pch=19,col=color.scale(mammal_log_E_alpha_tiporder,extremes=c("blue","red")))
color.legend(-255,-125,-155,-115,legend=c(-1.80,4.20),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")
for(i in 1:length(mammal_orderover50)){
  arc.cladelabels(tree=pruned_mammaltree_best,text=mammal_ordernodes$Order[mammal_ordernodes$num.species>50][i],
                  mammal_orderover50[i],ln.offset = 1.03,lab.offset = 1.07)
}

plot(pruned_squamatetree,cex=0.65,no.margin=TRUE)
tiplabels(pch=19,col=color.scale(squamate_log_bodymass_tiporder,extremes=c("blue","red")))
color.legend(0,60,40,61,legend=c(0.90,10.62),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

plot(pruned_squamatetree,cex=0.65)
tiplabels(pch=19,col=color.scale(squamate_log_C_E_tiporder,extremes=c("blue","red")))
color.legend(0,60,40,61,legend=c(-2.44,2.42),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

plot(pruned_squamatetree,cex=0.65)
tiplabels(pch=19,col=color.scale(squamate_log_I_m_tiporder,extremes=c("blue","red")))
color.legend(0,60,40,61,legend=c(-6.30,-1.95),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

plot(pruned_amphibiantree,cex=0.65,no.margin=TRUE)
tiplabels(pch=19,col=color.scale(amphibian_log_bodymass_tiporder,extremes=c("blue","red")))
#color.legend(0,60,40,61,legend=c(0.90,10.62),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

plot(pruned_amphibiantree,cex=0.65)
tiplabels(pch=19,col=color.scale(amphibian_log_C_E_tiporder,extremes=c("blue","red")))
color.legend(0,60,40,61,legend=c(-2.44,2.42),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

plot(pruned_amphibiantree,cex=0.65)
tiplabels(pch=19,col=color.scale(amphibian_log_I_m_tiporder,extremes=c("blue","red")))
color.legend(0,60,40,61,legend=c(-6.30,-1.95),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

plot(pruned_amphibiantree,cex=0.65)
tiplabels(pch=19,col=color.scale(amphibian_log_E_alpha_tiporder,extremes=c("blue","red")))
color.legend(0,60,40,61,legend=c(-0.32,3.26),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

plot(pruned_birdtree1,no.margin = TRUE,type="fan",show.tip.label = FALSE)
tiplabels(pch=19,col=color.scale(bird_log_bodymass_tiporder,extremes=c("blue","red")))
color.legend(-150,-100,-100,-90,legend=c(1.15,8.99),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

plot(pruned_birdtree1,no.margin = TRUE,show.tip.label = FALSE,type="fan")
tiplabels(pch=19,col=color.scale(bird_log_C_E_tiporder,extremes=c("blue","red")))
color.legend(-150,-100,-100,-90,legend=c(1.51,5.36),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

plot(pruned_birdtree1,no.margin = TRUE,show.tip.label = FALSE,type="fan")
tiplabels(pch=19,col=color.scale(bird_log_I_m_tiporder,extremes=c("blue","red")))
color.legend(-150,-100,-100,-90,legend=c(-1.94,0.43),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

plot(pruned_birdtree1,type="fan",no.margin = TRUE,show.tip.label = FALSE)
tiplabels(pch=19,col=color.scale(bird_log_E_alpha_tiporder,extremes=c("blue","red")))
color.legend(-150,-100,-100,-90,legend=c(0.54,3.57),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

plot(pruned_tetrapodtree, no.margin = TRUE,type="fan",show.tip.label = FALSE)
tiplabels(pch=19,col=color.scale(log_bodymass_tiporder,extremes=c("blue","red")))
color.legend(-150,-100,-100,-90,legend=c(-0.545,18.82),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

plot(pruned_tetrapodtree,no.margin = TRUE,show.tip.label = FALSE,type="fan")
tiplabels(pch=19,col=color.scale(log_C_E_tiporder,extremes=c("blue","red")))
color.legend(-150,-100,-100,-90,legend=c(-7.338,5.762),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

plot(pruned_tetrapodtree,no.margin = TRUE,show.tip.label = FALSE,type="fan")
tiplabels(pch=19,col=color.scale(log_I_m_tiporder,extremes=c("blue","red")))
color.legend(-150,-100,-100,-90,legend=c(-15.66,0.676),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

plot(pruned_tetrapodtree,type="fan",no.margin = TRUE,show.tip.label = FALSE)
tiplabels(pch=19,col=color.scale(log_E_alpha_tiporder,extremes=c("blue","red")))
color.legend(-150,-100,-100,-90,legend=c(-3.045,4.650),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")



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
# View(mammal_bodymass_C_E_matrix)
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


# Pairwise Components of Invariants ---------------------------------------
# Mammal matrix with components of invariants
mammalcompmatrix<-as.matrix(mammaltraits[,c(1,6:10)])
#order based on the phylogeny tip labels
mammalcompmatrix<-mammalcompmatrix[match(pruned_mammaltree_best$tip.label,mammalcompmatrix[,1]),]
rownames(mammaltraitmatrix)<-mammaltraitmatrix[,1]
mammaltraitmatrix<-mammaltraitmatrix[,-1]

# Bird matrix with components of invariants
birdcompmatrix<-as.matrix(birdtraits[,6:11])
rownames(birdcompmatrix)<-birdtraits$taxaname

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

# MCMCglmm -----------------------------------------
# prior <- list(G=list(G1=list(V=diag(3),nu=0.02)),
#               R=list(V=diag(3),nu=0.02))
# prior <- list(G=list(G1=list(V=diag(1),nu=0.02)),
#               R=list(V=diag(3),nu=0.02))
univariate_prior <- list(G=list(G1=list(V=1,nu=0.02)),R=list(V=1,nu=0.02))

# Birds
inv_bird_phylo <- inverseA(pruned_birdtree1)$Ainv
# inv_bird_phylo <- inverseA(pruned_birdtree1)$Ainv
rownames(bird_phylo_order_traits) = bird_phylo_order_traits$taxaname

# bird_mcmc_model <- MCMCglmm(cbind(bird_log_C_E_tiporder,
#                                   bird_log_E_alpha_tiporder,
#                                   bird_log_I_m_tiporder) ~ bird_log_bodymass_tiporder,
#                             # random = ~us(trait):taxaname,
#                             random = ~taxaname,
#                             rcov = ~us(trait):units,
#                             family = rep("gaussian",3),
#                             ginverse = list(taxaname = inv_bird_phylo),
#                             data = as.data.frame(bird_phylo_order_traits),
#                             prior = prior,
#                             nitt = 60000,
#                             burnin = 1000,
#                             thin = 500)
C_E_bird_mcmc_model <- MCMCglmm(bird_log_C_E_tiporder ~ bird_log_bodymass_tiporder,
                                random = ~taxaname,
                                family = "gaussian",
                                ginverse = list(taxaname = inv_bird_phylo),
                                data = as.data.frame(bird_phylo_order_traits),
                                prior = univariate_prior,
                                nitt = 60000,
                                burnin = 1000,
                                thin = 500)
summary(C_E_bird_mcmc_model)
C_E_bird_lambda <- C_E_bird_mcmc_model$VCV[,'taxaname']/
  (C_E_bird_mcmc_model$VCV[,'taxaname']+C_E_bird_mcmc_model$VCV[,'units'])
mean(C_E_bird_lambda)
HPDinterval(C_E_bird_lambda)

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

# PGLS --------------------------------------------------------

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
#summary(gls(mammal_log_C_E_tiporder~mammal_log_bodymass_tiporder,correlation = corBrownian(phy=pruned_mammaltree_di),data = mammal_phylo_order_traits,method = "ML"))

#Birds
#linear model
summary(lm(as.numeric(log_C_E)~as.numeric(log_bodymass),data = as.data.frame(birdtraitmatrix)))
#PGLS
#Use Brownian motion
summary(gls(bird_log_C_E_tiporder~bird_log_bodymass_tiporder,correlation = corBrownian(phy=pruned_birdtree1),data=bird_phylo_order_traits,method="ML"))
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
#Use Kappa model
rownames(squamate_phylo_order_traits)<-squamate_phylo_order_traits$taxaname
squamate_phylo_order_traits_compdata<-comparative.data(pruned_squamatetree,squamate_phylo_order_traits,names.col = 'taxaname')
summary(pgls(squamate_log_C_E_tiporder~squamate_log_bodymass_tiporder, data = squamate_phylo_order_traits_compdata,kappa='ML'))
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
