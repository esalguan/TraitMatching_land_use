### MERGING TRAITS AND INTERACTIONS DATASETS (EPHI DATA-AUGUST2023)

# LOAD LIBRARIES AND SET WD

library(dplyr)
library(randomForest)
library(rgbif)
library(reshape2)
library(data.table)

setwd(dir="C:/Users/guevara/Dropbox/PC/Documents/TraitMatching")


#loading interaction data:
tab=fread("InteractionsAg.txt") # 

#tab=tab %>% group_by(site.x) %>% mutate(elevation=median(Elevation_waypoint,na.rm=T),lat_site=median(Latitude_waypoint,na.rm=T),lon_site=median(Longitude_waypoint,na.rm=T))

# Correct typos and species names
tab$plant_species=gsub("[[:punct:]]", "",tab$plant_species)
tab$plant_species=stringi::stri_trans_general(tab$plant_species, "latin-ascii")
tab$plant_species=trimws(tab$plant_species)

bidon=data.frame(plant_species=sort(unique(tab$plant_species)))
for(i in 1:nrow(bidon)){
  sear=bidon[i,"plant_species"]
  if(sear=="Tillandsia venusta"){sear="Tillandsia venusta Silveira"}
  obj=name_backbone(name=sear, kingdom='Plantae') #look for the given species
  if(length(obj$rank)>0){  #if obj is not empty
    bidon$rang[i]=obj$rank  #keep the rank of the match
    bidon$confi[i]=obj$confidence #confidence in the match
    bidon$kingdom[i]=obj$kingdom #kingdom
    bidon$canon[i]=obj$canonicalName #latin name according to the GBIF
    bidon$family[i]=obj$family #latin name according to the GBIF
    bidon$genus[i]=ifelse(length(obj$genus)>0,obj$genus,NA) #latin name according to the GBIF
    if(length(obj$order)>0){bidon$order[i]=obj$order}else{bidon$order[i]=NA} #keep the order according to the GBIF
  }
}

bidon[bidon$canon %in% bidon$canon[duplicated(bidon$canon)],]

dim(tab)
tab=merge(tab,bidon,by="plant_species",all.x=T,all.y=F)
dim(tab)

##### START OF ANALYSIS JUNE 2023 #####

# Add a value of "1" to each pairwise interaction registered by cameras

tab$Y = 1

# choose columns to use 

tab1=tab[,c(1,2,3,4,5,6,13,16,21,22,23,24,25,26,43)]

# Remove non-hummingbird species

tab2<-tab1[!(tab1$hummingbird_species=="Atlapetes tricolor" | tab1$hummingbird_species=="Setophaga pitiayumi" | tab1$hummingbird_species=="Diglossa albilatera"),]
tab3<-tab2[!(tab2$hummingbird_species=="Euphonia xanthogaster" | tab2$hummingbird_species=="Aulacorhynchus haematopygus"),]


#aggregate interaction at the day level for each waypoint:

tab3=tab3 %>% group_by(site.x, waypoint, plant_species,hummingbird_species,piercing) %>% summarize(Y=sum(Y), Days=length(unique(date)))

# Filter piercing interactions

#tab2 <- tab2 %>% filter(!piercing %in% c("yes"))

tab3$Rate<-as.numeric(round(tab3$Y/tab3$Days,2)) # Daily interaction rates
#tab2$RateHr<-as.numeric(round(tab2$Y/tab2$duration_sampling_hours,2))

# Join plant trait data
plant_trait=fread("PlantTraits_06_2023.txt", header=T)
plant_trait$final_plant_name=gsub("Â","",plant_trait$final_plant_name,fixed=T)
plant_trait$final_plant_name=gsub("[[:punct:]]", "",plant_trait$final_plant_name)
plant_trait$final_plant_name=stringi::stri_trans_general(plant_trait$final_plant_name, "latin-ascii")
plant_trait$final_plant_name[plant_trait$final_plant_name=="AnthopterusÂ verticillatus"]="Anthopterus verticillatus"
plant_trait$final_plant_name=trimws(plant_trait$final_plant_name)
plant_trait$final_plant_name[plant_trait$final_plant_name=="Bomalera lutea"]="Bomarea lutea" 
plant_trait$final_plant_name[plant_trait$final_plant_name=="Abutilon hibrido"]="Abutilon hybridum" 
plant_trait$final_plant_name[plant_trait$final_plant_name=="Abutilon pictum"]="Callianthe picta"
plant_trait$final_plant_name[plant_trait$final_plant_name=="Reneleamia sp2"]="Renealmia sp2"
plant_trait$final_plant_name[plant_trait$final_plant_name=="Nassa grandiflora"]="Nasa grandiflora"
plant_trait$final_plant_name[plant_trait$final_plant_name=="Sp indt"]=NA
plant_trait$final_plant_name[plant_trait$final_plant_name==""]=NA
plant_trait$final_plant_name[plant_trait$final_plant_name=="Columnea leopardus"]="Gasteranthus leopardus"
plant_trait <- plant_trait %>% mutate(final_plant_name=recode(final_plant_name, "Columnea sp nov"="Columnea angulata"))
plant_trait <- plant_trait %>% mutate(final_plant_name=recode(final_plant_name, "Macleania cordifolia"="Macleania ericae"))
plant_trait <- plant_trait %>% mutate(final_plant_name=recode(final_plant_name, "Podandrogyne sp1"="Podandrogyne websteri"))
plant_trait <- plant_trait %>% mutate(final_plant_name=recode(final_plant_name, "Podandrogyne sp"="Podandrogyne websteri"))
plant_trait <- plant_trait %>% mutate(final_plant_name=recode(final_plant_name, "Glossoloma sp"="Glossoloma ichthyoderma"))
plant_trait <- plant_trait %>% mutate(final_plant_name=recode(final_plant_name, "Bomarea sp2"="Bomarea spissiflora"))
plant_trait <- plant_trait %>% mutate(final_plant_name=recode(final_plant_name, "Columnea sp1"="Columnea herthae"))
plant_trait$final_plant_name[plant_trait$final_plant_name=="Psychotria mashpi"]="Psychotria sp1"
plant_trait$final_plant_name[plant_trait$final_plant_name=="Psamissia ulbrichiana"]="Psammisia ulbrichiana"
plant_trait$final_plant_name[plant_trait$final_plant_name=="Mezobromelia capituligera"]="Cipuropsis capituligera"
plant_trait$final_plant_name[plant_trait$final_plant_name=="Cleome anomala"]="Andinocleome anomala"
plant_trait$final_plant_name[plant_trait$final_plant_name=="Calathea roseobracteata"]="Goeppertia roseobracteata"
plant_trait$final_plant_name[plant_trait$final_plant_name=="Aphelandra flammea"]="Aphelandra vitellina"
plant_trait$final_plant_name[plant_trait$final_plant_name=="Bartsia mutica"]="Neobartsia mutica"

# Correct few names in the Interactions database
#tab1$plant_species[tab2$plant_species=="Pitcairnia conmixta"]="Pitcairnia commixta"
tab3$plant_species[tab3$plant_species=="Renealmia aurantiifera"]="Renealmia aurantifera"
#tab2$plant_species[tab2$plant_species=="Burmeistera betulum"]="Burmeistera velutina"

plant_trait <- plant_trait %>% mutate_all(na_if,"")
names(plant_trait)=gsub(" ","_",names(plant_trait),fixed=T)
names(plant_trait)=gsub("Â","",names(plant_trait),fixed=T)
Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

# Extract the mean of plant traits

plant_trait2=plant_trait %>% dplyr::group_by(family_name,final_plant_name) %>% 
             dplyr::summarise(meanTube=mean(as.numeric(as.character(tube_length)),na.rm=T),
                              meanH_dorsal=mean(as.numeric(as.character(H_dorsal)),na.rm=T),
                              meanR_dorsal=mean(as.numeric(as.character(R_dorsal)),na.rm=T),
                             meanR_dorsal=mean(as.numeric(as.character(R_dorsal)),na.rm=T),
                              meanCurvature_dorsal=mean(as.numeric(as.character(curvature__dorsal)),na.rm=T),
                              meanH_middle=mean(as.numeric(as.character(H_middle)),na.rm=T),
                              meanR_middle=mean(as.numeric(as.character(R_middle)),na.rm=T),
                              meanCurvature_middle=mean(as.numeric(as.character(Curvature_middle)),na.rm=T),
                              meanH__ventral=mean(as.numeric(as.character(H_ventral)),na.rm=T),
                              meanR_ventral=mean(as.numeric(as.character(R_ventral)),na.rm=T),
                             meanCurvature_ventral=mean(as.numeric(as.character(Curvature_ventral)),na.rm=T),
                             meanLateral_opening=mean(as.numeric(as.character(opening_corolla_lateral)),na.rm=T),
                             meanDorsal_opening=mean(as.numeric(as.character(opening_corolla_dorsal)),na.rm=T),
                             meanStigma_lenght=mean(as.numeric(as.character(stigma_length)),na.rm=T),
                             meanAnther_lenght=mean(as.numeric(as.character(anther_length)),na.rm=T),meanDelta=mean(as.numeric(as.character(delta)),na.rm=T))

# Change column name of plants in plant traits data base
names(plant_trait2)[2]="plant_species"

#merge it with interaction data
tab3=merge(tab3,plant_trait2,by=c("plant_species"),all.x=T,all.y=TRUE)

#import hummingbird trait data:
humm_trait=fread("HummingbirdMorphologyShort.csv")
humm_trait$SpID[humm_trait$SpID=="Thalurania fannyi"]="Thalurania colombica"
humm_trait$SpID[humm_trait$SpID=="Schistes geoffroyi"]="Schistes albogularis"
humm_trait$SpID[humm_trait$SpID=="Colibri thalassinus"]="Colibri cyanotus"

moyenne=function(x){mean(x,na.rm=T)}

humm_trait2=humm_trait[,c("SpID","ExpC", "Body","Bwidth","TotC","Wchord","Bdepth","Wwide","Wlength","Rasp","Rform","WiLo","Wtap","Warea","Tail","Foot")] %>%
  group_by(SpID) %>% summarise_each(funs(moyenne))
names(humm_trait2)[1]="hummingbird"
names(humm_trait2)[2:ncol(humm_trait2)]=paste0("mean",names(humm_trait2)[2:ncol(humm_trait2)])

#lili=list.files()
#lili=lili[grep("NW",lili)]
#lili=lili[grep("Humm",lili)]
#humm_trait=NULL
#for(i in lili){
 # bidon=fread(i)
#  humm_trait=rbind(humm_trait,bidon)
#}

humm_trait$bird_species[humm_trait$bird_species=="Thalurania fannyi"]="Thalurania colombica"
#humm_trait3=humm_trait %>% group_by(bird_species) %>% summarise(meanBill=mean(meanBill,na.rm=T),meanBody=mean(meanBody,na.rm=T),meanWingL=mean(meanWing,na.rm=T))
#names(humm_trait3)[1]="hummingbird"
#humm_trait4=merge(humm_trait2,humm_trait3,by=c("hummingbird"),all=T)
#humm_trait4$meanExpC[is.na(humm_trait4$meanExpC)]=humm_trait4$meanExpC2[is.na(humm_trait4$meanExpC)]
#humm_trait4$meanExpC[is.na(humm_trait4$meanBody)]=humm_trait4$meanBody2[is.na(humm_trait4$meanBody)]
#humm_trait4$meanExpC[is.na(humm_trait4$Wlength)]=humm_trait4$meanWingL2[is.na(humm_trait4$meanWlength)]

# Concatenate column names
names(humm_trait2)[1]="hummingbird_species"
humm_trait2$hummingbird_species[humm_trait2$hummingbird_species=="Amazilia franciae"]="Uranomitra franciae"
humm_trait2$hummingbird_species[humm_trait2$hummingbird_species=="Amazilia amabilis"]="Polyerata amabilis"


#merge it with interaction data
tab3=merge(tab3,humm_trait2,by=c("hummingbird_species"),all.x=T, all.y=FALSE)

tab3=subset(tab3, !is.na(plant_species) & !is.na(hummingbird_species))

# Input 0s
mat=reshape2::dcast(tab3,site.x+plant_species+meanTube+meanExpC
                    ~hummingbird_species,value.var="Rate", 
                     fun.aggregate = length,
                     fill=0)

#re-make a row table: (Bayesian Stuff)
tab4=reshape2::melt(mat,id.var=c("site.x","plant_species","meanTube", "meanExpC"),variable.name = "hummingbird_species", value.name = "Y")


# convert corolla length in the right units (plants and hb lengths in mm)

plot(density(tab4$meanTube, na.rm = TRUE))
plot(density(tab4$meanExpC, na.rm = TRUE))

# Correct corolla length for the two tables 0 inputed and without 0s
tab3$meanTube = 10*tab3$meanTube
tab4$meanTube = 10*tab4$meanTube

plot(density(tab3$meanTube, na.rm = TRUE))

## Write the two files

fwrite(tab4,"interaction_data_day_0s.txt",sep="\t")
fwrite(tab3,"interaction_data_day_Rates.txt",sep="\t")


