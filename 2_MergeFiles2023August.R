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

# Correct typos and species names
# tab$plant_species=gsub("[[:punct:]]", "",tab$plant_species)
# tab$plant_species=stringi::stri_trans_general(tab$plant_species, "latin-ascii")
# tab$plant_species=trimws(tab$plant_species)

##### START OF ANALYSIS JUNE 2023 #####

# Add a value of "1" to each pairwise interaction registered by cameras

tab$Y = 1

# choose columns to use 

tab1=tab[,c(1,2,3,5,12,16,20,21,22,23,24,25,26,34,35,36)]

# Remove non-hummingbird species

tab2<-tab1[!(tab1$hummingbird_species=="Atlapetes tricolor" | tab1$hummingbird_species=="Setophaga pitiayumi" | tab1$hummingbird_species=="Diglossa albilatera"),]
tab3<-tab2[!(tab2$hummingbird_species=="Euphonia xanthogaster" | tab2$hummingbird_species=="Aulacorhynchus haematopygus" | tab2$hummingbird_species=="Henicorhina leucophrys"),]
tab3<-tab3[!(tab3$hummingbird_species=="Diglossa lafresnayii"),]


#aggregate interaction at the day level for each waypoint:

tab3=tab3 %>% group_by(site.x, waypoint, plant_species, piercing, duration_sampling_hours,hummingbird_species) %>% summarize(Y=sum(Y), Days=length(unique(date)))
tab3$Rate<-as.numeric(round(tab3$Y/tab3$Days,0)) # Daily interaction rates


# Filter piercing interactions

#tab2 <- tab2 %>% filter(!piercing %in% c("yes"))

# Input 0s for non observed interactions
mat=reshape2::dcast(tab3,site.x+plant_species+duration_sampling_hours+waypoint+piercing
                    ~hummingbird_species,value.var="Rate", 
                    fun.aggregate = sum,
                    fill=0)

#re-make a row table: (Bayesian Stuff)
tab4=reshape2::melt(mat,id.var=c("site.x","plant_species","duration_sampling_hours","waypoint", "piercing"),variable.name = "hummingbird_species", value.name = "Y")



###  Join plant trait data
plant_trait=fread("PlantTraits_06_2023.txt", header=T)

## Concatenate names of few species between the data bases
plant_trait$final_plant_name=stringi::stri_trans_general(plant_trait$final_plant_name, "latin-ascii")
plant_trait$final_plant_name=trimws(plant_trait$final_plant_name)
plant_trait$final_plant_name[plant_trait$final_plant_name=="Abutilon hibrido"]="Abutilon hybridum" 
plant_trait$final_plant_name[plant_trait$final_plant_name=="Abutilon pictum"]="Callianthe picta"
plant_trait$final_plant_name[plant_trait$final_plant_name==""]=NA
plant_trait$final_plant_name[plant_trait$final_plant_name=="Mezobromelia capituligera"]="Cipuropsis capituligera"
plant_trait$final_plant_name[plant_trait$final_plant_name=="Cleome anomala"]="Andinocleome anomala"
plant_trait$final_plant_name[plant_trait$final_plant_name=="Calathea roseobracteata"]="Goeppertia roseobracteata"
plant_trait$final_plant_name[plant_trait$final_plant_name=="Aphelandra flammea"]="Aphelandra vitellina"
plant_trait$final_plant_name[plant_trait$final_plant_name=="Bartsia mutica"]="Neobartsia mutica"

# Correct few names in the Interactions database
tab4$plant_species[tab4$plant_species=="Renealmia aurantiifera"]="Renealmia aurantifera"

#plant_trait <- plant_trait %>% mutate_all(na_if,"")
names(plant_trait)=gsub(" ","_",names(plant_trait),fixed=T)
names(plant_trait)=gsub("Â","",names(plant_trait),fixed=T)

# Extract the mean of plant corolla length

plant_trait2=plant_trait %>% dplyr::group_by(final_plant_name) %>% 
  dplyr::summarise(meanTube=mean(as.numeric(as.character(tube_length)),na.rm=T)) 

# Change column name of plants in plant traits data base
names(plant_trait2)[1]="plant_species"

#merge it with interaction data
tab3=merge(tab4,plant_trait2,by=c("plant_species"),all.x=T,all.y=TRUE)

# Load hummingbird trait data:
humm_trait=fread("HummingbirdMorphologyShort.csv")

# Concatenate names between databases
humm_trait$SpID[humm_trait$SpID=="Thalurania fannyi"]="Thalurania colombica"
humm_trait$SpID[humm_trait$SpID=="Schistes geoffroyi"]="Schistes albogularis"
humm_trait$SpID[humm_trait$SpID=="Colibri thalassinus"]="Colibri cyanotus"

# Extract the mean for each hummingbird trait by species
moyenne=function(x){mean(x,na.rm=T)}

humm_trait2=humm_trait[,c("SpID","ExpC", "Body","Bwidth","TotC","Wchord","Bdepth","Wwide","Wlength","Rasp","Rform","WiLo","Wtap","Warea","Tail","Foot")] %>%
  group_by(SpID) %>% summarise_each(funs(moyenne))
names(humm_trait2)[1]="hummingbird"
names(humm_trait2)[2:ncol(humm_trait2)]=paste0("mean",names(humm_trait2)[2:ncol(humm_trait2)])


# Concatenate column names
names(humm_trait2)[1]="hummingbird_species"
humm_trait2$hummingbird_species[humm_trait2$hummingbird_species=="Amazilia franciae"]="Uranomitra franciae"
humm_trait2$hummingbird_species[humm_trait2$hummingbird_species=="Amazilia amabilis"]="Polyerata amabilis"


#merge it with interaction data
tab3=merge(tab3,humm_trait2,by=c("hummingbird_species"),all.x=T, all.y=FALSE)

# Remove rows withouts species id
tab3=subset(tab3, !is.na(plant_species) & !is.na(hummingbird_species))

# Check traits distribution

plot(density(tab3$meanTube, na.rm = TRUE))
plot(density(tab3$meanExpC, na.rm = TRUE))

# Correct corolla length, note that in this plant trait database corolla length is in cm 
# i.e the longest flower, B. sanguinea cannot be only 25 mm

tab3$meanTube = 10*tab3$meanTube


plot(density(tab3$meanTube, na.rm = TRUE))

## Write the two files

#fwrite(tab3,"interaction_data_day_0s_durationsampling.txt",sep="\t") # activate when removing piercing "yes" observations

fwrite(tab3,"interaction_data_day_0s_durationsampling_piercing.txt",sep="\t")



