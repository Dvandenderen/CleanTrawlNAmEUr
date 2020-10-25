rm(list=ls())

### Libraries
library(data.table)
library(dplyr)

### Load files
# First setting the working directory to the main folder with the unzipped csv files
#setwd("E:/DATA/Norway/btraal")

# we create a vector list with the filenames that match with a .csv ending
files = list.files('E:/DATA/Norway/btraal',pattern="*.csv")

# then we call a lapply function that takes x (every csv) and calls it back to a rbind. Check the seperator to see if it's correct
norw_dat = do.call(rbind, lapply(files, function(x) read.csv(paste('E:/DATA/Norway/btraal/',x,sep=''), stringsAsFactors = FALSE, header = TRUE, sep = ";")))
rm(files)

# change colnames from Norwegian to new names in English
setnames(norw_dat, old = c("aar","mnd","lengde","bredde","redskap","starttid","stopptid","taueT","bunndyp",
                           "opening","dist","tilstand","kvalitet","delnr","akode","art","latin","maal_fangst","fangstKvant",
                           "fangstAnt","maal_lprov","lengdemaal","lengdeProveKv","lengdeProveAnt","interv","kjonn"), 
         new = c("Year","Month","ShootLong","ShootLat","Gear","ShootTimeB","ShootTimeE","HaulDur",
                 "Depth","Netopening","Distance","quality_gear","quality_haul","SubSampleNr",
                 "SpecCode","AkodeName","ScientificName","MeasureType","Weight","NoMeas","MeasureType2","LengthMethod",
                 "WeightSubSample","AbundanceSubSample","Interv","Sex"))


##########################################################################################
#### CHANGE SPECIES LATIN NAMES
##########################################################################################

# make sure our species names start with a capital letter, and then only lower case
#str(norw_dat)

# First we steal a function from the interwebs
capitalize <- function(x){
  first <- toupper(substr(x, start=1, stop=1)) ## capitalize first letter
  rest <- tolower(substr(x, start=2, stop=nchar(x)))   ## everything else lowercase
  paste0(first, rest)
}

# It requires the column to be factors
norw_dat$ScientificName <- as.factor(norw_dat$ScientificName)

# and then we run the function on it
levels(norw_dat$ScientificName) <- capitalize(levels(norw_dat$ScientificName))
rm(capitalize)


##########################################################################################
#### CREATE HAULD ID
##########################################################################################

# Give survey name
norw_dat$Survey <- rep("NorBTS",each=length(unique(rownames(norw_dat))))

# Haulid
norw_dat$HaulID <- paste(norw_dat$Survey, norw_dat$Year,norw_dat$Month,norw_dat$Gear,norw_dat$ShootLong,norw_dat$ShootLat, norw_dat$Depth, norw_dat$ShootTimeB)

# Recalculate the haul duration because the column has weird values
# start time: ShootTimeB in XYZW where XY are hours from 0 to 24 and ZW are minutes from 0 to 59
# end time: ShootTimeE
norw_dat[norw_dat$ShootTimeE==-1,]$ShootTimeE <- 'NA'
norw_dat[norw_dat$ShootTimeB==-1,]$ShootTimeB <- 'NA'
norw_dat$ShootTimeB <- as.numeric(as.vector(norw_dat$ShootTimeB))
norw_dat$ShootTimeE <- as.numeric(as.vector(norw_dat$ShootTimeE))

times <- data.frame(cbind(norw_dat$HaulID, norw_dat$ShootTimeB, norw_dat$ShootTimeE))
names(times) <- c('HaulID','ShootTimeB','ShootTimeE')
times <- subset(times, !is.na(times$ShootTimeB))
times <- subset(times, !is.na(times$ShootTimeE))
for(i in 1:ncol(times)){times[,i] <- as.character(times[,i])}
# add 0 as characters to have length 4 of times
times[nchar(times$ShootTimeB)==2,]$ShootTimeB <- paste('00',times[nchar(times$ShootTimeB)==2,]$ShootTimeB, sep='')
times[nchar(times$ShootTimeB)==3,]$ShootTimeB <- paste('0',times[nchar(times$ShootTimeB)==3,]$ShootTimeB, sep='')
times[nchar(times$ShootTimeB)==1,]$ShootTimeB <- paste('000',times[nchar(times$ShootTimeB)==1,]$ShootTimeB, sep='')
times[nchar(times$ShootTimeE)==2,]$ShootTimeE <- paste('00',times[nchar(times$ShootTimeE)==2,]$ShootTimeE, sep='')
times[nchar(times$ShootTimeE)==3,]$ShootTimeE <- paste('0',times[nchar(times$ShootTimeE)==3,]$ShootTimeE, sep='')
times[nchar(times$ShootTimeE)==1,]$ShootTimeE <- paste('000',times[nchar(times$ShootTimeE)==1,]$ShootTimeE, sep='')

# count minutes and hours for begining and end
times$minB <- as.numeric(as.vector(substr(times$ShootTimeB, start=1, stop=2)))*60+as.numeric(as.vector(substr(times$ShootTimeB, start=3, stop=4))) 
times$minE <- as.numeric(as.vector(substr(times$ShootTimeE, start=1, stop=2)))*60+as.numeric(as.vector(substr(times$ShootTimeE, start=3, stop=4))) 
times$duration <- times$minE-times$minB
times[times$minB>1320 & times$minE<120,]$duration <- times[times$minB>1320 & times$minE<120,]$minE-times[times$minB>1320 & times$minE<120,]$minB+1440
times[times$minB>1080 & times$minE<420,]$duration <- times[times$minB>1080 & times$minE<420,]$minE-times[times$minB>1080 & times$minE<420,]$minB+1440
# all remaining times are too long or start before begining time -> to be removed
times <- subset(times, times$duration>0)
# let's check the very high times: higher than 8h?
times <- unique(times)
times$ShootTimeB <- times$ShootTimeE <- times$minB <- times$minE <- NULL
setnames(times, old='duration', new='HaulDur2')

# join back with norw_dat
norw_dat0 <- left_join(norw_dat, times, by='HaulID')
nrow(norw_dat)==nrow(norw_dat0)
norw_dat <- norw_dat0


##########################################################################################
#### SELECT GEAR TYPES
##########################################################################################

# Remove all hauls done with "wrong" gear types. Keeping "torske", "reke", "benthos
removed_gear <- c("3113","3114","3115","3118", # konsumtål
                  "3119", # sestad trål
                  "3130","3173","3174","3175","3176","3177", # industritrål
                  "3131","3171","3172", # tobistrål
                  "3132","3133","3134", # single, dobbelt and trippel
                  "3136", #bomtrål
                  "3279", #fisketrål
                  "3400", #trål
                  "3401", #IKMT
                  "3410", "3411", "3412", #semipelagisk
                  "3415", # partrål
                  "3420","3421","3422", "3423", #krabbetrål
                  "3430", #plankton
                  "3440" #bomtrål
)
norw_dat <- norw_dat[!norw_dat$Gear %in% removed_gear,]
rm(removed_gear)


##########################################################################################
#### REMOVE BAD QUALITY HAULS
##########################################################################################
# Remove bad quality hauls and gears
norw_dat <- subset(norw_dat, norw_dat$quality_gear %in% c(1,2))
norw_dat <- subset(norw_dat, norw_dat$quality_haul %in% c(1,2))

# Is there still empty species names and abundances?
check.sp <- subset(norw_dat, norw_dat$ScientificName=='') # all hauls from 1981 and 1982 with no ab/weight/spp specified
norw_dat <- subset(norw_dat, norw_dat$ScientificName!='') # remove rows with empty rows

check.ab <- subset(norw_dat, is.na(norw_dat$NoMeas)) # ok
check.sub.ab <- subset(norw_dat, is.na(norw_dat$AbundanceSubSample))
check.sum <- subset(norw_dat, is.na(norw_dat$Sum))
check.sub.w <- subset(norw_dat, is.na(norw_dat$WeightSubSample)) # same as abundance


##########################################################################################
#### STANDARDIZE UNITS AND REMOVE NEGATIVE VALUES
##########################################################################################

# HaulDuration: if the range 1-60m then minutes. If 0-1, in hours
# ICES data in minutes, convert all in minutes 1h <-> 60min
# -1, data unavailable, so insert NA

norw_dat[norw_dat$HaulDur<=1,]$HaulDur <- norw_dat[norw_dat$HaulDur<=1,]$HaulDur*60
norw_dat[norw_dat$HaulDur<10 & norw_dat$Distance>2,]$HaulDur <- norw_dat[norw_dat$HaulDur<10 & norw_dat$Distance>2,]$HaulDur*60
norw_dat[norw_dat$HaulDur<0,]$HaulDur <- NA

# Transform distance nautical miles to km
# 1nm <-> 1.852km

norw_dat$Distance <- norw_dat$Distance*1.852/1
norw_dat[norw_dat$Distance<0,]$Distance <- NA

# Change net opening to DoorSpread
setnames(norw_dat, old = "Netopening", new="DoorSpread")
norw_dat[norw_dat$DoorSpread<0,]$DoorSpread <- NA
norw_dat$DoorSpread <- norw_dat$DoorSpread/1000 # transform m into km

# Transform abundance and weight into the same units, transform weight measures all in kg
# for column Weight, use MeasureType
# for column NoMeas, use MeasureType as is category 6, *1000 individuals
# No document for conversion factors from L weight measurements!!!
# No liters measurements after 2001, so ok if we only select from 2005
# Two rows are in MeasureType or MeasureType==6, but in 1993 and 1995, so will be removed
norw_dat[norw_dat$MeasureType==5,]$Weight <- norw_dat[norw_dat$MeasureType==5,]$Weight*1000
norw_dat[norw_dat$MeasureType==6,]$Weight <- norw_dat[norw_dat$MeasureType==6,]$Weight*1000*1000
norw_dat[norw_dat$MeasureType==6,]$NoMeas <- norw_dat[norw_dat$MeasureType==6,]$NoMeas*1000
norw_dat[norw_dat$MeasureType==7,]$Weight <- norw_dat[norw_dat$MeasureType==7,]$Weight*1000
norw_dat[norw_dat$MeasureType==8,]$Weight <- norw_dat[norw_dat$MeasureType==8,]$Weight*1000
norw_dat[norw_dat$MeasureType==9,]$Weight <- norw_dat[norw_dat$MeasureType==9,]$Weight/1000

# Correction factors for gutted/without head and L transfo. might exist, but cannot find it

# Transform units from the sub-samples not possible because of NAs
norw_dat[is.na(norw_dat$WeightSubSample),]$WeightSubSample <- -1
norw_dat[is.na(norw_dat$AbundanceSubSample),]$AbundanceSubSample <- -1

norw_dat[norw_dat$MeasureType2==5,]$WeightSubSample <- norw_dat[norw_dat$MeasureType2==5,]$WeightSubSample*1000
norw_dat[norw_dat$MeasureType2==6,]$WeightSubSample <- norw_dat[norw_dat$MeasureType2==6,]$WeightSubSample*1000*1000
norw_dat[norw_dat$MeasureType2==6,]$AbundanceSubSample <- norw_dat[norw_dat$MeasureType2==6,]$AbundanceSubSample*1000
norw_dat[norw_dat$MeasureType2==7,]$WeightSubSample <- norw_dat[norw_dat$MeasureType2==7,]$WeightSubSample*1000
norw_dat[norw_dat$MeasureType2==8,]$WeightSubSample <- norw_dat[norw_dat$MeasureType2==8,]$WeightSubSample*1000
norw_dat[norw_dat$MeasureType2==9,]$WeightSubSample <- norw_dat[norw_dat$MeasureType2==9,]$WeightSubSample/1000

# Replace all -1 by NAs
norw_dat[norw_dat$WeightSubSample==(-1),]$WeightSubSample <- NA
norw_dat[norw_dat$AbundanceSubSample==(-1),]$AbundanceSubSample <- NA
norw_dat[norw_dat$Weight==(-1),]$Weight <- NA
norw_dat[norw_dat$NoMeas==(-1),]$NoMeas <- NA


##########################################################################################
#### CHANGE FORMAT AND AGGREGATE
##########################################################################################

library(reshape2)
library(dplyr)
# rehape format with length measurements and delete 0
norw_dat <- melt(norw_dat, c(names(norw_dat)[1:28], names(norw_dat)[70:73]), c(29:69), variable.name='Length', value.name='NumLen')

sum.pos <- norw_dat %>% #data with abundance at length, 354061 unique speciesxHauldIDs, it works!
  filter(Sum>0) %>%
  mutate(#HaulSpe = paste(HaulID, ScientificName, sep=' '),
    Length=replace(Length, is.na(NumLen), NA)) %>%
  filter(!is.na(Length))

sum.na <- norw_dat %>%
  filter(is.na(Sum)) %>%
  mutate(Length=replace(Length, is.na(NumLen), NA)) %>% # give NA to Length when there is no abundance at length
  distinct() # remove duplicates without length compisition data

norw_dat <- rbind(sum.pos, sum.na)

# Estimate missing swept areas
norw_dat <- norw_dat %>%
  mutate(Area.swept = DoorSpread*Distance)

nor <- norw_dat %>%
  select(HaulID, Year, Area.swept, HaulDur, Gear, Depth, Distance) %>%
  filter(Year>1989,
         !is.na(HaulDur)) %>%
  distinct()

par(mfrow=c(1,2))
plot(Area.swept ~ HaulDur, data=nor)
plot(Area.swept ~ Depth, data=nor)

nor$Dur2 <- (nor$HaulDur-mean(nor$HaulDur))^2
lm0 <- lm(Area.swept ~ HaulDur + Dur2, data=nor)

pred0 <- predict(lm0, newdata=nor, interval='confidence', level=0.95)
nor <- cbind(nor,pred0)
nor[is.na(nor$Area.swept),]$Area.swept <- nor[is.na(nor$Area.swept),]$fit

nor <- nor %>%
  select(HaulID, Area.swept) %>%
  dplyr::rename(Area2=Area.swept) %>%
  filter(Area2>=0)

nor2 <- left_join(norw_dat, nor, by='HaulID')
nor2 <- nor2 %>%
  mutate(Area.swept = coalesce(Area.swept,Area2))
norw_dat <- nor2  

# Continue cleaning
norw_dat <- norw_dat %>%
  mutate(Quarter = ceiling(as.numeric(Month)/3),
         numcpue = NoMeas/Area.swept, # nbr / km2
         wtcpue = Weight/Area.swept, # kg / km2
         numh = NoMeas*60/HaulDur2, # nbr / hour
         wgth = Weight*60/HaulDur2, # kg / h
         SubFactor.Ab = NoMeas/Sum,
         numlencpue = NumLen*SubFactor.Ab/Area.swept, # raise abundance at length to the whole sample / swept area of haul Dur
         numlenh = NumLen*SubFactor.Ab*60/HaulDur2,
         Survey = 'NorBTS',
         Season = 'NA',
         SBT=NA, 
         SST=NA,
         HaulDur = HaulDur2,
         Species = ScientificName) %>%
  select(Survey, HaulID, Year, Month, Quarter, Season, ShootLat, ShootLong, HaulDur, Area.swept, Gear, Depth, SBT, SST, Species,
         numcpue, wtcpue, numh, wgth, Length, numlencpue, numlenh)


# Change lengths
norw_dat$Length <- as.factor(norw_dat$Length)
kk <- levels(norw_dat$Length)[1:40]
Min <- as.numeric(substr(kk, start=2, stop=4)) ## get lower interval value
Max <- as.numeric(substr(kk, start=6, stop=8))   ## get higher interval value
kkk <- cbind(Min,Max)
kkkk <- apply(kkk, 1, FUN=mean)
levels(norw_dat$Length) <- c(kkkk, 5500)

# To keep for th merge script
#norw_dat <- subset(norw_dat, !norw_dat$Distance==0)
# Remove haul duration lower than 20' and higher than 2h
#norw_dat <- subset(norw_dat, norw_dat$HaulDur2<120 & norw_dat$HaulDur2>20)



##########################################################################################
#### CLEAN SPECIES NAMES
##########################################################################################

library(worms)
library(worrms)
library(crul)
library(urltools)

# First we create a species list with unique species names
sp_list <- as.data.frame(norw_dat$Species)
colnames(sp_list)[1] <- c("species") # changing the column name
sp_list <- data.frame(sp_list[!duplicated(sp_list[c("species")]),])
colnames(sp_list)[1] <- c("species") # changing the column name
sp_list <- subset(sp_list, !is.na(sp_list$species))

# Clean species names
sp_list$species <- as.character(sp_list$species, stringsAsFactors = FALSE)

cleanspl <- function(name){
  temp <- paste0(toupper(substr(name, 1, 1)), tolower(substr(name, 2, nchar(name))))
  temp <- gsub("\\((.+?)\\)$", "", temp)
  temp <- gsub("\\.$", "", temp) #remove last .
  temp <- gsub(" $", "", temp) #remove empty space
  temp <- gsub("            $", "", temp) #remove empty spaces
  temp <- gsub("^ ", "", temp)
  temp <- gsub(" .$", "", temp) #remove last single character
  temp <- gsub(" .$$", "", temp) #remove last double characters
  temp <- gsub(" unid$", "", temp)
  temp <- gsub(" unident$", "", temp)
  temp <- gsub(" spp$", "", temp)
  temp <- gsub(" spp.$", "", temp)
  temp <- gsub(" sp$", "", temp)
  temp <- gsub(" sp.$", "", temp)
  temp <- gsub(" s.c$", "", temp)
  temp <- gsub(" s.p$", "", temp)
  temp <- gsub(" s.f$", "", temp)
  temp <- gsub(" s.o$", "", temp)
  temp <- gsub(" so$", "", temp)
  temp <- gsub(" yoy$", "", temp)
  temp <- gsub(" YOY$", "", temp)
  temp <- gsub(" n. gen$", "", temp)
  temp <- gsub(" aff.$", "", temp)
  temp <- gsub(" kroyer", "", temp)
  temp <- gsub("-occidentale", "", temp)
  temp <- gsub("_rubricing", "", temp)
  temp <- gsub("-dilectus", "", temp)
  temp <- gsub(" cf tubicola", "", temp)
  temp <- gsub(",$", "", temp)
  temp <- gsub(",", " ", temp)
  return(temp)
}
sp_list$species.cleaned <- cleanspl(sp_list$species)
sp_list$species.cleaned <- cleanspl(sp_list$species.cleaned)

# Creating a for loop that check each species names with WoRMS and returns aphiaID to a new column in the species list
sp_list$aphiaID <- NA
for(i in 1:length(unique(sp_list$species.cleaned))){
  print(i)
  y <- sp_list[i,1]
  sp_list$aphiaID[i] <- tryCatch(as.data.frame(wm_name2id(name = y)), error=function(err) NA)
}

sp_list$aphiaID <- as.character(sp_list$aphiaID)
#setwd('C:/Users/auma/Documents/PhD DTU Aqua/(iv) Clean surveys/2. Clean taxonomy')
#write.csv(sp_list, file='sp_list_europe_2004_2017.csv', row.names=FALSE)

clean.names <- read.csv('C:/Users/auma/Documents/PhD DTU Aqua/(iv) Clean surveys/2. Clean taxonomy/check.species.old.names.WORMS.csv')
clean.names$aphiaID <- NULL
setnames(sp_list, old='species.cleaned', new='ScientificName')
# species = to match back with dataset
# scientific name = merge with worms correct names
# ScientificName_worms= correct names for some species (the ones that did not have the aphiaID)
sp_list <- left_join(sp_list, clean.names, by='ScientificName')
sp_list_ok <- subset(sp_list, sp_list$aphiaID!='NA')
sp_list_change <- subset(sp_list, sp_list$aphiaID=='NA')
sp_list_change$aphiaID <- sp_list_change$AphiaID_worms
sp_list_change$species.cleaned <- NULL
sp_list_change$ScientificName <- sp_list_change$ScientificName_worms

sp_list <- rbind(sp_list_ok, sp_list_change)
sp_list$AphiaID_worms <- sp_list$ScientificName_worms <- NULL
sp_list <- subset(sp_list, sp_list$ScientificName!='')
sp_list$aphiaID <- as.numeric(sp_list$aphiaID)

# The Aphia ID loop returns NAs and negative ID. Check, nrows has to be 0
sp.pb<-subset(sp_list, is.na(aphiaID) | aphiaID<0) 
dim(sp.pb) # good

# list with only aphia IDs
aphia_list <- c(sp_list$aphiaID) 

# remove duplicates
aphia_list <- aphia_list[!duplicated(aphia_list)]

# creating taxonomy tables for each species
my_sp_taxo <- wm_record_(id = aphia_list)

# row binds all the results and pass to data frame. 
df_test <- data.frame(do.call(rbind, my_sp_taxo))
df_test$url <- df_test$lsid <- df_test$citation <- NULL
df_test$isExtinct <- df_test$modified <- df_test$valid_authority <- df_test$unacceptreason <- NULL
df_test$authority <- df_test$status <- df_test$taxonRankID <- df_test$isBrackish <- df_test$isFreshwater <- df_test$isTerrestrial <- df_test$match_type <- NULL

# In the class column, we only keep the 5 class we want, corresponding to fish species
df_test <- subset(df_test, class %in% c("Elasmobranchii","Actinopterygii","Holocephali","Myxini","Petromyzonti")) 

# Only keep species rank
df_test <- subset(df_test, df_test$rank=='Species')

# List of names to keep
keep_sp <- data.frame(df_test) # subsetting
keep_sp <- data.frame(unlist(keep_sp$scientificname)) #unlisting
names(keep_sp) <- 'rightname'
sp_list <- subset(sp_list, ScientificName %in% keep_sp$rightname)
sp_list <- sp_list[!duplicated(sp_list$aphiaID),]
keep_sp$rightname <- as.character(keep_sp$rightname)
identical(sort(sp_list$ScientificName), sort(keep_sp$rightname))

norw_dat <- subset(norw_dat, norw_dat$Species %in% keep_sp$rightname)
setnames(sp_list, old='species', new='Species')
norw_dat <- left_join(norw_dat, sp_list, 'Species')
norw_dat$AphiaID <- norw_dat$aphiaID
norw_dat$aphiaID <- NULL
norw_dat <- norw_dat %>% 
  mutate(Species = ScientificName)

norw_dat <- norw_dat %>%
  select(Survey, HaulID, Year, Month, Quarter, Season, ShootLat, ShootLong, HaulDur, Area.swept, Gear, Depth, SBT, SST, Species, numcpue, 
         wtcpue, numh, wgth, Length, numlencpue, wtlencpue, numlenh, wgtlenh)

rm(df_test, keep_sp, my_sp_taxo, norw_dat0, sp_list, sp_list_change, sp_list_ok, sp.pb, check.ab, check.sp, check.sub.ab)
rm(check.sub.w, check.sum, clean.names, sum.na, sum.pos, times)
rm(i, aphia_list, kk, kkk, Max, Min, y, cleanspl, kkkk)

save(norw_dat, file='data/norBTS.25.10.2020.RData')


##########################################################################################
#### CHECKING SPATIAL DISTRIBUTION OF DATA POINTS
##########################################################################################

require(ggplot2)

coords <- norw_dat %>%
  filter(Month %in% c(8,9)) %>% 
  select(ShootLat, ShootLong, Survey) %>%
  distinct()

ggplot(coords,aes(ShootLong,ShootLat))+
  #borders(xlim=c(-120,-110),ylim=c(40,41),fill="azure3",colour = "black") +
  borders(xlim=c(-20,50),ylim=c(54,82),fill="lightgrey",colour = "lightgrey") +
  coord_quickmap(xlim=c(-20,50),ylim=c(54,82))+theme_bw()+
  geom_point(cex = 1, col='navyblue')

coordY <- norw_dat %>%
  filter(Month %in% c(8,9)) %>% 
  select(HaulID, ShootLat, ShootLong, Survey, Year) %>%
  distinct() %>% 
  group_by(Year) %>% 
  summarize(number = length(unique(HaulID)))

# spp recorded during summer survey: august/september:
spp.summer<- norw_dat %>%
  filter(Month %in% c(8,9)) %>% 
  select(Species) %>%
  distinct()
spp.summer <- sort(spp.summer)
write.csv(spp.summer, file='spp.summer.csv')
