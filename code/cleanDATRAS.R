#############################################################
#### Code to download and clean survey data from DATRAS
#### Coding: Aurore Maureaud, March 2021
#############################################################
Sys.setenv(LANG = "en")
rm(list=ls())


##########################################################################################
#### LOAD LIBRARIES
##########################################################################################
library(data.table)
library(dplyr)
library(icesDatras)
library(worms)
library(worrms)
library(crul)
library(urltools) # check if you have Rcpp installed, no need to load


##########################################################################################
#### LOAD FILES
##########################################################################################
last.year <- 2019

# Haul info from Datras
hh.ns <- getDATRAS(record='HH', survey='NS-IBTS', years=c(1967:last.year), quarters=c(1,3))
hh.baltic <- getDATRAS(record='HH', survey='BITS', years=c(1991:last.year), quarters=c(1,4))
hh.evhoe <- getDATRAS(record='HH', survey='EVHOE', years=c(1997:last.year), quarters=4)
hh.cgfs <- getDATRAS(record='HH', survey='FR-CGFS', years=c(1998:last.year), quarters=4)
hh.igfs <- getDATRAS(record='HH', survey='IE-IGFS', years=c(2003:last.year), quarters=4)
hh.nigfs <- getDATRAS(record='HH', survey='NIGFS', years=c(2005:last.year), quarters=c(1:4))
hh.pt <- getDATRAS(record='HH', survey='PT-IBTS', years=c(2002:last.year), quarters=c(3:4))
hh.rock <- getDATRAS(record='HH', survey='ROCKALL', years=c(1999:2009), quarters=3)
hh.scorock <- getDATRAS(record='HH', survey='SCOROC', years=c(2011:last.year), quarters=3)
hh.swc <- getDATRAS(record='HH', survey='SWC-IBTS', years=c(1985:2010), quarters=c(1:4))
hh.scowcgfs <- getDATRAS(record='HH', survey='SCOWCGFS', years=c(2011:last.year), quarters=c(1:4))
hh.canmar <- getDATRAS(record='HH', survey='Can-Mar', years=c(1970:last.year), quarters=c(1:4))

hh <- rbind(hh.ns, hh.baltic, hh.evhoe, hh.cgfs, hh.igfs, hh.nigfs, hh.pt, hh.rock, hh.scorock, 
            hh.swc, hh.scowcgfs, hh.canmar)

# Length info from DATRAS
hl.ns <- getDATRAS(record='HL', survey='NS-IBTS', years=c(1967:last.year), quarters=c(1,3))
hl.baltic <- getDATRAS(record='HL', survey='BITS', years=c(1991:last.year), quarters=c(1,4))
hl.evhoe <- getDATRAS(record='HL', survey='EVHOE', years=c(1997:last.year), quarters=4)
hl.cgfs <- getDATRAS(record='HL', survey='FR-CGFS', years=c(1998:last.year), quarters=4)
hl.igfs <- getDATRAS(record='HL', survey='IE-IGFS', years=c(2003:last.year), quarters=4)
hl.nigfs <- getDATRAS(record='HL', survey='NIGFS', years=c(2005:last.year), quarters=c(1:4))
hl.pt <- getDATRAS(record='HL', survey='PT-IBTS', years=c(2002:last.year), quarters=c(3:4))
hl.rock <- getDATRAS(record='HL', survey='ROCKALL', years=c(1999:2009), quarters=3)
hl.scorock <- getDATRAS(record='HL', survey='SCOROC', years=c(2011:last.year), quarters=3)
hl.swc <- getDATRAS(record='HL', survey='SWC-IBTS', years=c(1985:2010), quarters=c(1:4))
hl.scowcgfs <- getDATRAS(record='HL', survey='SCOWCGFS', years=c(2011:last.year), quarters=c(1:4))
hl.canmar <- getDATRAS(record='HL', survey='Can-Mar', years=c(1970:last.year), quarters=c(1:4))

hl <- rbind(hl.ns, hl.baltic, hl.evhoe, hl.cgfs, hl.igfs, hl.nigfs, hl.pt, hl.rock, hl.scorock,
            hl.swc, hl.scowcgfs, hl.canmar)

rm(hl.ns, hl.baltic, hl.evhoe, hl.cgfs, hl.igfs, hl.nigfs, hl.pt, hl.rock, hl.scorock, hl.swc, hl.scowcgfs, hl.canmar, 
   hh.ns, hh.baltic, hh.evhoe, hh.cgfs, hh.igfs, hh.nigfs, hh.pt, hh.rock, hh.scorock, hh.swc, hh.scowcgfs, hh.canmar)


##########################################################################################
#### CREATE A UNIQUE HAUL ID
##########################################################################################
hl$HaulID <- paste(hl$Survey, hl$Year,hl$Quarter, hl$Country, hl$Ship, hl$Gear, hl$StNo, hl$HaulNo)
hl$SweepLngt <- hl$SpecCodeType <- hl$SpecCode <- hl$Sex <- hl$DateofCalculation <- hl$RecordType <- NULL
hh$HaulID <- paste(hh$Survey, hh$Year,hh$Quarter, hh$Country, hh$Ship, hh$Gear, hh$StNo, hh$HaulNo)

# Is the HaulID unique?
hhn <- unique(hh$HaulID)
length(hhn)==nrow(hh)

# check which one is not
# pb <- c()
# for (i in 1:length(hhn)){
#  j <- which(hh$HaulID==hhn[i])
#  if(length(j)>1){pb <- hhn[i]}
# }

# > pb
# [1] "NS-IBTS 1995 1 NA AA36 GOV 999 999"

hh$DateofCalculation <- hh$ThClineDepth <- hh$ThermoCline <- hh$SwellHeight <- hh$SwellDir <- hh$WindSpeed <- hh$WindDir <- hh$BotCurSpeed <- NULL
hh$BotCurDir <- hh$SurCurSpeed <- hh$SurCurDir <- hh$SpeedWater <- hh$TowDir <- hh$WgtGroundRope <- hh$KiteDim <- hh$Buoyancy <- NULL
hh$DoorWgt <- hh$DoorSurface <- hh$WarpDen <- hh$Warpdia <- hh$Warplngt <- hh$Tickler <- hh$Rigging <- hh$Netopening <- NULL
hh$HydroStNo <- hh$HaulLat <- hh$SweepLngt <- hh$HaulLong <- hh$DayNight <- hh$Stratum <- hh$TimeShot <- hh$Day <- hh$RecordType <- hh$GearExp <- hh$DoorType <- NULL

hh <- hh %>% filter(HaulID!="NS-IBTS 1995 1 NA AA36 GOV 999 999") # remove the non-unique HaulID in hh and hl
hl <- hl %>% filter(HaulID!="NS-IBTS 1995 1 NA AA36 GOV 999 999")

# Only keep hauls where there is the length composition. 70273 hauls in hh and 70273 in hl
hh <- subset(hh, hh$HaulID %in% hl$HaulID)
hl <- subset(hl, hl$HaulID %in% hh$HaulID)

# save step for future use (save outside github as file is large)
#if(identical(sort(unique(hh$HaulID)),sort(unique(hl$HaulID)))){
# save(hh, file='data/HH.28.02.2021.RData')
# save(hl, file='data/HL.28.02.2021.RData')
# }
#load('data/HH.28.02.2021.RData')
#load('data/HL.28.02.2021.RData')

##########################################################################################
#### MERGE HH and HL FILES
##########################################################################################

haulidhl <- sort(unique(hl$HaulID))
haulidhh <- sort(unique(hh$HaulID))
identical(haulidhh, haulidhl)

#survey <- merge(hh, hl, by='HaulID', all.x=FALSE, all.y=TRUE)
survey <- right_join(hh, hl, by=c('HaulID','Survey','Quarter','Country','Ship','Gear','StNo','HaulNo','Year'))
nrow(survey)==nrow(hl)

survey <- survey %>% 
  dplyr::rename(SBT = BotTemp,
                SST = SurTemp,
                Speed = GroundSpeed,
                AphiaID = Valid_Aphia)

### Check if the HaulID is unique
### Not the case for the baltic sea, a lot of duplicates!!!
#ids <- unique(hh$HaulID)
#pb <- vector()
# for(i in 1:length(ids)){
#   x <- which(hh$HaulID==ids[i])
#   if(length(x)>1){pb[length(pb)+1] <- ids[i]}
# }
# print(pb) # dim 0 ok!


##########################################################################################
#### REMOVE INVALID DATA
##########################################################################################
survey <- survey %>% 
  filter(HaulVal %in% 'V', #Remove invalid hauls
         !is.na(AphiaID), # Remove invalid species records
         SpecVal %in% c(1,10,4,7),
         DataType %in% c('S','R','C'))


##########################################################################################
#### RESCALE DATA INTO ABUNDANCE FOR THE HAUL DURATION AND ABUNDANCE AT LENGTH
##########################################################################################
# If Data Type=='C', abundance at length already re-adjusted with time so get back the abundance for the actual duration of the haul.
# If data type=='R', abundance at length is multiplied by sub-factor and adjusted to time
survey$CatCatchWgt = as.numeric(survey$CatCatchWgt)

survey <- survey %>% 
  mutate(HLNoAtLngt = case_when(DataType=='C' ~ HLNoAtLngt*SubFactor*HaulDur/60,
                                DataType %in% c('S','R') ~ HLNoAtLngt*SubFactor),
         TotalNo = case_when(DataType=='C' ~ TotalNo*HaulDur/60, 
                             DataType %in% c('S','R') ~ TotalNo),
         CatCatchWgt = case_when(DataType=='C' ~ CatCatchWgt*HaulDur/60,
                                 DataType %in% c('S','R') ~ CatCatchWgt)) %>% 
  select(-HaulVal, -DataType, -StdSpecRecCode, -SpecVal, -SubWgt, -SubFactor) %>% 
  mutate(Survey = if_else(Survey=='SCOWCGFS', 'SWC-IBTS', Survey)) %>% 
  mutate(Survey = if_else(Survey=='SCOROC','ROCKALL',Survey)) %>% 
  filter(!(Survey=="NS-IBTS" & BySpecRecCode %in% c(0,2,3,4,5)), # remove hauls where not all species are recorded
         !(Survey=="BITS" & BySpecRecCode==0))
 
##########################################################################################
#### GET THE SWEPT AREA in km2
##########################################################################################
survey <- survey %>% 
  mutate(WingSpread = replace(WingSpread, WingSpread==-9, NA),
         DoorSpread = replace(DoorSpread, DoorSpread==-9, NA),
         Speed = replace(Speed, Speed==-9, NA),
         Distance = replace(Distance, Distance==-9, NA),
         Depth = replace(Depth, Depth==-9, NA),
         Area.swept = Distance*0.001*DoorSpread*0.001,
         Area.swept = if_else(is.na(Area.swept), Speed*1.852*HaulDur/60*DoorSpread*0.001, Area.swept))

# Re-estimate the swept area from a linear model per survey
### EVHOE ###
evhoe <- survey %>%
  filter(Survey=='EVHOE') %>%
  select(-TotalNo, -NoMeas, -CatCatchWgt, -LngtCode, -LngtClass, -HLNoAtLngt, -AphiaID) %>%
  distinct()

# par(mfrow=c(2,2))
# plot(Area.swept ~ Depth, data=evhoe)
# plot(Area.swept ~ HaulDur, data=evhoe)
# plot(Area.swept ~ Speed, data=evhoe)
# plot(Area.swept ~ Distance, data=evhoe)

evhoe$HaulDur[evhoe$HaulDur == 1470] <- NA # remove one outlier for model fit
evhoe$Area.swept[evhoe$Area.swept == 0] <- NA # remove zeros from area swept
evhoe$Depth <- log(evhoe$Depth)
lm0 <- lm(Area.swept ~ HaulDur + Depth, data=evhoe)
summary(lm0)

pred0 <- predict.lm (object=lm0, newdata=evhoe, interval='confidence', level=0.95)
evhoe <- cbind(evhoe, pred0)
evhoe[is.na(evhoe$Area.swept),]$Area.swept <- evhoe[is.na(evhoe$Area.swept),]$fit

evhoe <- evhoe %>%
  select(HaulID, Area.swept) %>%
  dplyr::rename(Area2=Area.swept) # a few NA's
area2 <- evhoe

### North Sea ###
nsibts <- survey %>%
  filter(Survey=='NS-IBTS',
         #Year>1989,
         !is.na(Depth)) %>%
  select(Year, HaulID, HaulDur, Area.swept, Depth, Ship, Gear, GearEx.x, DoorType, Speed, Distance) %>%
  distinct()

# par(mfrow=c(2,2))
# plot(Area.swept ~ HaulDur, data=nsibts, log='xy')
# plot(Area.swept ~ Depth, data=nsibts)
# plot(Area.swept ~ Distance, data=nsibts)
# plot(Area.swept ~ Speed, data=nsibts)

nsibts$Area.swept[nsibts$Area.swept < 0.0035] <- NA # remove one outlier (doesn't affect prediction)
nsibts$Depth <- log(nsibts$Depth)
lm0 <- lm(Area.swept ~ HaulDur + Depth, data=nsibts)

pred0 <- predict(lm0, newdata=nsibts, interval='confidence', level=0.95)
nsibts <- cbind(nsibts,pred0)
nsibts[is.na(nsibts$Area.swept),]$Area.swept <- nsibts[is.na(nsibts$Area.swept),]$fit

nsibts <- nsibts %>%
  select(HaulID, Area.swept) %>%
  dplyr::rename(Area2=Area.swept)
area2 <- rbind(nsibts, area2)

### SWC-IBTS ###
swc <- survey %>%
  filter(Survey=='SWC-IBTS') %>%
  select(Year, HaulDur, Area.swept, Depth, Ship, Gear, HaulID, Speed, Distance) %>%
  distinct()

# plot(Area.swept ~ HaulDur, data=swc)
# plot(Area.swept ~ Depth, data=swc, log='x')
swc$Depth <- log(swc$Depth)
lm0 <- lm(Area.swept ~ HaulDur + Depth, data=swc)

pred0 <- predict(lm0, newdata=swc, interval='confidence', level=0.95)
swc <- cbind(swc,pred0)
swc[is.na(swc$Area.swept),]$Area.swept <- swc[is.na(swc$Area.swept),]$fit

swc <- swc %>%
  select(HaulID, Area.swept) %>%
  dplyr::rename(Area2=Area.swept)
area2 <- rbind(area2, swc)

### BITS ###
bits <- survey %>%
  filter(Survey=='BITS') %>%
  select(Year, HaulDur, Area.swept, Depth, Ship, Gear, HaulID) %>%
  distinct()

# plot(Area.swept ~ HaulDur, data=bits)
# plot(Area.swept ~ Depth, data=bits, log='x')
lm0 <- lm(Area.swept ~ HaulDur + log(Depth), data=bits)

pred0 <- predict(lm0, newdata=bits, interval='confidence', level=0.95)
bits <- cbind(bits,pred0)
bits[is.na(bits$Area.swept),]$Area.swept <- bits[is.na(bits$Area.swept),]$fit

bits <- bits %>%
  select(HaulID, Area.swept) %>%
  dplyr::rename(Area2=Area.swept)
area2 <- rbind(area2, bits)

### IE-IGFS ###
ie <- survey %>%
  filter(Survey=='IE-IGFS',
         Year>1989) %>%
  select(Year, HaulDur, Area.swept, Depth, Ship, Gear, HaulID) %>%
  distinct()

# plot(Area.swept ~ HaulDur, data=ie)
# plot(Area.swept ~ Depth, data=ie, log='x')
ie$Depth <- log(ie$Depth)
lm0 <- lm(Area.swept ~ HaulDur + Depth, data=ie)

pred0 <- predict(lm0, newdata=ie, interval='confidence', level=0.95)
ie <- cbind(ie,pred0)
ie[is.na(ie$Area.swept),]$Area.swept <- ie[is.na(ie$Area.swept),]$fit

ie <- ie %>%
  select(HaulID, Area.swept) %>%
  dplyr::rename(Area2=Area.swept)
area2 <- rbind(area2, ie)

# FR-CGFS very few hauls with swept area data
cgfs <- survey %>%
  filter(Survey=='FR-CGFS') %>%
  select(Year, HaulDur, Area.swept, Depth, Ship, Gear, HaulID, Speed, Distance) %>%
  distinct()

# par(mfrow=c(2,2))
# plot(Area.swept ~ HaulDur, data=cgfs)
# plot(Area.swept ~ Depth, data=cgfs)
# plot(Area.swept ~ Distance, data=cgfs) # Distance always reported

lm0 <- lm(Area.swept ~ HaulDur + Depth + Distance, data=cgfs)

pred0 <- predict(lm0, newdata=cgfs, interval='confidence', level=0.95)
cgfs <- cbind(cgfs,pred0)
cgfs[is.na(cgfs$Area.swept),]$Area.swept <- cgfs[is.na(cgfs$Area.swept),]$fit

cgfs <- cgfs %>%
  select(HaulID, Area.swept) %>%
  dplyr::rename(Area2=Area.swept)
area2 <- rbind(area2, cgfs)

### NIGFS ###
nigfs <- survey %>%
  filter(Survey=='NIGFS') %>%
  mutate(DurQ = ifelse(HaulDur<40,'S','L')) %>%
  select(Year, HaulDur, Area.swept, Depth, Ship, Gear, HaulID, DurQ, Speed, Distance) %>%
  distinct()

# par(mfrow=c(1,2))
# plot(Area.swept ~ HaulDur, data=nigfs)
# plot(Area.swept ~ Depth, data=nigfs)

# Model for short hauls
nigfsS <- nigfs %>%
  filter(DurQ=='S',
         !is.na(Depth))
# par(mfrow=c(2,2))
# plot(Area.swept ~ HaulDur, data=nigfsS)
# plot(Area.swept ~ Depth, data=nigfsS)
# plot(Area.swept ~ Speed, data=nigfsS)
nigfsS$Depth2 <- (nigfsS$Depth-mean(nigfsS$Depth))^2
lm0 <- lm(Area.swept ~ HaulDur + Depth + Depth2, data=nigfsS)

pred0 <- predict(lm0, newdata=nigfsS, interval='confidence', level=0.95)
nigfsS <- cbind(nigfsS,pred0)
nigfsS[is.na(nigfsS$Area.swept),]$Area.swept <- nigfsS[is.na(nigfsS$Area.swept),]$fit

# Model for short hauls
nigfsL <- nigfs %>%
  filter(DurQ=='L',
         !is.na(Depth))
# par(mfrow=c(2,2))
# plot(Area.swept ~ HaulDur, data=nigfsL)
# plot(Area.swept ~ Depth, data=nigfsL)
# plot(Area.swept ~ Speed, data=nigfsL)
nigfsL$Depth2 <- (nigfsL$Depth-mean(nigfsL$Depth))^2
lm0 <- lm(Area.swept ~ HaulDur + Depth + Depth2, data=nigfsL)

pred0 <- predict(lm0, newdata=nigfsL, interval='confidence', level=0.95)
nigfsL <- cbind(nigfsL,pred0)
nigfsL[is.na(nigfsL$Area.swept),]$Area.swept <- nigfsL[is.na(nigfsL$Area.swept),]$fit

nigfs <- rbind(nigfsL, nigfsS)
nigfs <- nigfs %>%
  select(HaulID, Area.swept) %>%
  dplyr::rename(Area2=Area.swept)
area2 <- rbind(area2, nigfs)

### ROCKALL ###
rock <- survey %>%
  filter(Survey=='ROCKALL') %>%
  select(Year, HaulDur, Area.swept, Depth, Ship, Gear, HaulID, Speed, Distance) %>%
  distinct()

# par(mfrow=c(2,2))
# plot(Area.swept ~ HaulDur, data=rock)
# plot(Area.swept ~ Depth, data=rock)
# plot(Area.swept ~ Speed, data=rock)
# plot(Area.swept ~ Distance, data=rock, log='x')

lm0 <- lm(Area.swept ~ HaulDur + Depth, data=rock)

pred0 <- predict(lm0, newdata=rock, interval='confidence', level=0.95)
rock <- cbind(rock,pred0)
rock[is.na(rock$Area.swept),]$Area.swept <- rock[is.na(rock$Area.swept),]$fit

rock <- rock %>%
  select(HaulID, Area.swept) %>%
  dplyr::rename(Area2=Area.swept)
area2 <- rbind(area2, rock)

### PORTUGAL ###
# no swept area information, use data from all other surveys
pt <- survey %>%
   filter(!is.na(HaulDur),
         !is.na(Depth),
         !is.na(Speed),
         Survey != 'Can-Mar') %>%
  select(Survey, Year, HaulDur, Area.swept, Depth, Ship, Gear, HaulID, Speed, Distance) %>%
  distinct()

# par(mfrow=c(2,2))
# plot(Area.swept ~ HaulDur, data=pt)
# plot(Area.swept ~ Depth, data=pt)
# plot(Area.swept ~ Speed, data=pt)
# plot(Area.swept ~ Distance, data=pt)
pt$Speed[pt$Speed >30] <- NA
lm0 <- lm(Area.swept ~ HaulDur + Depth + Speed, data=pt)

pred0 <- predict(lm0, newdata=pt, interval='confidence', level=0.95)
pt <- cbind(pt,pred0)
pt[is.na(pt$Area.swept),]$Area.swept <- pt[is.na(pt$Area.swept),]$fit

pt <- pt %>%
  filter(Survey=='PT-IBTS') %>%
  select(HaulID, Area.swept) %>%
  dplyr::rename(Area2=Area.swept)
area2 <- rbind(area2, pt)

### Can-mar ###
cmar <- survey %>%
  filter(Survey=='Can-Mar') %>%
  select(HaulID, Area.swept) %>%
  dplyr::rename(Area2=Area.swept)
area2 <- rbind(area2, cmar)

rm(bits, cgfs, ie, nsibts, pt, nigfsL, nigfsS, nigfs, pred0, lm0, evhoe, swc, rock, cmar)

# Paste new estimates to survey data frame
area2 <- area2 %>% distinct()
survey0 <- left_join(survey, area2, by='HaulID')
survey0 <- survey0 %>%
  mutate(Area.swept = coalesce(Area.swept, Area2)) %>%
  select(-Area2) %>%
  filter(is.na(Area.swept) | Area.swept>0)
survey <- survey0


##########################################################################################
#### GET CPUEs AND RIGHT COLUMNS NAMES
##########################################################################################

# Remove data without length composition or negative values
xx <- subset(survey, HLNoAtLngt<0 | is.na(LngtClass))
no_length_hauls <- sort(unique(xx$HaulID))

# Only keep abundances/weight
survey <- survey %>%
  filter(!(HaulID %in% no_length_hauls)) %>% # remove hauls without length data
  mutate(numcpue = TotalNo/Area.swept, # abundance/km2
         wtcpue = CatCatchWgt/(Area.swept*1000), #weight in kg/km2
         numh = (TotalNo*60)/HaulDur, # abundance/hour
         wgth = CatCatchWgt*60/(HaulDur*1000), #weight in kg/h
         num = TotalNo, #raw number of individuals
         wgt = CatCatchWgt/1000, # raw weight in kg         
         numlencpue = HLNoAtLngt/Area.swept, #abundance/km2 per length class
         numlenh = HLNoAtLngt*60/HaulDur, #abundance/h per length class
         Season = 'NA',
         Depth = replace(Depth, Depth<0, NA),
         SBT = replace(SBT, SBT<0, NA),
         SST = replace(SST, SST<0, NA),
         LngtClass = ifelse(LngtCode %in% c('.','0'), LngtClass*0.1, LngtClass)) %>% # fix unit of length class
  dplyr::rename(Length = LngtClass) %>% 
  # group_by(Survey, HaulID, StatRec, Year, Month, Quarter, Season, ShootLat, ShootLong, HaulDur, Area.swept, Gear, Depth, SBT, SST, AphiaID, Length) %>%
  # summarize_at(.vars=c('numcpue', 'wtcpue', 'numh', 'wgth', 'num', 'wgt', 'numlencpue','numlenh'), .funs=function(x) sum(x, na.rm=T)) %>%
  select(Survey, HaulID, StatRec, Year, Month, Quarter, Season, ShootLat, ShootLong, HaulDur, Area.swept, Gear, Depth, SBT, SST,
         AphiaID, CatIdentifier, numcpue, wtcpue, numh, wgth, num, wgt, Length, numlencpue, numlenh)
survey <- data.frame(survey)


##########################################################################################
#### Clean species names
##########################################################################################

survey$Species <- NA
dat.ices <- survey
aphia_list <- unique(dat.ices$AphiaID)
aphia_list <- aphia_list[!duplicated(aphia_list)]

# creating taxonomy tables for each species
my_sp_taxo <- wm_record_(id = aphia_list)

# row binds all the results and pass to data frame. 
df_test <- data.frame(do.call(rbind, my_sp_taxo))
df_test$url <- df_test$lsid <- df_test$citation <- NULL
df_test$isExtinct <- df_test$modified <- df_test$valid_authority <- df_test$unacceptreason <- NULL
df_test$authority <- df_test$status <- df_test$taxonRankID <- df_test$isBrackish <- df_test$isFreshwater <- df_test$isTerrestrial <- df_test$match_type <- NULL
#check if it identified everything
dim(subset(df_test, is.na(df_test$phylum))) # ok

# In the class column, we only keep the 5 groups we want. 
df_test <- subset(df_test, class %in% c("Elasmobranchii","Actinopteri","Holocephali","Myxini","Petromyzonti")) 

keep_sp <- data.frame(df_test) # subsetting
keep_sp <- data.frame(unlist(keep_sp$valid_name)) #unlisting
names(keep_sp) <- 'ScientificName'
keep_ap <- data.frame(df_test) # subsetting
keep_ap <- data.frame(unlist(keep_ap$AphiaID))
names(keep_ap) <- 'AphiaID'
keep_gen <- data.frame(df_test) # subsetting
keep_gen <- data.frame(unlist(keep_gen$genus))
names(keep_gen) <- 'Genus'
keep_fa <- data.frame(df_test) # subsetting
keep_fa <- data.frame(unlist(keep_fa$family))
names(keep_fa) <- 'Family'
keep <- cbind(keep_ap, keep_sp, keep_gen, keep_fa)

dat.ices <- subset(dat.ices, dat.ices$AphiaID %in% keep_ap$AphiaID)
dat.ices <- left_join(dat.ices, keep, by='AphiaID')
dat.ices$Species <- dat.ices$ScientificName
dat.ices$ScientificName <- NULL
survey <- dat.ices

survey <- survey %>%
  select(Survey, HaulID, StatRec, Year, Month, Quarter, Season, ShootLat, ShootLong, HaulDur, Area.swept, 
         Gear, Depth, SBT, SST, Family, Genus, Species, CatIdentifier, numcpue, wtcpue, numh, wgth, num, wgt, Length, numlencpue, numlenh)
survey$AphiaID <- NULL


### Code to integrate from Anna Rindorf on species bycatch corrections
survey <- data.frame(survey)
survey <- survey %>%
  mutate(Species = recode(Species,'Dipturus batis'='Dipturus','Dipturus flossada'='Dipturus',
                          'Dipturus batis-complex'='Dipturus','Dipturus intermedia'='Dipturus',
                          'Dipturus'='Dipturus','Liparis montagui'='Liparis',
                          'Liparis liparis'='Liparis','Liparis liparis liparis'='Liparis',
                          'Chelon aurata'='Chelon','Chelon ramada'='Chelon',
                          'Mustelus mustelus/asterias'='Mustelus','Mustelus'='Mustelus',
                          'Mustelus mustelus'='Mustelus','Mustelus asterias'='Mustelus',
                          'Alosa'='Alosa','Alosa alosa'='Alosa','Alosa fallax'='Alosa',
                          'Argentina'='Argentina','Argentinidae'='Argentina',
                          'Argentina silus'='Argentina','Argentina sphyraena'='Argentina',
                          'Callionymus reticulatus'='Callionymus','Callionymus maculatus'='Callionymus',
                          'Ciliata mustela'='Ciliata','Ciliata septentrionalis'='Ciliata',
                          'Gaidropsarus'='Gaidropsarus','Gaidropsaurus macrophthalmus'='Gaidropsarus',
                          'Gaidropsaurus mediterraneus'='Gaidropsarus','Gaidropsaurus vulgaris'='Gaidropsarus',
                          'Sebastes'='Sebastes','Sebastes norvegicus'='Sebastes','Sebastes mentella'='Sebastes',
                          'Sebastes marinus'='Sebastes','Syngnathus'='Syngnatus',
                          'Syngnathus rostellatus'='Syngnatus','Syngnathus acus'='Syngnatus',
                          'Syngnathus typhle'='Syngnatus','Nerophis ophidion'='Syngnatus',
                          'Pomatoschistus'='Pomatoschistus','Pomatoschistus microps'='Pomatoschistus',
                          'Pomatoschistus minutus'='Pomatoschistus','Pomatoschistus pictus'='Pomatoschistus',
                          'Lesueurigobius'='Gobius','Gobius cobitis'='Gobius','Gobius niger'='Gobius',
                          'Leusueurigobius friesii'='Gobius','Neogobius melanostomus'='Gobius',
                          'Neogobius'='Gobius'))


##########################################################################################
#### RE-CALCULATE WEIGHTS
##########################################################################################
detach(package:worms)
detach(package:plyr)

# select only certain gears 
# 1. summary of gears per survey
gears <- data.frame(survey) %>% 
  group_by(Survey, Gear) %>% 
  summarise(hauls = length(unique(HaulID)), years = length(unique(Year))) %>% 
  select(Survey, Gear, hauls, years)

# 2. only select certain gears per survey (GOV and/or most dominant in cases without GOV)
survey <- survey %>% 
  filter(!(Survey=="NS-IBTS" & Gear %in% c('ABD', 'BOT', 'DHT', 'FOT', 'GRT', 'H18', 'HOB', 'HT', 'KAB', 'VIN')),
         !(Survey=="BITS" & Gear %in% c('CAM', 'CHP', 'DT', 'EGY', 'ESB', 'EXP', 'FOT', 'GRT', 'H20', 'HAK', 'LBT','SON')),
         !(Survey=="PT-IBTS" & Gear=='CAR'),
         !(Survey=="Can-Mar" & Gear=='Y36'))


# 3. associate an LME to each haul and make final list of species
### Prepare list for estimating length-weight parameters
list.taxa <- survey %>% 
  select(HaulID, Survey, ShootLat, ShootLong, Family, Genus, Species) %>% 
  distinct()

# get LME
library(rgdal)
shape1 <- readOGR(dsn = "LME shapefile",layer="lme66")
coords <- list.taxa %>%
  dplyr::select(ShootLat, ShootLong, Survey) %>%
  distinct()
str(coords)

coordinates(coords) <- ~ ShootLong + ShootLat
proj4string(coords) <- proj4string(shape1)
lme <- over(coords, shape1)

coords <- list.taxa %>%
  dplyr::select(ShootLat, ShootLong, Survey) %>%
  distinct()
coords <- cbind(coords, lme$LME_NUMBER)
setnames(coords, old='lme$LME_NUMBER', new='lme')

# plot
# ggplot(coords,aes(ShootLong,ShootLat))+
#   borders('world', xlim=c(-90,50), ylim=c(25,85), fill='black', colour='black') +
#   coord_quickmap(xlim=c(-90,50), ylim=c(25,85))+
#   geom_polygon(data=shape1, aes(y=lat, x=long, group=group), fill='lightgrey', colour='black')+
#   theme_bw()+xlab('')+ylab('')+
#   geom_point(cex = 0.2, colour='blue')

coords$lme <- as.factor(coords$lme)
#Select from each LME 50 long and lat
ind <- c()
for (i in 1:nlevels(coords$lme)){
  ind <- c(ind, sample(which(coords$lme==levels(coords$lme)[i]), 50, replace = FALSE))
}
long50 <- coords$ShootLong[ind]
lat50 <- coords$ShootLat[ind]
lme50 <- rep(levels(coords$lme), each=50)
#For each haul without LME find a close LME that has an LME number already
nlme <- subset(coords, is.na(lme)) # many hauls without LME 710
nlme$ShootLat <- as.numeric(as.vector(nlme$ShootLat))
nlme$ShootLong <- as.numeric(as.vector(nlme$ShootLong))
long50 <- as.numeric(as.vector(long50))
lat50 <- as.numeric(as.vector(lat50))
dilme <- c()
for (i in 1:length(lme50)){
  dilme <- cbind(dilme, (nlme$ShootLat-lat50[i])**2 + (nlme$ShootLong-long50[i])**2)
}
mindi <- apply(dilme, 1, which.min) 
coords$lme[is.na(coords$lme)] <- lme50[mindi] # assign the closest LME number to each haul without LME


#Check
coords$ShootLat <- as.numeric(as.vector(coords$ShootLat))
coords$ShootLong <- as.numeric(as.vector(coords$ShootLong))

# rockall not assigned to Faroe plateau but to celtic sea LME
coords$lme <- as.character(coords$lme)
coords <- coords %>%
  mutate(lme = replace(lme, Survey  =='ROCKALL', '60')) %>%
    as.data.frame()
#plot(coords$ShootLong, coords$ShootLat, col=rainbow(length(unique(coords$lme)))[as.factor(coords$lme)], pch=".")

survey <- left_join(survey, coords, by=c('ShootLat', 'ShootLong','Survey')) 
survey <- survey %>% filter(Species!='Gobioidei')

library(tidyverse)
list.taxa <- survey %>% 
  select(Family, Genus, Species, lme) %>% 
  distinct() %>%
  mutate(fao = 27,
         Subspecies = str_split(Species, pattern = " ", simplify=T)[,3],
         Species = str_split(Species, pattern = " ", simplify=T)[,2],
         Species = if_else(Subspecies!="", paste(Species, Subspecies, sep=" "), Species))
write.csv(data.frame(list.taxa), file="traits/taxa.DATRAS.FB.tofill5.csv", row.names=FALSE) # not filled yet, continue with 4 (from Aurore)
save(survey, file = "data/DATRAS_before_lw_19Jul2021.RData")


# 4. re-calculate weights with length-weight relationships
load('data/DATRAS_before_lw_19Jul2021.RData') # note not on github to keep it small in size
datalw <- read.csv('traits/taxa.DATRAS.FB_filled4.csv') %>% 
  mutate(Taxon = case_when(level=='family' ~ family,
                           level=='genus' ~ genus,
                           level=='species' ~ paste(genus, species, sep=" ")),
         lme = as.factor(lme)) %>% 
  select(-fao,-family,-genus,-species)

survey <- survey %>% 
  mutate(Taxon = case_when(is.na(Species) & is.na(Genus) ~ Family,
                           Species=="" & is.na(Genus) ~ Family,
                           is.na(Species) & !is.na(Genus) ~ Genus,
                           Species=="" & !is.na(Genus) ~ Genus,
                           !is.na(Species) ~ Species))

survey[survey$Species=='Syngnatus',]$Taxon <- 'Syngnathus'
survey[survey$Species=='Syngnatus',]$Species <- 'Syngnathus'

# make sure ROCKALL is part of LME 24 (change when there is a filled version 5)
survey <- survey %>%
  mutate(lme = replace(lme, Survey  =='ROCKALL', '24')) %>%
  as.data.frame()

# summarize abundance/weight at the haul level
survey.num <- left_join(survey, datalw, by=c('Taxon','lme')) %>% 
  select(Survey,HaulID,StatRec,Year,Month,Quarter,Season,ShootLat,ShootLong,HaulDur,Area.swept,Gear,Depth,SBT,SST,Family,Genus,Species,Taxon,
         CatIdentifier,numcpue,numh,num) %>% 
  distinct() %>% 
  group_by(Survey,HaulID,StatRec,Year,Month,Quarter,Season,ShootLat,ShootLong,HaulDur,Area.swept,Gear,Depth,SBT,SST,Family,Genus,Species,Taxon) %>%
  summarize_at(.vars=c('numcpue', 'numh', 'num'), .funs = function(x) sum(x)) %>% 
  ungroup()

survey.wgt <- left_join(survey, datalw, by=c('Taxon','lme')) %>% 
  select(Survey,HaulID,StatRec,Year,Month,Quarter,Season,ShootLat,ShootLong,HaulDur,Area.swept,Gear,Depth,SBT,SST,Family,Genus,Species,Taxon,
         CatIdentifier,wtcpue,wgth,wgt) %>% 
  distinct() %>% 
  group_by(Survey,HaulID,StatRec,Year,Month,Quarter,Season,ShootLat,ShootLong,HaulDur,Area.swept,Gear,Depth,SBT,SST,Family,Genus,Species,Taxon) %>%
  summarize_at(.vars=c('wtcpue', 'wgth', 'wgt'), .funs = function(x) sum(x)) %>% 
  ungroup()

survey1 <- full_join(survey.num, survey.wgt, 
                     by=c('Survey','HaulID','StatRec','Year','Month','Quarter',
                          'Season','ShootLat','ShootLong','HaulDur','Area.swept',
                          'Gear','Depth','SBT','SST','Family','Genus','Species','Taxon'))

# summarize abundance/weight from length data
survey2 <- left_join(survey, datalw, by=c('Taxon','lme')) %>% 
  mutate(wgtlencpue = numlencpue*a*Length^b/1000, # divide by 1000 to get kg/km2
         wgtlenh = numlenh*a*Length^b/1000) %>% # divide by 1000 to get kg/h
  group_by(Survey,HaulID,StatRec,Year,Month,Quarter,Season,ShootLat,ShootLong,HaulDur,Area.swept,Gear,Depth,SBT,SST,Family,Genus,Species,Taxon, a, b) %>% 
  summarize_at(.vars=c('numlencpue','numlenh','wgtlencpue','wgtlenh'), .funs=function(x) sum(x)) %>% 
  ungroup()

# merge both and compare
nrow(survey1)==nrow(survey2)
survey3 <- full_join(survey1, survey2, by=c('Survey','HaulID','StatRec','Year','Month','Quarter','Season','ShootLat','ShootLong','HaulDur',
                                             'Area.swept','Gear','Depth','SBT','SST','Family','Genus','Species','Taxon'))

library(ggplot2)

# correlation between abundances to check calculations are right
cor(x = survey3$numh, y = survey3$numlenh, method = 'pearson')
xx <- subset(survey3, !is.na(numcpue))
cor(x = xx$numcpue, y = xx$numlencpue, method = 'pearson')

# check weights
xx <- subset(survey3, wtcpue   >0 & wgtlencpue>0)
cor(x = xx$wtcpue , y = xx$wgtlencpue, method = 'pearson')

xx <- subset(survey3, wgth>0 & wgtlenh>0)
cor(x = xx$wgth, y = xx$wgtlenh, method = 'pearson')

### cor = 0.92 and 0.90 so something does not work.

##########################################################################################
# CHECK PER SURVEY
##########################################################################################

# no zeros
xx <- subset(survey3, wgth>0 & wgtlenh>0)

# rockall looks OK
ggplot(subset(xx, Survey=='ROCKALL'), aes(x=wgth, y=wgtlenh)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed", size=0.5) + scale_x_log10() + scale_y_log10() 

# IE-IGFS looks OK
ggplot(subset(xx, Survey=='IE-IGFS'), aes(x=wgth, y=wgtlenh)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed", size=0.5) + scale_x_log10() + scale_y_log10() 

# NIGFS looks OK
ggplot(subset(xx, Survey=='NIGFS'), aes(x=wgth, y=wgtlenh)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed", size=0.5) + scale_x_log10() + scale_y_log10() 

# PT-IBTS looks OK
ggplot(subset(xx, Survey=='PT-IBTS'), aes(x=wgth, y=wgtlenh)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed", size=0.5) + scale_x_log10() + scale_y_log10() 

# FR-CGFS looks OK
ggplot(subset(xx, Survey=='FR-CGFS'), aes(x=wgth, y=wgtlenh)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed", size=0.5) + scale_x_log10() + scale_y_log10() 

# SWC-IBTS issue
ggplot(subset(xx, Survey=='SWC-IBTS'), aes(x=wgth, y=wgtlenh)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed", size=0.5) + scale_x_log10() + scale_y_log10()

comp <- subset(xx, Survey=='SWC-IBTS') %>% 
  select(HaulID,wgtlenh,wgth) %>% 
  distinct() %>% 
  group_by(HaulID) %>%
  summarize_at(.vars=c('wgtlenh', 'wgth'), .funs = function(x) sum(x)) %>% 
  ungroup() %>%
  as.data.frame()

ggplot(comp, aes(x=wgth, y=wgtlenh)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed", size=0.5) + scale_x_log10() + scale_y_log10()

comp$factor <-   comp$wgtlenh / comp$wgth 
plot(comp$factor)
resc <- comp$HaulID[comp$factor > 40]

# after check with original haul length data (HL) for some resc haulid, weight is clearly wrong factor 100  
survey3 <- survey3 %>%
  mutate(wtcpue = if_else(HaulID %in% resc, wtcpue*100,wtcpue),
         wgth = if_else(HaulID %in% resc , wgth*100,wgth),
         wgt = if_else(HaulID %in% resc , wgt*100,wgt))

# BITS issue
ggplot(subset(xx, Survey=='BITS'), aes(x=wgth, y=wgtlenh)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed", size=0.5) + scale_x_log10() + scale_y_log10() 

comp <- subset(xx, Survey=='BITS') %>% 
  select(HaulID,wgtlenh,wgth) %>% 
  distinct() %>% 
  group_by(HaulID) %>%
  summarize_at(.vars=c('wgtlenh', 'wgth'), .funs = function(x) sum(x)) %>% 
  ungroup() %>%
  as.data.frame()

ggplot(comp, aes(x=wgth, y=wgtlenh)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed", size=0.5) + scale_x_log10() + scale_y_log10()

comp$factor <-   comp$wgtlenh / comp$wgth
plot(comp$factor)
resc <- comp$HaulID[comp$factor > 40]

# after check with original haul length data (HL) for some resc haulid, weight is clearly wrong factor 100  
survey3 <- survey3 %>%
  mutate(wtcpue = if_else(HaulID %in% resc, wtcpue*100,wtcpue),
         wgth = if_else(HaulID %in% resc , wgth*100,wgth),
         wgt = if_else(HaulID %in% resc , wgt*100,wgt))

# EVHOE may have an issue, no changes as not very clear
ggplot(subset(xx, Survey=='EVHOE'), aes(x=wgth, y=wgtlenh)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed", size=0.5) + scale_x_log10() + scale_y_log10()

comp <- subset(xx, Survey=='EVHOE') %>% 
  select(HaulID,wgtlenh,wgth) %>% 
  distinct() %>% 
  group_by(HaulID) %>%
  summarize_at(.vars=c('wgtlenh', 'wgth'), .funs = function(x) sum(x)) %>% 
  ungroup() %>%
  as.data.frame()

ggplot(comp, aes(x=wgth, y=wgtlenh)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed", size=0.5) + scale_x_log10() + scale_y_log10()

comp$factor <-   comp$wgtlenh / comp$wgth 
plot(comp$factor)

# NS - IBTS issue
ggplot(subset(xx, Survey=='NS-IBTS'), aes(x=wgth, y=wgtlenh)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed", size=0.5) + scale_x_log10() + scale_y_log10() 

comp <- subset(xx, Survey=='NS-IBTS') %>% 
  select(HaulID,wgtlenh,wgth) %>% 
  distinct() %>% 
  group_by(HaulID) %>%
  summarize_at(.vars=c('wgtlenh', 'wgth'), .funs = function(x) sum(x)) %>% 
  ungroup() %>%
  as.data.frame()

ggplot(comp, aes(x=wgth, y=wgtlenh)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed", size=0.5) + scale_x_log10() + scale_y_log10()

comp$factor <-   comp$wgtlenh / comp$wgth
comp$uni <- c(1:nrow(comp))
plot(comp$factor~comp$uni,ylim=c(0,120))
points(comp$factor[comp$factor > 20]~comp$uni[comp$factor > 20],col="red")
points(comp$factor[comp$factor > 8 & comp$factor <20]~comp$uni[comp$factor  > 8 & comp$factor <20],col="blue")

# two issues - one estimate 100 times higher based on length, the other 10 times
resc <- comp$HaulID[comp$factor > 20] 
resc2 <- comp$HaulID[comp$factor > 8 & comp$factor <20]

# after check with original haul length data (HL) for some resc haulid, weight is clearly wrong factor 100 
# and also a cluster of factor 10
survey3 <- survey3 %>%
  mutate(wtcpue = if_else(HaulID %in% resc, wtcpue*100,wtcpue),
         wgth = if_else(HaulID %in% resc , wgth*100,wgth),
         wgt = if_else(HaulID %in% resc , wgt*100,wgt))

survey3 <- survey3 %>%
  mutate(wtcpue = if_else(HaulID %in% resc2, wtcpue*10,wtcpue),
         wgth = if_else(HaulID %in% resc2 , wgth*10,wgth),
         wgt = if_else(HaulID %in% resc2 , wgt*10,wgt))

# Can-MAR not needed weight is filled for all

# check again correlations
xx <- subset(survey3, wtcpue> 0 & wgtlencpue>0)
cor(x = xx$wtcpue , y = xx$wgtlencpue, method = 'pearson') # looks better

xx <- subset(survey3, wgth>0 & wgtlenh>0)
cor(x = xx$wgth, y = xx$wgtlenh, method = 'pearson') # looks better

# now check per haul without zeros, NAs
xx <- subset(survey3, wtcpue>0 & wgtlencpue>0)

comp <- xx %>% 
  select(HaulID,wgtlencpue,wtcpue) %>% 
  distinct() %>% 
  group_by(HaulID) %>%
  summarize_at(.vars=c('wgtlencpue', 'wtcpue'), .funs = function(x) sum(x)) %>% 
  ungroup() %>%
  as.data.frame()

ggplot(comp, aes(x=wtcpue, y=wgtlencpue)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed", size=0.5) + scale_x_log10() + scale_y_log10()

cor(x = xx$wtcpue , y = xx$wgtlencpue, method = 'pearson')
# [1] 0.9635742

##########################################################################################
#### SAVE DATA
##########################################################################################
survey3 <- survey3 %>% 
  select(-num, -wgt) %>%
  as.data.frame()
save(survey3, file='data/ICESsurveys18July2021.RData')
