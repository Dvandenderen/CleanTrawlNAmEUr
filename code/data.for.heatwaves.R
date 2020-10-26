rm(list=ls())

# Libraries
library(data.table)
library(dplyr)
library(rgdal)

# Load data
# ICES data
load('data/ICESSurveys26102020.RData')
ices <- survey
rm(survey)
# remove LTP = latvian pelagic trawl
# PT-IBTS --> CAR and NCT keep both because CAR covers 2003 and 2004 and NCT 2002, 2005-2014
# NS-IBTS --> mainly ABD, DHT, GRT
# BITS --> plenty
# except for BITS, we only keep GOV and ROT
ices <- subset(ices, !Gear %in% c('LPT','ABD','DHT','GRT','H18','VIN','HT','BOT','FOT','SOV','HOB','H12','KAB'))
ices$StatRec <- as.character(ices$StatRec)
ices$Species <- as.character(ices$Species)

# Norway data
load('data/NORBTS26102020.RData')
norw_dat <- subset(norw_dat, !is.na(HaulDur))
norw_dat <- subset(norw_dat, HaulDur<120) # less than 2 hours of sampling
norw_dat$Gear <- as.character(norw_dat$Gear) 
norw_dat$StatRec <- as.factor(norw_dat$StatRec)

# Merge
identical(names(norw_dat), names(ices))
survey <- rbind(norw_dat, ices)

survey$Length <- as.numeric(as.vector(survey$Length))
survey$SST <- as.numeric(as.vector(survey$SST))
survey$SBT <- as.numeric(as.vector(survey$SBT))
survey$Quarter <- as.numeric(as.vector(survey$Quarter))
survey$Year <- as.numeric(as.vector(survey$Year))
survey$StatRec <- as.character(survey$StatRec)


# Check all species names ================================================
##########################################################################
##########################################################################
# list.species <- sort(unique(survey$Species))
# correct.names <- read.csv('data/list.spp.correct.names.csv')
# correct.names$Match.type <- correct.names$ScientificName <- NULL
# correct.names$Species <- as.character(correct.names$Species)
# correct.names$ScientificName_accepted <- as.character(correct.names$ScientificName_accepted)
# 
# # join the correct list of species names
# survey0 <- left_join(survey, correct.names, by='Species')
# xx <- subset(survey0, is.na(survey0$ScientificName_accepted))
# dim(xx) # ok
# nrow(survey0)==nrow(survey)
# survey <- survey0
# rm(xx, survey0, correct.names)
# 
# # replace species names
# survey$Species <- NULL
# setnames(survey, old='ScientificName_accepted', new='Species')


# Create final datasets ==================================================
##########################################################################
##########################################################################

### DATA WITH LENGTH DATA
# surveylen <- survey %>%
#   mutate(numlencpue = replace(numlencpue, numlencpue==0, NA),
#          numlenh = replace(numlenh, numlenh==0, NA),
#          Length = replace(Length, Length==0, NA)) %>%
#   filter(!is.na(Area.swept))
# 
# sub.survey2 <- subset(surveylen, is.na(numlencpue)) #remove when hauls do not have abundance at length 
# sub.h2 <- unique(sub.survey2$HaulID) #only one haul from iceland
# surveylen <- subset(surveylen, !HaulID %in% sub.h2)
# 
# rm(sub.survey, sub.h, sub.survey2, sub.h2)
# 
# save(surveylen, file='data/SurveyLength26102020.RData')


### DATA WITH WEIGHT AND ABUNDANCE, NO LENGTH DATA
surveycpue <- survey %>%
  select(-Length, -numlencpue, -numlenh) %>%
  mutate(SBT = replace(SBT, SBT== -9999, NA),
         SST = replace(SST, SST== -9999, NA),
         Area.swept = replace(Area.swept, Area.swept==0, NA),
         HaulDur = replace(HaulDur, HaulDur==0, NA)) %>%
  # filter(!Survey %in% c('AI','EBS','BSS','GOA','NBS') && (!is.na(HaulDur) | !is.na(Area.swept)), 
  #        #for NOAA Alaska & co., there is no info on area.swept but data already stdd
  #        #commands to remove data without area swept & haul duration
  #        !Survey %in% c('AI','EBS','BSS','GOA','NBS') && (Area.swept>0 |  HaulDur>0)) %>%
  distinct()

# Add ICES rectangles for all surveys
rect <- readOGR(dsn = "data/ICES_rectangles/ICES_Statistical_Rectangles_Eco.shp",layer="ICES_Statistical_Rectangles_Eco")

hh <- surveycpue %>% 
  select(HaulID, ShootLat, ShootLong) %>% 
  distinct()

coordinates(hh) <- ~ ShootLong + ShootLat
proj4string(hh) <- proj4string(rect)
tr <- over(hh, rect)
tr <- tr %>% 
  select(ICESNAME)
hh <- surveycpue %>% 
  select(HaulID, ShootLat, ShootLong) %>% 
  distinct()
hh <- cbind(hh, tr)
surveycpue <- left_join(surveycpue, hh, by=c('HaulID','ShootLat','ShootLong')) %>% 
  select(-StatRec)

### Format for marine heatwaves
surveycpue <- surveycpue %>% 
  dplyr::rename(region = Survey,
         haulid = HaulID,
         year = Year,
         month = Month,
         quarter = Quarter,
         lat = ShootLat,
         long = ShootLong,
         hauldur = HaulDur,
         area.swept = Area.swept,
         depth = Depth,
         spp = Species,
         stratum = ICESNAME) %>% 
  select(-Season, -Gear, -SBT, -SST)

save(surveycpue, file='data/SurveyCPUEs26102020.RData')
