rm(list=ls())

# Libraries
library(data.table)
library(dplyr)

# Load data
# ICES data
load('data/ICESSurveys25102020.RData')
ices <- survey
rm(survey)
# remove LTP = latvian pelagic trawl
# PT-IBTS --> CAR and NCT keep both because CAR covers 2003 and 2004 and NCT 2002, 2005-2014
# NS-IBTS --> mainly ABD, DHT, GRT
# BITS --> plenty
# except for BITS, we only keep GOV and ROT
ices <- subset(ices, !Gear %in% c('LPT','ABD','DHT','GRT','H18','VIN','HT','BOT','FOT','SOV','HOB','H12','KAB'))

# Norway data
load('data/NORBTS.25.10.2020.RData')
norw_dat <- subset(norw_dat, !is.na(HaulDur))
norw_dat <- subset(norw_dat, HaulDur<120) # less than 2 hours of sampling
norw_dat$Gear <- as.character(norw_dat$Gear) 

# Merge
identical(names(norw_dat), names(ices))
survey <- rbind(norw_dat, ices)

survey$Length <- as.numeric(as.vector(survey$Length))
survey$SST <- as.numeric(as.vector(survey$SST))
survey$SBT <- as.numeric(as.vector(survey$SBT))
survey$Quarter <- as.numeric(as.vector(survey$Quarter))
survey$Year <- as.numeric(as.vector(survey$Year))


# Check all species names ================================================
##########################################################################
##########################################################################
list.species <- sort(unique(survey$Species))

setwd('C:/Users/auma/Documents/PhD DTU Aqua/(iv) Clean surveys/Clean and merge')
#write.csv(list.species, 'list.spp.to.check.csv')
correct.names <- read.csv('list.spp.correct.names.csv')
correct.names$Match.type <- correct.names$ScientificName <- NULL

# join the correct list of species names
survey0 <- left_join(survey, correct.names, by='Species')
xx <- subset(survey0, is.na(survey0$ScientificName_accepted))
dim(xx) # ok
nrow(survey0)==nrow(survey)
survey <- survey0
rm(xx, survey0, correct.names)

# replace species names
survey$Species <- NULL
setnames(survey, old='ScientificName_accepted', new='Species')


# Create final datasets ==================================================
##########################################################################
##########################################################################

### DATA WITH LENGTH DATA
surveylen <- survey %>%
  mutate(numlencpue = replace(numlencpue, numlencpue==0, NA),
         numlenh = replace(numlenh, numlenh==0, NA),
         Length = replace(Length, Length==0, NA)) %>%
  filter(!is.na(Area.swept),
         !Survey %in% c('DFO-HTS','DFO-QCS','DFO-WCVI')) %>%
  select(-wtlencpue, -wgtlenh)

sub.survey <- subset(surveylen, is.na(surveylen$Length))
sub.h <- unique(sub.survey$HaulID)
surveylen <- subset(surveylen, !HaulID %in% sub.h) #remove hauls with missing length info or partial length info.

sub.survey2 <- subset(surveylen, is.na(numlencpue)) #remove when hauls do not have abundance at length 
sub.h2 <- unique(sub.survey2$HaulID) #only one haul from iceland
surveylen <- subset(surveylen, !HaulID %in% sub.h2)

rm(sub.survey, sub.h, sub.survey2, sub.h2)

setwd('C:/Users/auma/Documents/PhD DTU Aqua/(v) Trade-offs between multiple functions/Analyses')
save(surveylen, file='SurveyLengths.31.10.2019.RData')


### DATA WITH WEIGHT AND ABUNDANCE, NO LENGTH DATA
surveycpue <- survey %>%
  select(-Length, -numlencpue, -numlenh, -wgtlenh, -wtlencpue) %>%
  mutate(SBT = replace(SBT, SBT== -9999, NA),
         SST = replace(SST, SST== -9999, NA),
         Area.swept = replace(Area.swept, Area.swept==0, NA),
         HaulDur = replace(HaulDur, HaulDur==0, NA)) %>%
  filter(!Survey %in% c('AI','EBS','BSS','GOA','NBS') && (!is.na(HaulDur) | !is.na(Area.swept)), 
         #for NOAA Alaska & co., there is no info on area.swept but data already stdd
         #commands to remove data without area swept & haul duration
         !Survey %in% c('AI','EBS','BSS','GOA','NBS') && (Area.swept>0 |  HaulDur>0)) %>%
  mutate(numcpue = replace(numcpue, numcpue==0, NA),
         numh = replace(numh, numh==0, NA),
         wtcpue = replace(wtcpue, wtcpue==0, NA),
         wgth = replace(wgth, wgth==0, NA)) %>%
  distinct()

setwd('C:/Users/auma/Documents/PhD DTU Aqua/(iv) Clean surveys/Clean and merge')
save(surveycpue, file='SurveyCpues.4.06.2020.RData')

# map weight and abundances from the different surveys
sum.surveycpue <- surveycpue %>% 
  group_by(HaulID, Survey, Year, Month, ShootLat, ShootLong, HaulDur, Area.swept, Quarter) %>% 
  summarize(wtcpue=sum(wtcpue, na.rm=T),
            numcpue=sum(numcpue, na.rm=T),
            wgth=sum(wgth, na.rm=T),
            numh=sum(numh, na.rm=T))
ggplot(sum.surveycpue, aes(x=ShootLong, y=ShootLat, colour=log(wtcpue)))+geom_point()
ggplot(sum.surveycpue, aes(x=ShootLong, y=ShootLat, colour=log(numcpue)))+geom_point()

ggplot(sum.surveycpue, aes(x=ShootLong, y=ShootLat, colour=log(numh)))+geom_point()
ggplot(sum.surveycpue, aes(x=ShootLong, y=ShootLat, colour=log(wgth)))+geom_point()

# check differences between surveys
ggplot(sum.surveycpue, aes(x=as.factor(Survey), y=numcpue)) + geom_boxplot() + scale_y_log10() +
  theme_bw() + coord_flip()
ggplot(sum.surveycpue, aes(x=as.factor(Survey), y=wtcpue)) + geom_boxplot() + scale_y_log10() +
  theme_bw() + coord_flip()
ggplot(sum.surveycpue, aes(x=as.factor(Survey), y=numh)) + geom_boxplot() + scale_y_log10() +
  theme_bw() + coord_flip()
ggplot(sum.surveycpue, aes(x=as.factor(Survey), y=wgth)) + geom_boxplot() + scale_y_log10() +
  theme_bw() + coord_flip()

ggplot(sum.surveycpue, aes(x=as.factor(Survey), y=HaulDur)) + geom_boxplot() + scale_y_log10() +
  theme_bw() + coord_flip()
ggplot(sum.surveycpue, aes(x=as.factor(Survey), y=Area.swept)) + geom_boxplot() + scale_y_log10() +
  theme_bw() + coord_flip()

ggplot(sum.surveycpue, aes(x=as.factor(Survey), y=numcpue)) + geom_boxplot() + scale_y_log10() +
  theme_bw() + coord_flip() + facet_grid(. ~ as.factor(Quarter))
ggplot(sum.surveycpue, aes(x=as.factor(Survey), y=wtcpue)) + geom_boxplot() + scale_y_log10() +
  theme_bw() + coord_flip() + facet_grid(. ~ as.factor(Quarter))

ggplot(sum.surveycpue, aes(x=as.factor(Quarter), y=wtcpue)) + geom_boxplot() + scale_y_log10() +
  theme_bw() + facet_wrap(. ~ as.factor(Survey), ncol=5)
ggplot(sum.surveycpue, aes(x=as.factor(Quarter), y=numcpue)) + geom_boxplot() + scale_y_log10() +
  theme_bw() + facet_wrap(. ~ as.factor(Survey), ncol=5)


# Plot the survey sampling areas =========================================
##########################################################################
##########################################################################
library(ggplot2)
library(gganimate)

## PLOT animated map of the surveys
coords <- surveylen %>%
  select(ShootLat, ShootLong, Survey) %>%
  distinct()

# associate a different color per survey
coords$color <- NA
choice <- c('orange','#6495ED','#BFEFFF','navyblue','indianred1','#54FF9F','blue','darksalmon','orange',
            'aquamarine4','tomato','darkred','darkorchid','gold','forestgreen','lightgoldenrod','steelblue4','yellow','mediumaquamarine',
            'orange','yellow','tomato','green','blue','navyblue','black','grey','grey50','grey80', 'grey')
surveys <- sort(unique(coords$Survey))
for (i in 1:length(surveys)){coords[coords$Survey==surveys[i],]$color <- choice[i]} 

# plot
plot.surveys <- ggplot(coords,aes(ShootLong,ShootLat))+
  #borders(xlim=c(-120,-110),ylim=c(40,41),fill="azure3",colour = "black") +
  borders(xlim=c(-190,55),ylim=c(20,85),fill='white', colour = "lightgrey") +
  coord_quickmap(xlim=c(-190,55), ylim=c(20,85))+
  theme_bw()+xlab('')+ylab('')+
  geom_point(cex = 0.5, col=adjustcolor(coords$color, alpha=0.8))+
  transition_manual(factor(Survey, levels=c('BSS-US','NBS-US','EBS','AI','GOA','DFO-HTS','DFO-QCS','DFO-SOG','DFO-WCHG','DFO-WCVI','WCTRI','WCANN','GMEX','NEUS','SEUS','SCS','GOSL','ICE-GFS','NorBTS','BITS','NS-IBTS','FR-CGFS','SWC-IBTS',
                                            'ROCKALL','IE-IGFS','NIGFS','EVHOE','PT-IBTS')), cumulative=T)

# animate
survey.maps <- animate(plot.surveys, fps=30, nframes=100)

# create a GIF
anim_save(filename='map.surveys.GIF', animation = last_animation(), path = '~/PhD DTU Aqua/(iv) Clean surveys')

