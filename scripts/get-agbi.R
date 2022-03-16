library(allodb)
library(ggplot2)
library(dplyr)

#################################################################################################
## read in data
#################################################################################################

finaldat = readRDS("data/dbh-reconstructed.RDS")

# remove negative dbh values
# these arose because we subtracted the ring widths from the measured dbh
# need to go back and resolve in file that constructs that data frame
finaldat = finaldat[which(finaldat$dbh>0),]
# which(finaldat$dbh<0)

#################################################################################################
## aboveground biomass (agb) calculation
#################################################################################################

# use get_biomass function from allodb
finaldat$agb= get_biomass(dbh = finaldat$dbh,
                          genus = "Picea",
                          species = "glauca",
                          coords = finaldat[,c('long', 'lat')])

# for each site, for each year, sum tree agb's 
# number of trees sampled differs at each site, so not useful as is
agb.total = finaldat %>% 
  group_by(year, site.orig) %>% 
  summarize(agb.total = sum(agb), .groups='keep')

# for each site, for each year, find mean tree agb 
# number of trees sampled differs at each site, so mean more useful that total
agb.mean = finaldat %>% 
  group_by(year, site.orig) %>% 
  summarize(agb.total = mean(agb), .groups='keep')

#################################################################################################
## aboveground biomass increment (agbi) calculation
## this is how much new biomass is put on in a given year
#################################################################################################

# for each site, for each year, for each tree
# determine agbi
dat.agbi = finaldat %>% 
  group_by(tree.orig, site.orig) %>%
  arrange(year, .by_group=TRUE) %>%
  mutate(agbi = agb - lag(agb))

# most sites don't go back very far
# should look at this earlier when doing exploratory data analysis
# will need to determine appropriate cutoff
dat.agbi = dat.agbi[which(dat.agbi$year>1970),]

dat.agbi$size = NA
dat.agbi$size[which(dat.agbi$dbh>100)] = 'big'
dat.agbi$size[which((dat.agbi$dbh<100)&(dat.agbi$dbh>40))] = 'med'
dat.agbi$size[which(dat.agbi$dbh<40)] = 'small'


# different number of trees sampled at each site
# so mean agbi for each site, for each year 
# more useful than total agbi
agbi.mean = dat.agbi %>% 
  group_by(year, site.orig, lat , long, elev) %>% 
  summarize(agbi.mean = mean(agbi, na.rm=TRUE), .groups='keep')

# most sites don't go back very far
# should look at this earlier when doing exploratory data analysis
# will need to determine appropriate cutoff
agbi.mean = agbi.mean[which(agbi.mean$year>1970),]

saveRDS(agbi.mean, 'data/agbi.mean.RDS')

agbi.mean.big = dat.agbi %>% 
  filter(size %in% c('med', 'big')) %>% 
  group_by(year, site.orig, lat , long) %>% 
  summarize(agbi.mean = mean(agbi, na.rm=TRUE), .groups='keep')

saveRDS(agbi.mean.big, 'data/agbi.mean.big.RDS')

#################################################################################################
## visualize agbi
#################################################################################################

ggplot(data=dat.agbi) +
  geom_boxplot(aes(x=agbi, y=site.orig))

ggplot(data=agbi.mean) +
  geom_boxplot(aes(x=agbi.mean, y=site.orig))

ggplot(data=agbi.mean) +
  geom_line(aes(x=year, y=agbi.mean, group=site.orig))

ggplot(data=agbi.mean) +
  geom_point(aes(x=lat, y=agbi.mean, group=site.orig))

ggplot(data=agbi.mean) +
  geom_boxplot(aes(y=agbi.mean, x=factor(year)))


agbi.site.mean = dat.agbi %>% 
  group_by(site.orig, lat , long) %>% 
  summarize(agbi.mean = mean(agbi, na.rm=TRUE), .groups='keep')

ggplot(data=agbi.site.mean) +
  geom_point(aes(x=lat, y=agbi.mean, group=site.orig))

ggplot(data=agbi.site.mean) +
  geom_histogram(aes(x=agbi.mean, y =..density..))


agbi.year.mean = dat.agbi %>%
  group_by(year) %>%
  summarize(agbi.mean = mean(agbi, na.rm=TRUE), .groups='keep')

ggplot(data=agbi.year.mean) +
  geom_point(aes(x=year, y=agbi.mean))

#
agbi.year.size.mean = dat.agbi %>%
  group_by(year, size) %>%
  summarize(agbi.mean = mean(agbi, na.rm=TRUE), .groups='keep')

ggplot(data=agbi.year.size.mean) +
  geom_point(aes(x=year, y=agbi.mean, colour=size))

