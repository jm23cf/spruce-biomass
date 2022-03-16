library(ggplot2)
library(tidyr)
library(scales)
library(forcats)

agbi.mean = readRDS('data/agbi.mean.RDS')
# agbi.mean = readRDS('data/agbi.mean.big.RDS')

agbi.mean = agbi.mean[which(agbi.mean$year>1985),]

#################################################################################################
## read in climate data
#################################################################################################

tmean = read.csv('data/tmean_CRU.csv', stringsAsFactors = FALSE)

sites = unique(agbi.mean$site.orig)
N_sites = length(sites)

cor.tmean = data.frame(site.id = character(0), 
                     site.num=numeric(0), 
                     lat = numeric(0), 
                     long=numeric(0),
                     data.frame(matrix(NA, nrow=0, ncol=20))) 

for (i in 1:N_sites) {
  print(i)
  print(sites[i])
  
  # site.id = strsplit(fnames[i], '[.]')[[1]][1]
  # site.num = meta[match(toupper(site.id), meta$study.name), 'X']
  
  tmean.sub = tmean[which(tmean$site == sites[i]),]
  
  # crn = read.crn(paste0("Output/crn/",fnames[i]))
  # crn.df = data.frame(year=rownames(crn), value=crn[,1])
  
  agbi.site = agbi.mean[which(agbi.mean$site.orig == sites[i]),]
  
  dat = merge(agbi.site, tmean.sub)#, by.x='year', by.y='Year')
  
  dat.wide = pivot_wider(dat, 
                    id_cols = c('year', 'site', 'lat', 'long', 'agbi.mean'), 
                    names_from = 'month',
                    values_from = 'tmean', 
                    names_sort = TRUE)
  dat.wide = data.frame(dat.wide)
  
  dat.prev = dat.wide[,c('year', paste0('X',seq(5, 12)))]
  dat.prev$year = dat.prev$year + 1
  colnames(dat.prev) = c('year', paste0('P',seq(5, 12)))
  
  dat.wide = merge(dat.wide, dat.prev)
  
  # cor.tmean = cor(dat$agbi.mean, foo[,6])
  cor.vec = cor(dat.wide$agbi.mean, dat.wide[,6:ncol(dat.wide)])
  
  cor.tmean.sub = data.frame(site.id  = sites[i],
                       site.num = i,
                       lat  = dat.wide$lat[1],
                       long = dat.wide$long[1],
                       cor.vec)
  cor.tmean = rbind(cor.tmean, cor.tmean.sub)
  
}

# image(t(as.matrix(cor.dat[,5:ncol(cor.dat)])))

cor.tmean.melt = melt(cor.tmean, id.vars=c('site.id', 'site.num', 'lat', 'long'))
cor.tmean.melt$variable = factor(cor.tmean.melt$variable, levels=c(paste0('P',seq(5, 12)), paste0('X',seq(1, 12))))

write.csv(cor.tmean, 'output/cor.dat.tmean.csv', row.names=FALSE)

#################################################################################################
## read in ppt data
#################################################################################################

ppt = read.csv('data/ppt_CRU.csv', stringsAsFactors = FALSE)

sites = unique(agbi.mean$site.orig)
N_sites = length(sites)

cor.ppt = data.frame(site.id = character(0), 
                     site.num=numeric(0), 
                     lat = numeric(0), 
                     long=numeric(0),
                     data.frame(matrix(NA, nrow=0, ncol=12))) 

for (i in 1:N_sites) {
  print(i)
  print(sites[i])
  
  # site.id = strsplit(fnames[i], '[.]')[[1]][1]
  # site.num = meta[match(toupper(site.id), meta$study.name), 'X']
  
  ppt.sub = ppt[which(ppt$site == sites[i]),]
  
  # crn = read.crn(paste0("Output/crn/",fnames[i]))
  # crn.df = data.frame(year=rownames(crn), value=crn[,1])
  
  agbi.site = agbi.mean[which(agbi.mean$site.orig == sites[i]),]
  
  dat = merge(agbi.site, ppt.sub)#, by.x='year', by.y='Year')
  
  dat.wide = pivot_wider(dat, 
                         id_cols = c('year', 'site', 'lat', 'long', 'agbi.mean'), 
                         names_from = 'month',
                         values_from = 'ppt', 
                         names_sort = TRUE)
  dat.wide = data.frame(dat.wide)
  
  dat.prev = dat.wide[,c('year', paste0('X',seq(5, 12)))]
  dat.prev$year = dat.prev$year + 1
  colnames(dat.prev) = c('year', paste0('P',seq(5, 12)))
  
  dat.wide = merge(dat.wide, dat.prev)
  
  # cor.tmean = cor(dat$agbi.mean, foo[,6])
  cor.vec = cor(dat.wide$agbi.mean, dat.wide[,6:ncol(dat.wide)])
  
  cor.ppt.sub = data.frame(site.id  = sites[i],
                       site.num = i,
                       lat  = dat.wide$lat[1],
                       long = dat.wide$long[1],
                       cor.vec)
  cor.ppt = rbind(cor.ppt, cor.ppt.sub)
  
}

image(t(as.matrix(cor.ppt[,5:ncol(cor.ppt)])))

cor.ppt.melt = melt(cor.ppt, id.vars=c('site.id', 'site.num', 'lat', 'long'))
cor.ppt.melt$variable = factor(cor.ppt.melt$variable, levels=c(paste0('P',seq(5, 12)), paste0('X',seq(1, 12))))


write.csv(cor.ppt, 'output/cor.dat.ppt.csv', row.names=FALSE)

#################################################################################################
## read in climate data
#################################################################################################

tmin = read.csv('data/tmin_CRU.csv', stringsAsFactors = FALSE)

sites = unique(agbi.mean$site.orig)
N_sites = length(sites)

cor.tmin = data.frame(site.id = character(0), 
                     site.num=numeric(0), 
                     lat = numeric(0), 
                     long=numeric(0),
                     data.frame(matrix(NA, nrow=0, ncol=12))) 

for (i in 1:N_sites) {
  print(i)
  print(sites[i])
  
  # site.id = strsplit(fnames[i], '[.]')[[1]][1]
  # site.num = meta[match(toupper(site.id), meta$study.name), 'X']
  
  tmin.sub = tmin[which(tmin$site == sites[i]),]
  
  # crn = read.crn(paste0("Output/crn/",fnames[i]))
  # crn.df = data.frame(year=rownames(crn), value=crn[,1])
  
  agbi.site = agbi.mean[which(agbi.mean$site.orig == sites[i]),]
  
  dat = merge(agbi.site, tmin.sub)#, by.x='year', by.y='Year')
  
  dat.wide = pivot_wider(dat, 
                         id_cols = c('year', 'site', 'lat', 'long', 'agbi.mean'), 
                         names_from = 'month',
                         values_from = 'tmin', 
                         names_sort = TRUE)
  dat.wide = data.frame(dat.wide)
  
  dat.prev = dat.wide[,c('year', paste0('X',seq(5, 12)))]
  dat.prev$year = dat.prev$year + 1
  colnames(dat.prev) = c('year', paste0('P',seq(5, 12)))
  
  dat.wide = merge(dat.wide, dat.prev)
  
  # cor.tmin = cor(dat$agbi.mean, foo[,6])
  cor.vec = cor(dat.wide$agbi.mean, dat.wide[,6:ncol(dat.wide)])
  
  cor.tmin.sub = data.frame(site.id  = sites[i],
                       site.num = i,
                       lat  = dat.wide$lat[1],
                       long = dat.wide$long[1],
                       cor.vec)
  cor.tmin = rbind(cor.tmin, cor.tmin.sub)
  
}

image(t(as.matrix(cor.tmin[,5:ncol(cor.tmin)])))

cor.tmin.melt = melt(cor.tmin, id.vars=c('site.id', 'site.num', 'lat', 'long'))
cor.tmin.melt$variable = factor(cor.tmin.melt$variable, levels=c(paste0('P',seq(5, 12)), paste0('X',seq(1, 12))))

write.csv(cor.tmin, 'output/cor.dat.tmin.csv', row.names=FALSE)

#################################################################################################
## tmax
#################################################################################################

tmax = read.csv('data/tmax_CRU.csv', stringsAsFactors = FALSE)

sites = unique(agbi.mean$site.orig)
N_sites = length(sites)

cor.tmax = data.frame(site.id = character(0), 
                      site.num=numeric(0), 
                      lat = numeric(0), 
                      long=numeric(0),
                      data.frame(matrix(NA, nrow=0, ncol=12))) 

for (i in 1:N_sites) {
  print(i)
  print(sites[i])
  
  # site.id = strsplit(fnames[i], '[.]')[[1]][1]
  # site.num = meta[match(toupper(site.id), meta$study.name), 'X']
  
  tmax.sub = tmax[which(tmax$site == sites[i]),]
  
  # crn = read.crn(paste0("Output/crn/",fnames[i]))
  # crn.df = data.frame(year=rownames(crn), value=crn[,1])
  
  agbi.site = agbi.mean[which(agbi.mean$site.orig == sites[i]),]
  
  dat = merge(agbi.site, tmax.sub)#, by.x='year', by.y='Year')
  
  dat.wide = pivot_wider(dat, 
                         id_cols = c('year', 'site', 'lat', 'long', 'agbi.mean'), 
                         names_from = 'month',
                         values_from = 'tmax', 
                         names_sort = TRUE)
  dat.wide = data.frame(dat.wide)
  
  dat.prev = dat.wide[,c('year', paste0('X',seq(5, 12)))]
  dat.prev$year = dat.prev$year + 1
  colnames(dat.prev) = c('year', paste0('P',seq(5, 12)))
  
  dat.wide = merge(dat.wide, dat.prev)
  
  # cor.tmax = cor(dat$agbi.mean, foo[,6])
  cor.vec = cor(dat.wide$agbi.mean, dat.wide[,6:ncol(dat.wide)])
  
  cor.tmax.sub = data.frame(site.id  = sites[i],
                            site.num = i,
                            lat  = dat.wide$lat[1],
                            long = dat.wide$long[1],
                            cor.vec)
  cor.tmax = rbind(cor.tmax, cor.tmax.sub)
  
}

image(t(as.matrix(cor.tmax[,5:ncol(cor.tmax)])))

cor.tmax.melt = melt(cor.tmax, id.vars=c('site.id', 'site.num', 'lat', 'long'))
cor.tmax.melt$variable = factor(cor.tmax.melt$variable, levels=c(paste0('P',seq(5, 12)), paste0('X',seq(1, 12))))

write.csv(cor.tmax, 'output/cor.dat.tmax.csv', row.names=FALSE)

###############################################################################################
## calculate drought index
###############################################################################################

# for each site, for each year, determine drought index
# drought index in chen:
# average temperature from april to june of current growing season
# divided by total precip from september previous year to june of the current

# determine cumulative precip from prev sept to current june
ppt.cum = data.frame(site.id = character(0), 
                     site.num=numeric(0), 
                     lat = numeric(0), 
                     long=numeric(0),
                     year = numeric(0),
                     ppt = numeric(0))

for (i in 1:N_sites) {
  print(i)
  print(sites[i])
  
  # site.id = toupper(strsplit(fnames[i], '[.]')[[1]][1])
  # site.num = meta[match(toupper(site.id), meta$study.name), 'X']
  
  ppt.sub = ppt[which(ppt$site == sites[i]),]
  
  ppt.sub= pivot_wider(ppt.sub, 
                         id_cols = c('year', 'site'), 
                         names_from = 'month',
                         values_from = 'ppt', 
                         names_sort = TRUE)
  
  ppt.sub = data.frame(ppt.sub)
  
  agbi.site = agbi.mean[which(agbi.mean$site.orig == sites[i]),]
  
  ppt.sub = merge(agbi.site, ppt.sub)
  
  years = sort(ppt.sub$year)
  N_years = length(years)
  
  for (j in 1:N_years){
    if (j == 1) {
      next
    }
    sum.prev = sum(ppt.sub[which(ppt.sub$year == years[j-1]),16:19], na.rm=TRUE)
    sum.current = sum(ppt.sub[which(ppt.sub$year == years[j]),8:13], na.rm=TRUE)
    sum.tot = sum.prev + sum.current
    
    
    ppt.cum.sub = data.frame(site.id = sites[i],
                             site.num=i,
                             lat = ppt.sub$lat[1],
                             long= ppt.sub$long[1],
                             year = years[j],
                             ppt = sum.tot)
    ppt.cum = rbind(ppt.cum, ppt.cum.sub)
    
  }
  
}

# determine average temperature for current april, may, june
# take average of tmean values for these months
tmean.avg = data.frame(site.id = character(0), 
                       site.num=numeric(0), 
                       lat = numeric(0), 
                       long=numeric(0),
                       year = numeric(0),
                       tmean = numeric(0))


#help andria this is where we get stuck!!!
for (i in 1:N_sites) {
  print(i)
  print(sites[i])
  
  # site.id = toupper(strsplit(fnames[i], '[.]')[[1]][1])
  # site.num = meta[match(toupper(site.id), meta$study.name), 'X']
  
  tmean.sub = tmean[which(tmean$site == sites[i]),]
  if(any(duplicated(tmean.sub))){
    tmean.sub = tmean.sub[!duplicated(tmean.sub),]
  }
  
  tmean.sub= pivot_wider(tmean.sub, 
                       id_cols = c('year', 'site'), 
                       names_from = 'month',
                       values_from = 'tmean', 
                       names_sort = TRUE)
  
  tmean.sub = data.frame(tmean.sub)
  
  agbi.site = agbi.mean[which(agbi.mean$site.orig == sites[i]),]
  
  tmean.sub = merge(agbi.site, tmean.sub)
  
  years = sort(tmean.sub$year)
  N_years = length(years)
  
  for (j in 1:N_years){
    tmean.current = mean(as.numeric(tmean.sub[which(tmean.sub$year == years[j]),11:13], na.rm=TRUE))
    
    tmean.avg.sub = data.frame(site.id = sites[i],
                               site.num=i,
                               lat = tmean.sub$lat[1],
                               long= tmean.sub$long[1],
                               year = years[j],
                               tmean = tmean.current)
    tmean.avg = rbind(tmean.avg, tmean.avg.sub)
    
  }
  
}

# merge the data frame of average temp and cumulative ppt
di.dat = merge(tmean.avg, ppt.cum)

# add a drought index column
# determined by average temp divided by cumulative ppt 
di.dat$drought = di.dat$tmean/di.dat$ppt

write.csv(di.dat, 'data/drought_CRU.csv', row.names=FALSE)

#################################################################################################
## drought index
#################################################################################################

di = read.csv('data/drought_CRU.csv', stringsAsFactors = FALSE)

cor.di = data.frame(site.id = character(0), 
                    site.num=numeric(0), 
                    lat = numeric(0), 
                    long=numeric(0),
                    data.frame(matrix(NA, nrow=0, ncol=3))) 

for (i in 1:N_sites) {
  print(i)
  print(sites[i])
  
  site.id = sites[i]
  site.num = i
  
  di.sub = di[which(di$site.id == site.id),]
  
  agbi.site = agbi.mean[which(agbi.mean$site.orig == sites[i]),]
  
  dat = merge(agbi.site, di.sub)#, by.x='year', by.y='Year')
  
  dat.wide = dat
  
  # dat.prev = dat.wide[,c('year', 'may', 'june', 'july', 'aug', 'sep', 'oct', 'nov', 'dec')]
  # dat.prev$year = dat.prev$year + 1
  # colnames(dat.prev) = c('year', paste0('P',c('may', 'june', 'july', 'aug', 'sep', 'oct', 'nov', 'dec')))
  # 
  # dat.wide = merge(dat.wide, dat.prev)
  # 
  # cor.tmax = cor(dat$agbi.mean, foo[,6])
  cor.vec = cor(dat.wide$agbi.mean, dat.wide[,9:ncol(dat.wide)])
  
  cor.di.sub = data.frame(site.id  = site.id,
                          site.num = site.num,
                          lat  = dat.wide$lat[1],
                          long = dat.wide$long[1],
                          cor.vec)
  cor.di = rbind(cor.di, cor.di.sub)
  
}

image(t(as.matrix(cor.di[,5:ncol(cor.di)])))

dnames = c('tmean.grow', 'ppt.dormant', 'drought')
colnames(cor.di) = c('site.id', 'site.num', 'lat', 'long', 'tmean.grow', 'ppt.dormant', 'drought')

cor.di.melt = melt(cor.di, id.vars=c('site.id', 'site.num', 'lat', 'long'))
cor.di.melt$variable = factor(cor.di.melt$variable, levels=dnames)

write.csv(cor.di.melt, 'output/cor.dat.di.csv', row.names=FALSE)


#################################################################################################
## plot correlation
#################################################################################################

dat = cor.tmean.melt
#dat = cor.di.melt
dat$site.id = fct_reorder(dat$site.id, dat$lat)

ggplot(data=dat) + 
  geom_tile(aes(x=variable, y=site.id, fill=value)) +
  scale_fill_gradient2(low = muted("red"),
                       mid = "white",
                       high = muted("blue"),
                       midpoint = 0,
                       space = "Lab",
                       na.value = "grey50")

ggplot(data=dat) +
  geom_point(aes(x=variable, y=value))

ggplot(data=dat) +
  geom_boxplot(aes(x=variable, y=value)) +
  geom_hline(yintercept=0, linetype='dashed', col = 'red')

ggplot(data=dat) +
  geom_point(aes(x=lat, y=value, colour=variable))

ggplot(data=dat) +
  geom_point(aes(x=lat, y=value)) +
  geom_smooth(aes(x=lat, y=value), method=lm) +
  facet_wrap(~variable)

ggplot(data=dat) +
  geom_histogram(aes(x=value)) + 
  facet_wrap(~variable)


