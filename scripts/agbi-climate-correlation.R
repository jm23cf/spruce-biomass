library(ggplot2)
library(tidyr)
library(scales)
library(forcats)

agbi.mean = readRDS('data/agbi.mean.RDS')
# agbi.mean = readRDS('data/agbi.mean.big.RDS')

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

#################################################################################################
## plot correlation
#################################################################################################
#Can set dat to cor.tmean.melt, etc#
dat = cor.tmean.melt
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


