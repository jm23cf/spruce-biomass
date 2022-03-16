# library(raster)
# library(ncdf4)
# library(sp)
# library(elevatr)
# library(rgdal)
library(reshape2)

# alb_proj = '+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
# +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'


# pm = readRDS('data/pollen_modern_pivot.RDS')

site.meta = read.csv('data/site_meta.csv', stringsAsFactors = FALSE)
site.meta$site = site.meta$transect

site.meta$site = ifelse(nchar(site.meta$site)==1, paste0('0', site.meta$site), site.meta$site)
# 
# #this changes back to regular lat long
# #assigning a crs to the pollen coordinates
# spdf <- SpatialPointsDataFrame(coords = pm[,c('x','y')], 
#                                data = pm,
#                                proj4string = CRS('+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
# +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'))
# 
# pm_t = spTransform(spdf, CRS("+init=epsg:4326"))
# 
# ll = data.frame(coordinates(pm_t))
# 
# # +init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 
# proj = '+init=epsg:4326 +proj=longlat +ellps=WGS84
# +datum=WGS84 +no_defs +towgs84=0,0,0'
# elev = get_elev_point(ll, 
#                       prj=proj, 
#                       src="aws")

site.meta.cru = data.frame(ID1=site.meta$site,
                       ID2=rep('region1',nrow(site.meta)), 
                       lat = site.meta$lat,
                       long = -site.meta$long,
                       el = site.meta$elev)
colnames(site.meta.cru) = c('ID1', 'ID2', 'lat', 'long', 'el')

write.csv(site.meta.cru, 'data/climateNA_input.csv', row.names = FALSE, quote=FALSE, na='.', eol = "\r\n")


# tmin_month_pm = melt(tmin_month_pm)
# colnames(tmin_month_pm) = c('site', 'year', 'tmin')
# 
# tmin_pm = rbind(tmin_pm, 
#                 data.frame(month = i, tmin_month_pm))
# tmax_pm = data.frame(month=numeric(0),
#                      site=numeric(0),
#                      year=character(0),
#                      tmax=numeric(0))
# 
# tmin_pm = data.frame(month=numeric(0),
#                      site=numeric(0),
#                      year=character(0),
#                      tmin=numeric(0))
# 
# ppt_pm = data.frame(month=numeric(0),
#                     site=numeric(0),
#                     year=character(0),
#                     ppt=numeric(0))

clim = read.csv('data/climateNA_1901-2019.csv', stringsAsFactors = FALSE)




ppt = data.frame(site = clim[,c('ID1')],
                 year = clim[,'Year'],
                 clim[,which(substr(colnames(clim), 1, 3)=='PPT')])
colnames(ppt) = c('site', 'year', seq(1, 12))
ppt_m = melt(ppt, id.vars=c('site', 'year'))
colnames(ppt_m) = c('site', 'year', 'month', 'ppt')
ppt_m = ppt_m[, c('month', 'site', 'year', 'ppt')]
write.csv(ppt_m, 'Data/ppt_CRU.csv', row.names=FALSE)

tmean = data.frame(site = clim[,c('ID1')],
                 year = clim[,'Year'],
                 clim[,which(substr(colnames(clim), 1, 3)=='Tav')])
colnames(tmean) = c('site', 'year', seq(1, 12))
tmean_m = melt(tmean, id.vars=c('site', 'year'))
colnames(tmean_m) = c('site', 'year', 'month', 'tmean')
tmean_m = tmean_m[, c('month', 'site', 'year', 'tmean')]
write.csv(tmean_m, 'Data/tmean_CRU.csv', row.names=FALSE)

tmin = data.frame(site = clim[,c('ID1')],
                   year = clim[,'Year'],
                   clim[,which(substr(colnames(clim), 1, 3)=='Tmi')])
colnames(tmin) = c('site', 'year', seq(1, 12))
tmin_m = melt(tmin, id.vars=c('site', 'year'))
colnames(tmin_m) = c('site', 'year', 'month', 'tmin')
tmin_m = tmin_m[, c('month', 'site', 'year', 'tmin')]
write.csv(tmin_m, 'Data/tmin_CRU.csv', row.names=FALSE)

tmax = data.frame(site = clim[,c('ID1')],
                   year = clim[,'Year'],
                   clim[,which(substr(colnames(clim), 1, 3)=='Tma')])
colnames(tmax) = c('site', 'year', seq(1, 12))
tmax_m = melt(tmax, id.vars=c('site', 'year'))
colnames(tmax_m) = c('site', 'year', 'month', 'tmax')
tmax_m = tmax_m[, c('month', 'site', 'year', 'tmax')]
write.csv(tmax_m, 'Data/tmax_CRU.csv', row.names=FALSE)
