library(ggplot2)
library(rgdal)
# library(sf)
library(broom)

# read in data file
dat = read.csv('data/site_meta.csv')
dat$long = -dat$long

#this changes back to regular lat long
#assigning a crs to the pollen coordinates
dat_sp <- SpatialPointsDataFrame(coords = dat[,c('long','lat')], 
                                 data = dat,
                                 proj4string = CRS("+init=epsg:4326"))

# #transforming to the albedo crs: epsg 102001
# pol_transform = spTransform(spdf, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


alb_proj = '+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

regions <- readOGR("data/map-data/geographic/ne_50m_admin_1_states_provinces_lakes.shp", 'ne_50m_admin_1_states_provinces_lakes', encoding='UTF-8')
regions <- subset(regions, geonunit %in% c("Canada", "United States of America"))
regions <- subset(regions, !(name %in% c("Hawaii"))) # region is defined in the first part of the code (see above)
# region is defined in the first part of the code (see above)
# regions = spTransform(regions, alb_proj)
regions = spTransform(regions, CRS("+init=epsg:4326"))


# regions = subset(regions, name %in% c('Alberta'))

# ecoregions for north america
eco = readOGR('data/map-data/ecoregions/', layer='NA_CEC_Eco_Level3')
eco@data$id = rownames(eco@data)

# shapefile reprojection
# eco_reproj = spTransform(eco, alb_proj)
eco_reproj = spTransform(eco, CRS("+init=epsg:4326"))

ggplot() +
  geom_polygon(data=regions, aes(long,lat, group = group, fill = group)) +
  geom_path(data=regions, aes(long,lat, group = group), color="white") +
  geom_path(data = eco, aes(x=long, y=lat, group=group)) +

  # geom_point(data=dat, aes(x=long, y=lat)) +
  # coord_equal(xlim = c(-123, -108),
  #             ylim = c(48, 60.5)) +
  # coord_map(xlim = c(-123, -108),
  #           ylim = c(48, 60.5)) +
  # xlim(xlim = c(-123, -108)) +
  theme_bw() +
  coord_equal() +
  guides(fill = FALSE)


ggplot() +
  geom_path(data = eco, aes(x=long, y=lat, group=group)) +
  geom_polygon(data=regions, aes(long,lat, group = group, fill = group)) +
  geom_path(data=regions, aes(long,lat, group = group), color="white") +
  # geom_point(data=dat, aes(x=long, y=lat)) +
  # coord_equal(xlim = c(-123, -108),
  #             ylim = c(48, 60.5)) +
  # coord_map(xlim = c(-123, -108),
  #           ylim = c(48, 60.5)) +
  # xlim(xlim = c(-123, -108)) +
  theme_bw() +
  coord_equal() +
  guides(fill = FALSE)


b = extent(c(-122, -108, 47, 62))

eco_clipped <- crop(eco_reproj, b)

eco_df <- merge(fortify(eco_clipped), as.data.frame(eco_clipped), by.x="id", by.y=0)

N_ecos = length(unique(eco_df$NA_L3NAME))

regions_clipped <- crop(regions, b)


library(RColorBrewer)
col_fun <- colorRampPalette(brewer.pal(12,"Set1"))
col_fun(21)

ggplot() +
  geom_polygon(data=eco_df, aes(long,lat, group = group, fill = NA_L2NAME), alpha=0.7) +
  scale_fill_manual(values = col_fun(N_ecos)) +
  # scale_fill_brewer(type="qual", palette="Pastel1") + 
  # geom_path(data=eco_reproj, aes(x=long, y=lat, group = NA_L2NAME, fill = NA_L2NAME)) +
  # geom_path(data = eco_reproj, aes(x=long, y=lat, group=NA_L2NAME)) +
  geom_path(data = regions_clipped, aes(long,lat, group = group), colour="gray22") +
  geom_point(data=dat, aes(x=long, y=lat)) +
  # coord_equal(xlim = c(-123, -108),
  #             ylim = c(48, 60.5)) +
  # coord_map(xlim = c(-123, -108),
  #           ylim = c(48, 60.5)) +
  # xlim(xlim = c(-123, -108)) +
  # ylim(ylim = c(40, 70)) +
  theme_bw() +
  coord_equal() #+
  # guides(fill = 'none')

# ggplot() + 
#   geom_polygon(data=regions, aes(long,lat, group = group, fill = group)) + 
#   geom_path(data=regions, aes(long,lat, group = group), color="white") +
#   geom_path(data = eco, aes(x=long, y=lat, group=group)) +
#   # geom_point(data=dat, aes(x=long, y=lat)) +
#   # coord_equal(xlim = c(-123, -108),
#   #             ylim = c(48, 60.5)) + 
#   # coord_map(xlim = c(-123, -108),
#   #           ylim = c(48, 60.5)) +
#   # xlim(xlim = c(-123, -108)) +
#   theme_bw() + 
#   coord_equal() +
#   guides(fill = FALSE)
