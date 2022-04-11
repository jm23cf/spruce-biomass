library(dplR)
library(reshape2)
library(ggplot2)
library(dplyr)

# source('fixup.r')
####################################################################################
## read and combine rw measurements
####################################################################################

# read rwl files
fnames = list.files(path="data/rwl/", pattern=".raw$")

# lapply(fnames, function(x) fixup(paste0("DATA/rwl/", x)))

dat = list()
for (i in 1:length(fnames)){
  fname = fnames[i]
  print(fname)
  dat[[i]] = read.rwl(paste0("data/rwl/", fname))
}

# dat = lapply(fnames, function(x) read.rwl(paste0("alberta-transect/rwl/", x)))
dat.all = combine.rwl(dat)
# colnames(dat.all) = paste0('0', rep(seq(1, 9), each=2), rep(c('A', 'B'), times=9))

ids = read.ids(dat.all, stc = c(3,2,1))
dat.all = treeMean(dat.all, ids=ids)

dat.all = data.frame(year=rownames(dat.all), dat.all)

# write combine core rwl file to use for crossdating
#write.rwl(dat.all, "Data/spruce-live.rwl")

dat.long = melt(dat.all)
dat.long$variable = as.numeric(substr(dat.long$variable, 2, 4))
dat.long = dat.long[!is.na(dat.long$value),]
dat.long$year = as.numeric(as.character(dat.long$year))


ids$site.orig = substr(rownames(ids), 1, 2)
ids$tree.orig = substr(rownames(ids), 4, 5)

####################################################################################
## read tree metadata and summarize
####################################################################################

meta = read.csv('data/tree_meta.csv')
meta = meta[which(meta$AD == "L"),]

# for now skip tree 06L15
meta = meta[which(!is.na(meta$dbh)),]

ggplot() + 
  geom_histogram(data=meta, aes(x=dbh, y=..density..))

meta$size = NA
meta$size[which(meta$dbh <= 10)] = 'small' 
meta$size[which((meta$dbh > 10) & (meta$dbh <= 30))] = 'medium' 
meta$size[which(meta$dbh > 30)] = 'large' 

ggplot(data=meta) + 
  geom_histogram(aes(x=dbh, y=..density..)) +
  facet_grid(size~.)


ggplot() + 
  geom_histogram(data=meta, aes(x=dbh, y=..density.., fill=size), 
                 alpha=0.5, breaks = seq(0,501, by=10)) +
  theme_bw()


ggplot() + 
  geom_histogram(data=meta, aes(x=dbh, fill=size), 
                 alpha=0.5, breaks = seq(0,501, by=10)) +
  theme_bw() + 
  xlab('DBH') +
  ylab('Count') +
  theme(text = element_text(size=12))
ggsave('figures/dbh-histogram-all-trees.png')
  
table(meta$size)/nrow(meta)

####################################################################################
## reconstruct dbh
####################################################################################

sites = unique(ids$site.orig)

dat.long$dbh = NA

# cores=c("A","B")
for (site in sites){
  
  print(site)
  
  if (site %in% c("0N", "RR", "0S", "UU", "VV", "0W")){
    next
  }
  # 
  trees = unique(ids[which(ids$site.orig == site), "tree.orig"])
  trees = as.numeric(trees)
  for (tree in trees){
    
    if ((site=='06') & (tree==15)){
      next
    }
    
    # tree_idx = which(tree_sub$tree == tree)
    DBH=meta[which((meta$TreeNum==tree)&(meta$Stand==site)),"dbh"]
    
    tree.dplr = unique(ids[which((ids$site.orig == site) & (as.numeric(ids$tree.orig) == tree)),'tree'])
    
    rwdat=dat.long[which(dat.long$variable == tree.dplr),]
    
    year_hi = max(rwdat$year)
    year_lo = min(rwdat$year)
    
    for (year in year_hi:year_lo){
      # yearidx=which(coredat$year==year)
      idx = which((dat.long$variable == tree.dplr)&(dat.long$year==year))
      
      if (year == year_hi) {
        #DBH = DBH
        dat.long[idx,"dbh"] = DBH
      } else {
        idxp=which((dat.long$variable == tree.dplr)&(dat.long$year==(year+1)))
        DBH_prev = dat.long[idxp,"dbh"]
        # dat.long[idx,"DBH"]=DBH_prev-2*dat.long[idx,"value"]/10
        dat.long[idx,"dbh"]=DBH_prev-2*dat.long[idxp,"value"]/10
      }
      
      # print(DBH_prev)
    } 
    # }
  }
}


dat.long$site.orig = ids[match(dat.long$variable, ids$tree), 'site.orig'] 

dat.long$tree.orig = ids[match(dat.long$variable, ids$tree), 'tree.orig']
colnames(dat.long) = c('year', 'tree', 'value', 'dbh', 'site.orig', 'tree.orig')

site.meta = read.csv('data/site_meta.csv', stringsAsFactors = FALSE)
site.meta$site = site.meta$transect

site.meta$site = ifelse(nchar(site.meta$site)==1, paste0('0', site.meta$site), site.meta$site)

dat.long$lat = site.meta[match(dat.long$site.orig, site.meta$site),'lat']
dat.long$long = site.meta[match(dat.long$site.orig, site.meta$site),'long']
dat.long$elev = site.meta[match(dat.long$site.orig, site.meta$site),'elev']

dat.long$size = NA
dat.long$size[which(dat.long$dbh <= 10)] = 'small' 
dat.long$size[which((dat.long$dbh > 10) & (dat.long$dbh <= 30))] = 'medium' 
dat.long$size[which(dat.long$dbh > 30)] = 'large' 

saveRDS(dat.long, 'data/dbh-reconstructed.RDS')

freq.size = dat.long %>% 
  group_by(year, size) %>% 
  summarise(n = n(), .groups='keep')

ggplot() + 
  geom_point(data=freq.size, aes(x=year, y=n, colour=size), 
                 alpha=0.5) +
  theme_bw() + 
  xlab('DBH') +
  ylab('Count') +
  theme(text = element_text(size=12))
ggsave('figures/dbh-histogram-all-trees.png')
