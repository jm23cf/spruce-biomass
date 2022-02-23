library(dplyr)
library(ggplot2)

dat = readRDS('Data/dbh-reconstructed.RDS')
dat = dat[which((dat$year>=1968)&(dat$year<2009)),]

dbh.total = dat %>% 
  group_by(site.orig, year) %>% 
  summarize(dbh.sum = sum(dbh), .groups="keep")

ggplot(data=dbh.total) +
  geom_line(aes(x=year, y=dbh.sum, colour=site.orig))


count = dat %>% 
  group_by(site.orig, year) %>% 
  summarize(count = n_distinct(tree.orig), .groups="keep")

ggplot(data=count) +
  geom_line(aes(x=year, y=count, colour=site.orig))
