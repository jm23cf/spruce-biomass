library(EcoMem)
library(plyr)
library(kableExtra)
library(ggplot2)

# Load tree growth data
tree.growth = read.csv("data/JEcol-2018-0266.R1/West/TreeWest.csv",
                       header = TRUE, stringsAsFactors = FALSE)

# Load ftc defoliation data
ftc.defol = read.csv("data/JEcol-2018-0266.R1/West/DefolWest.csv",
                     header = TRUE, stringsAsFactors = FALSE)

# Define function to convert diameter (cm) to basal area (cm^2)
dbh2ba = function(d){
  (d^2)*(pi/4)
}

# Convert diameter to basal area in current and previous year
tree.growth$ba = dbh2ba(tree.growth$dbh)
tree.growth$ba.lag = dbh2ba(tree.growth$dbh.lag)

# Take difference between current and previous year basal area
# to estimate annual basal area increment (cm^2 per year)
tree.growth$bai = with(tree.growth, ba-ba.lag)

# Calculate mean annual basal area increment and age for
# sampled trembling aspen trees by study site (stand)
stand.vars = ddply(tree.growth[tree.growth$Species == "Potr",],
                   .(Stand,Year),summarize,
                   age = mean(age), gr = mean(bai))

# Form binary moderate-to-severe defoliation variable
ftc.defol$ftc = with(ftc.defol, ifelse(Defol > 1, 1, 0))

# Identify study sites (stands) where trembling aspen
# is present
host.stands = sort(unique(tree.growth$Stand[tree.growth$Host == "Host"]))

# Subset defoliation data to include only host stands
ftc.defol = ftc.defol[ftc.defol$Stand %in% host.stands,]

# Form model data frame
boreal.dat = join(ftc.defol, stand.vars)

# Fit ecological memory model
mod = ecomem(gr ~ age + ftc, data = boreal.dat, mem.vars = "ftc",
             L = 12, timeID = "Year", groupID = "Stand")

# Summarize marginal posterior distributions of model parameters
boreal.mem.summ = memsum(mod, verbose = FALSE)
boreal.mem.summ

# # Print summary
# kable(boreal.mem.summ, "latex", booktabs = TRUE, digits = 3,
#       caption="\\label{tab:memsum}Summary of marginal posterior
# distributions for ecological memory model parameters generated
# using the \\texttt{memsum()}
# function.") %>%
#   kable_styling(latex_options = c("striped", "hold_position"))

# Plot boreal tree growth memory to ftc defoliation
plotmem(mod)

#################################################################################################
## try with agbi and drought
#################################################################################################

drought = read.csv('data/drought_CRU.csv')
agbi.mean = readRDS('data/agbi.mean.RDS')

dat = merge(agbi.mean, drought)

# Fit ecological memory model
mod = ecomem(agbi.mean ~ drought, data = dat, mem.vars = "drought",
             L = 6, timeID = "year", groupID = "site.id")

# Summarize marginal posterior distributions of model parameters
boreal.mem.summ = memsum(mod, verbose = FALSE)
boreal.mem.summ 
# # Print summary
# kable(boreal.mem.summ, "latex", booktabs = TRUE, digits = 3,
#       caption="\\label{tab:memsum}Summary of marginal posterior
# distributions for ecological memory model parameters generated
# using the \\texttt{memsum()}
# function.") %>%
#   kable_styling(latex_options = c("striped", "hold_position"))

# Plot boreal tree growth memory to ftc defoliation
plotmem(mod)


# Fit ecological memory model
mod = ecomem(agbi.mean ~ ppt, data = dat, mem.vars = "ppt",
             L = 6, timeID = "year", groupID = "site.id")

# Summarize marginal posterior distributions of model parameters
boreal.mem.summ = memsum(mod, verbose = FALSE)
boreal.mem.summ 
