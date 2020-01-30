### Download raw FIA data tables and calculate summaries

## Load the rFIA package
library(rFIA)
library(rgdal)

## How many cores do you have?
parallel::detectCores(logical = FALSE)
## How many do you want to use?
cores <- 1
#######################################################################
##### Download FIA data---- 
########################################################################

## Download and save the FIA data for each state which intersects APPA

at <- getFIA(states = c('CT', 'GA', 'ME', 'MD', 'MA', 'NH', 'NJ', 'NY', 'NC', 'PA', 'TN', 'VT', 'VA'),
             nCores = cores,dir = './FIA/', common = TRUE )
  
# testing just NH for quick download
nh <- getFIA(states = c("NH"),
             nCores = cores,dir = './FIA/', common = TRUE )#saves to object if dir isn't specific

## Load Ecoregion polygons that are clipped to the HUC10 shell

eco <- readOGR('./ecoregions', 'at_ecoSub')
proj4string(eco)# check projection

################################################################################################
##### Spatially subset FIA data by ecoregion within the HUC10 shell; include all inventories----
##################################################################################################

at_FIA <- clipFIA(at, mask = eco, matchEval= FALSE, mostRecent = FALSE, nCores = cores)

# to access only the recent inventories 
at_FIAMR <- clipFIA(at, mask = NULL, matchEval= FALSE, mostRecent = TRUE, nCores = cores)

#######################################################################
### Save the raw data tables prior to calculating the stats----
#######################################################################
# to get raw plot-level data including metadata run the following commands for each metric.
# if you desire further groupings see function documentation References section (FIA Database User Guide)

# Trees Per Acre (TPA) per species

tpaSppPlot <- tpa(at, byPlot= TRUE, grpBy = c(STATECD, ECOSUBCD, OWNGRPCD, QA_STATUS,PLOT,SUBP,STATUSCD,TREE, DIA),
                  bySpecies = TRUE, bySizeClass =TRUE, method = "annual")

# Stand Structure
ssPlot <- standStruct(at, byPlot= TRUE, grpBy = c(STATECD, ECOSUBCD, OWNGRPCD, QA_STATUS,PLOT),
                       method = "annual")




#### Calculate statistics per ecoregion over time ---- 
### there are a few methods for producing estimates in the package, specified under the "method" argument.

### exponential moving average calculated per excoregion and year
start <- lubridate::now()
ss <- standStruct(at, polys = eco, tidy = FALSE, nCores = cores, method = 'ema')
tpaS <- tpa(at, polys = eco, bySpecies = TRUE, treeType = 'live', treeDomain = DIA >= 5, nCores = cores, method = 'ema')
tpa <- tpa(at, polys = eco, treeType = 'live', treeDomain = DIA >= 5, nCores = cores, method = 'ema')
div <- diversity(at, polys = eco, nCores = cores, treeDomain = DIA >= 5, method = 'ema')
gmS <- growMort(at, polys = eco, bySpecies = TRUE, nCores = cores, treeDomain = DIA >= 5, method = 'ema')
gm <- growMort(at, polys = eco, nCores = cores, treeDomain = DIA >= 5, method = 'ema')
vrS  <- vitalRates(at, polys = eco, bySpecies = TRUE, nCores = cores, treeDomain = DIA >= 5, method = 'ema')
vr  <- vitalRates(at, polys = eco, nCores = cores, treeDomain = DIA >= 5, method = 'ema')
bioS <- biomass(at, polys = eco, bySpecies = TRUE, nCores = cores, treeDomain = DIA >= 5, method = 'ema')
bio <- biomass(at, polys = eco, nCores = cores, treeDomain = DIA >= 5, method = 'ema')
regenS <- tpa(at, polys = eco, bySpecies = TRUE, treeType = 'live', treeDomain = DIA < 5, nCores = cores, method = 'ema')
regen <- tpa(at, polys = eco, treeType = 'live', treeDomain = DIA < 5, nCores = cores, method = 'ema')
snag <- tpa(at, polys = eco, treeType = 'dead', treeDomain = DIA >= 5, nCores = cores, method = 'ema')
snagV <- biomass(at, polys = eco, treeType = 'dead', treeDomain = DIA >= 5, nCores = cores, method = 'ema')
snagLD <- tpa(at, polys = eco, treeType = 'dead', treeDomain = DIA >= 11.81102, nCores = cores, method = 'ema')
snagVLD <- biomass(at, polys = eco, treeType = 'dead', treeDomain = DIA >= 11.81102, nCores = cores, method = 'ema')
dw <- dwm(at, polys = eco, tidy = FALSE, nCores = cores, method = 'ema')
inv <- invasive(at, polys = eco, nCores = cores, method = 'ema')
lubridate::now() - start


setwd('/EMA/')

# Save all dataframes as .csv
write.csv(ss, 'ss.csv')
write.csv(div, file = 'div.csv')
write.csv(tpaS, file = 'tpaS.csv')
write.csv(tpa, file = 'tpa.csv')
write.csv(gmS, file = 'gmS.csv')
write.csv(gm, file = 'gm.csv')
write.csv(vrS, file = 'vrS.csv')
write.csv(vr, file = 'vr.csv')
write.csv(bioS, file = 'bioS.csv')
write.csv(bio, file = 'bio.csv')
write.csv(regenS, file = 'regenS.csv')
write.csv(regen, file = 'regen.csv')
write.csv(snag, file = 'snag.csv')
write.csv(snagV, file = 'snagV.csv')
write.csv(snagLD, file = 'snagLD.csv')
write.csv(snagVLD, file = 'snagVLD.csv')
write.csv(dw, file = 'dw.csv')
write.csv(inv, file = 'inv.csv')

#### Visualize

plotFIA(bio, BIO_AG_ACRE, facet = TRUE, line.width = .1,
        plot.title = 'Exponential Moving Average', 
        transform = 'sqrt', legend.title = 'AG Biomass per Acre', legend.height = .7)



