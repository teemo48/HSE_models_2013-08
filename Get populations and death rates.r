# Get meso incidence.r

# Get meso incidence by country, from various sources
#   This could be done in uniform way, from a programming standpoint, by going to the OECD data
#   That data may not be updated quite as promptly as the national data


# =============================================================
# Directories
# =============================================================

decipher.dir    = '/Users/timothywyant/Documents'
analysis.dir    = paste( decipher.dir, 'Decipher projects/248 Combined asbestos trusts/Analysis/HSE models 2013-08', sep='/')
data.dir        = paste( analysis.dir, 'Data', sep='/')
function.dir    = paste( analysis.dir, 'Functions', sep='/')
report.dir      = paste( analysis.dir, 'Reports', sep='/')
output.dir      = paste( analysis.dir, 'Output', sep='/')

UK.demog.dir    = paste( data.dir, 'UK.demog', sep='/')
UK.meso.dir     = paste( data.dir, 'UK.meso', sep='/')
Aus.demog.dir   = paste( data.dir, 'Aus.demog', sep='/')
Aus.meso.dir    = paste( data.dir, 'Aus.meso', sep='/')
US.demog.dir    = paste( data.dir, 'US.demog', sep='/')
US.meso.dir     = paste( data.dir, 'US.meso', sep='/')

# Reports
quick.summary.dir = paste( report.dir, 'Quick summary of HSE models', sep='/')


# =============================================================
# Libraries and functions
# =============================================================

require('ggplot2')
require('plyr')
require('reshape2')
require('lubridate')
require('scales')
require('stringr')
require('testthat')

require('data.table')
require('Hmisc')
require('knitr')
require('RColorBrewer')
require('xlsx')
require('zoo')


# The package depends on having X11 installed, which is not the default for Mountain Lion
# If you try to load the library, and X11 is not installed, you may get a fatal error for the Rstudio session
#       I.e., save stuff before trying
# For recent versions of Mac OS X, you can download and install X11 from here: 
#       http://xquartz.macosforge.org/landing/

require('demography')

sessionInfo()


setwd(function.dir)
func.names = dir()
if ( length(func.names) > 0) {
  for (i in 1:length(func.names)) {
    source( func.names[i])
    # Use option local=TRUE for caching functions in knitr
  }
}
func.names


# ================================================================
# Options
# ================================================================
options( stringsAsFactors=FALSE)


# ====================================================================================
# Collect population, historic and projected death rates from Human Mortality Database
# ====================================================================================
Aus.demog = hmd.mx('AUS', 'tw@deciph.com', 'TW_HumanMortality', 'Australia')
UK.demog = hmd.mx('GBR_NP', 'tw@deciph.com', 'TW_HumanMortality', 'UK')
US.demog = hmd.mx('USA', 'tw@deciph.com', 'TW_HumanMortality', 'US')

# Populations
head(US.demog$pop$male)

# Death rates
head( US.demog$rate$male)
historic.death.rate.male.60 = data.frame( year       = US.demog$year, 
                                          death.rate = US.demog$rate$male[ 61, ],
                                          source='Historic')

# Projected death rates -- 50 years
# Lee-Carter model

US.LC.f    = lca( US.demog, 'female')
US.LC.m    = lca( US.demog, 'male')
US.fcast.f = forecast( US.LC.f)
US.fcast.m = forecast( US.LC.m)

names(US.fcast.m$rate)
head(US.fcast.m$rate$male)
age = US.fcast.m$age
   
future.death.rate.male.60 = US.fcast.m$rate$male[ 61, ]
example = data.frame( year=US.fcast.m$year, death.rate=future.death.rate.male.60, source='Projected')

example = rbind( historic.death.rate.male.60, example)

death.rate.p = ggplot( example, aes( year, death.rate, color=source) )
death.rate.p + geom_point() + labs( title='Death rates, 60 year-old U.S. male')

qplot( year, death.rate, data=example, color=source, ylim=c(0,.03)) + labs( title='Death rates, 60 year-old U.S. male')
                       
                       
# ====================================================================================
# Save datasets
# ====================================================================================

Aus.LC.f    = lca( US.demog, 'female')
Aus.LC.m    = lca( US.demog, 'male')
Aus.fcast.f = forecast( US.LC.f)
Aus.fcast.m = forecast( US.LC.m)

setwd( Aus.demog.dir)
save( Aus.demog, Aus.fcast.f, Aus.fcast.m, file='Australia pop, hist and proj death rates.rdata')


UK.LC.f    = lca( US.demog, 'female')
UK.LC.m    = lca( US.demog, 'male')
UK.fcast.f = forecast( US.LC.f)
UK.fcast.m = forecast( US.LC.m)

setwd( UK.demog.dir)
save( UK.demog, UK.fcast.f, UK.fcast.m, file='UK pop, hist and proj death rates.rdata')


setwd( US.demog.dir)
save( US.demog, US.fcast.f, US.fcast.m, file='US pop, hist and proj death rates.rdata')
