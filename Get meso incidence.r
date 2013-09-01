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

# The demography package is used by some of the HSE modeling programs, but not all
# It is not used in this one
# The package depends on having X11 installed, which is not the default for Mountain Lion
# If you try to load the library, and X11 is not installed, you will get a fatal error for the Rstudio session
#       I.e., save stuff before trying
# For recent versions of Mac OS X, you can download and install X11 from here: 
#       http://xquartz.macosforge.org/landing/
# require('demography')

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
# Australia mesothelioma incidence
# ====================================================================================

setwd( Aus.meso.dir)
dir()
     
males   = get.mesos.from.xls( 'mesothelioma.xls', 'Incidence', vname.row=10, xrows=25:52, cols=1:19)            
females = get.mesos.from.xls( 'mesothelioma.xls', 'Incidence', vname.row=10, xrows=25:52, cols=27:45)               
               
male.tot   = apply(   males[ , 2:ncol(males)], 1, sum) 
female.tot = apply( females[ , 2:ncol(males)], 1, sum) 


df = data.frame( year=males$year, male.tot, female.tot)  

pa  = ggplot( df, aes( year, male.tot))
pb  = pa + geom_point()
pc  = pb + stat_smooth(method=loess)
#pc  = pb + stat_smooth()  # method=auto
mp  = pc
mp + labs(  title='Australia males')
  
pa  = ggplot( df, aes( year, female.tot))
pb  = pa + geom_point()
pc  = pb + stat_smooth(method=loess)
#pc  = pb + stat_smooth()  # method=auto
fp  = pc
fp + labs(  title='Australia females')

setwd( Aus.meso.dir)
save( males, females, male.tot, female.tot, mp, fp, file='Australia meso incidence through 2009.rdata')


# ====================================================================================
# UK mesothelioma incidence (deaths, actually)
# ====================================================================================

setwd( UK.meso.dir)
dir()

# UK puts a (p) beside a provisional year
fix.yr = data.frame( from.yr='2010',to.yr=2010)
fix.yr

males   = get.mesos.from.xls( 'meso02.xls', 'MESO02', vname.row=6, xrows=7:49, cols=2:21, fix.yr=fix.yr)            
females = get.mesos.from.xls( 'meso03.xls', 'MESO03', vname.row=6, xrows=7:49, cols=2:21, fix.yr=fix.yr)               

male.tot   = apply(   males[ , 2:ncol(males)], 1, sum) 
female.tot = apply( females[ , 2:ncol(males)], 1, sum) 


df = data.frame( year=males$year, male.tot, female.tot)  

pa  = ggplot( df, aes( year, male.tot))
pb  = pa + geom_point()
pc  = pb + stat_smooth(method=loess)
#pc  = pb + stat_smooth()  # method=auto
mp  = pc
mp + labs(  title='UK males')

pa  = ggplot( df, aes( year, female.tot))
pb  = pa + geom_point()
pc  = pb + stat_smooth(method=loess)
#pc  = pb + stat_smooth()  # method=auto
fp  = pc
fp + labs(  title='UK females')

setwd( UK.meso.dir)
save( males, females, male.tot, female.tot, mp, fp, file='UK meso incidence through 2010')

               
               
               
               


# ================================================================
# US meso incidence
# ================================================================

# TBD