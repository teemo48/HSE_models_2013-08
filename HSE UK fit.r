# HSE UK fit.r

# Test my new UK fit with splines


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

# For fitting the HSE likelihood
require( 'splines')
require( 'stats4')

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


# ================================================================
# Get UK incidence data
# ================================================================
setwd( UK.meso.dir)
l = load( 'UK meso incidence through 2010')
l
plot( male.tot, ylim=c(0,max(male.tot)))
   points( female.tot)

# Saved ggplots
mp
fp

head(males,20)
tail(males,22)

male.mesos = melt( males, id.vars='year')
names(male.mesos) = c( 'year', 'age.group', 'n.meso')
head(male.mesos)
tail(male.mesos)
str(male.mesos)



# ================================================================
# Get UK population data
# ================================================================
setwd( UK.demog.dir)
l = load('UK pop, hist and proj death rates.rdata')
l

# Historic counts through 2011
names(UK.demog)
UK.demog$type
expect_identical( UK.demog$label, 'UK')
UK.demog$year
UK.demog$age
names( UK.demog$pop)
head( UK.demog$pop$male)     # Array of population counts, age 0:110 by year 1922:2011
tail( UK.demog$pop$male)
head( UK.demog$pop$female)

male.pop           = UK.demog$pop$male
rownames(male.pop) = 0:110
tail(male.pop)
str(male.pop)

male.pop = melt( male.pop)
names(male.pop) = c( 'age', 'year', 'n')
tail(male.pop)
str(male.pop)

age.group = cut( male.pop$age, c( -1, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, 120))
levels(age.group) = names(males)[2:ncol(males)]
male.pop$age.group = age.group
tail(male.pop,30)
with( male.pop, table(age.group))

male.pop = ddply( male.pop, .( year, age.group), summarise, n = sum(n))
head(male.pop,25)
tail(male.pop,25)

# Assign an age to each group for fitting purposes
ages = data.frame( age.group = levels( age.group), age=seq( 2, 92, 5))
male.pop = merge( male.pop, ages, all.x=TRUE)
tail( male.pop, 25)
with( male.pop, table( age.group, age))



# Forecast population counts -- not need until doing predicted meso incidence

# UK.fcast.m$label
# UK.fcast.m$age
# UK.fcast.m$year
# names(UK.fcast.m$rate)
# str(UK.fcast.m$rate$male)   # Array of death rates  -- age by year, 2011:2060
# names(UK.fcast.m$fitted)
# 
# plot( male.tot, ylim=c(0,max(male.tot)))
# points( female.tot)
# 
# # Saved ggplots
# mp
# fp
# 
# head(UK.demog,20)
# tail(males,22)


# ================================================================
# Data frame with grouped population counts and meso counts
# ================================================================
male.data = merge( male.pop, male.mesos)
str(male.data)
head(male.data,30)
tail(male.data,30)

# Use the notation of Tan and Warren
male.hx = male.data
names( male.hx) = c( 'age.group', 'T', 'P', 'A', 'Y')
head(male.hx)



# ================================================================
# Background rates
# ================================================================

# Male background rate
B.rate = 1.5/1e6     # per Tan and Warren

B.calc = function( data, background.rate, L, k) {
  # Calculate the number of background meso cases (B) by age and year
  # Notation is from Tan and Warrent
  # A       Age
  # T       Year
  # B_AT    Number of background cases, given A and T
  # P       Population, given A and T
  # Y       Total recorded meso cases, given A and T
  # k       Exponent of risk increase as a function of lagged exposure
  # L       Lag in years from first exposure 
  
  require( 'testthat')
  require( 'plyr')
  
  expect_true( is.data.frame(data))
  expect_true( 'A' %in% names(data))
  expect_true( 'T' %in% names(data))
  expect_true( 'P' %in% names(data))
  
  temp  = within( data, {
    B.risk.factor = pmax( 0, A-L)^k
  })
  
  by.yr = ddply( temp, .(T), summarise,
                 P_T                = sum(P),
                 sum.B.risk.factors = sum( B.risk.factor)
  )

  temp2      = merge( temp, by.yr, all.x=TRUE)
  temp3      = within( temp2, {
    B_T  = background.rate * P_T
    B_AT = ( B.risk.factor / sum.B.risk.factors) * B_T
  })

  temp3
}

temp = B.calc( data=male.hx, background.rate=B.rate, L=10, k=2)

check = ddply( temp, .(T), summarise,
               sum.B_AT = sum( B_AT),
               sum.P    = sum(P),
               rate     = sum.B_AT / sum.P
)
check

male.hx = temp


# ================================================================
# Build the exposure history template
# ================================================================


build.exposure.hx = function( data, L) {
  # Build an array with one record per exposure year
  
  # Notation is from Tan and Warren

  # A       Age
  # T       Year
  # B_AT    Number of background cases, given A and T
  # P       Population, given A and T
  # Y       Total recorded meso cases, given A and T
  # k       Exponent of risk increase as a function of lagged exposure
  # L       Lag in years from first exposure 
  # l       Exposure age is L-l
  
  require( 'testthat')
  require( 'plyr')
  
  expect_true( is.data.frame(data))
  expect_true( 'A' %in% names(data))
  expect_true( 'T' %in% names(data))
  expect_true( 'P' %in% names(data))
  
  expect_true( min(data$A) ==  max(data$A))
  expect_true( min(data$A) >= 2)
               
  l        = 1:(data$A-1)
  a        = data$A - l
  t        = data$T - l
  risk.yrs = pmax( 0, l + 1 - L)
  
  df       = data.frame( T=data$T, A=data$A, l, a, t, risk.yrs, P=data$P, Y=data$Y, B_AT=data$B_AT)
  df       = arrange(df, T, A, l)
  
  data.matrix(df)
}

i = male.hx$T == 2010 & male.hx$A == 52
build.exposure.hx( data=male.hx[i,], L=10)

exposure.hx = dlply( male.hx, .(T,A), build.exposure.hx, L=10)
exposure.hx = do.call( rbind, exposure.hx)
head(exposure.hx,20)

earliest.exposure.year = 1915
i                    = exposure.hx[ , 't'] < earliest.exposure.year
exposure.hx = exposure.hx[ !i, ]


# ================================================================
# Build the indices of exposure 
# ================================================================

# Indices 

min.expose.age = min( exposure.hx[ , 'a'] )
max.expose.age = max( exposure.hx[ , 'a'] )
ages           = min.expose.age:max.expose.age
age.index      = ns( ages, knots=c(30))
age.index

min.expose.year = min( exposure.hx[ , 't'] )
max.expose.year = max( exposure.hx[ , 't'] )
years           = min.expose.year:max.expose.year
year.index      = ns( years, knots=c(1960))
year.index

# Initial values of coefficients -- age
y = dnorm( ages, mean=30, sd=.005)
fit = lm( y ~ -1 + age.index)
coef.age = coef(fit)
coef.age
plot( min.expose.age:max.expose.age, exp( predict(fit)))

# Initial values of coefficients -- year
y         = dnorm( years, mean=1960, sd=.001)
fit       = lm( y ~ -1 + year.index)
coef.year = coef(fit)
coef.year
plot( min.expose.year:max.expose.year, exp( predict(fit)))


# ================================================================
# Build the matrices and the coefficient vector
# ================================================================

# x matrix

intercept = matrix( rep(1, nrow(exposure.hx)), nrow(exposure.hx), 1)
m         = match( exposure.hx[ , 'a'], ages)
x.age     = age.index[ m,]
m         = match( exposure.hx[ , 't'], years)
x.year    = year.index[ m,]
log.risk.yrs = matrix( log( exposure.hx[ , 'risk.yrs']), nrow(exposure.hx), 1)
x            = cbind( intercept, x.age, x.year, log.risk.yrs)
dim(x)
head(x)

start.vals = list( alpha = log( 1/1e12), 
                   age1  = coef.age[1],
                   age2  = coef.age[2],
                   year1 = coef.year[1],
                   year2 = coef.year[2],   
                   k     = 2
)

beta0 = do.call( rbind, start.vals)

fixed.vals = list( k=2)




TA  = paste( exposure.hx[,'T'], str_pad( exposure.hx[,'A'], 2))
uTA = unique(TA)
expect_identical(  TA, sort( TA))
expect_identical( uTA, sort(uTA))

aggregator = matrix( 0, length(uTA), nrow(exposure.hx))
dim(aggregator)
dim(exposure.hx)

for (i in 1:length(uTA)) {
  j = uTA[i] == TA
  aggregator[i,] = j
}
rownames(aggregator) = uTA
a = apply( aggregator, 1, sum)
table(a)
head(aggregator[,1:33])


x.beta0 = x %*% beta0
lambda0 = aggregator %*% exp( x %*% beta0)
dim(lambda0)
head(lambda0,33)


Pdf         = ddply( data.frame( exposure.hx), .(T,A), summarise, P=min(P))
P           = matrix( Pdf$P, nrow(Pdf), 1)
rownames(P) = paste( Pdf$T, str_pad( Pdf$A,2))

Ydf         = ddply( data.frame( exposure.hx), .(T,A), summarise, Y=min(Y))
Y           = matrix( Ydf$Y, nrow(Ydf), 1)
rownames(Y) = paste( Ydf$T, str_pad( Ydf$A,2))

expect_identical( rownames(P), rownames(lambda0))
expect_identical( rownames(Y), rownames(lambda0))

# Check for instances in which lambda0 is 0 and there are cases of meso
# These may be okay to keep in if we are fitting with a baseline exposure, if the baseline rate is > 0
# These will probably blow up the likelihood otherwise
i = Y > 0 & lambda0 == 0
sum(i)
Y[ i, 1]
Y[ i, 1] = 0

# The log likelihood with the starting parameter values
loglik0 = sum( Y*log(lambda0) - lambda0, na.rm=TRUE)  # Get rid of the NANs for the youngest ages
loglik0



# ================================================================
# Maximize the likelihood
# ================================================================

minusloglik = function( alpha, age1, age2, year1, year2, k) {
  
  beta   = matrix( c( alpha, age1, age2, year1, year2, k), 6, 1)
  lambda = aggregator %*% exp( x %*% beta) * P
  
#   browser()
#   plot( lambda, P)
  
  loglik = sum( Y*log(lambda) - lambda, na.rm=TRUE)
  -loglik
}

start.vals
fixed.vals
for (i in 1:length(start.vals)) { names(start.vals[[i]]) = NULL}
fixed.vals.temp = start.vals[2:6]
beta.hat = mle( minusloglik, start=start.vals, fixed=fixed.vals)

beta.hat
summary(beta.hat)
AIC(beta.hat)
vcov(beta.hat)

# These take awhile to run, even with just one parameter chosen for profiling
# See profile-methods {stats4}
# The function confint() calls profile(), so no need to invoke profile() first if all you want is a confint
# prof = profile(beta.hat, which=1)
# plot(prof, absVal=FALSE)
# plot.prof)
# confint(beta.hat,parm=1)


# ================================================================
# Examine the quality of the fit
# ================================================================

examine.hse.fit = function( mle.fit, aggregator, x, Y, P, title.text,
                            years, year.index, ages, age.index) {

# =================================  Calculate the expected values
beta           = matrix( coef(mle.fit), length(coef(mle.fit)), 1)
rownames(beta) = names(coef(mle.fit))
beta

# =================================  Expected values v actual
lambda      = aggregator %*% exp( x %*% beta) * P

lambda.plot = qplot( lambda, Y) + ggtitle(title.text)
sum.lambda  = sum(lambda)
sum.Y       = sum(Y)
print( lambda.plot)
print( data.frame( sum.lambda, sum.Y))

# =================================  Expected values v actual -- by year
splits = str_split( rownames(lambda), ' ')
f      = function(x) data.frame( T=x[1], A=x[length(x)])
TA     = ldply( splits, f)
describe(TA)
df = data.frame( TA, lambda, Y)
head(df)
by.year = ddply( df, .(T), summarise,
                 Y.hat = sum(lambda),
                 Y     = sum(Y)
)

by.year.plot = ggplot( by.year, aes(x=T, group='No group')) + 
  ggtitle(title.text) + 
  geom_point(aes(y=Y, shape='Actual', linetype='Actual', color='Actual')) + 
  geom_line(aes(y=Y.hat, shape='Fitted', linetype='Fitted', color='Fitted')) + 
  scale_shape_manual('', values=c(19, NA)) + 
  scale_linetype_manual('', values=c(0, 1)) +
  scale_colour_manual( '', values=c('black','red'))
print(by.year.plot)

# =================================  Indices
# Year index
year.index
years
exposure.year.index =  exp( beta['year1',] * year.index[,1] +  beta['year2',] * year.index[,2])
i = years == 1960
exposure.year.index = exposure.year.index / exposure.year.index[i]
plot( years, exposure.year.index) 
  title( main=paste( title.text, '\n', 'Exposure year index relative to 1960'))
  abline( v=1960, col='red')
  abline( v=1982, col='blue')

# Age index
age.index
ages
exposure.age.index =  exp( beta['age1',] * age.index[,1] +  beta['age2',] * age.index[,2])
i = ages == 30
exposure.age.index = exposure.age.index / exposure.age.index[i]
plot( ages, exposure.age.index) 
  title( main=paste( title.text, '\n', 'Exposure age index relative to age 30'))
  abline( v=30, col='red')


# =================================  Save plots, stats, and input
list( mle.fit=mle.fit, 
      aggregator = aggregator,
      x          = x,
      Y          = Y, 
      P          = P, 
      title.text = title.text,
      lambda.plot= lambda.plot, 
      sum.lambda = sum.lambda,
      sum.Y      = sum.Y,
      by.year.plot        = by.year.plot,
      exposure.year.index = exposure.age.index,
      exposure.age.index  = exposure.age.index)
}
       
summ = examine.hse.fit( mle.fit=beta.hat, 
                        aggregator = aggregator, 
                        x = x, Y = Y, P = P, 
                        title.text = 'Annual meso counts, UK males, k fixed, 2df for indices',
                        years=years, year.index=year.index, ages=ages, age.index=age.index)       

















