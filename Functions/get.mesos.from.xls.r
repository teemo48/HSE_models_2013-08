get.mesos.from.xls = function( file='mesothelioma.xls', sheetname='Incidence', vname.row=1,  xrows=2:10, cols=1:19, fix.yr=NULL) {

  # Get mesothelioma incidence counts from published xls or xlsx workbooks
  
  require('testthat')
  require('stringr')
  
  expect_false( options( 'stringsAsFactors')[[1]])
  
  if ( !is.null( fix.yr)) {
    expect_is   ( fix.yr, 'data.frame')
    expect_equal( sum( names( fix.yr) %in% c('from.yr','to.yr')), 2)
    expect_is   ( fix.yr$from, 'character')
    expect_is   ( fix.yr$to, 'numeric')
  }
  
  ma        = read.xlsx( file, sheetName=sheetname, rowIndex=vname.row, colIndex=cols, header=FALSE, as.data.frame=FALSE)
  vnames    = do.call( cbind, ma)
  vnames[1] = 'year'
  
  mn        = read.xlsx( file, sheetName=sheetname, rowIndex=xrows, colIndex=cols, header=FALSE, as.data.frame=TRUE)
  
  
  
  # Some published counts have non-integer model estimates, especially in most recent year
  j         =  2:ncol(mn)
  mn[ , j]  = colwise( round)(mn[ , j])
  
  names(mn) = vnames

  if ( !is.null( fix.yr)) { 
    for ( ir in 1:nrow(fix.yr)) {
      i = str_detect( mn$year, fix.yr$from.yr)
      mn$year[i] = fix.yr$from.yr[ir]
    }
    mn$year = as.numeric( mn$year)
  }
  
  expect_true( is.numeric( mn$year))

  mn
}