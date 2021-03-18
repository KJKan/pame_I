# function to fit the model of choice to a data set of choice
fitModel  <- function( data, params, nobs = NA, optimizer = "ucminf" ) {
  type <- params$type
  params$type <- NULL
  params$optimizer <- optimizer
  if( is.na( nobs ) ) params$data <- data else
  { params$covs <- data
  params$nobs <- nobs }
  if( type == "ggm") do.call( ggm, params ) %>% runmodel else
    if( type == "lvm") do.call( lvm, params ) %>% runmodel }

# function to extract fit measures from the nested list of results
extractFitm <- function( res, fitmeasure ){
  
  set <- as.list( names( res[[1]] ) )
  out <- lapply( set, function( s ) sapply( res, 
                                            function( model ) sapply( model[[s]], 
                                                                      function(i) i@fitmeasures[ fitmeasure ] ) ) )
  names( out ) <- set
  
  return( out )
}

# function to plot histograms of a fitmeasure
histFitm <- function( fits ){
  
  l <- matrix( 1:( length( fits )*ncol( fits[[1]] ) ),
               length( fits ),
               byrow = TRUE )
  layout( l )
  
  for(i in names( fits ) )
    for( j in colnames( fits[[1]] ) )
      hist( unlist( fits[[i]][,j] ),
            xlab = rownames( fits[[1]] )[1],
            main = paste( 'True model:', i, '; Fitted', j ) )
}
