# ----- Data simulation

# Clear working memory
rm(list = ls())

# Load required packages and functions
library( "psychonetrics" )
library( "MASS" )
library( "dplyr" )
source( '0_helperfunctions.R' )

# load models and standardized model implied covariance matrices
load( url( 'https://www.dropbox.com/s/8o197jshr0itki9/models.Rdata?raw=TRUE' ) ) # models
load( url( 'https://www.dropbox.com/s/0e55spt5nzmigei/st_sigmas.Rdata?raw=TRUE' ) ) # st_sigmas

# Setup
set.seed( 03012021 )
nrep <- 1000                      # number of replications per condition
ny   <- ncol( st_sigmas[[1]] )    # number of variables 
n    <- 1112                      # sample size used ( Hungarian WAIS-IV sample size )

# generate nrep data sets according to the sigmas
simdat <- lapply( st_sigmas, 
                  function( sigma ) replicate( nrep, 
                                               mvrnorm( n, 
                                                        rep( 0, ny ), 
                                                        sigma ) ) ) 
