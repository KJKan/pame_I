# ----- Data simulation

# Clear working memory
rm(list = ls())

# Load required packages
library( "psychonetrics" )
library( "dplyr" )
library( "devtools" )
libraray( "MASS")
source_url( 'https://raw.githubusercontent.com/KJKan/pame_I/main/helperfunctions.R' )

# load models and standardized model implied covariance matrices
load( url( 'https://www.dropbox.com/s/8o197jshr0itki9/models.Rdata?raw=TRUE' ) ) # models
load( url( 'https://www.dropbox.com/s/0e55spt5nzmigei/st_sigmas.Rdata?raw=TRUE' ) ) # st_sigmas

# Setup
set.seed( 03012021 )
nrep <- 1000                      # number of replications per condition
ny   <- ncol( st_sigmas[[1]] )    # number of variables 
n    <- 1425                      # sample size used ( German WAIS-IV sample size )

# generate nrep data sets according to the sigmas
simdat <- lapply( st_sigmas, 
                  function( sigma ) replicate( nrep, 
                                               mvrnorm( n, 
                                                        rep( 0, ny ), 
                                                        sigma ) ) ) 
