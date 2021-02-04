# ---------------- Data Simulation

# Clear working memory
rm( list = ls() )

# Load require packages
library( "psychonetrics" )
library( "dplyr" )

# Load the Hugarian WAIS-IV model implied matrices that will be used to generate data with
#  1. Higher order factor model ( 'g' model )
#  2. Bifactor model
#  2. Network model (Kan et al., 2020)
load( url ( "https://github.com/KJKan/pame_I/blob/main/st_sigmas.Rdata?raw=true" ) )

# Setup
set.seed( 03012021 ) # = start internship Tasos
nrep <- 1000         # number of replications per condition
n    <- 1112         # sample size used ( Hungarian WAIS-IV sample size )

# Simulate data  
simdata <- lapply( st_sigmas, 
                   function(i)
                     { 
                      replicate( nrep, 
                                 mvrnorm( n, rep( 0, ncol( i ) ), i ),
                                 simplify = FALSE ) 
                     }
                   )
      



