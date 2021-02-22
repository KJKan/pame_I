# ----- Fit models

# Clear working memory
rm(list = ls())

# Load required packages
library( "psychonetrics" )
library( "dplyr" )
library( "devtools" )
source_url( 'https://raw.githubusercontent.com/KJKan/pame_I/main/helperfunctions.R' )

# load models and simulated data
load( url( 'https://www.dropbox.com/s/8o197jshr0itki9/models.Rdata?raw=TRUE' ) ) # models
load( url( 'https://www.dropbox.com/s/hlqu44ltycc3gzt/simdat.Rdata?raw=TRUE' ) ) # simulated data

# fit all models to all datasets
simres <- lapply( models, 
                  function( model ) lapply( simdat, 
                                            function(i) apply( i, 
                                                               3, 
                                                               function( dat ) fitModel( dat, 
                                                                                         model ) ) ) )
