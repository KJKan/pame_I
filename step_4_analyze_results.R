# ----- Data analysis

# Clear working memory
rm(list = ls())

# Load required packages
library( "psychonetrics" )
library( "dplyr" )
source( 'helperfunctions.R' )

# load list of results 
load( url( 'https://www.dropbox.com/s/w3yqqxdz54ic3rd/simres.Rdata?raw=TRUE' ) )

# Extract results
rmseas <- extractFitm( simres, 'rmsea' )
cfis   <- extractFitm( simres, 'cfi'   )
nfis   <- extractFitm( simres, 'nfi'   )

# makes plots
#pdf( 'simplots.pdf' )
histFitm( rmseas )
histFitm( cfis )
histFitm( nfis )
#dev.off()
