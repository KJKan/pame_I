# ----- Data analysis

# Clear working memory
rm(list = ls())

# Load required packages
library( "psychonetrics" )
library( "dplyr" )
library( "devtools" )
source_url( 'https://raw.githubusercontent.com/KJKan/pame_I/main/helperfunctions.R' )

# load list of results 
load( url( 'https://www.dropbox.com/s/w3yqqxdz54ic3rd/simres.Rdata?raw=TRUE' ) )

# Extract results
rmseas  <- extractFitm( simres, 'rmsea'  )
cfis    <- extractFitm( simres, 'cfi'    )
nfis    <- extractFitm( simres, 'nfi'    )
chisqs  <- extractFitm( simres, 'chisq'  )
dfs     <- extractFitm( simres, 'df'     )
pvalues <- extractFitm( simres, 'pvalue' )
aics    <- extractFitm( simres, 'aic.ll' )
bics    <- extractFitm( simres, 'bic'    )
dchisqs <- lapply( chisqs, function(i) apply(i, 1, function(i) i$HF-i$BF ) )
ddfs    <- lapply( dfs,    function(i) apply(i, 1, function(i) i$HF-i$BF ) )
ps_diff <- lapply( dchisqs, function(i) 1- pchisq( i, df = 11 ) )


# ------------ checks

# makes plots (distributions are according to expectations)
#pdf( 'simplots.pdf' )
histFitm( rmseas )  # near perfect if the fitted model is the true model
histFitm( cfis )    # near perfect if the fitted model is the true model
histFitm( nfis )    # near perfect if the fitted model is the true model
histFitm( chisqs )  # distributed around df if the fitted model is the true model
histFitm( pvalues ) # uniformly distributed if the fitted model is the true model
                    # skewed to the right if the model is not the true model 
                    # (accept when the fitted model is a model in which the true model is nested)
#dev.off()

# tables (AIC and BIC pick the true model when the true model is included in the comparison )
lapply( aics, function(i) table( apply(i, 1, which.min ) ) )
lapply( bics, function(i) table( apply(i, 1, which.min ) ) )



# ------------ test hypothesis 

# = 'if the network is not considered, the bifactor model is preferred over the higher order factor model 
# ( as a summary of the data ) 

lapply( aics, function(i) table( apply(i[,1:2], 1, which.min ) ) )$NW
lapply( bics, function(i) table( apply(i[,1:2], 1, which.min ) ) )$NW

# And what can be expected if the bifactor model would be the true model?

# AIC and BIC prefer the bifactor model
lapply( aics, function(i) table( apply(i[,1:2], 1, which.min ) ) )$BF
lapply( bics, function(i) table( apply(i[,1:2], 1, which.min ) ) )$BF


# Show in figures
layout( matrix( 1:9, 3, 3, TRUE ) )
hist( ps_diff$HF, main = paste( 'Comparison HF and BF', '\nTrue model = HF' ), xlab = 'P (chisq diff)' )
hist( ps_diff$BF, main = paste( 'Comparison HF and BF', '\nTrue model = BF' ), xlab = 'P (chisq diff)' )
hist( ps_diff$NW, main = paste( 'Comparison HF and BF', '\nTrue model = NW' ), xlab = 'P (chisq diff)' )

# absolute fit (chisq): ACCEPT THE MODEL IN 95% OF THE CASES
hist( unlist( pvalues$BF[,'BF'] ), main = paste( 'BF absolute fit',    '\nTrue model = BF'), xlab = 'P (chisq)' )
# approximate fit: NEAR PERFECT
hist( unlist( nfis$BF[,'BF'] ),    main = paste( 'BF approximate fit', '\nTrue model = BF'), xlab = 'nfi' )
# close fit: NEAR PERFECT
hist( unlist( rmseas$BF[,'BF'] ),  main = paste( 'BF close fit',       '\nTrue model = BF'), xlab = 'RMSEA' )

# absolute fit (chisq): reject the model
hist( unlist( pvalues$NW[,'BF'] ), main = paste( 'BF absolute fit',    '\nTrue model = NW'), xlab = 'P (chisq)' )
# approximate fit: good
hist( unlist( nfis$NW[,'BF'] ),    main = paste( 'BF approximate fit', '\nTrue model = NW'), xlab = 'nfi' )
# close fit: acceptable(?)
hist( unlist( rmseas$NW[,'BF'] ),  main = paste( 'BF close fit',       '\nTrue model = NW'), xlab = 'RMSEA' )



# ------------ Conclusion

# the empirical results are more in line 
# with the situation in which the true model is a network model
# than the situation in which the true model is a bifactor model
# !
