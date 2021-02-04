# -------------- Preparation; getting the model implied matrices

# Clear working memory
rm(list = ls())

# Load required packages
library( "psychonetrics" )
library( "qgraph" )
library( "dplyr" )

# Load WAIS-IV correlation matrices
load( url ( "https://github.com/KJKan/mcfarland/blob/master/WAIS_Hungary.Rdata?raw=true" ) )
load( url ( "https://github.com/KJKan/mcfarland/blob/master/WAIS_US.Rdata?raw=true" ) )

# WAIS-IV sample Sizes
n_US      <- 1800 
n_Hungary <- 1112 

# ----

# The WAIS-IV subtests; observed variables 
yvars     <- colnames( WAIS_US )

# Number of observed variables
ny        <- length( yvars ) 

# covariance matrix used in psychonetrics models
cov       <- ( n_Hungary - 1 )/n_Hungary*WAIS_Hungary

# latent constructs to be measured (etas)
lvars     <- c( "P", # Perceptual
                "V", # Verbal
                "W", # Working Memory
                "S"  # Speed 
              )

# Number of latent constructs
ne        <- length( lvars ) 

# theoretical pattern of factor loadings (simple structure)
lambda_g  <- lambda_b <- matrix( c ( #P  V  W  S  g 
                         1, 0, 0, 0, 0,# BD
                         0, 1, 0, 0, 0, # SI
                         0, 0, 1, 0, 0, # DS
                         1, 0, 0, 0, 0, # MR
                         0, 1, 0, 0, 0, # VC
                         0, 0, 1, 0, 0, # AR 
                         0, 0, 0, 1, 0, # SS
                         1, 0, 0, 0, 0, # VP
                         0, 1, 0, 0, 0, # IN
                         0, 0, 0, 1, 0, # CD
                         0, 0, 1, 0, 0, # LN
                         0, 0, 1, 0, 0, # FW  
                         0, 1, 0, 0, 0, # CO
                         0, 0, 0, 1, 0, # CA
                         1, 0, 0, 0, 0  # PC 
                       ),
                     ncol = ne + 1, 
                     byrow = TRUE )

# In the bifactor model the subtests measure g directly
lambda_b[ , ne + 1 ] <- 1

# # factor loadings in the second order (in the g model only)
beta_g     <- matrix( c ( #P  V  W  S g  
                           0, 0, 0, 0, 1, # P
                           0, 0, 0, 0, 1, # V
                           0, 0, 0, 0, 1, # W
                           0, 0, 0, 0, 1, # S,
                           0, 0, 0, 0, 0  # g
                         ),
                     ncol = ne + 1, 
                     byrow = TRUE )

# Exract a network form the US Sample
NWModel_US <- ggm( covs = ( n_US - 1 )/n_US*WAIS_US,
                   omega = "Full",
                   nobs = n_US )

# Prune it
NWModel_US <- NWModel_US %>% prune( alpha = 0.01, recursive = TRUE )

# Aim for further improvement
NWModel_US <- NWModel_US %>% stepup 

# Extract the adjacency matrix and use it as confirmatory network in the Hungarian sample
omega <- 1*( getmatrix( NWModel_US, "omega" ) !=0 )



# ----- The models in psychonetrics

# Higher order factor model
HFModel   <- lvm( covs = cov, 
                  nobs = n_Hungary,
                  lambda = lambda_g, 
                  beta = beta_g,
                  sigma_zeta = 'empty',
                  identification = "variance" )

# Bifactor model
BFModel   <- lvm( covs = cov, 
                  nobs = n_Hungary,
                  lambda = lambda_b, 
                  sigma_zeta = 'empty',
                  identification = "variance" )

# Network model
NWModel   <- ggm( covs = cov,
                  nobs = n_Hungary,
                  omega = omega )

# Put all models in a list
models    <- list( HFModel, BFModel, NWModel )

# Run all models on the observed Hungarian WAIS-IV covariance (correlation) matrix 
results   <- lapply( models, function(i) i %>% runmodel ) 

# Extract the model standardized model implied covariance (correlation) matrices
st_sigmas <- lapply( results, 
                     function(i) { cov2cor ( matrix( getmatrix( i, "sigma"  ),
                                                     ny, ny, TRUE, 
                                                     list( yvars, yvars  ) ) ) } )
# Save the matrices
save( st_sigmas, file = 'st_sigmas.Rdata' )
