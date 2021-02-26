# Why bifactor models fit better than higher order factor models: A network perspective

## Table of content
- [Introduction](#Intro)
- [Simulations](#Simulations)
      - [Measurement Model](#Measurement)
- [References](#References)

# Introduction <a name="Intro"></a>

This is a simulation study conducted in order to explain why bifactor models fit significantly better than higher order factor models.

The aim is to test the suggestion by Kan et al (2020) that if a network model is the actual data generating mechanism, a bifactor model will likely outperform the higher order factor model in summarizing the data.

To this end we first simulate data according three models:
- A (second order) g model                                                    
- A bifactor model 
- A network model extracted from the US standardization sample and refitted (confirmatory) on the German standardization sample correlation matrix.

Next, we fit all three models on all these data. This in order to show that the AIC and BIC would pick the true model, if the true model would be in the set of models considered.

Next, we show the results in case the network model would not be considered and the competition is between the higher order factor model and bifactor model. In that case the bifactor indeed beats the higher order factor model.

As, mentioned, the data concerned the WAIS-IV German validation sample data (correlation matrix). With thanks to Jens Lange for providing these data.

# Simulations <a name="Simulations"></a>

## Preparation <a name="Preparation"></a>

Let's clear our workspace first (run this line only if you really want that; you will lose everything you had in the workspace)

```{r}
rm( list = ls() )
```
Load required packages and some helper functions

```{r}
library( "psychonetrics" )
library( "dplyr" )
library( "devtools" )
source_url( 'https://raw.githubusercontent.com/KJKan/pame_I/main/helperfunctions.R' )
```

Our aim is to use a WAIS-IV network extracted previously (Kan et al., 2020) and to refit that network (confirmatory) in the German standardization sample. So let's load these matrices. 

```{r}
load( url ( "https://github.com/KJKan/mcfarland/blob/master/WAIS_US.Rdata?raw=true" ) )
load( url ( "https://github.com/KJKan/pame_I/blob/main/WAIS_Germany.Rdata?raw=true" ) )
```

The sample sizes are:

```{r}
n_US      <- 1800 
n_Germany <- 1425
```

What subtests does the WAIS-IV include?

```{r}
yvars     <- colnames( WAIS_US )
```
How many subtests are that?

```{r}
# Number of observed variables
ny        <- length( yvars ) 
```


```{r}
# covariance matrix used in psychonetrics models
cov       <- ( n_Germany - 1 )/n_Germany*WAIS_Germany
```

Theoretically, the WAIS-IV measures the following latent traits:

```{r}
lvars  <- c( "P", # Perceptual
             "V", # Verbal
             "W", # Working Memory
             "S"  # Speed 
            )

```
 So 4 in total. In g theory, the variable g explains the correlations among those factors.

```{r}
ne <- length( lvars ) 
```

The pattern of factor loadings is as follows:

```{r}
lambda_g  <- lambda_b <- matrix( c ( #P  V  W  S  g 
                                      1, 0, 0, 0, 0, # BD
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
```

In the bifactor model the subtests measure g directly.

```{r}
lambda_b[ , ne + 1 ] <- 1
```
Not that this implies that tests are suddenly not unidimensional anymore!

In the higher order factor model, tests remain unidimensional. As mentioned, the correlations bewteen the first order factors are explained by their common dependence on g, the general factor of intelligence. 

The pattern of factor loadings on the second level is thus:

```{r}
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
```
                       
As a result scores on all subtests are all positively intercorrelated. This robust empirical finding is commonly referred to as 'the positive manifold'.

In the network approach, the positive manifold is due to a manifold of positive interactions. No latent variables need to be assumed.

How can we model this? Here's an example of the extraction of a network. Note that this an exploratory analysis.

```{r}
# Extract a network form the US Sample
NWModel_US <- ggm( covs = ( n_US - 1 )/n_US*WAIS_US,
                   omega = "Full",
                   nobs = n_US )

# Prune it
NWModel_US <- NWModel_US %>% prune( alpha = 0.01, recursive = TRUE )

# Aim for further improvement
NWModel_US <- NWModel_US %>% stepup 
```

However, now we have such a network, we can fit it confirmatory on any other sample (in OpenMx or in Psychonetrics). Here's the pattern of relations.

```{r}
# Extract the adjacency matrix and use it as confirmatory network in the German sample
omega      <- 1*( getmatrix( NWModel_US, "omega" ) !=0 )
```

Here the aim is to fit all three models on the German standardization sample data. Let's first collect - per model - all important matrices and other pieces of information

```{r}
# parameters of the Higher Order Factor Model (HF)
paramsHF  <- list( lambda = lambda_g, 
                   beta = beta_g,
                   sigma_zeta = 'empty',
                   identification = "variance",
                   type = 'lvm' ) 

# parameters of the BifFactor Model (BF)
paramsBF  <- list( lambda = lambda_b, 
                   sigma_zeta = 'empty',
                   identification = "variance",
                   type = 'lvm' )

# parameters of the Network Model (NW)
paramsNW  <- list( omega = omega,
                   type = 'ggm' )

# Store them in a list
models    <- list( HF = paramsHF, BF = paramsBF, NW = paramsNW )
```

Next actually fit the models on the data. With function lapply, we only need one line of code.

```{r}
results   <- lapply( models, function(i) fitModel( cov, i, n_Germany ) )
```

The model implied correlations, we will use in the next step, the actual data generation. Let's extract those correlations from the results.

```{r}
# Extract the model standardized model implied covariance (correlation) matrices
st_sigmas <- lapply( results, 
                     function(i) { cov2cor ( matrix( getmatrix( i, "sigma"  ),
                                                     ny, ny, TRUE, 
                                                     list( yvars, yvars  ) ) ) } )
                                                     
```


# References <a name="References"></a>

Epskamp, S. (2019). *Psychonetrics*. R package. version 0.7.1.

Hood, S. B. (2008). *Latent variable realism in psychometrics.* Indiana University Press

Jensen, A. R. (1998). *The g factor: The science of mental ability*. Westport, CT: Praeger.

[Kan, K.J., de Jonge, H., van der Maas, H.L.J., Levine, S.Z., & Epskamp, S. (2020). How to Compare Latent Factor Models and Psychometric Network Models. *Journal of Intelligence*.](https://github.com/KJKan/mcfarland/blob/master/jintelligence-844295.pdf)

Kan, K. J., van der Maas, H. L., & Levine, S. Z. (2019). Extending psychometric network analysis: Empirical evidence against g in favor of mutualism?. *Intelligence, 73*, 52-62.

Schermelleh-Engel, K., Moosbrugger, H., & Müller, H. (2003). Evaluating the fit of structural equation models: Tests of significance and descriptive goodness-of-fit measures. *Methods of psychological research online, 8(2)*, 23-74.

Wechsler, D. (2008). *Wechsler adult intelligence Scale–Fourth edition (WAIS–IV)*. San Antonio, TX: The Psychological Corporation.




