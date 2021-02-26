# Why bifactor models fit better than higher order factor models: A network perspective

## Table of content
- [Introduction](#Intro)
- [Simulations](#Simulations)
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

Let's clear our workspace first (run this line only if you really want that; you will lose everything you had in the workspace)

```{r}
rm( list = ls() )
```

# References <a name="References"></a>

Epskamp, S. (2019). *Psychonetrics*. R package. version 0.7.1.

Hood, S. B. (2008). *Latent variable realism in psychometrics.* Indiana University Press

Jensen, A. R. (1998). *The g factor: The science of mental ability*. Westport, CT: Praeger.

[Kan, K.J., de Jonge, H., van der Maas, H.L.J., Levine, S.Z., & Epskamp, S. (2020). How to Compare Latent Factor Models and Psychometric Network Models. *Journal of Intelligence*.](https://github.com/KJKan/mcfarland/blob/master/jintelligence-844295.pdf)

Kan, K. J., van der Maas, H. L., & Levine, S. Z. (2019). Extending psychometric network analysis: Empirical evidence against g in favor of mutualism?. *Intelligence, 73*, 52-62.

Schermelleh-Engel, K., Moosbrugger, H., & Müller, H. (2003). Evaluating the fit of structural equation models: Tests of significance and descriptive goodness-of-fit measures. *Methods of psychological research online, 8(2)*, 23-74.

Wechsler, D. (2008). *Wechsler adult intelligence Scale–Fourth edition (WAIS–IV)*. San Antonio, TX: The Psychological Corporation.




