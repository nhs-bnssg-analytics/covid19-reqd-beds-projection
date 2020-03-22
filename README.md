# covid19_reqd_beds_projection
simple script to generate a projection of beds required to support given trajectory of covid19 cases requiring hospitalisation


### author:   BNSSG Modelling and Analytics
### version:  1.0
### date:     20 March 2020
### contact:  bnssg.analytics@nhs.net


# OVERVIEW

- this function outputs bed requirement given i) expected number of hospitalisations over time, and ii) detail on expected length of stay distribution 
separate use of this tool is envisaged for estimating ICU bed requirement, general acute beds, as well as resources like ventilators
- for each use, provide the demand (i.e. number of hospitalisations) and information pertaining to LOS for that service being considered

# INPUTS

 the user must input :
 
 - the expected hospitalisations over time 9these can relate to ICU beds, general beds, community beds, etc)
 - the corresponding expected length of stay detail is also entered (this consists of median length of stay and the 95th quantile)
 - additionally an optional tolerance can be placed around the expected hospitalisations in order to represent uncertainty
   (the effect of this on the expected hospitalisations can be seen in the bands on the outputs plot (see later))

# ENGINE
 - simulates the arrival and lengths of stay through sampling from the Poisson and lognormal distributions respectively
 note the Poisson rate parameter (taken from expected hospitalisations by day) is scaled normally according to selected tolerance (if optionally configured)
 - the lognormal ditribution parameters are from (automatically) matching quantiles using the exact formulae

# OUTPUTS

- a table of results for expected bed requirement is output, including mean, median and various quantiles
- a figure is also plot (as a pdf) containing shaded bands based on these quantiles
- this also includes the bands for the normally-distributed tolerance on the (inputted) expected hospitalisations (if optionally configured)
- bands represent a 40% chance of being in the central band (brightest red) and 30%, 20%, 5% and 3% in the (paired) outer bands (with 2% not displayed)

# FUNCTION ARGUMENTS

`cases` must be a data.frame containing daily hospitalisation numbers (column 2 'hospitalisations') over the considered date range (column 1 'dates')

`los_median` is the median length of stay (LOS) for hospitalised cases (in days)

`los_95 is the 95th quantile of LOS, i.e. the LOS (in days) such that only 1 in 20 of patients experience a greater duration

`tol` is an optional tolerance parameter (default 25) which can be flexed to represent a level of uncertainty around the case projection (range 0 to 100)

`nreps` is the number of simulation replications (more reps, greater accuracy, but takes longer)
