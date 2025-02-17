# HGDMr
An R implementation of the Hysteretic and Gatekeeping Depressions Model (HGDM)

HGDM is a model of the variable connected/contributing fractions of basins
in the Canadian Prairies. The hydrology of this region is dominated by
the presence of millions of depressions ("sloughs" or "potholes"). As water
is added to a basin containing numerous small depressions, the connected/contributing fraction
increases. When water is removed from the depressions through evaporation or
infiltration, the connected fraction is zero, so the behaviour of the small
depressions is hysteretic with respect to the storage of water. 

Large depressions (having an area greater than approximately 5% of the total depressional area)
exhibit "gatekeeping", whereby they prevent upstream flows from exiting the basin until
the large depression is filled.

The **HGDMr** function `HGDM` models both behaviours far more efficiently than previously developed models. It
computes the constantly changing depressional storage and the net discharge flux. Note that
the function does not do any form of routing.

**HGDMr** works very well with fluxes computed for Canadian Prairie basins in
PHyDAP - Prairie Hydrology Design and Analysis Product (<https://doi.org/10.20383/102.0694>).

For more information about HGDM see Shook and Pomeroy (2025):

Shook, Kevin R., and John W. Pomeroy. “The Hysteretic and Gatekeeping Depressions Model - A New Model for Variable Connected Fractions of Prairie Basins.” Journal of Hydrology 654 (June 1, 2025): 132821. https://doi.org/10.1016/j.jhydrol.2025.132821.
