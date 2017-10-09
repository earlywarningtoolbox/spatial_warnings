[![Build Status](https://travis-ci.org/spatial-ews/spatialwarnings.svg?branch=master)](https://travis-ci.org/spatial-ews/spatialwarnings)

Spatial early warning signs - R package
=======================================

The package provided here helps computing spatial early warning signals of
critical transitions. This is part of a collaborative project between [Sonia
Kefi's group](http://sonia.kefi.fr/) (Institut de Sciences d'Evolution, CNRS,
IRD, Université Montpellier, France) and [Vishwesha
Guttal's](https://teelabiisc.wordpress.com/) (Center for Ecological Sciences,
Indian Institute of Science, Bangalore, India).

The R package provides several sets of functions related to the computation of
early warning signals of ecosystem tipping points and irreversible 
transitions (also known as *catastrophic shifts*). In particular, it 
facilitates computing those indicators, assess their significance and plot 
their trends.

## Contributors

Alain Danet, [Alex Genin (Maintainer)](mailto:alexandre.genin@umontpellier.fr), 
Vishwesha Guttal, Sonia Kefi, Sabiha Majumder, Sumithra Sankaran, [Florian Schneider](mailto:florian.schneider@univ-montp2.fr)

## Installation

The developement version of this package can be installed using the 
`devtools` package in R:

```
if ( ! require(devtools) ) {
  install.packages("devtools")
}
devtools::install_github("spatial-ews/spatialwarnings")
```

CRAN version coming soon. Stay tuned. 

## The spatial indicators

Ecological systems can suffer drastic transitions such as desertification or 
eutrophication, sometimes even after a slight change in one or more external 
parameters, such as aridity or nutrient input. These qualitative changes in the 
behavior of a system at a threshold represents a critical or bifurcation point, 
and can give rise to *catastrophic shifts* when associated with irreversibility. 
A growing body of litterature suggests that a dynamical system should exhibit 
certain measurable properties around those critical points.

This package aims at providing a practical set of tools for the detection of 
these upcoming critical points in spatial datasets, by using indicators based on 
those properties. Those indicators fall within broad families around which the 
package is centered:

  * "Generic" spatial indicators
  * Spectrum-based indicators
  * Indicators based on patch-size distributions

Each of these indicator types can be computed with this package. Their 
significance can be assessed using permutation-based tests and results can 
be displayed using familiar summary/plot methods. 

## Code sample 

```r
library(ggplot2)
library(spatialwarnings)

serengeti.ic <- generic_spews(serengeti, 
                              subsize = 5, 
                              moranI_coarse_grain = TRUE)
serengeti.test <- indictest(serengeti.ic)

plot(serengeti.test, along = serengeti.rain) + 
  geom_vline(xintercept = 593, color = "red", linetype = "dashed") + 
  labs(x = "Annual rainfall", 
       y = "Mean cover/indicator value", 
       title = "Early warning signals of a shift in tree cover in Serengeti, Tanzania (Eby et al. 2016)", 
       subtitle = "Grey ribbons indicate the 5-95% quantiles of the null distribution") 

```
![Example result](./web/serengeti_example.png)

<!-- More extensive information is provided in the vignette file included in the
package. TODO: ADD PUBLICATION WHEN DONE -->

## Original authors and License

This package is derived from the [Dakos et al.'s work](https://github.com/earlywarningtoolbox/spatial_warnings) on early warnings signals (see also the
reference website for the [early-warnings signals toolbox](http://www.early-warning-signals.org/)).

### License

This work is licensed under an MIT license. Some code included in unit tests has
been written by Cosma Rohilla Shalizi [http://bactra.org/](http://bactra.org/)
and is redistributed in its entirety with the R package as specified in its
README file. 

The MIT License (MIT)

Copyright (c) 2015 the authors

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

## References

[Kéfi S, Guttal V, Brock WA, Carpenter SR, Ellison AM, et al. (2014) 
Early Warning Signals of Ecological Transitions: Methods for Spatial Patterns. 
PLoS ONE 9(3): e92097. doi:10.1371/journal.pone.0092097](http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0092097)

