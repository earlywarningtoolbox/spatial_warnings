Spatial early warning signs - R package spatial_warnings
========================================================

The package provided here helps computing spatial early warning signals of 
critical points. This is part of a collaborative project between the group of [Sonia Kéfi](http://sonia.kefi.fr/) (Institut de Sciences d'Evolution, CNRS, IRD, Université Montpellier, France) and [Vishwesha Guttal](https://teelabiisc.wordpress.com/) (Center for Ecological Sciences, Indian Institute of Science, Bangalore, India).

The R package provides several sets of functions related to the computation of
early warning signals of irreversible transitions (also known as *catastrophic
shifts*). In particular, it allows the conversion of input data to binary 
matrices (the type of data these indicators are developed for), the computation 
of those indicators and tools to help diagnostic and plot the outcome of these
analyses. 

## Contributors

Alain Danet, Alex Genin, Vishwesha Guttal, Sonia Kefi, Sabiha Majumder, Sumithra Sankaran, [Florian Schneider (Maintainer)](mailto:florian.schneider@univ-montp2.fr)

## Installation

This package is still under heavy development: the best way to install it is 
using the `devtools` package in R: 
  
```
if ( ! require(devtools) ) { 
  install.packages("devtools")
}
devtools::install_github("fdschneider/spatial_warnings")
```

## The spatial indicators 

Ecological systems can suffer drastic transitions such as desertification or 
eutrophication after a slight change in one or more external parameters, such as 
aridity or nutrient input. These qualitative changes in the behavior of a system 
at a threshold represents a critical or bifurcation point, and can give rise to 
irreversible *catastrophic shifts* when associated with irreversibility. A 
growing body of litterature suggests that a dynamical system should exhibit 
certain measurable properties around those critical points. Based on those 
properties, a set of spatial indicators of those critical points have been 
developed and fall withing several broad categories. 

  * "Generic" spatial indicators
  * Patch-size distribution
  * Fractal and periodic geometry

The package also provides several helper functions associated with the use of 
binary matrices such as patch-labelling functions. 

## Original authors and License

This package is derived from the [work of Dakos et al.](https://github.com/earlywarningtoolbox/spatial_warnings) on early warnings signals (see also the 
reference website for the [early-warnings signals toolbox](http://www.early-warning-signals.org/)).

## License

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
