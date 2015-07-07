Spatial early warning signs
===========================

# Spatial indicators

## Generic spatial indicators

[generic_spatial_indicators.R](https://github.com/fdschneider/spatial_warnings/blob/master/R/generic_spatial_indicators.R)

This functions computes skewness, variance and autocorrelation with lag-1 (Moran's I).

This function depends on:

 - [morancorrelation_ews.R](https://github.com/fdschneider/spatial_warnings/blob/master/R/morancorrelation_ews.R): computes autocorrelation function with lag-1 (Moran's I).

 - [indicator_skewness.R](https://github.com/fdschneider/spatial_warnings/blob/master/R/indicator_skewness.R): computes skewness.

 - [indicator_variance_main.R](https://github.com/fdschneider/spatial_warnings/blob/master/R/indicator_variance_main.R): computes variance.
 

## Fractal Geometries

[Indicator_FractalGeometry.R](https://github.com/fdschneider/spatial_warnings/blob/master/R/Indicator_FractalGeometry.R)

This function computes fractal geometries and also patch sizes.

Its depends on:

 - [FGcore.R](https://github.com/fdschneider/spatial_warnings/blob/master/R/FGcore.R): This function computes area and perimeter of patches in a matrix.

 - [lbl.R](https://github.com/fdschneider/spatial_warnings/blob/master/R/lbl.R): This function labels all the patchs and take care of boundary conditions.

## Patch size

[fitpsd.R, `fitpsd()`](https://github.com/fdschneider/spatial_warnings/blob/master/R/fitpsd.R): This function returns and compares alternative model fits for cumulative patch size distributions.

Its depends on:

 -[fitpsd.R, `psd()`](https://github.com/fdschneider/spatial_warnings/blob/master/R/fitpsd.R): This function returns a table of cumulative patch sizes, i.e. the number and proportion of patches larger than each unique patch size.

 -[patchsizedistr_ews.R](https://github.com/fdschneider/spatial_warnings/blob/master/R/patchsizedistr_ews.R): Alternative to `psd()` defined above.
