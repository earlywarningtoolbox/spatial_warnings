Spatial early warning signs
===========================

# Spatial indicators

## Generic spatial indicators

[generic_spatial_indicators.R](https://github.com/fdschneider/spatial_warnings/blob/master/R/generic_spatial_indicators.R)

This functions computes skewness, variance and autocorrelation with lag-1 (Moran's I).

This function depends on:

 - [morancorrelation_ews.R](https://github.com/fdschneider/spatial_warnings/blob/master/R/morancorrelation_ews.R): computes autocorrelation function with lag-1 (Moran's I).

 - [indicator_skewness.R](https://github.com/fdschneider/spatial_warnings/blob/master/R/indicator_skewness.R)

 - [indicator_variance_main.R](https://github.com/fdschneider/spatial_warnings/blob/master/R/indicator_variance_main.R)
 

## Fractal Geometries

[Indicator_FractalGeometry.R](https://github.com/fdschneider/spatial_warnings/blob/master/R/Indicator_FractalGeometry.R)

This function computes fractal geometries and also patch sizes.

Its depends on:

 - [FGcore.R](https://github.com/fdschneider/spatial_warnings/blob/master/R/FGcore.R): This function computes area and perimeter of patches in a matrix.

 - [lbl.R](https://github.com/fdschneider/spatial_warnings/blob/master/R/lbl.R): This function labels all the patchs and take care of boundary conditions.

## Patch size

 - []
