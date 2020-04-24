# 
# This file will test the handling of external gis formats
# 

context("Handling of gis formats") 
if ( requireNamespace("png", quietly = TRUE) && 
     requireNamespace("raster", quietly = TRUE) ) { 
    
  test_that("RasterLayers are correctly handled", { 
    
    tmpj <- paste0(tempfile(), ".png")
    orig <- serengeti[[1]]
    origl <- list(orig, orig)
    png::writePNG(orig * 1.0, tmpj)
    
    # Single raster and list of rasters
    rast <- raster::raster(tmpj) > 0
    rastl <- c(rast, rast) 
        
    # Make sure we warn the use if the logical type of the data is lost when 
    # using raster data. 
    m <- convert_to_matrix(raster::raster(tmpj))
    expect_warning( check_mat(m) )
    
    # Make sure we stop if the passed raster has multiple layers 
    expect_error({ 
      convert_to_matrix(raster::stack(rast, rast))
    }, regexp = "spatialwarnings cannot use RasterBrick/RasterStack objects")
    expect_error({ 
      convert_to_matrix(raster::brick(rast, rast))
    }, regexp = "spatialwarnings cannot use RasterBrick/RasterStack objects")
    
    # Make sure the raster is properly handled. The suppressWarnings call is 
    # here to suppress spectral_sews complaining about using default arguments
    suppressWarnings({ 
      expect_equal(as.data.frame(spectral_sews(rast)), 
                   as.data.frame(spectral_sews(orig)))
      expect_equal(as.data.frame(spectral_sews(rastl)), 
                   as.data.frame(spectral_sews(origl)))
    })
    
    # Patch sews
    expect_equal(as.data.frame(patchdistr_sews(rast)), 
                 as.data.frame(patchdistr_sews(orig)))
    expect_equal(as.data.frame(patchdistr_sews(rastl)), 
                 as.data.frame(patchdistr_sews(origl)))
    
    # Simple sews 
    expect_equal(as.data.frame(compute_indicator(rastl, raw_cg_moran)), 
                 as.data.frame(compute_indicator(origl, raw_cg_moran)))
    expect_equal(as.data.frame(compute_indicator(rast, raw_cg_moran)), 
                 as.data.frame(compute_indicator(orig, raw_cg_moran)))
    
    # Display matrix should handle rasters transparently
    expect_true( inherits(display_matrix(rast), "gg") )
    expect_true( inherits(display_matrix(rastl), "gg") )
    
    unlink(tmpj)
  })
  
}
