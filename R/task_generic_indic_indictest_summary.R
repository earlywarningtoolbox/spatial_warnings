# 
# 
# This function prints a pretty table of a generic_spews_test object
# 
#'@export
print.generic_spews_test <- function(obj, ...) { 
    

  cat('\n')
  cat(' Generic Spatial Early-Warnings Summary\n') 
  cat('\n')
  
  cat(' Matrix', '   Mean', '  Variance (P>null)', 
      '    Skewness (P>null)', '   Moran\'s I (P>null)', '\n')
  
  if ( 'replicate' %in% colnames(obj) ) { 
    for (i in unique(obj[ ,'replicate'])) { 
      print_one_replicate(subset(obj, replicate == i), i)
    }
  } else { 
    print_one_replicate(obj, 1)
  }
  
  cat('\n')
  cat(' Significance tested against', attr(obj, 'nreplicates'), 
      'randomly shuffled matrices\n')
  cat(" Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1", '\n')
  cat('\n')
}

print_one_replicate <- function(tab, n) { 

  f <- function(v) { sprintf(paste0('%3.4f'), v) }
  
  n_str <- paste0('    ',
                  sprintf("%3.0f", n))        # 1 char
  
  dat <- subset(tab, indicator == 'Mean')
  mean_str <- paste0('  ',
                     f(dat[ ,'value']),          # 7 chars
                     '')
  
  dat <- subset(tab, indicator == 'Variance')
  variance_str <- paste0('     ',                       # 1 char
                         f(dat[ ,'value']),          # 7 chars
                         '   ',                        # 1 char
                         f(dat[ ,'pval']),
                         ' ',
                         pval_stars(dat[ ,'pval'])) # 4 chars
  
  dat <- subset(tab, indicator == 'Skewness')
  skewness_str <- paste0('  ',
                         f(dat[ ,'value']),          # 7 chars
                         '   ',                        # 1 char
                         f(dat[ ,'pval']),
                         ' ',
                         pval_stars(dat[ ,'pval'])) # 4 chars
  dat <- subset(tab, indicator == 'Moran\'s I')
  moran_str <- paste0('  ',
                      f(dat[ ,'value']),          # 7 chars
                      '   ',                        # 1 char
                      f(dat[ ,'pval']),
                      ' ',
                      pval_stars(dat[ ,'pval'])) # 4 chars
  
  cat(paste0(n_str, mean_str, variance_str, skewness_str, moran_str, "\n"))
  
}

pval_stars <- function(value) { 
  if (value < 0.001) { 
    return('*** ' )
  } else if (value < 0.01) { 
    return('**  '  )
  } else if (value < 0.05) { 
    return('*   '   )
  } else if (value < 0.1) { 
    return('.   '   )
  } else { 
    return('    ')
  }
}
