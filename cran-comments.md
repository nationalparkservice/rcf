# R CMD Check

There were no ERRORs or WARNINGs

There were 3 NOTEs:

* Found the following hidden files and directories:
    .here
  These were most likely included in error. See section 'Package
  structure' in the 'Writing R Extensions' manual.
  
*   Non-standard files/directories found at top level:
    'BAND_future_means.csv' 'BAND_future_means_pca.csv'
    'BAND_month_summary_c.csv' 'BAND_pca.png' 'BAND_pca_summary.csv'
    'BAND_thresholds.csv' 'BAND_year_summary_c.csv'
    'BAND_year_summary_pca.csv'
    
* This is the first submission to CRAN
    
The files are not actually created in the package, nor are they included in the package, but they are "created" in the vignette
