Chapter\_9
================

Code for the Chapter 9 of the [Modern Dive
book](https://moderndive.com/9-hypothesis-testing.html).

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.3     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(infer)
library(moderndive)
library(nycflights13)
library(ggplot2movies)
library(ggthemes)
```

Set the palette and the running theme for ggplot2.

``` r
theme_set(theme_bw())
theme_update(axis.text.x = element_text(
angle = -45,
hjust = 0,
vjust = 0.5
))
```

Document the information about the analysis session

``` r
sessionInfo()
```

    ## R version 4.1.1 (2021-08-10)
    ## Platform: x86_64-apple-darwin17.0 (64-bit)
    ## Running under: macOS Big Sur 10.16
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRblas.0.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices datasets  utils     methods   base     
    ## 
    ## other attached packages:
    ##  [1] ggthemes_4.2.4      ggplot2movies_0.0.1 nycflights13_1.0.2 
    ##  [4] moderndive_0.5.2    infer_1.0.0         forcats_0.5.1      
    ##  [7] stringr_1.4.0       dplyr_1.0.7         purrr_0.3.4        
    ## [10] readr_2.0.1         tidyr_1.1.3         tibble_3.1.3       
    ## [13] ggplot2_3.3.5       tidyverse_1.3.1    
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_1.0.7           lubridate_1.7.10     formula.tools_1.7.1 
    ##  [4] assertthat_0.2.1     digest_0.6.27        utf8_1.2.2          
    ##  [7] R6_2.5.1             cellranger_1.1.0     backports_1.2.1     
    ## [10] reprex_2.0.1         evaluate_0.14        httr_1.4.2          
    ## [13] pillar_1.6.2         rlang_0.4.11         readxl_1.3.1        
    ## [16] rstudioapi_0.13      rmarkdown_2.10       munsell_0.5.0       
    ## [19] broom_0.7.9          compiler_4.1.1       modelr_0.1.8        
    ## [22] janitor_2.1.0        xfun_0.25            pkgconfig_2.0.3     
    ## [25] htmltools_0.5.1.1    tidyselect_1.1.1     fansi_0.5.0         
    ## [28] crayon_1.4.1         tzdb_0.1.2           dbplyr_2.1.1        
    ## [31] withr_2.4.2          grid_4.1.1           jsonlite_1.7.2      
    ## [34] gtable_0.3.0         lifecycle_1.0.0      DBI_1.1.1           
    ## [37] magrittr_2.0.1       scales_1.1.1         cli_3.0.1           
    ## [40] stringi_1.7.3        renv_0.14.0          fs_1.5.0            
    ## [43] snakecase_0.11.0     xml2_1.3.2           ellipsis_0.3.2      
    ## [46] generics_0.1.0       vctrs_0.3.8          tools_4.1.1         
    ## [49] glue_1.4.2           hms_1.1.0            yaml_2.2.1          
    ## [52] colorspace_2.0-2     operator.tools_1.6.3 rvest_1.0.1         
    ## [55] knitr_1.33           haven_2.4.3
