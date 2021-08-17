Chapter\_5
================

Code for the Chapter 5 of the [Modern Dive
book](https://moderndive.com/5-regression.html#regression).

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
library(moderndive)
library(skimr)
library(gapminder)
```

``` r
glimpse(evals)
```

    ## Rows: 463
    ## Columns: 14
    ## $ ID           <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17…
    ## $ prof_ID      <int> 1, 1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, …
    ## $ score        <dbl> 4.7, 4.1, 3.9, 4.8, 4.6, 4.3, 2.8, 4.1, 3.4, 4.5, 3.8, 4.…
    ## $ age          <int> 36, 36, 36, 36, 59, 59, 59, 51, 51, 40, 40, 40, 40, 40, 4…
    ## $ bty_avg      <dbl> 5.000, 5.000, 5.000, 5.000, 3.000, 3.000, 3.000, 3.333, 3…
    ## $ gender       <fct> female, female, female, female, male, male, male, male, m…
    ## $ ethnicity    <fct> minority, minority, minority, minority, not minority, not…
    ## $ language     <fct> english, english, english, english, english, english, eng…
    ## $ rank         <fct> tenure track, tenure track, tenure track, tenure track, t…
    ## $ pic_outfit   <fct> not formal, not formal, not formal, not formal, not forma…
    ## $ pic_color    <fct> color, color, color, color, color, color, color, color, c…
    ## $ cls_did_eval <int> 24, 86, 76, 77, 17, 35, 39, 55, 111, 40, 24, 24, 17, 14, …
    ## $ cls_students <int> 43, 125, 125, 123, 20, 40, 44, 55, 195, 46, 27, 25, 20, 2…
    ## $ cls_level    <fct> upper, upper, upper, upper, upper, upper, upper, upper, u…

``` r
evals_ch5 <- evals %>%
  select(ID, score:bty_avg)

glimpse(evals_ch5)
```

    ## Rows: 463
    ## Columns: 4
    ## $ ID      <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,…
    ## $ score   <dbl> 4.7, 4.1, 3.9, 4.8, 4.6, 4.3, 2.8, 4.1, 3.4, 4.5, 3.8, 4.5, 4.…
    ## $ age     <int> 36, 36, 36, 36, 59, 59, 59, 51, 51, 40, 40, 40, 40, 40, 40, 40…
    ## $ bty_avg <dbl> 5.000, 5.000, 5.000, 5.000, 3.000, 3.000, 3.000, 3.333, 3.333,…

``` r
set.seed(42)

evals_ch5 %>%
  sample_n(size = 5)
```

    ## # A tibble: 5 × 4
    ##      ID score   age bty_avg
    ##   <int> <dbl> <int>   <dbl>
    ## 1    49   4.5    33    4.67
    ## 2   321   3.8    52    2.33
    ## 3   153   4.2    52    4.83
    ## 4    74   3.8    42    4.83
    ## 5   228   4.7    39    8.17

``` r
evals_ch5 %>%
  summarise(across(c(bty_avg, score),
                   list(mean = mean, median = median)))
```

    ## # A tibble: 1 × 4
    ##   bty_avg_mean bty_avg_median score_mean score_median
    ##          <dbl>          <dbl>      <dbl>        <dbl>
    ## 1         4.42           4.33       4.17          4.3

``` r
evals_ch5 %>%
  select(bty_avg, score) %>%
  skim()
```

|                                                  |            |
|:-------------------------------------------------|:-----------|
| Name                                             | Piped data |
| Number of rows                                   | 463        |
| Number of columns                                | 2          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| numeric                                          | 2          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate | mean |   sd |   p0 |  p25 |  p50 | p75 | p100 | hist  |
|:---------------|-----------:|---------------:|-----:|-----:|-----:|-----:|-----:|----:|-----:|:------|
| bty\_avg       |          0 |              1 | 4.42 | 1.53 | 1.67 | 3.17 | 4.33 | 5.5 | 8.17 | ▃▇▇▃▂ |
| score          |          0 |              1 | 4.17 | 0.54 | 2.30 | 3.80 | 4.30 | 4.6 | 5.00 | ▁▁▅▇▇ |

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
    ##  [1] gapminder_0.3.0  skimr_2.1.3      moderndive_0.5.2 forcats_0.5.1   
    ##  [5] stringr_1.4.0    dplyr_1.0.7      purrr_0.3.4      readr_2.0.1     
    ##  [9] tidyr_1.1.3      tibble_3.1.3     ggplot2_3.3.5    tidyverse_1.3.1 
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_1.0.7           lubridate_1.7.10     formula.tools_1.7.1 
    ##  [4] assertthat_0.2.1     digest_0.6.27        utf8_1.2.2          
    ##  [7] repr_1.1.3           R6_2.5.0             cellranger_1.1.0    
    ## [10] backports_1.2.1      reprex_2.0.1         evaluate_0.14       
    ## [13] highr_0.9            httr_1.4.2           pillar_1.6.2        
    ## [16] rlang_0.4.11         readxl_1.3.1         rstudioapi_0.13     
    ## [19] rmarkdown_2.10       munsell_0.5.0        broom_0.7.9         
    ## [22] compiler_4.1.1       modelr_0.1.8         janitor_2.1.0       
    ## [25] xfun_0.25            base64enc_0.1-3      pkgconfig_2.0.3     
    ## [28] htmltools_0.5.1.1    tidyselect_1.1.1     fansi_0.5.0         
    ## [31] crayon_1.4.1         tzdb_0.1.2           dbplyr_2.1.1        
    ## [34] withr_2.4.2          grid_4.1.1           jsonlite_1.7.2      
    ## [37] gtable_0.3.0         lifecycle_1.0.0      DBI_1.1.1           
    ## [40] magrittr_2.0.1       infer_1.0.0          scales_1.1.1        
    ## [43] cli_3.0.1            stringi_1.7.3        renv_0.14.0         
    ## [46] fs_1.5.0             snakecase_0.11.0     xml2_1.3.2          
    ## [49] ellipsis_0.3.2       generics_0.1.0       vctrs_0.3.8         
    ## [52] tools_4.1.1          glue_1.4.2           hms_1.1.0           
    ## [55] yaml_2.2.1           colorspace_2.0-2     operator.tools_1.6.3
    ## [58] rvest_1.0.1          knitr_1.33           haven_2.4.3
