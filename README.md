## Replication folder for the paper: "News Under Pressure? Science Politicization of Mainstream Media in Times of Crisis and Uncertainty"

## Overview

This repository contains code, data, and outputs to reproduce the analyses, figures, and tables for the paper.  
Scripts use `here` for path handling and write outputs into the `figures/` and `tables/` folders.

## Requirements

- R 4.5.0 (tested on Apple silicon/macOS)
- Core packages used across scripts (install if missing):

```r
pkgs <- c(
  "here","tidyverse","data.table","lubridate","zoo",
  "lme4","lmerTest","performance",
  "gt","htmltools","patchwork","scales",
  "dtw","proxy"
)
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))
```

## How to reproduce results

1. Open the `science_politicization_replication_folder.RProj` file at the repository root.
2. Ensure the working directory is the project root (the RProj handles this).
3. Run scripts in `code/main_text/` to regenerate main figures/tables. Run scripts in `code/appendix/` to regenerate appendix figures/tables. All outputs are written to `figures/` and `tables/`.

To run all scripts in a folder in alphanumeric order:


```r 
run_all <- function(dir) {
  scripts <- list.files(dir, pattern = "[.][Rr]$", full.names = TRUE)
  scripts <- scripts[order(tolower(basename(scripts)))]
  for (f in scripts) {
    message("→ Sourcing: ", f)
    source(f, echo = TRUE, max.deparse.length = Inf)
  }
}
# Examples:
# run_all("code/main_text")
# run_all("code/appendix")

```

## Session Info


```R
R version 4.5.0 (2025-04-11)
Platform: aarch64-apple-darwin20
Running under: macOS Sequoia 15.6.1

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
LAPACK: /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.1

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

time zone: Europe/Berlin
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] scales_1.4.0       htmltools_0.5.8.1  gt_1.0.0           lmerTest_3.1-3     lme4_1.1-37        Matrix_1.7-3       dtw_1.23-1        
 [8] proxy_0.4-27       performance_0.13.0 data.table_1.17.0  patchwork_1.3.0    here_1.0.1         lubridate_1.9.4    forcats_1.0.0     
[15] stringr_1.5.1      dplyr_1.1.4        purrr_1.0.4        readr_2.1.5        tidyr_1.3.1        tibble_3.2.1       ggplot2_3.5.2     
[22] tidyverse_2.0.0    zoo_1.8-14        

loaded via a namespace (and not attached):
 [1] gtable_0.3.6        xfun_0.52           insight_1.1.0       lattice_0.22-6      tzdb_0.5.0          numDeriv_2016.8-1.1 vctrs_0.6.5        
 [8] tools_4.5.0         Rdpack_2.6.4        generics_0.1.3      pkgconfig_2.0.3     RColorBrewer_1.1-3  lifecycle_1.0.4     compiler_4.5.0     
[15] farver_2.1.2        textshaping_1.0.1   litedown_0.7        sass_0.4.10         pillar_1.10.2       nloptr_2.2.1        MASS_7.3-65        
[22] reformulas_0.4.1    boot_1.3-31         nlme_3.1-168        commonmark_1.9.5    tidyselect_1.2.1    digest_0.6.37       stringi_1.8.7      
[29] labeling_0.4.3      splines_4.5.0       rprojroot_2.0.4     fastmap_1.2.0       grid_4.5.0          cli_3.6.5           magrittr_2.0.3     
[36] utf8_1.2.5          withr_3.0.2         timechange_0.3.0    ragg_1.4.0          hms_1.1.3           rbibutils_2.3       markdown_2.0       
[43] mgcv_1.9-1          rlang_1.1.6         Rcpp_1.0.14         glue_1.8.0          xml2_1.3.8          rstudioapi_0.17.1   minqa_1.2.8        
[50] R6_2.6.1            systemfonts_1.2.3   fs_1.6.6  
```

### Note
For reproducibility, scripts should be executed within the provided R project environment, and it is better to restart an R session after running each script to avoid potential conflicts.

