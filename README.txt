Interpretable Machine Learning for Survival Analysis

Sophie Hanna Langbein, Mateusz Krzyziński, Mikołaj Spytek, Hubert Baniecki, Przemysław Biecek, Marvin N. Wright

Author of the code: Sophie Hanna Langbein (langbein@leibniz-bips.de, sophie_langbein@gmx.de)


The R code can be found in utils.R, plotting_functions.R, simulation_example_pfi_ice_pdp.R and simulation_example_ale.R, simulation_example_fi.R, real_data_example.R
The code files need to be run in the following order: simulation_example_pfi_ice_pdp.R -> simulation_example_ale.R -> simulation_example_fi.R -> real_data_example.R

Files and Folder structure:
Folders:
data: folder containing the data to be analyzed
figures_iml: folder in which all figures generated in the code and contained in the paper are saved
files:
utils.R: code file containing helper functions
plotting_functions.R: code file containing plotting functions
simulation_example_pfi_ice_pdp.R: code file containing the simulation example for permutation feature importance, individual conditional expectation plots and partial dependence plots in the methods section of the paper
simulation_example_ale.R: code file containing the simulation example for accumulated local effects plots in the methods section of the paper
simulation_example_fi.R: code file containing the simulation example for the feature interaction H-statistics plots in the methods section of the paper
real_data_example.R: code file containing the example of an IML analysis on real data (GBSG2 dataset)

The code was produced with the following versions of R and packages:

R version 4.4.0 (2024-04-24)
Platform: aarch64-apple-darwin20
Running under: macOS Sonoma 14.6.1

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

time zone: Europe/Berlin
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base

other attached packages:
 [1] survivalmodels_0.1.19 wesanderson_0.3.7     keras_2.15.0
 [4] tensorflow_2.16.0     mlr3proba_0.6.3       mlr3verse_0.2.8
 [7] mlr3_0.19.0           ggbeeswarm_0.7.2      ranger_0.16.0
[10] survex_1.2.0          ggcorrplot_0.1.4.1    survival_3.5-8
[13] finalfit_1.0.7        survminer_0.4.9       ggpubr_0.6.0
[16] ggplot2_3.5.1         mice_3.16.0           dplyr_1.1.4
[19] haven_2.5.4

loaded via a namespace (and not attached):
  [1] rstudioapi_0.16.0       jsonlite_1.8.8          shape_1.4.6.1
  [4] magrittr_2.0.3          jomo_2.7-6              modeltools_0.2-23
  [7] ooplah_0.2.0            DALEX_2.4.3             nloptr_2.0.3
 [10] vctrs_0.6.5             minqa_1.2.7             base64enc_0.1-3
 [13] rstatix_0.7.2           forcats_1.0.0           distr6_1.8.4
 [16] broom_1.0.6             mitml_0.4-5             parallelly_1.37.1
 [19] palmerpenguins_0.1.1    mlr3tuning_0.20.0       zoo_1.8-12
 [22] uuid_1.2-0              whisker_0.4.1           lifecycle_1.0.4
 [25] iterators_1.0.14        pkgconfig_2.0.3         Matrix_1.7-0
 [28] R6_2.5.1                future_1.33.2           clue_0.3-65
 [31] digest_0.6.35           colorspace_2.1-0        patchwork_1.2.0
 [34] rprojroot_2.0.4         mlr3misc_0.15.0         tfruns_1.5.3
 [37] fansi_1.0.6             km.ci_0.5-6             abind_1.4-5
 [40] compiler_4.4.0          here_1.0.1              withr_3.0.0
 [43] backports_1.5.0         carData_3.0-5           spacefillr_0.3.3
 [46] param6_0.2.4            ggsignif_0.6.4          pan_1.9
 [49] MASS_7.3-60.2           tools_4.4.0             vipor_0.4.7
 [52] set6_0.2.6              beeswarm_0.4.0          prabclus_2.3-3
 [55] nnet_7.3-19             glue_1.7.0              lgr_0.4.4
 [58] nlme_3.1-164            grid_4.4.0              checkmate_2.3.1
 [61] cluster_2.1.6           generics_0.1.3          gtable_0.3.5
 [64] KMsurv_0.1-5            class_7.3-22            tidyr_1.3.1
 [67] data.table_1.15.4       hms_1.1.3               car_3.1-2
 [70] utf8_1.2.4              flexmix_2.3-19          foreach_1.5.2
 [73] pillar_1.9.0            dictionar6_0.1.3        robustbase_0.99-2
 [76] bbotk_0.8.0             splines_4.4.0           mlr3tuningspaces_0.5.0
 [79] lattice_0.22-6          tidyselect_1.2.1        knitr_1.47
 [82] mlr3mbo_0.2.2           gridExtra_2.3           RhpcBLASctl_0.23-42
 [85] stats4_4.4.0            xfun_0.44               diptest_0.77-1
 [88] DEoptimR_1.1-3          mlr3extralearners_0.8.0 boot_1.3-30
 [91] codetools_0.2-20        kernlab_0.9-32          mlr3viz_0.8.0
 [94] mlr3cluster_0.1.9       tibble_3.2.1            cli_3.6.2
 [97] rpart_4.1.23            xtable_1.8-4            reticulate_1.38.0
[100] munsell_0.5.1           mlr3learners_0.6.0      mlr3filters_0.8.0
[103] survMisc_0.5.6          Rcpp_1.0.12             globals_0.16.3
[106] zeallot_0.1.0           png_0.1-8               parallel_4.4.0
[109] mlr3hyperband_0.5.0     mclust_6.1.1            paradox_1.0.0
[112] lme4_1.1-35.3           listenv_0.9.1           glmnet_4.1-8
[115] mlr3pipelines_0.5.2     mlr3data_0.7.0          scales_1.3.0
[118] mlr3fselect_0.12.0      purrr_1.0.2             crayon_1.5.2
[121] fpc_2.2-12              rlang_1.1.4
