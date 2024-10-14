Interpretable Machine Learning for Survival Analysis

Sophie Hanna Langbein, Mateusz Krzyziński, Mikołaj Spytek, Hubert Baniecki, Przemysław Biecek, Marvin N. Wright

Author of the code: Sophie Hanna Langbein (langbein@leibniz-bips.de, sophie_langbein@gmx.de)


The R code can be found in utils.R, plotting_functions.R, simulation_example_ice_pdp.R and simulation_example_ale.R, simulation_example_fi.R, real_data_example.R
The code files need to be run in the following order: simulation_example_ice_pdp.R -> simulation_example_ale.R -> simulation_example_fi.R -> real_data_example.R

Files and Folder structure:
Folders:
data: folder containing the data to be analyzed
figures_iml: folder in which all figures generated in the code and contained in the paper are saved
files:
utils.R: code file containing helper functions
plotting_functions.R: code file containing plotting functions
simulation_example_ice_pdp.R: code file containing the simulation example for permutation feature importance, individual conditional expectation plots and partial dependence plots in the methods section of the paper
simulation_example_ale.R: code file containing the simulation example for accumulated local effects plots in the methods section of the paper
simulation_example_fi.R: code file containing the simulation example for the feature interaction H-statistics plots in the methods section of the paper
real_data_example.R: code file containing the example of an IML analysis on real data (GBSG2 dataset)

The code was produced with the following versions of R and packages:

R version 4.4.0 (2024-04-24)
Platform: aarch64-apple-darwin20
Running under: macOS 15.0.1

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
 [1] ranger_0.16.0         simsurv_1.0.0         data.table_1.15.4
 [4] dplyr_1.1.4           ggbeeswarm_0.7.2      ggnewscale_0.5.0
 [7] survminer_0.4.9       ggpubr_0.6.0          survAUC_1.3-0
[10] ggplot2_3.5.1         survex_1.2.0          randomForestSRC_3.3.1
[13] pec_2023.04.12        prodlim_2023.08.28    survival_3.5-8

loaded via a namespace (and not attached):
 [1] gridExtra_2.3       sandwich_3.1-0      rlang_1.1.4         magrittr_2.0.3
 [5] multcomp_1.4-25     polspline_1.1.25    compiler_4.4.0      vctrs_0.6.5
 [9] quantreg_5.98       stringr_1.5.1       pkgconfig_2.0.3     fastmap_1.2.0
[13] backports_1.5.0     KMsurv_0.1-5        utf8_1.2.4          rmarkdown_2.27
[17] MatrixModels_0.5-3  purrr_1.0.2         xfun_0.44           jsonlite_1.8.8
[21] timereg_2.0.5       broom_1.0.6         parallel_4.4.0      data.tree_1.1.0
[25] cluster_2.1.6       R6_2.5.1            stringi_1.8.4       RColorBrewer_1.1-3
[29] parallelly_1.37.1   car_3.1-2           rpart_4.1.23        numDeriv_2016.8-1.1
[33] Rcpp_1.0.12         iterators_1.0.14    knitr_1.47          future.apply_1.11.2
[37] zoo_1.8-12          base64enc_0.1-3     Matrix_1.7-0        splines_4.4.0
[41] nnet_7.3-19         tidyselect_1.2.1    rstudioapi_0.16.0   abind_1.4-5
[45] codetools_0.2-20    listenv_0.9.1       lattice_0.22-6      tibble_3.2.1
[49] withr_3.0.0         evaluate_0.24.0     foreign_0.8-86      future_1.33.2
[53] survMisc_0.5.6      pillar_1.9.0        carData_3.0-5       DiagrammeR_1.0.11
[57] checkmate_2.3.1     foreach_1.5.2       generics_0.1.3      munsell_0.5.1
[61] scales_1.3.0        globals_0.16.3      xtable_1.8-4        glue_1.7.0
[65] rms_6.8-1           Hmisc_5.1-3         tools_4.4.0         SparseM_1.83
[69] ggsignif_0.6.4      visNetwork_2.1.2    mvtnorm_1.2-5       grid_4.4.0
[73] DALEX_2.4.3         tidyr_1.3.1         colorspace_2.1-0    nlme_3.1-164
[77] patchwork_1.2.0     beeswarm_0.4.0      htmlTable_2.4.2     vipor_0.4.7
[81] Formula_1.2-5       cli_3.6.2           km.ci_0.5-6         fansi_1.0.6
[85] lava_1.8.0          gtable_0.3.5        rstatix_0.7.2       digest_0.6.35
[89] TH.data_1.1-2       htmlwidgets_1.6.4   htmltools_0.5.8.1   lifecycle_1.0.4
[93] MASS_7.3-60.2
