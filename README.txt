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

R version 4.5.1 (2025-06-13)
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
 [1] rlang_1.1.6           dplyr_1.1.4           ggbeeswarm_0.7.2      survAUC_1.3-0
 [5] data.table_1.17.8     pec_2025.06.24        prodlim_2025.04.28    survminer_0.5.1
 [9] ggpubr_0.6.1          ggplot2_4.0.0         ggnewscale_0.5.2      randomForestSRC_3.4.1
[13] ranger_0.17.0         survex_1.2.0          survival_3.8-3        simsurv_1.0.0

loaded via a namespace (and not attached):
  [1] RColorBrewer_1.1-3        rstudioapi_0.17.1         jsonlite_2.0.0            shape_1.4.6.1
  [5] magrittr_2.0.4            TH.data_1.1-4             farver_2.1.2              DALEX_2.5.2
  [9] rmarkdown_2.29            vctrs_0.6.5               base64enc_0.1-3           rstatix_0.7.2
 [13] htmltools_0.5.8.1         polspline_1.1.25          broom_1.0.10              Formula_1.2-5
 [17] parallelly_1.45.1         htmlwidgets_1.6.4         plyr_1.8.9                sandwich_3.1-1
 [21] zoo_1.8-14                commonmark_2.0.0          lifecycle_1.0.4           cmprsk_2.2-12
 [25] iterators_1.0.14          pkgconfig_2.0.3           Matrix_1.7-3              R6_2.6.1
 [29] fastmap_1.2.0             future_1.67.0             digest_0.6.37             numDeriv_2016.8-1.1
 [33] colorspace_2.1-1          patchwork_1.3.2           rprojroot_2.1.1           Hmisc_5.2-3
 [37] labeling_0.4.3            progressr_0.15.1          km.ci_0.5-6               abind_1.4-8
 [41] riskRegression_2025.09.17 compiler_4.5.1            here_1.0.2                withr_3.0.2
 [45] htmlTable_2.4.3           S7_0.2.0                  backports_1.5.0           carData_3.0-5
 [49] ggsignif_0.6.4            MASS_7.3-65               lava_1.8.1                quantreg_6.1
 [53] tools_4.5.1               vipor_0.4.7               foreign_0.8-90            beeswarm_0.4.0
 [57] future.apply_1.20.0       nnet_7.3-20               doFuture_1.1.2            glue_1.8.0
 [61] DiagrammeR_1.0.11         mets_1.3.7                nlme_3.1-168              gridtext_0.1.5
 [65] grid_4.5.1                checkmate_2.3.3           cluster_2.1.8.1           reshape2_1.4.4
 [69] generics_0.1.4            kernelshap_0.9.0          gtable_0.3.6              KMsurv_0.1-6
 [73] tidyr_1.3.1               xml2_1.4.0                car_3.1-3                 foreach_1.5.2
 [77] pillar_1.11.1             markdown_2.0              stringr_1.5.2             splines_4.5.1
 [81] ggtext_0.1.2              lattice_0.22-7            SparseM_1.84-2            tidyselect_1.2.1
 [85] rms_8.0-0                 knitr_1.50                gridExtra_2.3             litedown_0.7
 [89] xfun_0.53                 visNetwork_2.1.4          stringi_1.8.7             evaluate_1.0.5
 [93] codetools_0.2-20          data.tree_1.2.0           tibble_3.3.0              cli_3.6.5
 [97] rpart_4.1.24              xtable_1.8-4              survMisc_0.5.6            Rcpp_1.1.0
[101] globals_0.18.0            parallel_4.5.1            MatrixModels_0.5-4        listenv_0.9.1
[105] glmnet_4.1-10             viridisLite_0.4.2         mvtnorm_1.3-3             timereg_2.0.7
[109] scales_1.4.0              purrr_1.1.0               crayon_1.5.3              cowplot_1.2.0
[113] multcomp_1.4-28
