This repository contains R code for implementing health care provider clustering using fusion penalty in quasilikelihood. The completed code and results are available at https://github.com/liliwustl/quasi-likelihood

### Repository Contents

###Code_for_simulation Directory:

Restrictions apply to the availability of real data, which was used under license for this study. The readers can request if they have interest:
https://www.srtr.org/requesting-srtr-data/data-requests/
We also include a mock dataset.

Contained within the "Code_for_simulation" directory are the following files:

"functions_binary.R": This file contains R code for fitting binary outcomes. 

"functions_count.R": This file contains R code utilized for fitting count outcomes, such as the Poisson model. 

"functions_negative_binomial.R": This file contains R code utilized for fitting negative binomial model. 

"Generate Simulated data”This file contains R code utilized for generating simulated dataset in our paper. The user can adjust the parameters and data distribution as needed.

"Figure1supp.R": Utilized to generate Figure 1 in the supplementary material.

"Table1.R": Utilized to generate Table 1 in the manuscript.

"Table2.R": Utilized to generate Table 2 in the manuscript.

"Table3.R": Utilized to generate Table 3 in the manuscript.

"Table4-5.R": Utilized to generate Table 4 and Table 5 in the manuscript.

"master.R": A comprehensive script that navigates through our study, reproducing all results including tables and figures from the article, with a single command.

###Simulated data Directory :

Simulated data directory includes the dataset in the simulation study, which are generated randomly .

"poisson-50" contains the 100 datasets in Example 1 with m=50

"poisson-100" contains the 100 datasets in Example 1 with m=100

"logistic-50" contains the 100 datasets in Example 2 with m=50

"logistic-100" contains the 100 datasets in Example 2 with m=100

"nbinom-50" contains the 100 datasets in Example 3 with m=50

"nbinom-100" contains the 100 datasets in Example 3 with m=100

"5groups-50" contains the 100 datasets in Example 4 with m=50

"5groups-100" contains the 100 datasets in Example 4 with m=100


###Results Directory:

The "Results" directory contains all the intermediate results obtained from running the corresponding dataset in the Simulated data through our code.
Since the datasets are generated randomly, there may be slight fluctuations in the results compared to those presented in our paper. However, the conclusions remain consistent with our paper.
 
figure 1:  Figure 1 in the supplementary material.

Table 1:  Table 1 in the manuscript.

Table 2:  Table 2 in the manuscript.

Table 3:  Table 3 in the manuscript.

Table 4:  Table 4 in the manuscript.

Table 5:  Table 5 in the manuscript.


#### Session Information
```R
> sessionInfo()
R version 4.1.1 (2021-08-10)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19045)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252
[4] LC_NUMERIC=C                           LC_TIME=English_United States.1252    
system code page: 936

attached base packages:
[1] stats4    grid      stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] nlme_3.1-162      lme4_1.1-32       Matrix_1.5-4      MASS_7.3-58.3     flexclust_1.4-1   modeltools_0.2-23
[7] lattice_0.21-8   

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.11       rstudioapi_0.15.0 magrittr_2.0.3    splines_4.1.1     tidyselect_1.2.0  R6_2.5.1         
 [7] rlang_1.1.1       minqa_1.2.5       fansi_1.0.4       dplyr_1.1.2       tools_4.1.1       parallel_4.1.1   
[13] utf8_1.2.3        cli_3.6.1         class_7.3-19      tibble_3.2.1      lifecycle_1.0.4   nloptr_2.0.3     
[19] vctrs_0.6.3       glue_1.6.2        compiler_4.1.1    pillar_1.9.0      generics_0.1.3    boot_1.3-28      
[25] pkgconfig_2.0.3  
```
### Authors and Reference
 Liu, L., He, K., Wang, D., Ma, S., Qu, A., Luan, Y., Miller, J. P., Song, Y.， Liu, L. (2024+). Health care provider clustering using fusion penalty in quasi-likelihood. 


