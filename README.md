
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hivscore

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/jaromilfrossard/hivscore/branch/master/graph/badge.svg)](https://app.codecov.io/gh/jaromilfrossard/hivscore?branch=master)
<!-- badges: end -->

The `hivscore` package is an implementation of the VACS Index 1, the
VACS Index 2 and the Dat’AIDS score.

## Installation

You can install the development version of the `hivscore` package like
so:

``` r
remotes::install_github("jaromilfrossard/hivscore")
```

## Computing the scores

You can compute the VACS Index 1:

``` r
library(hivscore)
data("vacs1")
vacs1$score_vacs1 <- score_vacsindex1(
  age = vacs1$age, cd4_count = vacs1$cd4, hiv1_rna = vacs1$hiv1_rna,
  hgb = vacs1$hgb, fib4 = vacs1$fib4, egfr = vacs1$egfr,
  hepc = vacs1$hepc)
head(vacs1)
#>   age cd4 log_hiv1_rna hgb fib4 egfr hepc alb wbc bmi hiv1_rna vacs1    vacs2
#> 1  52 435          1.7  14 1.34   90 TRUE   4 5.5  25       50    23 51.11143
#> 2  30 435          1.7  14 1.34   90 TRUE   4 5.5  25       50    11 38.98529
#> 3  35 435          1.7  14 1.34   90 TRUE   4 5.5  25       50    11 44.19800
#> 4  40 435          1.7  14 1.34   90 TRUE   4 5.5  25       50    11 47.50081
#> 5  45 435          1.7  14 1.34   90 TRUE   4 5.5  25       50    11 49.47464
#> 6  50 435          1.7  14 1.34   90 TRUE   4 5.5  25       50    23 50.70043
#>   score_vacs1
#> 1          23
#> 2          11
#> 3          11
#> 4          11
#> 5          11
#> 6          23
```

For the VACS Index 2, you will use:

``` r
data("vacs2")
vacs2$score_vacs2 <- score_vacsindex2(
  age = vacs2$AGE, cd4_count = vacs2$CD4,vl_log = vacs2$VL_LOG,
  hgb = vacs2$HGB,fib4 = vacs2$FIB4,egfr = vacs2$EGFR,
  hepc = vacs2$HEPC==1, alb = vacs2$ALB, bmi = vacs2$BMI,
  wbc = vacs2$WBC)
head(vacs2)
#>   PARAM SET AGE  CD4 VL_LOG HGB FIB4 EGFR HEPC ALB  WBC  BMI SCORE_V2
#> 1    98  98  75   10    5.0   9 7.50    0    1   2 11.0 15.0  173.467
#> 2    99  99  75 1000    1.3   9 7.50    0    1   2 11.0 15.0  136.322
#> 3     0  50  52  435    1.7  14 1.34   90    0   4  5.5 25.3   44.419
#> 4     1   1  30  435    1.7  14 1.34   90    0   4  5.5 25.3   32.293
#> 5     1   2  35  435    1.7  14 1.34   90    0   4  5.5 25.3   37.506
#> 6     1   3  40  435    1.7  14 1.34   90    0   4  5.5 25.3   40.808
#>   score_vacs2
#> 1   173.46689
#> 2   136.32164
#> 3    44.41904
#> 4    32.29290
#> 5    37.50561
#> 6    40.80842
```

The Dat’AIDS score can be computed using:

``` r
data("dataids")
dataids$dataids <- score_dataids(
  age = dataids$age,cd4_count = dataids$cd4_count, nonhiv_cancer = dataids$nonhiv_cancer,
  cardio_disease = dataids$cardio_disease,egfr = dataids$egfr, cirrhosis = dataids$cirrhosis,
  low_bmi = dataids$low_bmi,anemia = dataids$anemia)
head(dataids)
#>   age cd4_count nonhiv_cancer cardio_disease egfr cirrhosis low_bmi anemia
#> 1  62       550         FALSE          FALSE   70     FALSE   FALSE  FALSE
#> 2  62       550         FALSE          FALSE   70     FALSE   FALSE   TRUE
#> 3  62       550         FALSE          FALSE   70     FALSE    TRUE  FALSE
#> 4  62       550         FALSE          FALSE   70     FALSE    TRUE   TRUE
#> 5  62       550         FALSE          FALSE   70      TRUE   FALSE  FALSE
#> 6  62       550         FALSE          FALSE   70      TRUE   FALSE   TRUE
#>   SCORE_DATAIDS dataids
#> 1             0       0
#> 2             6       6
#> 3            10      10
#> 4            16      16
#> 5            13      13
#> 6            19      19
```

## Predicting survival probabilities

Using the Dat’AIDS score, you can predict survival probabilities after 1
year:

``` r
dataids$surv_prob1 <- surv_dataids(1, score = dataids$dataids)
head(dataids)
#>   age cd4_count nonhiv_cancer cardio_disease egfr cirrhosis low_bmi anemia
#> 1  62       550         FALSE          FALSE   70     FALSE   FALSE  FALSE
#> 2  62       550         FALSE          FALSE   70     FALSE   FALSE   TRUE
#> 3  62       550         FALSE          FALSE   70     FALSE    TRUE  FALSE
#> 4  62       550         FALSE          FALSE   70     FALSE    TRUE   TRUE
#> 5  62       550         FALSE          FALSE   70      TRUE   FALSE  FALSE
#> 6  62       550         FALSE          FALSE   70      TRUE   FALSE   TRUE
#>   SCORE_DATAIDS dataids surv_prob1
#> 1             0       0  0.9938032
#> 2             6       6  0.9886812
#> 3            10      10  0.9831056
#> 4            16      16  0.9692791
#> 5            13      13  0.9772061
#> 6            19      19  0.9586541
```

## References

For the VACS Index 1:

- Tate, J. P., Justice, A. C., Hughes, M. D., Bonnet, F., Reiss, P.,
  Mocroft, A., … & Sterne, J. A. (2013). An internationally
  generalizable risk index for mortality after one year of
  antiretroviral therapy. AIDS (London, England), 27(4), 563.
- Bebu, I., Tate, J., Rimland, D., Mesner, O., Macalino, G. E., Ganesan,
  A., … & Infectious Disease Clinical Research Program HIV Working
  Group. (2014). The VACS index predicts mortality in a young, healthy
  HIV population starting highly active antiretroviral therapy. Journal
  of acquired immune deficiency syndromes (1999), 65(2), 226.
- Justice, A. C., Modur, S., Tate, J. P., Althoff, K. N., Jacobson, L.
  P., Gebo, K., … & Gange, S. J. (2013). Predictive accuracy of the
  Veterans Aging Cohort Study (VACS) index for mortality with HIV
  infection: a north American cross cohort analysis. Journal of acquired
  immune deficiency syndromes (1999), 62(2), 149.

For the VACS Index 2:

- Tate, J. P., Sterne, J. A., Justice, A. C., Study, V. A. C., &
  Antiretroviral Therapy Cohort Collaboration (ART-CC. (2019). Albumin,
  white blood cell count, and body mass index improve discrimination of
  mortality in HIV-positive individuals. AIDS (London, England), 33(5),
  903.
- McGinnis, K. A., Justice, A. C., Moore, R. D., Silverberg, M. J.,
  Althoff, K. N., Karris, M., … & Study, V. A. C. (2021). Discrimination
  And Calibration Of The Vacs Index 2.0 For Predicting Mortality Among
  People With Hiv In North America. Clinical Infectious Diseases: an
  Official Publication of the Infectious Diseases Society of America.

For the Dat’AIDS score:

- Hentzien M, Delpierre C, Pugliese P, Allavena C, Jacomet C, Valantin
  M-A, et al. (2018) Derivation and internal validation of a mortality
  risk index for aged people living with HIV: The Dat’AIDS score. PLoS
  ONE 13(4): e0195725. <https://doi.org/10.1371/journal.pone.0195725>
