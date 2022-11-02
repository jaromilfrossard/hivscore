testthat::expect_equal(
  discrete_age_dataids(c(0,60,65,75)),
  factor(c("<60","60-64","65-74",">=75"),levels = c("<60","60-64","65-74",">=75"),
         ordered = T))

testthat::expect_equal(
  discrete_age_vacs1(c(0L,50L,65L)),
  factor(c("<50","60-64",">=65"),levels = c("<50","60-64",">=65"),
         ordered = T))

testthat::expect_equal(
  discrete_cd4_dataids(c(0L,200L,350L,500L)),
  factor(c("<200","200-350","350-500",">=500"),levels = c("<200","200-350","350-500",">=500"),
         ordered = T))

testthat::expect_equal(
  discrete_cd4_vacs1(c(0L,50L,100L,200L,350L,500L)),
  factor(c("<50","50-99","100-199","200-349","350-499",">=500"),
         levels = c("<50","50-99","100-199","200-349","350-499",">=500"),
         ordered = T))

testthat::expect_equal(
  discrete_egfr_dataids(c(0L,30L,60L)),
  factor(c("<30","30-59",">=60"),
         levels = c("<30","30-59",">=60"),
         ordered = T))

testthat::expect_equal(
  discrete_egfr_vacs1(c(0L,30L,45L,60L)),
  factor(c("<30","30-44","45-59",">=60"),
         levels = c("<30","30-44","45-59",">=60"),
         ordered = T))

testthat::expect_equal(
  discrete_fib4_vacs1(c(0,1.45,3.25)),
  factor(c("<1.45","1.45-3.24",">=3.25"),
         levels = c("<1.45","1.45-3.24",">=3.25"),
         ordered = T))

testthat::expect_equal(
  discrete_hgb_vacs1(c(0L,10L,12L,14L)),
  factor(c("<10","10-11.9","12-13.9",">=14"),
         levels = c("<10","10-11.9","12-13.9",">=14"),
         ordered = T))

testthat::expect_equal(
  discrete_hiv1rna_vacs1(c(0L,500L,100000L)),
  factor(c("<500","500-1e5",">=1e5"),
         levels = c("<500","500-1e5",">=1e5"),
         ordered = T))
