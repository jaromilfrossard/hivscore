testthat::expect_equal(
  conv_g.ml_to_g.dl(c(12,13.5)),
  c(12,13.5)/100)

testthat::expect_equal(
  conv_g.dl_to_g.ml(c(12,13.5)),
  c(12,13.5)*100)

testthat::expect_equal(
  conv_mul_to_mm3(c(12,13.5)),
  c(12,13.5))

testthat::expect_equal(
  conv_mm3_to_mul(c(12,13.5)),
  c(12,13.5))

testthat::expect_equal(
  conv_K.ml_to_K.mm3(c(12,13.5)),
  c(12,13.5)/1000L)

testthat::expect_equal(
  conv_K.mm3_to_K.ml(c(12,13.5)),
  c(12,13.5)*1000L)

testthat::expect_equal(
  conv_cm_to_m(c(12,13.5)),
  c(12,13.5)/100L)

testthat::expect_equal(
  conv_m_to_cm(c(12,13.5)),
  c(12,13.5)*100L)

testthat::expect_equal(
  conv_mumol.l_to_mg.dl(c(12,13.5)),
  c(12,13.5)*0.0113096584483149)

testthat::expect_equal(
  conv_mg.dl_to_mumol.l(c(12,13.5)),
  c(12,13.5)/0.0113096584483149)
