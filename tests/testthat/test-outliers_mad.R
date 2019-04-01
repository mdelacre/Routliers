context("test-outliers_mad")

data(Attacks)
SOC <- rowMeans(Attacks[,c(
  "soc1r","soc2r","soc3r",
  "soc4","soc5","soc6",
  "soc7r","soc8","soc9",
  "soc10r","soc11","soc12","soc13")]
  )

res <- outliers_mad(x = SOC,na.rm=F)

test_that("data types correct", {
  expect_is(res,"outliers_mad")

})

