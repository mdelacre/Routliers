context("test-outliers_mahalanobis")

data(Attacks)
SOC <- rowMeans(Attacks[,c(
  "soc1r","soc2r","soc3r",
  "soc4","soc5","soc6",
  "soc7r","soc8","soc9",
  "soc10r","soc11","soc12","soc13")]
)
HSC <- rowMeans(Attacks[,22:46])

res <- outliers_mahalanobis(x = data.frame(SOC,HSC),na.rm=T)

test_that("data types correct", {
  expect_is(res,"outliers_mahalanobis")

})
