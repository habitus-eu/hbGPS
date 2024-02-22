library(hbGPS)
context("signalNoiseRatio")
test_that("signalNoiseRatio correctly extracts snr", {
  
  # Format 1
  satinfo_format1 = paste0("#32-26;#22-18;09-16;17-00;31-00;",
                           "#14-16;#20-22;#28-18;11-00;#19-27")
  df = data.frame(nsat_uv = "6(10)",
                  satinfo = satinfo_format1)
  out = signalNoiseRatio(df)
  expect_equal(out$df$snr_ratio, 60)
  expect_equal(out$df$snr, 143)
  expect_true(out$snr_available)
  expect_true(out$snr_ratio_available)
  
  df = data.frame(nsatused = 6,
                  nsatview = 10,
                  satinfo = satinfo_format1)
  out = signalNoiseRatio(df)
  expect_equal(out$df$snr_ratio, 60)
  expect_equal(out$df$snr, 143)
  expect_true(out$snr_available)
  expect_true(out$snr_ratio_available)
  
  # Format 2
  satinfo_format2 = paste0("(32-26);(22-18);(09-16);(17-00);(31-00);",
                           "(14-16);(20-22);(28-18);(11-00);(19-27)")
  df = data.frame(nsat_uv = "6 / 10",
                  satinfo = satinfo_format2)
  out = signalNoiseRatio(df)
  expect_equal(out$df$snr_ratio, 60)
  expect_equal(out$df$snr, 143)
  expect_true(out$snr_available)
  expect_true(out$snr_ratio_available)
  
  df = data.frame(nsatused = 6,
                  nsatview = 10,
                  satinfo = satinfo_format2)
  out = signalNoiseRatio(df)
  expect_equal(out$df$snr_ratio, 60)
  expect_equal(out$df$snr, 143)
  expect_true(out$snr_available)
  expect_true(out$snr_ratio_available)
})
