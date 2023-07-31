library(hbGPS)
context("hbGPS pipeline")
test_that("hbGPS pipeline process file 4 correctly", {
  # Prepare input data
  GGIR_file  = system.file("testfiles/4_GGIR.csv", package = "hbGPS")
  GGIRlegend_file  = system.file("testfiles/behavioralcodes2023-07-24.csv", package = "hbGPS")
  gps_file  = system.file("testfiles/4_GPSsnr.csv", package = "hbGPS")
  dn = "./ms5.outraw/40_100_400"
  if (!dir.exists(dn)) {
    dir.create(dn, recursive = TRUE)
  }
  file.copy(from = GGIR_file, to = dn)
  file.copy(from = GGIRlegend_file, to = "./ms5.outraw")
  
  # Prepare output folder
  outdir = "./output"
  if (!dir.exists(outdir)) {
    dir.create(outdir, recursive = TRUE)
  }
  
  # Run pipeline
  D = hbGPS(gps_file = gps_file,
            outputDir = outdir,
            idloc = 2,
            tz = "Asia/Shanghai",
            GGIRpath = "./ms5.outraw",
            time_format = "%d/%m/%Y %H:%M:%S")
  
  expect_equal(nrow(D), 20029)
  expect_equal(ncol(D), 44)
  
  expect_equal(length(unique(D$trip)), 39)
  expect_equal(mean(D$GGIR_ACC), 26.19259, tolerance = 0.0001)
  expect_equal(mean(D$snr), 256.9834, tolerance = 0.0001)
  expect_equal(mean(D$snr_ratio), 78.81412, tolerance = 0.0001)
  
  expect_equal(sd(D$deltaElevation), 5.630014, tolerance = 0.0001)
  expect_equal(mean(D$distance_m),  10.94871, tolerance = 0.0001)
  expect_equal(mean(D$deltaTime), 15.61311, tolerance = 0.0001)
  expect_equal(mean(D$speed_ms), 0.6386111, tolerance = 0.0001)
  expect_equal(mean(D$inclination_deg), -1.531331, tolerance = 0.00001)
  expect_equal(mean(D$bearing_deg), 10.82464, tolerance = 0.00001)
  expect_equal(mean(D$deltaBearing), 65.72964, tolerance = 0.00001)
  
  expect_equal(mean(D$tripMaxTimegGap), 0.7691348, tolerance = 0.000001)
  expect_equal(mean(D$tripEleSpeed_kmh), 0.03796242, tolerance = 0.000001)
  
  # Finally a crude global check to see whether anything has changed
  expect_equal(sum(rowSums(D[,c(11:15, 17:41)]), na.rm = TRUE), 26828374, tolerance = 0.1)
  
  # Clean up
  if (dir.exists(dn))  unlink(dn, recursive = TRUE)
  if (dir.exists(outdir))  unlink(outdir, recursive = TRUE)
})