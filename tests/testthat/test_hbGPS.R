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
            time_format = "%d/%m/%Y %H:%M:%S",
            return_object = TRUE)
  
  expect_equal(nrow(D), 20029)
  expect_equal(ncol(D), 43)
  
  expect_equal(length(unique(D$trip)), 40)
  expect_equal(mean(D$GGIR_ACC), 26.19259, tolerance = 0.0001)
  expect_equal(mean(D$snr), 256.9834, tolerance = 0.0001)
  expect_equal(mean(D$snr_ratio), 78.81412, tolerance = 0.0001)
  
  expect_equal(sd(D$deltaElevation), 5.630014, tolerance = 0.0001)
  expect_equal(mean(D$distance_m),  10.94871, tolerance = 0.0001)
  expect_equal(mean(D$deltaTime), 15.61311, tolerance = 0.0001)
  expect_equal(mean(D$speed_ms), 0.6386111, tolerance = 0.0001)
  expect_equal(mean(D$inclination_deg), -1.531331, tolerance = 0.00001)
  expect_equal(mean(D$bearing_deg), 10.62006, tolerance = 0.00001)
  expect_equal(mean(D$deltaBearing), 65.73111, tolerance = 0.00001)
  
  expect_equal(mean(D$tripMaxTimegGap), 0.7728793, tolerance = 0.000001)
  expect_equal(mean(D$tripEleSpeed_kmh), 0.03658415, tolerance = 0.000001)
  
  # Finally a crude global check to see whether anything has changed
  num_cols = which(unlist(lapply(D, is.numeric), use.names = FALSE) == TRUE)
  num_cols = num_cols[num_cols %in% which(colnames(D) == "timenum") == FALSE]
  expect_equal(sum(rowSums(D[, num_cols], na.rm = TRUE)), 27142209, tolerance = 0.1)
  
  # Check that it also works with a configFile
  DC = hbGPS(gps_file = gps_file,
            outputDir = outdir,
            idloc = 2,
            tz = "Asia/Shanghai",
            GGIRpath = "./ms5.outraw",
            time_format = "%d/%m/%Y %H:%M:%S",
            configFile =  system.file("testfiles/config_hbGPS.csv", package = "hbGPS"),
            return_object = TRUE)
  expect_equal(nrow(DC), 20029)
  expect_equal(ncol(DC), 43)
  expect_equal(length(unique(DC$trip)), 40)
  expect_equal(mean(DC$GGIR_ACC), 26.19259, tolerance = 0.0001)
  expect_equal(mean(DC$speed_ms), 0.6386111, tolerance = 0.0001)
  
  # Clean up
  if (dir.exists(dn))  unlink(dn, recursive = TRUE)
  if (dir.exists(outdir))  unlink(outdir, recursive = TRUE)
  if (dir.exists("./ms5.outraw"))  unlink("./ms5.outraw", recursive = TRUE)
})
