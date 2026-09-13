parse_horizon <- function(path) {
  line <- readLines(path)[[9]]
  nums <- scan(text = line, what = character(), quiet = TRUE)
  list(
    thickness = as.numeric(nums[[1]]),
    sat = as.numeric(nums[[2]]),
    fc = as.numeric(nums[[3]]),
    wp = as.numeric(nums[[4]]),
    ksat = as.numeric(nums[[5]]),
    cra = as.numeric(nums[[8]]),
    crb = as.numeric(nums[[9]]),
    cn = as.integer(sub(":.*", "", trimws(readLines(path)[[3]]))),
    rew = as.integer(sub(":.*", "", trimws(readLines(path)[[4]])))
  )
}

test_that("CRa/CRb and CN/REW match FAO default soils", {
  expect_equal(aquacropr:::.ac_soil_class(43, 33, 9, 500), 2L)
  cr <- aquacropr:::.ac_cra_crb(43, 33, 9, 500)
  expect_equal(cr$cra, -0.4536, tolerance = 1e-6)
  expect_equal(cr$crb, 0.83734, tolerance = 1e-5)

  expect_equal(aquacropr:::.ac_soil_class(55, 54, 39, 35), 4L)
  cr <- aquacropr:::.ac_cra_crb(55, 54, 39, 35)
  expect_equal(cr$cra, -0.6086, tolerance = 1e-6)
  expect_equal(cr$crb, 0.594642, tolerance = 1e-4)

  expect_equal(aquacropr:::.ac_cn_default(3000), 46L)
  expect_equal(aquacropr:::.ac_cn_default(500), 61L)
  expect_equal(aquacropr:::.ac_cn_default(125), 72L)
  expect_equal(aquacropr:::.ac_cn_default(35), 77L)

  expect_equal(aquacropr:::.ac_rew_default(33, 9), 11L)
  expect_equal(aquacropr:::.ac_rew_default(31, 15), 9L)
  expect_equal(aquacropr:::.ac_rew_default(54, 39), 14L)
})

test_that("write_sol writes a 7.x horizon row", {
  path <- file.path(withr_local_tempdir(), "silt.SOL")
  write_sol(path, data.frame(
    thickness = 4, sat = 43, fc = 33, wp = 9, ksat = 500,
    description = "silt"
  ))
  h <- parse_horizon(path)
  expect_equal(h$thickness, 4)
  expect_equal(h$sat, 43)
  expect_equal(h$ksat, 500)
  expect_equal(h$cn, 61L)
  expect_equal(h$rew, 11L)
  expect_equal(h$cra, -0.4536, tolerance = 1e-6)
  expect_equal(h$crb, 0.83734, tolerance = 1e-5)
  txt <- readLines(path)
  expect_equal(txt[[5]], sprintf("%8d                   : number of soil horizons", 1))
  expect_match(txt[[6]], "-9")
})

test_that("write_sol writes several horizons and accepts CN override", {
  path <- file.path(withr_local_tempdir(), "stack.SOL")
  write_sol(
    path,
    data.frame(
      thickness = c(0.3, 1.2),
      sat = c(46, 50),
      fc = c(31, 39),
      wp = c(15, 23),
      ksat = c(500, 125),
      description = c("loam", "clay loam")
    ),
    cn = 65,
    description = "two layers"
  )
  lines <- readLines(path)
  expect_equal(as.integer(sub(":.*", "", trimws(lines[[3]]))), 65L)
  expect_equal(length(lines), 10L)
  expect_match(lines[[10]], "clay loam")
})

test_that("write_sol rejects more than five horizons", {
  df <- data.frame(
    thickness = rep(0.2, 6),
    sat = 46, fc = 31, wp = 15, ksat = 500
  )
  expect_error(write_sol(tempfile(fileext = ".SOL"), df), "1 to 5")
})
