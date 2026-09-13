test_that("compute_generic_objective scores a single obs/sim pair", {
  obs <- data.table::data.table(Date = as.Date("2020-01-01") + 0:4, CC = c(20, 22, 25, 23, 21))
  sim <- data.table::data.table(Date = as.Date("2020-01-01") + 0:4, simCC = c(21, 21, 24, 24, 20))

  res <- compute_generic_objective(
    obs_data = obs, sim_data = sim,
    value_map = list(list(obs_col = "CC", sim_col = "simCC")),
    metric = "RMSE"
  )

  expect_type(res$summary, "double")
  expect_equal(res$summary, hydroGOF::rmse(sim$simCC, obs$CC))
})

test_that("objective_spec + evaluate_objective round-trip via read_out", {
  obs <- data.table::data.table(
    Date = as.Date(c("2011-03-01", "2011-03-02")),
    CC = c(12, 15)
  )
  obj <- objective_spec(
    obs_data = obs,
    value_map = list(list(obs_col = "CC", sim_col = "CC")),
    metric = "RMSE"
  )
  res <- evaluate_objective(obj, test_path("fixtures", "wheatCROP.OUT"))
  expect_type(res$summary, "double")
})
