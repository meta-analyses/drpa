# Tests for dose_response function

test_that("dose_response fails for invalid dose input", {
  expect_error(dose_response("all-cause-mortality", "fatal", NULL))
  expect_error(dose_response("all-cause-mortality", "fatal", "10"))
  expect_error(dose_response("all-cause-mortality", "fatal", NA))
})

test_that("dose_response fails for invalid quantile", {
  expect_error(dose_response("all-cause-mortality", "fatal", 10, quantile = NA))
  expect_error(dose_response("all-cause-mortality", "fatal", 10, quantile = "0.5"))
  expect_error(dose_response("all-cause-mortality", "fatal", 10, quantile = 1))
  expect_error(dose_response("all-cause-mortality", "fatal", 10, quantile = -0.1))
})

test_that("dose_response fails for invalid censor_method", {
  expect_error(dose_response("all-cause-mortality", "fatal", 10, censor_method = NA))
  expect_error(dose_response("all-cause-mortality", "fatal", 10, censor_method = 123))
  expect_error(dose_response("all-cause-mortality", "fatal", 10, censor_method = "invalid"))
})

test_that("dose_response fails for unsupported cause/disease", {
  expect_error(dose_response("unsupported-disease", "fatal", 10))
  expect_error(dose_response("fake-disease", "fatal", 10))
})

test_that("dose_response fails for invalid outcome_type", {
  expect_error(dose_response("all-cause-mortality", "invalid", 10))
  expect_error(dose_response("all-cause-mortality", "total", 10))
})

test_that("dose_response returns correct data frame structure", {
  result <- dose_response("all-cause-mortality", "fatal", 10)
  expect_s3_class(result, "data.frame")
  expect_named(result, "rr")
})

test_that("dose_response returns confidence intervals when requested", {
  result <- dose_response("all-cause-mortality", "fatal", 10, confidence_intervals = TRUE)
  expect_named(result, c("rr", "lb", "ub"))
  expect_true(result$lb < result$rr)
  expect_true(result$ub > result$rr)
})

test_that("dose_response works with different causes", {
  causes <- c("all-cause-mortality", "breast-cancer", "diabetes", "stroke")
  for (cause in causes) {
    if (file.exists(system.file("extdata", paste0(cause, "-fatal.csv"), package = "drpa"))) {
      result <- dose_response(cause, "fatal", 10)
      expect_s3_class(result, "data.frame")
      expect_type(result$rr, "double")
    }
  }
})

test_that("dose_response works with different outcome_types", {
  result_fatal <- dose_response("all-cause-mortality", "fatal", 10)
  result_nonfatal <- dose_response("all-cause-mortality", "fatal-and-non-fatal", 10)
  expect_s3_class(result_fatal, "data.frame")
  expect_s3_class(result_nonfatal, "data.frame")
})

test_that("dose_response applies 75thPercentile censor correctly", {
  result <- dose_response("all-cause-mortality", "fatal", 100, censor_method = "75thPercentile")
  expect_type(result$rr, "double")
})

test_that("dose_response applies WHO-DRL censor correctly", {
  result <- dose_response("all-cause-mortality", "fatal", 100, censor_method = "WHO-DRL")
  expect_type(result$rr, "double")
})

test_that("dose_response applies WHO-QRL censor correctly", {
  result <- dose_response("all-cause-mortality", "fatal", 100, censor_method = "WHO-QRL")
  expect_type(result$rr, "double")
})

test_that("dose_response works with no censor method", {
  result <- dose_response("all-cause-mortality", "fatal", 10, censor_method = "none")
  expect_type(result$rr, "double")
})

test_that("dose_response works with different quantiles", {
  result_median <- dose_response("all-cause-mortality", "fatal", 10, quantile = 0.5)
  result_25 <- dose_response("all-cause-mortality", "fatal", 10, quantile = 0.25)
  result_75 <- dose_response("all-cause-mortality", "fatal", 10, quantile = 0.75)
  expect_type(result_median$rr, "double")
  expect_type(result_25$rr, "double")
  expect_type(result_75$rr, "double")
})

test_that("dose_response returns valid RR values", {
  result <- dose_response("all-cause-mortality", "fatal", 10)
  expect_gte(result$rr, 0)
  expect_lte(result$rr, 2)
})

test_that("dose_response with confidence intervals has lb <= ub", {
  result <- dose_response("all-cause-mortality", "fatal", 10, confidence_intervals = TRUE)
  expect_lte(result$lb, result$rr)
  expect_gte(result$ub, result$rr)
})
