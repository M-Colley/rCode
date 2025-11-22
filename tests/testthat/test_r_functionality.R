library(testthat)
library(dplyr)

# Sample data for tests
sample_df <- tibble::tibble(
  ConditionID = rep(c("A", "B"), each = 5),
  value = c(1:5, 2:6)
)

sample_model <- data.frame(
  Df = c(1, 10),
  `F value` = c(5.2, NA),
  `Pr(>F)` = c(0.03, NA),
  check.names = FALSE
)
row.names(sample_model) <- c("Factor", "Residuals")

chi_model <- data.frame(
  Df = 1,
  ` Chi Sq` = 4.5,
  ` Pr(>Chi)` = 0.02,
  check.names = FALSE
)
row.names(chi_model) <- "Factor"

nparld_model <- list(
  ANOVA.test = data.frame(
    Statistic = 5.1,
    df = 1,
    `p-value` = 0.01,
    RTE = 0.62,
    check.names = FALSE,
    row.names = "Factor"
  )
)

art_model <- data.frame(
  Effect = c("Factor"),
  Df = 1,
  `F value` = 4.1,
  `Pr(>F)` = 0.04,
  Df.res = 12,
  check.names = FALSE
)

report_data <- tibble::tibble(
  group = rep(c("A", "B", "C"), each = 4),
  score = c(1:4, 2:5, 3:6)
)

dunn_object <- list(
  res = data.frame(
    Comparison = c("A - B", "A - C"),
    Z = c(2.4, 1.1),
    P.adj = c(0.01, 0.2)
  )
)

posthoc_stats <- list(
  subtitle_data = data.frame(
    estimate = 0.45,
    p.value = 0.04,
    statistic = 2.1,
    df = 1,
    df.error = 10,
    method = "Paired t-test"
  ),
  pairwise_comparisons_data = data.frame(
    p.value = 0.01,
    group1 = "A",
    group2 = "B"
  )
)

basic_plot <- ggplot2::ggplot(sample_df, ggplot2::aes(x = ConditionID, y = value)) + ggplot2::geom_point()

# FIXED: Custom with_mock replacement for Global Environment scripts
# This replaces the testthat::with_mocked_bindings call which fails without a package
with_mock <- function(..., .env = globalenv()) {
  dots <- match.call(expand.dots = FALSE)$...
  if (length(dots) == 0) return()
  
  # The last argument is the code block to execute
  code_expr <- dots[[length(dots)]]
  
  # The named arguments are the mocks
  mock_exprs <- dots[-length(dots)]
  mocks <- lapply(mock_exprs, eval, envir = parent.frame())
  
  original <- list()
  mocked_names <- names(mocks)
  
  # Apply mocks
  for (nm in mocked_names) {
    if (exists(nm, envir = .env)) {
      original[[nm]] <- get(nm, envir = .env)
    }
    if (bindingIsLocked(nm, .env)) try(unlockBinding(nm, .env), silent = TRUE)
    assign(nm, mocks[[nm]], envir = .env)
  }
  
  # Cleanup on exit
  on.exit({
    for (nm in mocked_names) {
      if (nm %in% names(original)) {
        if (bindingIsLocked(nm, .env)) try(unlockBinding(nm, .env), silent = TRUE)
        assign(nm, original[[nm]], envir = .env)
      } else {
        if (exists(nm, envir = .env)) rm(list = nm, envir = .env)
      }
    }
  }, add = TRUE)
  
  # Run the test code
  eval(code_expr, envir = parent.frame())
}


#### basic utilities ---------------------------------------------------------

test_that("basic utility helpers behave", {
  expect_true(1 %!in% 2:5)
  expect_false(2 %!in% 2:5)
  expect_equal(na.zero(c(1, NA, 3)), c(1, 0, 3))

  written <- NULL
  expect_equal(
    pathPrep(path = "C:/Temp/sample.txt", read_fn = function() stop("should not be called"), write_fn = function(x) written <<- x),
    "C:/Temp/sample.txt"
  )
  expect_equal(written, "C:/Temp/sample.txt")

  n_result <- n_fun(sample_df$value)
  expect_s3_class(n_result, "data.frame")
  expect_equal(n_result$label, paste0("n = ", length(sample_df$value)))

  with_mock(curl_has_internet = function(...) TRUE, {
    expect_true(havingIP())
  })

  layer <- stat_sum_df(mean)
  expect_s3_class(layer, "LayerInstance")

  expect_equal(normalize(c(1, 2, 3), 1, 3, 0, 1), c(0, 0.5, 1))

  with_mock(
    getRversion = function() package_version("4.5.2"),
    requireNamespace = function(pkg, quietly = TRUE) TRUE,
    packageVersion = function(pkg) package_version("9.9.9"),
    get_latest_package_version = function(pkg) package_version("9.9.9"),
    {
      expect_invisible(checkPackageVersions())
    }
  )
})


#### ggstatsplot wrappers ----------------------------------------------------

test_that("within and between wrappers choose correct type", {
  skip_if_not_installed("ggstatsplot")
  skip_if_not_installed("ggsignif")
  data <- tibble::tibble(group = rep(c("A", "B"), each = 4), value = c(rep(0, 4), rep(1, 4)))

  result <- with_mock(
    `ggstatsplot::ggwithinstats` = function(..., type) list(type = type),
    shapiro.test = function(...) list(p.value = 0.2),
    {
      ggwithinstatsWithPriorNormalityCheck(data, "group", "value", "Value")
    }
  )
  expect_equal(result$type, "p")

  np_result <- with_mock(
    ggwithinstats_wrapper = function(..., type) list(type = type),
    shapiro_test_wrapper = function(...) list(p.value = 0.001),
    {
      ggwithinstatsWithPriorNormalityCheck(data, "group", "value", "Value")
    }
  )
  expect_equal(np_result$type, "np")

  between <- with_mock(
    ggbetweenstats_wrapper = function(..., type) list(type = type),
    shapiro_test_wrapper = function(...) list(p.value = 0.001),
    pairwise_comparisons_wrapper = function(...) data.frame(group1 = "A", group2 = "B", `p.value` = 0.01, stringsAsFactors = FALSE),
    geom_signif_wrapper = function(...) ggplot2::geom_blank(),
    {
      ggbetweenstatsWithPriorNormalityCheck(data, "group", "value", "Value", c("A", "B"))
    }
  )
  expect_equal(between$type, "np")

  expect_s3_class(
    with_mock(
      ggbetweenstats_wrapper = function(...) ggplot2::ggplot(),
      pairwise_comparisons_wrapper = function(...) data.frame(group1 = "A", group2 = "B", `p.value` = 0.01, stringsAsFactors = FALSE),
      geom_signif_wrapper = function(...) ggplot2::geom_blank(),
      {
        ggbetweenstatsWithPriorNormalityCheckAsterisk(data, "group", "value", "Value", c("A", "B"))
      }
    ),
    "ggplot"
  )

  expect_s3_class(
    with_mock(
      ggwithinstats_wrapper = function(...) ggplot2::ggplot(),
      pairwise_comparisons_wrapper = function(...) data.frame(group1 = "A", group2 = "B", `p.value` = 0.01, stringsAsFactors = FALSE),
      geom_signif_wrapper = function(...) ggplot2::geom_blank(),
      shapiro_test_wrapper = function(...) list(p.value = 0.2),
      {
        ggwithinstatsWithPriorNormalityCheckAsterisk(data, "group", "value", "Value", c("A", "B"))
      }
    ),
    "ggplot"
  )
})


#### effect size helpers -----------------------------------------------------

test_that("effect size helpers print expected summaries", {
  wilcox_obj <- list(p.value = 0.04, data.name = "Sample")
  expect_output(rFromWilcox(wilcox_obj, 20), "Effect Size")
  expect_output(rFromWilcoxAdjusted(wilcox_obj, 20, 2), "Effect Size")
  
  # FIXED: Use four backslashes to match literal \effectsize in the output
  expect_output(rFromNPAV(0.02, 30), "\\\\effectsize")
})


#### debugging and assumptions ----------------------------------------------

test_that("debugging and assumption helpers work", {
  df <- data.frame(a = c(1, 2, 3), b = c("x", "y", "z"))
  expect_type(debug_contr_error(df)$nlevels, "integer")

  anova_df <- tibble::tibble(
    factor1 = rep(c("A", "B"), each = 10),
    factor2 = rep(c("X", "Y"), times = 10),
    outcome = rnorm(20)
  )
  expect_null(checkAssumptionsForAnova(anova_df, "outcome", c("factor1", "factor2")))
})


#### reporting helpers -------------------------------------------------------

test_that("reporting helpers include effect sizes", {
  expect_match(capture.output(reportNPAV(sample_model, dv = "score")), "eta")
  expect_match(capture.output(reportNPAVChi(chi_model, dv = "score", sample_size = 30)), "w=")
  expect_match(capture.output(reportART(art_model, dv = "score")), "eta")
  expect_match(capture.output(reportNparLD(nparld_model, dv = "score")), "RTE")
  expect_output(reportMeanAndSD(sample_df, iv = "ConditionID", dv = "value"), "m")

  expect_output(reportDunnTest(dunn_object, report_data, iv = "group", dv = "score"), "post-hoc")
  expect_output(reportDunnTestTable(dunn_object, report_data, iv = "group", dv = "score"), "Post-hoc")

  expect_match(
    capture.output(
      with_mock(
        extract_stats_wrapper = function(...) posthoc_stats,
        {
          reportggstatsplot(basic_plot, iv = "group", dv = "score")
        }
      )
    ),
    "significant"
  )

  expect_output(
    with_mock(
      extract_stats_wrapper = function(...) posthoc_stats,
      {
        reportggstatsplotPostHoc(report_data, basic_plot, iv = "group", dv = "score")
      }
    ),
    "post-hoc"
  )
})


#### data shaping ------------------------------------------------------------

test_that("data wrangling helpers behave", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  replaced <- replace_values(data.frame(x = c("neg2", "neg1", "0")), c("neg2", "neg1"), c("-2", "-1"))
  expect_equal(replaced$x[1:2], c("-2", "-1"))

  input_path <- tempfile(fileext = ".xlsx")
  output_path <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(data.frame(
    ID = 1:2,
    videoinfo_1 = c("a", "b"),
    Q1 = 1:2,
    Q2 = 3:4,
    videoinfo_2 = c("c", "d"),
    Q3 = 5:6,
    Q4 = 7:8
  ), input_path)
  reshape_data(input_path, marker = "videoinfo", output_filepath = output_path)
  expect_true(file.exists(output_path))

  skip_if_not_installed("emoa")
  pareto_df <- add_pareto_emoa_column(
    data = data.frame(trust = c(1, 2), predictability = c(2, 1)),
    objectives = c("trust", "predictability")
  )
  expect_true("PARETO_EMOA" %in% names(pareto_df))

  rei <- remove_outliers_REI(data.frame(matrix(sample(1:5, 20, replace = TRUE), ncol = 4)), range = c(1, 5))
  expect_true("REI" %in% names(rei))
})


#### plotting helpers --------------------------------------------------------

test_that("plotting helpers return ggplot objects", {
  skip_if_not_installed("see")
  skip_if_not_installed("ggpmisc")
  expect_s3_class(generateEffectPlot(sample_df, "ConditionID", "value", "ConditionID"), "ggplot")

  mobo_df <- tibble::tibble(
    Iteration = rep(1:6, each = 2),
    trust = rnorm(12),
    ConditionID = rep(c("A", "B"), times = 6)
  )
  expect_s3_class(generateMoboPlot(mobo_df, "Iteration", "trust"), "ggplot")

  mobo_df2 <- tibble::tibble(
    Iteration = rep(1:6, each = 2),
    trust = rnorm(12),
    Phase = rep(c("sampling", "optimization"), each = 6),
    ConditionID = rep(c("A", "B"), times = 6)
  )
  expect_s3_class(generateMoboPlot2(mobo_df2, x = "Iteration", y = "trust", fillColourGroup = "ConditionID"), "ggplot")
})


#### latex and misc ---------------------------------------------------------

test_that("latex helper and np.anova behave", {
  text <- "- significant effect\n- non-significant effect\nStandardized parameters were obtained by fitting the model"
  expect_match(latexify_report(text, only_sig = TRUE, remove_std = TRUE), "\\\\item")

  factor_df <- data.frame(y = factor(c("a", "b")), x = factor(c("a", "b")))
  expect_error(np.anova(y ~ x, data = factor_df), "invalid type")
})
