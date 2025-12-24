test_that("normalize scales values correctly", {
  # Test standard scaling 1-5 to 0-1
  input <- c(1, 2, 3, 4, 5)
  output <- normalize(input, 1, 5, 0, 1)

  expect_equal(output, c(0, 0.25, 0.5, 0.75, 1))

  # Test with single value
  expect_equal(normalize(10, 0, 100, 0, 1), 0.1)
})

test_that("replace_values swaps items correctly", {
  df <- data.frame(a = c("bad", "good", "bad"), b = 1:3)

  # Replace 'bad' with 'worse'
  result <- replace_values(df, "bad", "worse")

  expect_equal(result$a, c("worse", "good", "worse"))
  expect_equal(result$b, 1:3) # Ensure other columns are untouched
})



test_that("check_normality_by_group identifies distributions", {
  # Create perfect normal data
  set.seed(123)
  df_normal <- data.frame(
    group = rep(c("A", "B"), each = 20),
    value = rnorm(40)
  )

  # Should return TRUE (it uses Shapiro-Wilk internally)
  # Note: shapiro test is sensitive, so we expect TRUE for perfect normal data
  expect_true(check_normality_by_group(df_normal, "group", "value"))

  # Create obviously non-normal data (e.g., all identical values or extreme skew)
  df_skew <- data.frame(
    group = rep(c("A", "B"), each = 20),
    value = rep(1:5, 8) # Uniform-ish / discrete
  )
  # Modify one group to be extremely non-normal (constant)
  df_skew$value[df_skew$group == "A"] <- 1

  # Should return FALSE because variance is 0 or distribution is flat
  expect_false(check_normality_by_group(df_skew, "group", "value"))
})

test_that("not_empty throws error on NULL or NA", {
  expect_error(not_empty(NULL))
  expect_error(not_empty(NA))
  expect_true(not_empty(5))
})

