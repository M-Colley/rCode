test_that("generateEffectPlot returns a ggplot object", {
  # Create dummy data
  df <- data.frame(
    strat = rep(c("A", "B"), each = 10),
    emotion = rep(c("Happy", "Sad"), 10),
    score = rnorm(20)
  )

  # Run function
  p <- generateEffectPlot(
    data = df,
    x = "strat",
    y = "score",
    fillColourGroup = "emotion",
    ytext = "Score",
    xtext = "Strategy"
  )

  # Check if it is a ggplot class
  expect_s3_class(p, "ggplot")
})
