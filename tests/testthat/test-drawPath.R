test_that("drawPath returns a ggplot", {
  expect_equal(ggplot2::is.ggplot(drawPath(dag = ggdag::confounder_triangle(x_y_associated = TRUE),
                                  adj = c('z'),
                                  path = "x <- z -> y")), TRUE)
})
