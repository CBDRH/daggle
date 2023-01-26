test_that("Confounding pathway", {
  expect_equal(getOpenPaths(ggdag::confounder_triangle(x_y_associated = TRUE)),
               data.frame(
                 paths = c("x -> y", "x <- z -> y"),
                 open = c(TRUE, TRUE),
                 directed = c(TRUE, FALSE)
               ))
})

test_that("Controlled confounder", {
  expect_equal(getOpenPaths(ggdag::confounder_triangle(x_y_associated = TRUE), adj = 'z'),
               data.frame(
                 paths = c("x -> y"),
                 open = c(TRUE),
                 directed = c(TRUE)
               ))
})



test_that("Mediating pathway", {
  expect_equal(getOpenPaths(ggdag::mediation_triangle(x_y_associated = TRUE)),
               data.frame(
                 paths = c("x -> m -> y", "x -> y"),
                 open = c(TRUE, TRUE),
                 directed = c(TRUE, TRUE)
               ))
})

test_that("Controlled mediator", {
  expect_equal(getOpenPaths(ggdag::mediation_triangle(x_y_associated = TRUE), adj = 'm'),
               data.frame(
                 paths = c("x -> m -> y", "x -> y"),
                 open = c(FALSE, TRUE),
                 directed = c(TRUE, TRUE)
               ))
})



test_that("Collider pathway", {
  expect_equal(getOpenPaths(ggdag::collider_triangle(x_y_associated = TRUE)),
               data.frame(
                 paths = c("x -> y"),
                 open = c(TRUE),
                 directed = c(TRUE)
               ))
})

test_that("Controlled collider", {
  expect_equal(getOpenPaths(ggdag::collider_triangle(x_y_associated = TRUE), adj = 'm'),
               data.frame(
                 paths = c("x -> m <- y", "x -> y"),
                 open = c(TRUE, TRUE),
                 directed = c(FALSE, TRUE)
               ))
})
