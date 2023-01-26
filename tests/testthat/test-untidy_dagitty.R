tidy <- ggdag::tidy_dagitty(
          dagitty::dagitty( 'dag {
                            X [exposure]
                            Y [outcome]
                            Z
                            X <- Z -> Y
                            X -> Y
                          }'))

untidy <- untidy_dagitty(tidy)

test_that("Correct names", {
  expect_equal(names(untidy), c("dagitty", "r", "rString"))
})

test_that("dagitty output is dagitty", {
  expect_equal(dagitty::is.dagitty(untidy$dagitty), TRUE)
})

test_that("rString output is dagitty", {
  expect_equal(dagitty::is.dagitty(untidy$rString), TRUE)
})

