
rd <- randDAG(4, .4)

test_that("First element is a df", {
  expect_equal(is.data.frame(rd$data), TRUE)
})

test_that("Second element is a dagitty", {
  expect_equal(dagitty::is.dagitty(rd$dag), TRUE)
})
