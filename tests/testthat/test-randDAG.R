test_that("First element is a df", {
  rd <- randDAG(4, .4)
  expect_equal(is.data.frame(rd$data), TRUE)
})

test_that("Second element is a dagitty", {
  rd <- randDAG(4, .4)
  expect_equal(dagitty::is.dagitty(rd$dag), TRUE)
})
