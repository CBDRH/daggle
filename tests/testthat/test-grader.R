# First test

testDag1 <- dagitty::as.dagitty(
  "dag {
    X [exposure]
    Y [outcome]
    Z
    X -> Z
    X -> Y
  }"
)

test_that("User: NULL | Solution: No adjustment", {
  expect_equal(grader(NULL, testDag1), 'correct')
})

test_that("User: Z | Solution: No adjustment", {
  expect_equal(grader(c('Z'), testDag1), 'close')
})


# Second test

testDag2 <- dagitty::as.dagitty(
  "dag {
    X [exposure]
    Y [outcome]
    Z
    Z -> X
    Z -> Y
  }"
)

test_that("User: Z | Solution: Z", {
  expect_equal(grader(c('Z'), testDag2), 'correct')
})

test_that("User: NULL | Solution: Z", {
  expect_equal(grader(NULL, testDag2), 'incorrect')
})


# Third test

testDag3 <- dagitty::as.dagitty(
  "dag {
    X [exposure]
    Y [outcome]
    Z1
    Z2
    Z1 -> X
    Z1 -> Y
    Z2 -> X
    Z2 -> Y
  }"
)

test_that("User: NULL | Solution: Z1 and Z2", {
  expect_equal(grader(NULL, testDag3), 'incorrect')
})

test_that("User: Z1 | Solution: Z1 and Z2", {
  expect_equal(grader(c('Z1'), testDag3), 'incorrect')
})

test_that("User: Z2 | Solution: Z1 and Z2", {
  expect_equal(grader(c('Z2'), testDag3), 'incorrect')
})

test_that("User: Z1 & Z2 | Solution: Z1 and Z2", {
  expect_equal(grader(c('Z1', 'Z2'), testDag3), 'correct')
})

test_that("User: Z2 & Z1 | Solution: Z1 and Z2", {
  expect_equal(grader(c('Z2', 'Z1'), testDag3), 'correct')
})



# Test 4

testDag4 <- dagitty::as.dagitty(
  "dag {
    X [exposure]
    Y [outcome]
    Z1
    Z2
    Z3
    Z1 -> X
    Z1 -> Y
    Z2 -> X
    Z2 -> Z3
    Z3 -> Y
  }"
)

test_that("User: NULL | Solution: Z1&Z2 | Z1&Z3", {
  expect_equal(grader(NULL, testDag4), 'incorrect')
})

test_that("User: Z1 | Solution: Z1&Z2 | Z1&Z3", {
  expect_equal(grader(c('Z1'), testDag4), 'incorrect')
})

test_that("User: Z2 | Solution: Z1&Z2 | Z1&Z3", {
  expect_equal(grader(c('Z2'), testDag4), 'incorrect')
})

test_that("User: Z2 | Solution: Z1&Z2 | Z1&Z3", {
  expect_equal(grader(c('Z2'), testDag4), 'incorrect')
})

test_that("User: Z2&Z3 | Solution: Z1&Z2 | Z1&Z3", {
  expect_equal(grader(c('Z2', 'Z3'), testDag4), 'incorrect')
})

test_that("User: Z1&Z2 | Solution: Z1&Z2 | Z1&Z3", {
  expect_equal(grader(c('Z1', 'Z2'), testDag4), 'correct')
})

test_that("User: Z1&Z3 | Solution: Z1&Z2 | Z1&Z3", {
  expect_equal(grader(c('Z1', 'Z3'), testDag4), 'correct')
})

test_that("User: Z1,Z2&Z3 | Solution: Z1&Z2 | Z1&Z3", {
  expect_equal(grader(c('Z1', 'Z2', 'Z3'), testDag4), 'close')
})


# Test 5 (direct effect)

testDag5 <- dagitty::dagitty('dag {
X [exposure, pos="2.3,7.5"]
Y [outcome, pos="3.1,8.1"]
Z1 [pos="3.1,9.2"]
Z2 [pos="2.4,8.8"]
Z3 [pos="1.2,9.3"]
Z4 [pos="2.1,9.8"]
Z5 [pos="1.9,8.5"]
X -> Y
X -> Z2
X -> Z5
Z1 -> Y
Z2 -> Y
Z2 -> Z1
Z3 -> Z2
Z4 -> Z1
Z4 -> Z2
Z4 -> Z3
Z5 -> Y
Z5 -> Z1
Z5 -> Z3
Z5 -> Z4
}
')

test_that("User: NULL | Solution: Z1,Z2&Z5 | Z2,Z4,Z5", {
  expect_equal(grader(NULL, testDag5, effect = 'direct'), 'incorrect')
})

test_that("User: Z1 | Solution: Z1,Z2&Z5 | Z2,Z4,Z5", {
  expect_equal(grader(c('Z1'), testDag5, effect = 'direct'), 'incorrect')
})

test_that("User: Z1,Z2&Z5 | Solution: Z1,Z2&Z5 | Z2,Z4,Z5", {
  expect_equal(grader(c('Z1', 'Z2', 'Z5'), testDag5, effect = 'direct'), 'correct')
})

test_that("User: Z1,Z2&Z5 | Solution: Z1,Z4&Z5 | Z2,Z4,Z5", {
  expect_equal(grader(c('Z2', 'Z4', 'Z5'), testDag5, effect = 'direct'), 'correct')
})



# Test 6

testDag6 <- dagitty::as.dagitty(
  "dag {
    X [exposure]
    Y [outcome]
    Z1
    Z2
    X -> Y
    X -> Z1
    Y -> Z2
  }"
)

test_that("User: NULL | Solution: NULL", {
  expect_equal(grader(NULL, testDag6), 'correct')
})

test_that("User: NULL | Solution: NULL", {
  expect_equal(grader(c('Z1'), testDag6), 'close')
})

test_that("User: NULL | Solution: NULL", {
  expect_equal(grader(c('Z1', 'Z2'), testDag6), 'incorrect')
})
