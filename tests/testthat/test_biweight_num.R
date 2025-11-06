test_that("biweight_num",
{
  biw0 = biweight_num(
    matrix(c(.5, .5), ncol=2),
    matrix(c(numgrid$x, numgrid$y), ncol=2),
    value = as.matrix(100),
    radius = 2,
    normalize = T
  )
  expect_equal(biw0[1,1], 25)

  biw1 = biweight_num(
    matrix(c(.5, .5), ncol=2),
    matrix(c(numgrid$x, numgrid$y), ncol=2),
    value = as.matrix(100),
    radius = 2,
    normalize = F
  )
  expect_equal(biw1[1,1], 76.5625)

  biw2 = biweight_num(
    matrix(c(.5, .5, .5, .5), ncol=2),
    matrix(c(numgrid$x, numgrid$y), ncol=2),
    value = matrix(c(100, 100), ncol = 1),
    radius = c(2,1),
    normalize = T
  )
  expect_equal(biw2[1,1], 50)
})
