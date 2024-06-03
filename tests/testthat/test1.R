library(data.table)

test_that("Cbiweight OK",
{
  grid = fread("
  id;x;y
  a;0;0
  a;0;1
  a;1;1
  a;1;0
  ")

  biw1 = biweight_num(
    grid$x,
    grid$y,
    .5,
    .5,
    value.matrix = as.matrix(100),
    radius = 2,
    normalize = T
  )
  expect_equal(biw1[1,1], 25)

  biw1 = biweight_num(
    grid$x,
    grid$y,
    c(0.5, 0.5),
    c(0.5, 0.5),
    value.matrix = as.matrix(100),
    radius = c(2,1),
    normalize = T
  )
  expect_equal(biw1[1,1], 25)

  biw2 = biweight_num(
    grid$x,
    grid$y,
    0.5,
    0.5,
    value.matrix = as.matrix(100),
    radius = 2,
    normalize = F
  )
  # print(biw2)
  expect_equal(biw2[1,1], 76.5625)
})
