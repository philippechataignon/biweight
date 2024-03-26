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

  biw = Cbiweight(
    grid$x,
    grid$y,
    .5,
    .5,
    input_val = as.matrix(100),
    radius = 1,
    ind_normalize = T
  )

  print(biw)
  expect_equal(biw[1,1], 25)
})
