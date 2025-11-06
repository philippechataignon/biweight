test_that("biweight",
{
  biw0 = biweight(
    sfvalue,
    sfgrid,
    radius = 2,
    var = c("val1", "val2")
  )
  expect_equal(sum(biw0$val1), sum(sfvalue$val1))
  expect_equal(sum(biw0$val2), sum(sfvalue$val2))

  biw1 = biweight(
    sfvalue,
    sfgrid,
    radius = "radius"
  )
  expect_equal(sum(biw1$val1), sum(sfvalue$val1))
  expect_equal(sum(biw1$val2), sum(sfvalue$val2))
})
