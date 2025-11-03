suppressPackageStartupMessages(library(sf))
library(data.table)

test_that("biweight",
{
  grid = st_as_sf(
  fread("
  grid_id;x;y
  a;0;0
  b;0;1
  c;1;1
  d;1;0
  "),
  coords = c("x", "y")
  )

  value = st_as_sf(
  fread("
  geo;x;y;radius;val1;val2
  X;0;0;1;123;451
  X;0;1;2;12;456.7
  X;1;1;1;125;450.789
  X;1;0;2;23;457.789
  Y;0.4;0.8;1;23;454.789
  Y;0.7;1.2;2;13;458.789
  Y;1.2;1.4;1;127;451.789
  Y;1.1;0.3;2;127;455.789
  "),
  coords = c("x", "y")
  )

  biw0 = biweight(
    value,
    grid,
    radius = 2,
    var = c("val1", "val2")
  )
  expect_equal(sum(biw0$val1), sum(value$val1))
  expect_equal(sum(biw0$val2), sum(value$val2))

  biw1 = biweight(
    value,
    grid,
    radius = "radius"
  )
  expect_equal(sum(biw1$val1), sum(value$val1))
  expect_equal(sum(biw1$val2), sum(value$val2))
})
