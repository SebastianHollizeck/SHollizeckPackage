test_that("Lighter shading works", {
  expect_true(all(make_shades("goldenrod", n= 4) == c("#DAA520", "#E3BB57", "#ECD28F", "#F5E8C7")))
})

test_that("Darker shading works", {
  expect_true(all(make_shades("goldenrod", n= 4, lighter = FALSE) == c("#DAA520", "#A37B18", "#6D5210", "#362908")))
})

test_that("N is at least 1", {
  expect_error(make_shades("goldenrod", n=-1), "n must be at least 0")
})

test_that("N is not 0", {
  expect_warning(make_shades("goldenrod", n=0), "n should be bigger than 0")
})
