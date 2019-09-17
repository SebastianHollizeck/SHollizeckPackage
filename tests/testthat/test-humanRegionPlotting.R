# load XML
test_that("loading xml works", {
  expect_type(loadXML(), "externalptr")
})

test_that("loading non existing xml fails", {
  expect_error(loadXML("hasdkjhdak"), "Specified file does not exist")
})

# drawXML
test_that("drawing the XML works", {
  expect_silent(drawXML(loadXML()))
})

test_that("returning the picture object works", {
  expect_s4_class(drawXML(loadXML(),return=TRUE), "Picture")
})

# changePlottingStyle
## default works
test_that("changing to fill works", {
  expect_true(changePlottingStyle(loadXML(),organ="lung"))
})

## options work
test_that("changing to blue works", {
  expect_true(changePlottingStyle(loadXML(),organ="lung",color = 'blue'))
})

test_that("changing to stroke works", {
  expect_true(changePlottingStyle(loadXML(),organ="lung", style='stroke'))
})

test_that("changing to stroke and colour changes work and lwd", {
  expect_true(changePlottingStyle(loadXML(),organ="lung",color = 'blue', style='stroke', lwd=20))
})

## options fail
test_that("not a specified organ name", {
  expect_warning(changePlottingStyle(loadXML(),organ="humor"), "Could not find the specified organ")
})
