test_that("DiagrammeR() works as expected", {
  expect_no_warning(DiagrammeR(type = "grviz"))
  expect_error(DiagrammeR(type = "xxx"))
})
