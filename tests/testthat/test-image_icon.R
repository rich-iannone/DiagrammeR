context("Getting a web address for image icons is possible")

test_that("a web address for a graphics asset will be returned", {

  # Get the web address for the FontAwesome "database" icon
  fa_database_address <- image_icon("fa-database")

  # Expect that the returned object will be a vector of length 1
  expect_equal(length(fa_database_address), 1L)

  # Expect that the returned object will be of the "character" class
  expect_is(fa_database_address, "character")

  # Expect that the correct link will be returned
  expect_match(fa_database_address,
               "http://visualizers.co/fa/black/512/database.png")
})
