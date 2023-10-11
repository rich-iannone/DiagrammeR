context("Getting a table of X11 color names and hexadecimal values")

test_that("a data frame X11 color names and hexadecimal values can be made", {

  # Get a data frame object containing X11 color names and hex values
  x11_hex_df <- x11_hex()

  # Expect an object of class "data.frame"
  expect_s3_class(x11_hex_df, "data.frame")

  # Expect that the "x11_hex_df" data frame has 2 columns
  expect_equal(ncol(x11_hex_df), 2)

  # Expect that the "x11_hex_df" data frame has 655 rows
  expect_equal(nrow(x11_hex_df), 655)

  # Expect specific column names
  expect_in(colnames(x11_hex_df), c("x11_name", "hex"))

  # Expect that both columns are of the "character" class
  expect_type(x11_hex_df[,1], "character")
  expect_type(x11_hex_df[,2], "character")

  # Expect that the color names are lowercased with no spaces
  expect_match(x11_hex_df[,1], "[a-z1-4]")

  # Expect that the hexadecimal color values are well-formed
  expect_match(x11_hex_df[,2], "^#[a-f0-9]{6}$")
})
