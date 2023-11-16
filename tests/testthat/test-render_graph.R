test_that("No warning is produced.", {
  # this seems not to pass all the time.
  skip_on_cran()
  # before changing render_graph circa line 177, there was a warn
  withr::local_seed(10)
  expect_no_warning(create_graph() %>%
                      add_balanced_tree(
                        k = 2, h = 3) %>%
                      render_graph(layout = "tree"))
})
