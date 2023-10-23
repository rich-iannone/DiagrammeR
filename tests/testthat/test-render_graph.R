test_that("No warning is produced ", {
  # before changing render_graph circa line 177, there was a warn
  expect_no_warning(create_graph() %>%
    add_balanced_tree(
      k = 2, h = 3) %>%
    render_graph(layout = "tree"))
})
