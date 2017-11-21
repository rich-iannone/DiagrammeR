context("Defining node and edge aesthetics and data attributes")

test_that("creating node data attribute values is possible", {

  # Create a node data list object
  # using the `node_data()` function
  node_data <-
    node_data(
      values_1 = c(1, 2, 4, 5),
      values_2 = c("one", "two", "four", "five"))

  # Expect that a list object has
  # been generated
  expect_is(
    node_data, "list")

  # Expect specific names in the object
  expect_equal(
    names(node_data),
    c("values_1", "values_2"))

  # Expect specific values in each
  # of the list components
  expect_equal(
    node_data[[1]],
    c(1, 2, 4, 5))

  expect_equal(
    node_data[[2]],
    c("one", "two", "four", "five"))

  # Expect the function to return
  # an error when supplying data
  # attributes that are also node
  # aesthetic attributes
  expect_error(
    node_data(
      penwidth = c(1, 2, 4, 5)))

  # Expect an error if using the
  # attributes named `x` or `y`
  expect_error(
    node_data(
      x = c(1, 2, 4, 5)))

  expect_error(
    node_data(
      y = c(1, 2, 4, 5)))
})

test_that("creating edge data attribute values is possible", {

  # Create a edge data list object
  # using the `edge_data()` function
  edge_data <-
    edge_data(
      values_1 = c(1, 2, 4, 5),
      values_2 = c("one", "two", "four", "five"))

  # Expect that a list object has
  # been generated
  expect_is(
    edge_data, "list")

  # Expect specific names in the object
  expect_equal(
    names(edge_data),
    c("values_1", "values_2"))

  # Expect specific values in each
  # of the list components
  expect_equal(
    edge_data[[1]],
    c(1, 2, 4, 5))

  expect_equal(
    edge_data[[2]],
    c("one", "two", "four", "five"))

  # Expect the function to return
  # an error when supplying data
  # attributes that are also edge
  # aesthetic attributes
  expect_error(
    edge_data(
      penwidth = c(1, 2, 4, 5)))
})

test_that("creating node aesthetic attribute values is possible", {

  # Create a node aesthetic list object
  # using the `node_aes()` function
  node_aes <-
    node_aes(
      shape = c("circle", "rectangle"),
      style = c("filled", "filled"),
      penwidth = c(2.3, 5.3),
      color = c("red", "#FFFFFF"),
      fillcolor = c("orange", "#000000"),
      fontname = c("Helvetica", "Helvetica"),
      fontsize = c(12, 10.5),
      fontcolor = c("gray25", "#000000"),
      peripheries = c(1, 2),
      height = c(6.2, 5),
      width = c(5.5, 6.0),
      x = c(2.1, 5.3),
      y = c(2, 4.7),
      group = c(1, 1),
      tooltip = c("tooltip_1", "tooltip_2"),
      xlabel = c("xlabel_1", "xlabel_2"),
      URL = c("http://www.website.net", "http:://www.internet.org"),
      sides = c(4, 5),
      orientation = c(0, 45.5),
      skew = c(0.4, -0.8),
      distortion = c(-0.7, 0.3),
      gradientangle = c(1.2, 45.0),
      fixedsize = c(TRUE, FALSE),
      labelloc = c("T", "C"),
      margin = c("0.5,0.01", "0.1,0.05"))

  # Expect that a list object has
  # been generated
  expect_is(
    node_aes, "list")

  # Expect specific names in the object
  expect_equal(
    names(node_aes),
    c("shape", "style", "penwidth", "color",
      "fillcolor", "fontname", "fontsize",
      "fontcolor", "peripheries", "height",
      "width", "x", "y", "group", "tooltip",
      "xlabel", "URL", "sides", "orientation",
      "skew", "distortion", "gradientangle",
      "fixedsize", "labelloc", "margin"))

  # Expect specific values in each
  # of the list components
  expect_equal(node_aes[[1]], c("circle", "rectangle"))
  expect_equal(node_aes[[2]], c("filled", "filled"))
  expect_equal(node_aes[[3]], c(2.3, 5.3))
  expect_equal(node_aes[[4]], c("red", "#FFFFFF"))
  expect_equal(node_aes[[5]], c("orange", "#000000"))
  expect_equal(node_aes[[6]], c("Helvetica", "Helvetica"))
  expect_equal(node_aes[[7]], c(12, 10.5))
  expect_equal(node_aes[[8]], c("gray25", "#000000"))
  expect_equal(node_aes[[9]], c(1, 2))
  expect_equal(node_aes[[10]], c(6.2, 5))
  expect_equal(node_aes[[11]], c(5.5, 6.0))
  expect_equal(node_aes[[12]], c(2.1, 5.3))
  expect_equal(node_aes[[13]], c(2, 4.7))
  expect_equal(node_aes[[14]], c(1, 1))
  expect_equal(node_aes[[15]], c("tooltip_1", "tooltip_2"))
  expect_equal(node_aes[[16]], c("xlabel_1", "xlabel_2"))
  expect_equal(node_aes[[17]], c("http://www.website.net", "http:://www.internet.org"))
  expect_equal(node_aes[[18]], c(4, 5))
  expect_equal(node_aes[[19]], c(0, 45.5))
  expect_equal(node_aes[[20]], c(0.4, -0.8))
  expect_equal(node_aes[[21]], c(-0.7, 0.3))
  expect_equal(node_aes[[22]], c(1.2, 45.0))
  expect_equal(node_aes[[23]], c(TRUE, FALSE))
  expect_equal(node_aes[[24]], c("T", "C"))
  expect_equal(node_aes[[25]], c("0.5,0.01", "0.1,0.05"))

  # Expect the function to return
  # an error when supplying any
  # argument name that is not an
  # aesthetic attribute
  expect_error(
    node_aes(
      value_1 = c(1, 2, 4, 5)))
})


test_that("creating edge aesthetic attribute values is possible", {

  # Create an edge aesthetic list object
  # using the `edge_aes()` function
  edge_aes <-
    edge_aes(
      style = c("bold", "dashed"),
      penwidth = c(1.0, 1.5),
      color = c("red", "#FFFFFF"),
      arrowsize = c(0.5, 1.0),
      arrowhead = c("vee", "tee"),
      arrowtail = c("normal", "dot"),
      fontname = c("Helvetica", "Helvetica"),
      fontsize = c(12, 10.5),
      fontcolor = c("gray25", "#000000"),
      len = c(1.0, 0.5),
      tooltip = c("tooltip_1", "tooltip_2"),
      URL = c("http://www.website.net", "http:://www.internet.org"),
      label = c("label_1", "label_2"),
      labelfontname = c("Helvetica", "Helvetica"),
      labelfontsize = c(12, 10.5),
      labelfontcolor = c("gray25", "#000000"),
      labeltooltip = c("tooltip_1", "tooltip_2"),
      labelURL = c("http://www.website.net", "http:://www.internet.org"),
      edgetooltip = c("tooltip_1", "tooltip_2"),
      edgeURL = c("http://www.website.net", "http:://www.internet.org"),
      dir = c("forward", "back"),
      headtooltip = c("tooltip_1", "tooltip_2"),
      headURL = c("http://www.website.net", "http:://www.internet.org"),
      headclip = c(TRUE, FALSE),
      headlabel = c("label_1", "label_2"),
      headport = c("n", "ne"),
      tailtooltip = c("tooltip_1", "tooltip_2"),
      tailURL = c("http://www.website.net", "http:://www.internet.org"),
      tailclip = c(TRUE, FALSE),
      taillabel = c("label_1", "label_2"),
      tailport = c("s", "nw"),
      decorate = c(FALSE, TRUE))

  # Expect that a list object has
  # been generated
  expect_is(
    edge_aes, "list")

  # Expect specific names in the object
  expect_equal(
    names(edge_aes),
    c("style", "penwidth", "color", "arrowsize",
      "arrowhead", "arrowtail", "fontname", "fontsize",
      "fontcolor", "len", "tooltip", "URL",
      "label", "labelfontname", "labelfontsize", "labelfontcolor",
      "labeltooltip", "labelURL", "edgetooltip", "edgeURL",
      "headtooltip", "headURL", "headclip", "headlabel",
      "headport", "tailtooltip", "tailURL", "tailclip",
      "taillabel", "tailport", "dir", "decorate"))

  # Expect specific values in each
  # of the list components
  expect_equal(edge_aes[[1]], c("bold", "dashed"))
  expect_equal(edge_aes[[2]], c(1.0, 1.5))
  expect_equal(edge_aes[[3]], c("red", "#FFFFFF"))
  expect_equal(edge_aes[[4]], c(0.5, 1.0))
  expect_equal(edge_aes[[5]], c("vee", "tee"))
  expect_equal(edge_aes[[6]], c("normal", "dot"))
  expect_equal(edge_aes[[7]], c("Helvetica", "Helvetica"))
  expect_equal(edge_aes[[8]], c(12, 10.5))
  expect_equal(edge_aes[[9]], c("gray25", "#000000"))
  expect_equal(edge_aes[[10]], c(1.0, 0.5))
  expect_equal(edge_aes[[11]], c("tooltip_1", "tooltip_2"))
  expect_equal(edge_aes[[12]], c("http://www.website.net", "http:://www.internet.org"))
  expect_equal(edge_aes[[13]], c("label_1", "label_2"))
  expect_equal(edge_aes[[14]], c("Helvetica", "Helvetica"))
  expect_equal(edge_aes[[15]], c(12, 10.5))
  expect_equal(edge_aes[[16]], c("gray25", "#000000"))
  expect_equal(edge_aes[[17]], c("tooltip_1", "tooltip_2"))
  expect_equal(edge_aes[[18]], c("http://www.website.net", "http:://www.internet.org"))
  expect_equal(edge_aes[[19]], c("tooltip_1", "tooltip_2"))
  expect_equal(edge_aes[[20]], c("http://www.website.net", "http:://www.internet.org"))
  expect_equal(edge_aes[[21]], c("tooltip_1", "tooltip_2"))
  expect_equal(edge_aes[[22]], c("http://www.website.net", "http:://www.internet.org"))
  expect_equal(edge_aes[[23]], c(TRUE, FALSE))
  expect_equal(edge_aes[[24]], c("label_1", "label_2"))
  expect_equal(edge_aes[[25]], c("n", "ne"))
  expect_equal(edge_aes[[26]], c("tooltip_1", "tooltip_2"))
  expect_equal(edge_aes[[27]], c("http://www.website.net", "http:://www.internet.org"))
  expect_equal(edge_aes[[28]], c(TRUE, FALSE))
  expect_equal(edge_aes[[29]], c("label_1", "label_2"))
  expect_equal(edge_aes[[30]], c("s", "nw"))
  expect_equal(edge_aes[[31]], c("forward", "back"))
  expect_equal(edge_aes[[32]], c(FALSE, TRUE))

  # Expect the function to return
  # an error when supplying any
  # argument name that is not an
  # aesthetic attribute
  expect_error(
    edge_aes(
      value_1 = c(1, 2, 4, 5)))
})
