context("Creating shiny app code")

test_that("creating a shiny app with Graphviz DOT code is possible", {

  library(shiny)

  # Use Graphviz DOT code in a character object
  diagram_dot_code <- "
digraph {

# graph attributes
graph [overlap = true]

# node attributes
node [shape = box,
fontname = Helvetica,
color = blue]

# edge attributes
edge [color = gray]

# node statements
A; B; C; D; E
F [color = black]

# node attributes
node [shape = circle,
fixedsize = true,
width = 0.9]

# node statements
1; 2; 3; 4; 5; 6; 7; 8

# edge statements
A->1; B->2                   // gray
B->3 [color = red]           // red
B->4                         // gray
C->A [color = green]         // green
1->D; E->A; 2->4; 1->5; 1->F // gray
E->6; 4->6; 5->7; 6->7       // gray
3->8 [color = blue]          // blue
}
"

  # Create the shiny app "server" object
  server <- function(input, output) {
    output$diagram_dot_code <- renderGrViz({
      grViz({
        diagram_dot_code
      })
    })
  }

  # Create the shiny app "ui" object
  ui <- fluidPage(
    grVizOutput('diagram_dot_code', width = "100%", height = "760px")
  )

  # Create shiny app as an object
  the_shiny_app <- shinyApp(ui = ui, server = server)

  # Expect that the 'the_shiny_app' is indeed a shiny app
  expect_is(the_shiny_app, "shiny.appobj")

  # Expect that the shiny app contains the requisite components
  expect_is(the_shiny_app$httpHandler, "function")
  expect_is(the_shiny_app$serverFuncSource, "function")
  expect_null(the_shiny_app$onStart)
  expect_equal(length(the_shiny_app$options), 0L)
})

test_that("creating a shiny app with a graph object is possible", {

  library(shiny)

  # Create a node data frame
  nodes <-
    create_nodes(nodes = c("a", "b", "c", "d"),
                 label = FALSE,
                 type = "lower",
                 style = "filled",
                 color = "aqua",
                 shape = c("circle", "circle",
                           "rectangle", "rectangle"),
                 data = c(3.5, 2.6, 9.4, 2.7))

  # Create an edge data frame
  edges <-
    create_edges(from = c("a", "b", "c"),
                 to = c("d", "c", "a"),
                 relationship = "leading_to")

  # Create a graph object
  graph <-
    create_graph(nodes_df = nodes,
                 edges_df = edges,
                 node_attrs = "fontname = Helvetica",
                 edge_attrs = c("color = blue",
                                "arrowsize = 2"))

  # Create the shiny app "server" object
  server <- function(input, output) {
    output$diagram <- renderGrViz({
      grViz({
        graph$dot_code
      })
    })
  }

  # Create the shiny app "ui" object
  ui <- fluidPage(
    grVizOutput('diagram', width = "100%", height = "760px")
  )

  # Create shiny app as an object
  the_shiny_app <- shinyApp(ui = ui, server = server)

  # Expect that the 'the_shiny_app' is indeed a shiny app
  expect_is(the_shiny_app, "shiny.appobj")

  # Expect that the shiny app contains the requisite components
  expect_is(the_shiny_app$httpHandler, "function")
  expect_is(the_shiny_app$serverFuncSource, "function")
  expect_null(the_shiny_app$onStart)
  expect_equal(length(the_shiny_app$options), 0L)
})
