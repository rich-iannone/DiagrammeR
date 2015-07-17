context("Creating objects with DiagrammeR, mermaid, and grViz")

test_that("htmlwidgets object can be created", {

  diagrammer_htmlwidget <- DiagrammeR("
  graph LR
    A-->B
    A-->C
    C-->E
    B-->D
    C-->D
    D-->F
    E-->F
")

  # Expect that the objects inherit from "DiagrammeR" and "htmlwidget"
  expect_is(diagrammer_htmlwidget, "DiagrammeR")
  expect_is(diagrammer_htmlwidget, "htmlwidget")

  mermaid_htmlwidget <- mermaid("
  graph LR
           A-->B
           A-->C
           C-->E
           B-->D
           C-->D
           D-->F
           E-->F
")


  # Expect that the objects inherit from "DiagrammeR" and "htmlwidget"
  expect_is(mermaid_htmlwidget, "DiagrammeR")
  expect_is(mermaid_htmlwidget, "htmlwidget")
})
