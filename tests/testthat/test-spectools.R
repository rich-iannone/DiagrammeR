context("Graphviz specifications")

test_that("the specification tools are functional", {

  # Create a simple Graphviz graph specification and perform
  # a substitution
  spec <- "
   digraph { '@@1' }

  [1]: LETTERS[1]
  "

  # Create a 'replaced_spec' grViz object
  replaced_spec <- grViz(replace_in_spec(spec))

  # Expect that the object inherits from 'grViz' and 'htmlwidget'
  expect_is(replaced_spec, c("grViz", "htmlwidget"))

  # Create a simple Graphviz graph specification and perform
  # substitution using a vector
  spec <- "
 digraph a_nice_graph {
 node [fontname = Helvetica]
 a [label = '@@1']
 b [label = '@@2-1']
 c [label = '@@2-2']
 d [label = '@@2-3']
 e [label = '@@2-4']
 f [label = '@@2-5']
 g [label = '@@2-6']
 h [label = '@@2-7']
 i [label = '@@2-8']
 j [label = '@@2-9']
 a -> { b c d e f g h i j}
 }

 [1]: 'top'
 [2]: 10:20
 "
  # Create a 'replaced_spec' grViz object
  replaced_spec <- grViz(replace_in_spec(spec))

  # Expect that the object inherits from 'grViz' and 'htmlwidget'
  expect_is(replaced_spec, c("grViz", "htmlwidget"))
})
