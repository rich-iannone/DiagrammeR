# Selections

Occasionally, you’ll want to operate on a select group of nodes or
edges. Some functions affect a single node or edge while others (or,
sometimes, the same functions) operate on all nodes or edges in a graph.
Selections allow you to target specified nodes or edges and then apply
specialized functions to operate on just those selected entities. Most
of the selection functions support rudimentary set operations across
several calls of the selection functions (i.e., for the union,
intersection, or difference between selection sets of nodes or edges).

## Creating a Node Selection

Selecting nodes in a graph is accomplished by targeting a specific node
attribute (e.g., `type`, `label`, styling attributes such as `width`, if
available, or arbitrary data values available for nodes).

As with all of the `select_*()` functions, the graph argument is the
first in the function signature. This is beneficial for forward-piping
the graph in a pipeline and propagating a series of transformations on
the graph. There may be situations where several types of selections be
applied to the graphs nodes or edges (say, selecting all nodes of a
graph in the first step and then subtracting nodes of a specific type
from that group) and this is where the pipeline paradigm becomes
incredibly useful, and, as an added bonus, easy to read and to reason
about.

The main handle on selectivity for the
[`select_nodes()`](https://rich-iannone.github.io/DiagrammeR/reference/select_nodes.md)
function is through the node attributes of the graph’s internal NDF,
which contains columns for all possible node attributes in the graph. By
providing an expression in `conditions`, nodes will be placed in the
active selection where they are `TRUE`. Here are two common types of
expressions that work well for the `conditions` argument:

1.  a logical expression with a comparison operator (`>`, `<`, `==`, or
    `!=`)
2.  a regular expression for filtering via string matching

Let’s create a simple graph with four nodes that have numeric `data`
values. Then, we’ll inspect the graph’s internal NDF to see where we are
starting from.

``` r
# Create a node data frame
ndf <-
  create_node_df(
    n = 4,
    data = c(
      9.7, 8.5, 2.2, 6.0)
  )

# Create a new graph based on the ndf
graph <- create_graph(nodes_df = ndf)

# Inspect the graph's NDF
graph %>% get_node_df()
#>   id type label data
#> 1  1 <NA>  <NA>  9.7
#> 2  2 <NA>  <NA>  8.5
#> 3  3 <NA>  <NA>  2.2
#> 4  4 <NA>  <NA>  6.0
```

In the first example, nodes will be selected based on a logical
expression operating on a collection of numeric values. To examine the
result of the selection, the
[`get_node_df_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/get_node_df_ws.md)
will be used. This function will display a table for the active
selection of nodes in the graph (like
[`get_node_df()`](https://rich-iannone.github.io/DiagrammeR/reference/get_node_df.md),
but with a subset of nodes).

``` r
# Select nodes where the `data` attribute
# has a value greater than 7.0 (it's the
# first 2 nodes)
graph <-
  graph %>%
  select_nodes(
    conditions = data > 7.0
  )

# Get the graph's current selection
# of nodes as a table
graph %>% get_node_df_ws()
#>   id type label data
#> 1  1 <NA>  <NA>  9.7
#> 2  2 <NA>  <NA>  8.5
```

A selection of nodes can be obtained through a match using a regular
expression operating on a collection of character-based values (the node
attribute that is named `fruits`). This uses the
[`grepl()`](https://rdrr.io/r/base/grep.html) function in the expression
supplied to `conditions`. The regular expression to use is `^ap`, where
the `^` denotes the beginning of the text to the parsed.

``` r
# Create a node data frame
ndf <-
  create_node_df(
    n = 4,
    fruits = c(
      "apples", "apricots",
      "bananas", "plums")
  )

# Create a new graph based on the ndf
graph <- create_graph(nodes_df = ndf)

# Select nodes where the `fruits`
# attribute has a match on the first
# letters being `ap` (the first 2 nodes)
graph <-
  graph %>%
  select_nodes(
    conditions = grepl("^ap", fruits)
  )

# Get the graph's current selection
# of nodes as a table
graph %>% get_node_df_ws()
#>   id type label   fruits
#> 1  1 <NA>  <NA>   apples
#> 2  2 <NA>  <NA> apricots
```

The situation may arise when a more specialized match needs to be made
(i.e., matching this but not that, or, matching two different types of
things). This is where the `set_op` argument can become useful. When a
selection of nodes is obtained using
[`select_nodes()`](https://rich-iannone.github.io/DiagrammeR/reference/select_nodes.md)
(or any of the other `select_*()` functions that operate on nodes), the
selection is stored in the graph object. This is seen in the above
examples where
[`get_selection()`](https://rich-iannone.github.io/DiagrammeR/reference/get_selection.md)
was used to verify which nodes were in the selection. Because the
selection is retained (at least until
[`clear_selection()`](https://rich-iannone.github.io/DiagrammeR/reference/clear_selection.md)
is called, or, a selection of edges is made), multiple uses
[`select_nodes()`](https://rich-iannone.github.io/DiagrammeR/reference/select_nodes.md)
can modify the set of selected nodes depending on the option provided in
the `set_op` argument. These set operations are:

- `union`: creates a union of selected nodes in consecutive operations
  that create a selection of nodes (this is the default option)
- `intersect`: modifies the list of selected nodes such that only those
  nodes common to both consecutive node selection operations will
  retained
- `difference`: modifies the list of selected nodes such that the only
  nodes retained are those that are different in the second node
  selection operation compared to the first

These set operations behave exactly as the base R functions:
[`union()`](https://rdrr.io/r/base/sets.html),
[`intersect()`](https://rdrr.io/r/base/sets.html)/`intersection()`, and
[`setdiff()`](https://rdrr.io/r/base/sets.html) (which are actually used
internally). Furthermore, most of the `select_*()` functions contain the
`set_op` argument, so, they behave the same way with regard to modifying
the node or edge selection in a pipeline of selection operations. As
examples are important in fully understanding how these can work for
more complex selections, quite a few will be provided here.

The example graph will now be a bit more complex. It will contain 9
nodes, of three different `type`s (`fruit`, `veg`, and `nut`). The
`label` node attribute has the name of the food, and the `count`
attribute contains arbitrary numeric values.

``` r
# Create a node data frame
ndf <-
  create_node_df(
    n = 9,
    type = c(
      "fruit", "fruit", "fruit",
      "veg", "veg", "veg",
      "nut", "nut", "nut"),
    label = c(
      "pineapple", "apple", "apricot",
      "cucumber", "celery", "endive",
      "hazelnut", "almond", "chestnut"),
    count = c(
      6, 3, 8, 7, 2, 6, 9, 9, 7)
  )

# Create a new graph based on the ndf
graph <- create_graph(nodes_df = ndf)

# Inspect the graph's NDF
graph %>% get_node_df()
#>   id  type     label count
#> 1  1 fruit pineapple     6
#> 2  2 fruit     apple     3
#> 3  3 fruit   apricot     8
#> 4  4   veg  cucumber     7
#> 5  5   veg    celery     2
#> 6  6   veg    endive     6
#> 7  7   nut  hazelnut     9
#> 8  8   nut    almond     9
#> 9  9   nut  chestnut     7
```

Let’s successively use two
[`select_nodes()`](https://rich-iannone.github.io/DiagrammeR/reference/select_nodes.md)
calls to select all those foods that either begin with `c` or end with
`e`. Because this is an *OR* set, we will use the `union` set operation
in the second call of
[`select_nodes()`](https://rich-iannone.github.io/DiagrammeR/reference/select_nodes.md).

``` r
# Select all foods that either begin
# with `c` or ending with `e`
graph_1 <-
  graph %>%
  select_nodes(
    conditions = grepl("^c", label)
  ) %>%
  select_nodes(
    conditions = grepl("e$", label),
    set_op = "union"
  )

# Get the graph's current selection
# of nodes as a table
graph_1 %>% get_node_df_ws()
#>   id  type     label count
#> 1  1 fruit pineapple     6
#> 2  2 fruit     apple     3
#> 3  4   veg  cucumber     7
#> 4  5   veg    celery     2
#> 5  6   veg    endive     6
#> 6  9   nut  chestnut     7
```

The `conditions` don’t need to be related. In the first below, it’s a
matching expression using [`grepl()`](https://rdrr.io/r/base/grep.html).
The second is filtering to those nodes with `count < 5`.

``` r
# Select any food beginning with `a` and
# having a count less than 5
graph_2 <-
  graph %>%
  select_nodes(
    conditions = grepl("^a", label)
  ) %>%
  select_nodes(
    conditions = count < 5,
    set_op = "intersect"
  )

# Get the graph's current selection
# of nodes as a table
graph_2 %>% get_node_df_ws()
#>   id  type label count
#> 1  2 fruit apple     3
```

The following example contains a pair of
[`select_nodes()`](https://rich-iannone.github.io/DiagrammeR/reference/select_nodes.md)
statements, where the first filters nodes to a `fruit` group
(`type == "fruit"`) and then excludes a subset of these nodes (any fruit
containing “apple” in the name) using the `set_op = "difference"`.

``` r
# Select any fruit not containing
# `apple` in its name
graph_3 <-
  graph %>%
  select_nodes(
    conditions = type == "fruit"
  ) %>%
  select_nodes(
    conditions = grepl("apple", label),
    set_op = "difference"
  )

# Get the graph's current selection
# of nodes as a table
graph_3 %>% get_node_df_ws()
#>   id  type   label count
#> 1  3 fruit apricot     8
```

There is an additional filtering option available as the `nodes`
argument. Here, a vector of node ID values can be supplied and this will
indicate to the function that only that subset of nodes will be
considered for
[`select_nodes()`](https://rich-iannone.github.io/DiagrammeR/reference/select_nodes.md).
Note that, if nothing is provided in `conditions` and `nodes` is given a
vector of node ID values, it will be those very nodes that will make up
the selection in this function call. While this is convenient and often
a good method for selecting nodes (so long as one knows which node IDs
need to be selected), the function
[`select_nodes_by_id()`](https://rich-iannone.github.io/DiagrammeR/reference/select_nodes_by_id.md)
handles this use case more directly (as it only filters based on its
`nodes` argument).

``` r
# Create a node data frame
ndf <-
  create_node_df(
    n = 10,
    data = seq(0.5, 5, 0.5)
  )

# Create a new graph based on the ndf
graph <- create_graph(nodes_df = ndf)

# Inspect the graph's NDF
graph %>% get_node_df()
#>    id type label data
#> 1   1 <NA>  <NA>  0.5
#> 2   2 <NA>  <NA>  1.0
#> 3   3 <NA>  <NA>  1.5
#> 4   4 <NA>  <NA>  2.0
#> 5   5 <NA>  <NA>  2.5
#> 6   6 <NA>  <NA>  3.0
#> 7   7 <NA>  <NA>  3.5
#> 8   8 <NA>  <NA>  4.0
#> 9   9 <NA>  <NA>  4.5
#> 10 10 <NA>  <NA>  5.0
```

Let’s now perform a call to
[`select_nodes()`](https://rich-iannone.github.io/DiagrammeR/reference/select_nodes.md)
that contains both an expression for the `conditions` argument, and, a
range of node ID values in the `nodes` argument.

``` r
# Select from a subset of nodes
# (given as `nodes = 1:6`) where
# the `data` value is greater than `1.5`
graph <-
  graph %>%
  select_nodes(
    conditions = data > 1.5,
    nodes = 1:6
  )

# Get the graph's current selection
# of nodes as a table
graph %>% get_node_df_ws()
#>   id type label data
#> 1  4 <NA>  <NA>  2.0
#> 2  5 <NA>  <NA>  2.5
#> 3  6 <NA>  <NA>  3.0
```

## Creating an Edge Selection

Selecting edges in a graph is done in a manner quite similar to
selecting nodes. The primary means for targeting a specific edges is
through any available edge attributes (e.g., `rel`, styling attributes
such as `color`, or arbitrary data values available for edges).

We will start by creating a graph with four nodes and four edges. The
edges will have an edge attribute called `data` that contains numeric
values.

``` r
# Create a node data frame
ndf <- create_node_df(n = 4)

# Create an edge data frame
edf <-
  create_edge_df(
    from = c(1, 2, 3, 4),
      to = c(2, 3, 4, 1),
    data = c(
      8.6, 2.8, 6.3, 4.5)
  )

# Create a new graph from
# the NDF and EDF
graph <-
  create_graph(
    nodes_df = ndf,
    edges_df = edf
  )

# Inspect the graph's EDF
graph %>% get_edge_df()
#>   id from to  rel data
#> 1  1    1  2 <NA>  8.6
#> 2  2    2  3 <NA>  2.8
#> 3  3    3  4 <NA>  6.3
#> 4  4    4  1 <NA>  4.5
```

The following example shows how to create selections of edges based on a
logical expression operating on a collection of numeric values in the
`data` attribute.

``` r
# Select edges where the `data`
# attribute has a value
# greater than 5.0
graph <-
  graph %>%
  select_edges(
    conditions = data > 5.0
  )

# Get the graph's current selection
# of edges as a table
graph %>% get_edge_df_ws()
#>   id from to  rel data
#> 1  1    1  2 <NA>  8.6
#> 2  3    3  4 <NA>  6.3
```

## Selecting the Last Node or Edge in an NDF or EDF

You can select the last node or edge from the graph’s internal node data
frame (NDF) or internal edge data frame (EDF), respectively. Usually,
this will be the last node or edge created since new nodes or edges are
added to the bottom of the data frame and there is no shuffling of these
positions. Immediately after creating a single node or edge, calling
either the `select_last_node()` or the `select_last_edge()` functions
will result in a selection of the last node or edge created.

For both functions, `graph` is the only argument. If we provide a graph
object to
[`select_last_nodes_created()`](https://rich-iannone.github.io/DiagrammeR/reference/select_last_nodes_created.md)
we will get an active selection of nodes (the last nodes that were added
to the graph object). Likewise, the
[`select_last_edges_created()`](https://rich-iannone.github.io/DiagrammeR/reference/select_last_edges_created.md)
will create an active selection of edges, whichever edges were last
defined in a function call.

To begin, we’ll use the same graph as before but we’ll clear any active
selection using the
[`clear_selection()`](https://rich-iannone.github.io/DiagrammeR/reference/clear_selection.md)
function. To ensure that there is no active selection of nodes or edges,
we can use the
[`get_selection()`](https://rich-iannone.github.io/DiagrammeR/reference/get_selection.md)
function. A value of `NA` means that there is neither a selection of
nodes nor edges.

``` r
# Clear the graph's selection
graph <-
  graph %>%
  clear_selection()

# Check whether there is still
# a selection present
graph %>% get_selection()
#> [1] NA
```

If we use `select_last_nodes_created` on this graph, we find that the
active selection of nodes contains all the nodes in the graph, as they
were all produced in the
[`create_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/create_graph.md)
function (and no nodes were added in subsequent operations).

``` r
# Select the last node in the graph's NDF and confirm
# that the selection was made
graph %>%
  select_last_nodes_created() %>%
  get_node_df_ws()
#>   id type label
#> 1  1 <NA>  <NA>
#> 2  2 <NA>  <NA>
#> 3  3 <NA>  <NA>
#> 4  4 <NA>  <NA>
```

The same goes for the edges. Using
[`select_last_edges_created()`](https://rich-iannone.github.io/DiagrammeR/reference/select_last_edges_created.md)
will make a selection of edges that includes all edges in the graph.

``` r
# Select the last edge in the graph's EDF and confirm
# that the selection was made
graph %>%
  select_last_edges_created() %>%
  get_edge_df_ws()
#>   id from to  rel data
#> 1  1    1  2 <NA>  8.6
#> 2  2    2  3 <NA>  2.8
#> 3  3    3  4 <NA>  6.3
#> 4  4    4  1 <NA>  4.5
```

Where these functions become useful is during the building up of a graph
in multiple steps. In this next example, we will start with an empty
graph (using
[`create_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/create_graph.md)
by itself) then add a single node with
[`add_node()`](https://rich-iannone.github.io/DiagrammeR/reference/add_node.md),
and then select that very node with
[`select_last_nodes_created()`](https://rich-iannone.github.io/DiagrammeR/reference/select_last_nodes_created.md).
This selection-in-a-pipeline approach makes it easy to set node
attributes because we can use the
[`set_node_attrs_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/set_node_attrs_ws.md)
function. In this example, we’re adding a new `timestamp` attribute and
assigning a value (the system time). Because the selection is
persistent, we can set more attributes in further calls to
[`set_node_attrs_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/set_node_attrs_ws.md).
When we are done with this node selection, we call the
[`clear_selection()`](https://rich-iannone.github.io/DiagrammeR/reference/clear_selection.md)
function and return to a clean slate of no active selection present (it
removes any node or edge selection from the graph object). The example
proceeds to create another new node and adding attributes, and then a
new edge and edge attributes (with
[`set_edge_attrs_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/set_edge_attrs_ws.md)).
This is all facilitated with the
[`select_last_nodes_created()`](https://rich-iannone.github.io/DiagrammeR/reference/select_last_nodes_created.md)
and
[`select_last_edges_created()`](https://rich-iannone.github.io/DiagrammeR/reference/select_last_edges_created.md)
functions.

``` r
# Create a graph, node-by-node and
# edge-by-edge and add attributes
graph_2 <-
  create_graph() %>%
  add_node() %>%
  select_last_nodes_created() %>%
  set_node_attrs_ws(
    node_attr = "timestamp",
    value = as.character(Sys.time())
  ) %>%
  set_node_attrs_ws(
    node_attr = "type",
    value = "A"
  ) %>%
  clear_selection() %>%
  add_node() %>%
  select_last_nodes_created() %>%
  set_node_attrs_ws(
    node_attr = "timestamp",
    value = as.character(Sys.time())
  ) %>%
  set_node_attrs_ws(
    node_attr = "type",
    value = "B"
  ) %>%
  add_edge(
    from = 1,
    to = 2,
    rel = "AB"
  ) %>%
  select_last_edges_created() %>%
  set_edge_attrs_ws(
    edge_attr = "timestamp",
    value = as.character(Sys.time())
  ) %>%
  clear_selection()

# View the new graph
graph_2 %>% render_graph()
```

The graph shows two nodes connected together. Nothing more, nothing
less. The more interesting views of the data are of the node and edge
data frames, which now have several attributes set. Let’s have a look at
the graph’s internal node data frame…

``` r
# Inspect the new graph's NDF
graph_2 %>% get_node_df()
#>   id type label                  timestamp
#> 1  1    A  <NA> 2026-04-27 20:43:45.663804
#> 2  2    B  <NA> 2026-04-27 20:43:45.683663
```

…and let’s inspect the graph’s internal edge data frame.

``` r
# Inspect the new graph's EDF
graph_2 %>% get_edge_df()
#>   id from to rel                  timestamp
#> 1  1    1  2  AB 2026-04-27 20:43:45.701755
```

As can be seen, immediately invoking
[`select_last_nodes_created()`](https://rich-iannone.github.io/DiagrammeR/reference/select_last_nodes_created.md)
or
[`select_last_edges_created()`](https://rich-iannone.github.io/DiagrammeR/reference/select_last_edges_created.md)
after addition of new nodes or edges can be useful for working with the
newly made nodes/edges. Many functions ending with `_ws()` operate
specifically on selections of nodes or edges.
