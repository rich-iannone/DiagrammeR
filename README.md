<img src="inst/img/DiagrammeR.png">

[![Travis-CI Build Status](https://travis-ci.org/rich-iannone/DiagrammeR.svg?branch=master)](https://travis-ci.org/rich-iannone/DiagrammeR)
![](http://cranlogs.r-pkg.org/badges/grand-total/DiagrammeR?color=brightgreen)
[![codecov.io](https://codecov.io/github/rich-iannone/DiagrammeR/coverage.svg?branch=master)](https://codecov.io/github/rich-iannone/DiagrammeR?branch=master) 

With the **DiagrammeR** package you can create, modify, analyze, and visualize network graph diagrams. The output can be incorporated into **RMarkdown** documents, integrated with **Shiny** web apps, converted to other graph formats, or exported as **PNG**, **PDF**, or **SVG** files.

<img src="inst/img/simple_graph.png">

It's possible to make the above graph diagram using a combination of **DiagrammeR** functions strung together with the **magrittr** `%>%` pipe:

```r
library(DiagrammeR)

example_graph <-
  create_graph(
    directed = TRUE) %>%
  add_pa_graph(
    n = 50,
    m = 1,
    set_seed = 23) %>%
  add_gnp_graph(
    n = 50,
    p = 1/100,
    set_seed = 23) %>%
  join_node_attrs(
    df = get_betweenness(.)) %>%
  join_node_attrs(
    df = get_degree_total(.)) %>%
  colorize_node_attrs(
    node_attr_from = total_degree,
    node_attr_to = fillcolor,
    palette = "Greens",
    alpha = 90) %>%
  rescale_node_attrs(
    node_attr_from = betweenness,
    to_lower_bound = 0.5,
    to_upper_bound = 1.0,
      node_attr_to = height) %>%
  select_nodes_by_id(
    nodes = get_articulation_points(.)) %>%
  set_node_attrs_ws(
    node_attr = peripheries,
    value = 2) %>%
  set_node_attrs_ws(
    node_attr = penwidth,
    value = 3) %>%
  clear_selection() %>%
  set_node_attr_to_display(
    attr = NULL)
    
render_graph(
  graph = example_graph,
  layout = "nicely")
```

**DiagrammeR**'s graph functions allow you to create graph objects, modify those graphs, get information from the graphs, create a series of graphs, and do many other useful things.

This functionality makes it possible to generate a network graph with data available in tabular datasets. Two specialized data frames contain node data and attributes (node data frames) and edges with associated edge attributes (edge data frames). Because the attributes are always kept alongside the node and edge definitions (within the graph object itself), we can easily work with them.

## Graph Basics

Let's create a graph object with `create_graph()` and add some nodes and edges to it. Each node gets a new integer ID upon creation. Each edge also gets an ID starting from 1. The pipes between functions make the whole process readable and understandable.

```r
a_graph <-
  create_graph() %>%
  add_node() %>%
  add_node() %>%
  add_edge(
    from = 1,
    to = 2)
```

We can take away an edge by using `delete_edge()`.

```r
b_graph <-
  a_graph %>%
  delete_edge(
    from = 1,
    to = 2)
```

We can add a node to the graph while, at the same time, defining edges to or from existing nodes in the graph.

```r
c_graph <-
  b_graph %>%
  add_node(
    from = 1,
    to = 2)
```

Any time we add a node or edge to the graph, we can add node or edge attributes. These can be aesthetic properties (e.g., `color`, `shape`), grouping labels (e.g., `type` and `rel`), or data that is useful for calculations and for display purposes.

```r
d_graph <-
  c_graph %>%
  add_node(
    type = "type_a",
    color = "steelblue",
    value = 2.5) %>%
  add_edge(
    from = 1,
    to = 3,
    rel = "interacted_with",
    color = "gray65",
    value = 5.7)
```
    
Creating attributes and setting values for them is often useful because we can do further work with the attributes (e.g., mutate values or migrate them during traversals). Furthermore, we can create aesthetic properties based on numerical or categorical data.

Don’t worry if attribute values weren’t set during the creation of the associated nodes or edges. They are many functions available to select nodes or edges and work with their attributes (and often, this is the more efficient strategy as we can target nodes/edges based on their properties). Here is an example where we select a node based on its `value` attribute and modify its `color`:

```r
e_graph <-
  d_graph %>%
  select_nodes(
    conditions = value == 2.5) %>%
  set_node_attrs_ws(
    node_attr = color,
    value = "steelblue") %>%
  clear_selection()
```

We can create a graph object and add graph primitives such as paths, cycles, and trees to it. 

```r
f_graph <-
  create_graph() %>%
  add_path(n = 3) %>%
  add_cycle(n = 4) %>%
  add_balanced_tree(k = 2, h = 2)
```

You can add one or more randomly generated graphs to a graph object. Here, let's add a directed GNM graph with 10 nodes and 15 edges (the `set_seed` option makes the random graph reproducible). 

```r
g_graph <-
  create_graph() %>%
  add_gnm_graph(
    n = 10,
    m = 15,
    set_seed = 23)
```

The undirected version of this graph is can be made using:

```r
h_graph <-
  create_graph(
    directed = FALSE) %>%
  add_gnm_graph(
    n = 10,
    m = 15,
    set_seed = 23)
```

## Functions

There are lots of functions in the package. Below is a listing of all the functions available in the package, categorized by how the interact with the graph.

<img src="inst/img/functions.png">

## A Network Graph Example

Let's create a property graph that pertains to contributors to three software projects. This graph has nodes representing people and projects. The attributes `name`, `age`, `join_date`,  `email`, `follower_count`, `following_count`, and `starred_count` are specific to the `person` nodes while the `project`, `start_date`, `stars`, and `language` attributes apply to the `project` nodes. The edges represent the relationships between the people and the project.

The example graph file `repository.dgr` is available in the `extdata/example_graphs_dgr/` directory in the **DiagrammeR** package (currently, only for the **Github** version). We can load it into memory by using the `open_graph()` function, with `system.file()` to provide the location of the file within the package.

```r
library(DiagrammeR)

# Load in a the small repository graph
graph <-
  open_graph(
    system.file(
      "extdata/example_graphs_dgr/repository.dgr",
      package = "DiagrammeR"))
```

We can always view the property graph with the `render_graph()` function.

```r
render_graph(graph, layout = "kk")
```

<img src="inst/img/graph_example_1.png">

Now that the graph is set up, you can create queries with **magrittr** pipelines to get specific answers from the graph.

Get the average age of all the contributors. Select all nodes of type `person` (not `project`). Each node of that type has non-`NA` `age` attribute, so, get that attribute as a vector with `get_node_attrs_ws()` and then calculate the mean with **R**'s `mean()` function.

```r
graph %>% 
  select_nodes(
    conditions = type == "person") %>%
  get_node_attrs_ws(
    node_attr = age) %>%
  mean()
#> [1] 33.6
```

We can get the total number of commits to all projects. We know that all edges contain the numerical `commits` attribute, so, select all edges (`select_edges()` by itself selects all edges in the graph). After that, get a numeric vector of `commits` values and then get its `sum()` (all commits to all projects).

```r
graph %>% 
  select_edges() %>%
  get_edge_attrs_ws(
    edge_attr = commits) %>%
  sum()
#> [1] 5182
```

Single out the one known as Josh and get his total number of commits as a maintainer and as a contributor. Start by selecting the Josh node with `select_nodes(conditions = name == "Josh")`. In this graph, we know that all people have an edge to a project and that edge can be of the relationship (`rel`) type of `contributor` or `maintainer`. We can migrate our selection from nodes to outbound edges with `trav_out_edges()` (and we won't provide a condition, just all the outgoing edges from Josh will be selected). Now we have a selection of 2 edges. Get that vector of `commits` values with `get_edge_attrs_ws()` and then calculate the `sum()`. This is the total number of commits.

```r
graph %>% 
  select_nodes(
    conditions = name == "Josh") %>%
  trav_out_edge() %>%
  get_edge_attrs_ws(
    edge_attr = commits) %>%
  sum()
#> [1] 227
```

Get the total number of commits from Louisa, just from the maintainer role though. In this case we'll supply a condition in `trav_out_edge()`. This acts as a filter for the traversal and this means that the selection will be applied to only those edges where the condition is met. Although there is only a single value, we'll still use `sum()` after `get_edge_attrs_ws()` (a good practice because we may not know the vector length, especially in big graphs).

```r
graph %>% 
  select_nodes(
    conditions = name == "Louisa") %>%
  trav_out_edge(
    conditions = rel == "maintainer") %>%
  get_edge_attrs_ws(
    edge_attr = commits) %>%
  sum()
#> [1] 236
```

How do we do something more complex, like, get the names of people in graph above age 32? First, select all `person` nodes with `select_nodes(conditions = type == "person")`. Then, follow up with another `select_nodes()` call specifying `age > 32`. Importantly, have `set_op = "intersect"` (giving us the intersection of both selections).

Now that we have the starting selection of nodes we want, we need to get all values of these nodes' `name` attribute as a character vector. We do this with the `get_node_attrs_ws()` function. After getting that vector, sort the names alphabetically with the **R** function `sort()`. Because we get a named vector, we can use `unname()` to not show us the names of each vector component.

```r
graph %>% 
  select_nodes(
    conditions = type == "person") %>%
  select_nodes(
    conditions = age > 32,
    set_op = "intersect") %>%
  get_node_attrs_ws(
    node_attr = name) %>%
  sort() %>%
  unname()
#> [1] "Jack"   "Jon"    "Kim"    "Roger"  "Sheryl"
```

That **supercalc** project is progressing quite nicely. Let's get the total number of commits from all people to that most interesting project. Start by selecting that project's node and work backwards. Traverse to the edges leading to it with `trav_in_edge()`. Those edges are from committers and they all contain the `commits` attribute with numerical values. Get a vector of `commits` and then get the sum (there are `1676` commits).
```r
graph %>% 
  select_nodes(
    conditions = project == "supercalc") %>%
  trav_in_edge() %>%
  get_edge_attrs_ws(
    edge_attr = commits) %>%
  sum()
#> [1] 1676
```

Kim is now a contributor to the **stringbuildeR** project and has made 15 new commits to that project. We can modify the graph to reflect this.

First, add an edge with `add_edge()`. Note that `add_edge()` usually relies on node IDs in `from` and `to` when creating the new edge. This is almost always inconvenient so we can instead use node labels (we know they are unique in this graph) to compose the edge, setting `use_labels = TRUE`.

The `rel` value in `add_edge()` was set to `contributor` -- in a property graph we always have values set for all node `type` and edge `rel` attributes. We will set another attribute for this edge (`commits`) by first selecting the edge (it was the last edge made, so we can use `select_last_edges_created()`), then, use `set_edge_attrs_ws()` and provide the attribute/value pair. Finally, clear the active selections with `clear_selection()`. The graph is now changed, have a look.

```r
graph <- 
  graph %>%
  add_edge(
    from = "Kim",
    to = "stringbuildeR",
    rel = "contributor") %>%
  select_last_edges_created() %>%
  set_edge_attrs_ws(
    edge_attr = commits,
    value = 15) %>%
  clear_selection()

render_graph(graph, layout = "kk")
```

<img src="inst/img/graph_example_2.png">

Get all email addresses for contributors (but not maintainers) of the **randomizer** and **supercalc88** projects. With `trav_in_edge()` we just want the `contributer` edges/commits. Once on those edges, hop back unconditionally to the people from which the edges originate with `trav_out_node()`. Get the `email` values from those selected individuals as a sorted character vector. 

```r
graph %>% 
  select_nodes(
    conditions = 
      project == "randomizer" | 
      project == "supercalc") %>%
  trav_in_edge(
    conditions = rel == "contributor") %>%
  trav_out_node() %>%
  get_node_attrs_ws(
    node_attr = email) %>%
  sort() %>%
  unname()
#> [1] "j_2000@ultramail.io"      "josh_ch@megamail.kn"     
#> [3] "kim_3251323@ohhh.ai"      "lhe99@mailing-fun.com"   
#> [5] "roger_that@whalemail.net" "the_simone@a-q-w-o.net"  
#> [7] "the_will@graphymail.com" 
```

Which people have committed to more than one project? This is a matter of node degree. We know that people have edges outward and projects and edges inward. Thus, anybody having an outdegree (number of edges outward) greater than `1` has committed to more than one project. Globally, select nodes with that condition using `select_nodes_by_degree("outdeg > 1")`. Once getting the `name` attribute values from that node selection, we can provide a sorted character vector of names.

```r
graph %>%
  select_nodes_by_degree(
    expressions = "outdeg > 1") %>%
  get_node_attrs_ws(
    node_attr = name) %>%
  sort() %>%
  unname()
#> [1] "Josh"   "Kim"    "Louisa"
```

## Installation

**DiagrammeR** is used in an **R** environment. If you don't have an **R** installation, it can be obtained from the [**Comprehensive R Archive Network (CRAN)**](https://cran.r-project.org/).

You can install the development version of **DiagrammeR** from **GitHub** using the **devtools** package.

```r
devtools::install_github("rich-iannone/DiagrammeR")
```

Or, get it from **CRAN**.

```r
install.packages("DiagrammeR")
```

If you encounter a bug, have usage questions, or want to share ideas to make this package better, feel free to file an [issue](https://github.com/rich-iannone/DiagrammeR/issues).

## Code of Conduct

[Contributor Code of Conduct](https://github.com/rich-iannone/DiagrammeR/blob/master/CONDUCT.md). By participating in this project you agree to abide by its terms.

## License

MIT &copy; Richard Iannone
