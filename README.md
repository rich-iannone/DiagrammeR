<img src="inst/img/DiagrammeR.png">

[![Travis-CI Build Status](https://travis-ci.org/rich-iannone/DiagrammeR.svg?branch=master)](https://travis-ci.org/rich-iannone/DiagrammeR)
![](http://cranlogs.r-pkg.org/badges/grand-total/DiagrammeR?color=brightgreen)
[![codecov.io](https://codecov.io/github/rich-iannone/DiagrammeR/coverage.svg?branch=master)](https://codecov.io/github/rich-iannone/DiagrammeR?branch=master) 

With the **DiagrammeR** package you can create, modify, analyze, and visualize network graph diagrams. A collection of functions are available for working specifically with graph objects. The output can be viewed in the **RStudio** Viewer, incorporated in **RMarkdown**, integrated into **Shiny** web apps, converted into other graph formats, or exported as image, PDF, or SVG files.

<img src="inst/img/simple_graph.png">

It's possible to make the above graph diagram using a combination of **DiagrammeR** functions strung together with the **magrittr** `%>%` pipe:

```r
library(DiagrammeR)

create_random_graph(140, 100, set_seed = 23) %>%
  join_node_attrs(get_w_connected_cmpts(.)) %>%
  select_nodes_by_id(get_articulation_points(.)) %>%
  set_node_attrs_ws("peripheries", 2) %>%
  set_node_attrs_ws("width", 0.65) %>%
  set_node_attrs_ws("height", 0.65) %>%
  set_node_attrs_ws("penwidth", 3) %>%
  clear_selection() %>%
  add_global_graph_attrs(
    attr =
      c("color",  "penwidth", "width", "height"),
    value =
      c("gray80", "3",        "0.5",   "0.5"),
    attr_type =
      c("edge",   "edge",     "node",  "node")) %>%
  colorize_node_attrs(
    node_attr_from = "wc_component",
    node_attr_to = "fillcolor",
    alpha = 80) %>%
  set_node_attr_to_display() %>%
  select_nodes_by_degree("deg >= 3") %>%
  trav_both_edge() %>%
  set_edge_attrs_ws("penwidth", 4) %>%
  set_edge_attrs_ws("color", "gray60") %>%
  clear_selection() %>%
  render_graph()
```

**DiagrammeR**'s graph functions allow you to create graph objects, modify those graphs, get information from the graphs, create a series of graphs, perform scaling of attribute values with data values, and many other useful things.

This functionality makes it possible to generate a network graph with data available in tabular datasets. Two specialized data frames contain node data and attributes (node data frames) and edges with associated edge attributes (edge data frames). Because the attributes are always kept alongside the node and edge definitions (within the graph object itself), we can easily work with them and specify styling attributes to differentiate nodes and edges by size, color, shape, opacity, length, and more. Here are some of the available graph functions:

<img src="inst/img/graph_functions_1.png">
<img src="inst/img/graph_functions_2.png">
<img src="inst/img/graph_functions_3.png">
<img src="inst/img/graph_functions_4.png">
<img src="inst/img/graph_functions_5.png">
<img src="inst/img/graph_functions_6.png">
<img src="inst/img/graph_functions_7.png">
<img src="inst/img/graph_functions_8.png">
<img src="inst/img/graph_functions_9.png">

## Network Graph Example

Let's create a property graph by combining CSV data that pertains to contributors to three software projects. The CSV files (`contributors.csv`, `projects.csv`, and `projects_and_contributors.csv`) are available in the **DiagrammeR** package. Together they provide the properties `name`, `age`, `join_date`,  `email`, `follower_count`, `following_count`, and `starred_count` to the `person` nodes; `project`, `start_date`, `stars`, and `language` to the `project` nodes; and the `contributor_role` and `commits` properties to the edges.

```r
library(DiagrammeR)

# Create the main graph
graph <-
  create_graph() %>%
  set_graph_name("software_projects") %>%
  add_nodes_from_table(
    system.file(
      "extdata", "contributors.csv",
      package = "DiagrammeR"),
    set_type = "person",
    label_col = "name") %>%
  add_nodes_from_table(
    system.file(
      "extdata", "projects.csv",
      package = "DiagrammeR"),
    set_type = "project",
    label_col = "project") %>%
  add_edges_from_table(
    system.file(
      "extdata", "projects_and_contributors.csv",
      package = "DiagrammeR"),
    from_col = "contributor_name",
    to_col = "project_name",
    ndf_mapping = "label",
    rel_col = "contributor_role")
```

We can always view the property graph with the `render_graph()` function.

```r
render_graph(graph, output = "visNetwork")
```

<img src="inst/img/graph_example_1.png">

Now that the graph is set up, you can create queries with **magrittr** pipelines to get specific answers from the graph.

Get the average age of all the contributors. Select all nodes of type `person` (not `project`). Each node of that type has non-`NA` `age` attribute, so, cache that attribute with `cache_node_attrs_ws()` (this function caches a vector of node attribute values in the graph). Get the cache straight away and get its mean (with `get_cache()` and then `mean()`).

```r
graph %>% 
  select_nodes("type == 'person'") %>%
  cache_node_attrs_ws("age", "numeric") %>%
  get_cache() %>% 
  mean()
#> [1] 33.6
```

We can get the total number of commits to all projects. We know that all edges contain the numerical `commits` attribute, so, select all edges (`select_edges()` by itself selects all edges in the graph) and cache the `commits` values as a numeric vector with `cache_edge_attrs_ws()` (this is stored in the graph object itself). Immediately extract the cached vector with `get_cache()` and get its `sum()` (all commits to all projects).

```r
graph %>% 
  select_edges() %>%
  cache_edge_attrs_ws("commits", "numeric") %>%
  get_cache() %>%
  sum()
#> [1] 5182
```

Single out the one known as Josh and get his total number of commits as a maintainer and as a contributor. Start by selecting the Josh node with `select_nodes("name == 'Josh'")`. In this graph, we know that all people have an edge to a project and that edge can be of the relationship (`rel`) type of `contributor` or `maintainer`. We can migrate our selection from nodes to outbound edges with `trav_out_edges()` (and we won't provide a condition, just all the outgoing edges from Josh will be selected). Now we have a selection of 2 edges. Get a vector of `commits` values as a stored, numeric vector with `cache_edge_attrs_ws()`. Immediately, extract it from the graph with `get_cache()` and get the `sum()`. This is the total number of commits.

```r
graph %>% 
  select_nodes("name == 'Josh'") %>%
  trav_out_edge() %>%
  cache_edge_attrs_ws("commits", "numeric") %>%
  get_cache() %>% 
  sum()
#> [1] 227
```

Get the total number of commits from Louisa, just from the maintainer role though. In this case we'll supply a condition in `trav_out_edge()`. This acts as a filter for the traversal, nullifying the selection to those edges where the condition is not met. Although there is only a single value in the cache, we'll still use `sum()` after `get_cache()` (a good practice because we may not know the vector length, especially in big graphs).

```r
graph %>% 
  select_nodes("name == 'Louisa'") %>%
  trav_out_edge("rel == 'maintainer'") %>%
  cache_edge_attrs_ws("commits", "numeric") %>%
  get_cache() %>% 
  sum()
#> [1] 236
```

How do we do something more complex, like, get the names of people in graph above age 32? First select all `person` nodes with `select_nodes("type == 'person'")`. Then, follow up with another `select_nodes()` call specifying `age > 32`, and, importantly using `set_op = "intersect"` (giving us the intersection of both selections). Now we have the selection of nodes we want; get all values of these nodes' `name` attribute as a cached character vector with the `cache_node_attrs_ws()` function. Get that cache and sort the names alphabetically with the **R** function `sort()`.

```r
graph %>% 
  select_nodes("type == 'person'") %>%
  select_nodes("age > 32", set_op = "intersect") %>%
  cache_node_attrs_ws("name", "character") %>%
  get_cache() %>%
  sort()
#> [1] "Jack"   "Jon"    "Kim"    "Roger"  "Sheryl"
```

Another way to express the same selection of nodes is to use the `mk_cond()` (i.e., 'make condition') helper function to compose the selection conditions. It uses sets of 3 elements for each condition: (1) the node or edge attribute name (character value), (2) the conditional operator (character value), and (3) the non-attribute operand. A linking `&` or `|` between groups is used to specify `AND`s or `OR`s. The `mk_cond()` helper is also useful for supplying variables to a condition for a number of `select_...()` and all `trav_...()` functions.

```r
graph %>% 
  select_nodes(
    mk_cond(
      "type", "==", "person",
      "&",
      "age",  ">",  32)) %>%
  cache_node_attrs_ws("name", "character") %>%
  get_cache() %>%
  sort()
#> [1] "Jack"   "Jon"    "Kim"    "Roger"  "Sheryl"
```

That **supercalc** is progressing quite nicely. Let's get the total number of commits from all people to that most interesting project. Start by selecting that project's node and work backwards! Traverse to the edges leading to it with `trav_in_edge()`. Those edges are from committers and they all contain the `commits` attribute with numerical values. Cache those values, get the cache straight away, take the sum -> 1676 commits.
```r
graph %>% 
  select_nodes("project == 'supercalc'") %>%
  trav_in_edge() %>%
  cache_edge_attrs_ws("commits", "numeric") %>%
  get_cache() %>% 
  sum()
#> [1] 1676
```

How would we find out who committed the most to the **supercalc** project? This is an extension of the previous problem and there are actually a few ways to do this. We start the same way (at the project node, using `select_nodes()`), then:

- traverse to the inward edges [`trav_in_edge()`]
- cache the `commits` values found in these selected edges [`cache_edge_attrs_ws()`]
- this is the complicated part but it's good: (1) use `select_edges()`; (2) compose the edge selection condition with the `mk_cond()` helper, where the edge has a `commits` value equal to the largest value in the cache; (3) use the `intersect` set operation to restrict the selection to those edges already selected
- get a new cache of `commits` values (should only be a single value in this case)
- we want the person responsible for these commits; traverse to that node from the edge selection [`trav_out_node()`]
- cache the `name` values found in these selected nodes [`cache_node_attrs_ws()`]
- get the cache [`get_cache()`]

```r
graph %>% 
  select_nodes("project == 'supercalc'") %>%
  trav_in_edge() %>%
  cache_edge_attrs_ws("commits", "numeric") %>%
  select_edges(mk_cond("commits", "==", get_cache(.) %>% max()), "intersect") %>%
  cache_edge_attrs_ws("commits", "numeric") %>%
  trav_out_node() %>%
  cache_node_attrs_ws("name") %>%
  get_cache()
#> [1] "Sheryl"
```

What is the email address of the individual that contributed the least to the **randomizer** project? (We shall try to urge that person to do more.)

```r
graph %>% 
  select_nodes("project == 'randomizer'") %>%
  trav_in_edge() %>%
  cache_edge_attrs_ws("commits", "numeric") %>%
  trav_in_node() %>%
  trav_in_edge(mk_cond("commits", "==", get_cache(.) %>% min())) %>%
  trav_out_node() %>%
  cache_node_attrs_ws("email") %>%
  get_cache()
#> [1] "the_will@graphymail.com"
```

Kim is now a contributor to the **stringbuildeR** project and has made 15 new commits to that project. We can modify the graph to reflect this. First, add an edge with `add_edge()`. Note that `add_edge()` usually relies on node IDs in `from` and `to` when creating the new edge. This is almost always inconvenient so we can instead use node labels (ensure they are unique!) to compose the edge, setting `use_labels = TRUE`. The `rel` value in `add_edge()` was set to `contributor` -- in a property graph we should try to always have values set for all node `type` and edge `rel` attributes. We will set another attribute for this edge (`commits`) by first selecting the edge (it was the last edge made: use `select_last_edge()`), then, use `set_edge_attrs_ws()` and provide the attribute/value pair. Finally, deselect all selections with `clear_selection()`. The graph is now changed, have a look.

```r
graph <- 
  graph %>%
  add_edge(
    from = "Kim",
    to = "stringbuildeR",
    rel = "contributor",
    use_labels = TRUE) %>%
  select_last_edge() %>%
  set_edge_attrs_ws("commits", 15) %>%
  clear_selection()

render_graph(graph, output = "visNetwork")
```

<img src="inst/img/graph_example_2.png">

Get all email addresses for contributors (but not maintainers) of the **randomizer** and **supercalc88** projects. Multiple `select_nodes()` calls in succession is an `OR` selection of nodes (`project` nodes selected can be `randomizer` or `supercalc`). With `trav_in_edge()` we just want the `contributer` edges/commits. Once on those edges, hop back unconditionally to the people from which the edges originate with `trav_out_node()`. Get the `email` values from those selected individuals as a sorted character vector. 

```r
graph %>% 
  select_nodes("project == 'randomizer'") %>%
  select_nodes("project == 'supercalc'") %>%
  trav_in_edge("rel == 'contributor'") %>%
  trav_out_node() %>%
  cache_node_attrs_ws("email", "character") %>%
  get_cache() %>% 
  sort()
#> [1] "j_2000@ultramail.io"      "josh_ch@megamail.kn"     
#> [3] "kim_3251323@ohhh.ai"      "lhe99@mailing-fun.com"   
#> [5] "roger_that@whalemail.net" "the_simone@a-q-w-o.net"  
#> [7] "the_will@graphymail.com" 
```

Which people have committed to more than one project? This is a matter of node degree. We know that people have edges outward and projects and edges inward. Thus, anybody having an outdegree (number of edges outward) greater than `1` has committed to more than one project. Globally, select nodes with that condition using `select_nodes_by_degree("outdeg > 1")`. Once getting the `name` attribute values from that node selection, we can provide a sorted character vector of names.
```r
graph %>%
  select_nodes_by_degree("outdeg > 1") %>%
  cache_node_attrs_ws("name") %>%
  get_cache() %>% 
  sort()
#> [1] "Josh"   "Kim"    "Louisa"
```

## Installation

**DiagrammeR** is used in an **R** environment. If you don't have an **R** installation, it can be obtained from the [**Comprehensive R Archive Network (CRAN)**](https://cran.r-project.org/). It is recommended that [**RStudio**](http://www.rstudio.com/products/RStudio/) be used as the **R** IDE to take advantage of its rendering capabilities and the code-coloring support for **Graphviz** and **mermaid** diagrams.

You can install the development (v0.9.0) version of **DiagrammeR** from **GitHub** using the **devtools** package.

```r
devtools::install_github('rich-iannone/DiagrammeR')
```

Or, get the v0.8.4 release from **CRAN**.

```r
install.packages('DiagrammeR')
```
