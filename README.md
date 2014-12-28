DiagrammeR
==========

Create diagrams using R, with a light interface to the mermaid.js library. This works by using text that is similar to markdown. By doing this in R, we can also add some R code into the mix.

### Installation

Install DiagrammeR from GitHub using the `devtools` package:

```R
require(devtools)
install_github('DiagrammeR', 'rich-iannone')
```

### A Usage example

The following code is provided to the object `diagram`. All lines are terminated with a semicolon. The line `graph LR;` indicates that a graph running left to right is desired. Nodes are arbitrarily named and arrows such as `-->` indicate the type of arrow connection. The `render_diagram` function currently takes a single argument, which is character object that contains the diagram code:

```R
diagram <- "
graph LR;
A-->B;
A-->C;
C-->E;
B-->D;
C-->D;
D-->F;
E-->F;
"

render_diagram(diagram)
```

This renders the following image:

<img src="inst/Example_1.png">

