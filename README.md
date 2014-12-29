DiagrammeR
==========

Create diagrams using R, with a light interface to the mermaid.js library. This works by using text that is similar to markdown. By doing this in R, we can also add some R code into the mix.

### Installation

Install DiagrammeR from GitHub using the `devtools` package:

```R
require(devtools)
install_github('DiagrammeR', 'rich-iannone')
```

### Usage Examples

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

Alternatively, you could have the diagram flowing from top to bottom by using the statement `graph TB` in place of `graph LR`. Here is the result of that:


<img src="inst/Example_2.png">

Alright, here's another example. This one places some text inside the diagram objects. Also, there are some CSS styles to add a color fill to each of the diagram objects:

```R
diagram <- "
graph LR;
A(Rounded)-->B[Squared];
B-->C{A Decision};
C-->D[Square One];
C-->E[Square Two];
    
style A fill:#E5E25F;
style B fill:#87AB51;
style C fill:#3C8937;
style D fill:#23772C;
style E fill:#B6E6E6;
"
    
render_diagram(diagram)
```

What you get is this:

<img src="inst/Example_3.png">

