HTMLWidgets.widget({

  name: 'vivagraph',

  type: 'output',

  initialize: function(el, width, height) {
        
    return { }

  },

  renderValue: function(el, x, instance) {
    
    // clean out our el in case we are in Dynamic (shiny) situation
    el.innerHTML = "" ;
    
    var positions = [];
    
//    try {
      // much of this code copied / adapted from VivaGraphJS example 4
      //   https://github.com/anvaka/VivaGraphJS/blob/master/demos/tutorial_svg/04%20-%20Listen%20To%20Mouse%20Events.html
      

      // Step 2. We add nodes and edges to the graph:
      if( !(x.network === null) ){
        
        // Step 1. create a graph variable
        var graph= Viva.Graph.graph(),
            layout = Viva.Graph.Layout[x.layout](graph);
            
        //  for defined positions if provided
        if( !( x.positions === null )  && x.layout === "constant" ){
          
          x.positions.x.map( function( pos, inc ){
            positions.push({
              x : +x.positions.x[inc],
              y : +x.positions.y[inc]
            })
          })

          layout.placeNode(function(node) {
            // node.id - points to its position but you can do your
            // random logic here. E.g. read from specific node.data
            // attributes. This callback is expected to return object {x : .. , y : .. }
            
            return positions[ x.network.nodes_df.id.indexOf( node.id ) ]
          })
        }
        
        //use HTMLWidgets.dataframeToD3 to get data as array of objects
        
        HTMLWidgets.dataframeToD3(x.network.nodes_df).map( function(node){
          graph.addNode(
            node.id,
            node
          )
        })
        
        HTMLWidgets.dataframeToD3(x.network.edges_df).map( function(edge){
          graph.addLink(
            edge.from,
            edge.to,
            edge
          )
        })
  
        // Step 3. Render the graph.
        
        // for now only use svgGraphics View
        //   but eventually expand to webgl and possibly other ngraph views
        var graphics = Viva.Graph.View.svgGraphics(),
            nodeSize = 24,
            // we use this method to highlight all realted links
            // when user hovers mouse over a node:
            highlightRelatedNodes = function(nodeId, isOn) {
               // just enumerate all realted nodes and update link color:
               graph.forEachLinkedNode(nodeId, function(node, link){
                   var linkUI = graphics.getLinkUI(link.id);
                   if (linkUI) {
                     // linkUI is a UI object created by graphics below
                     linkUI.attr('stroke', isOn ? 'red' : 'gray');
                   }
               });
            };
  
        // Since we are using SVG we can easily subscribe to any supported
        // events (http://www.w3.org/TR/SVG/interact.html#SVGEvents ),
        // including mouse events:
        graphics.node(function(node) {
          
            var ui = Viva.Graph.svg('g')
  
            var svgText = Viva.Graph.svg('text')
                            .attr('y', '-12px')
                            .attr('x', '-8px')
                            .text(node.id);
  
            var img = Viva.Graph.svg('circle')
                        .attr('r', 7)
                        .attr('stroke', '#fff')
                        .attr('stroke-width', '1.5px');
                        
            ui.append( svgText );
            ui.append( img );
  
            ui.onmouseover = function() { // mouse over
                highlightRelatedNodes(node.id, true)
            };
            
            ui.onmouseout = function() { // mouse out
                highlightRelatedNodes(node.id, false)
            };
            
            return ui;
        }).placeNode(
            function(nodeUI, pos) {
                nodeUI.attr('transform', 'translate(' + (pos.x - 0)
                        + ',' + (pos.y - 0) + ')');
            }
        );
        
        graphics.link(function(link){
            return Viva.Graph.svg('path')
                      .attr('stroke', 'gray');
        }).placeLink(function(linkUI, fromPos, toPos) {
            var data = 'M' + fromPos.x + ',' + fromPos.y +
                       'L' + toPos.x + ',' + toPos.y;
            linkUI.attr("d", data);
        })
  
        // Finally render the graph with our customized graphics object:
        var renderer = Viva.Graph.View.renderer(graph,
                          {
                            container: el,
                            layout: layout,
                            graphics : graphics
                          }
                        )  ;
        renderer.run();
    
  
        //try to handle Chrome "bug"
        // see issue https://github.com/anvaka/VivaGraphJS/issues/108
        // by explicitly setting svg height and width
        el.getElementsByTagName("svg")[0].style.width="100%"
        el.getElementsByTagName("svg")[0].style.height="100%"      
        
        // set up a container for tasks to perform after completion
        //  one example would be add callbacks for event handling
        //  styling
        if (!(typeof x.tasks === "undefined") ){
          if ( (typeof x.tasks.length === "undefined") ||
           (typeof x.tasks === "function" ) ) {
             // handle a function not enclosed in array
             // should be able to remove once using jsonlite
             x.tasks = [x.tasks];
          }
          x.tasks.map(function(t){
            // for each tasks add it to the mermaid.tasks with el
            t.call(el);
          })
        }        
      }        
    /*  }

      
    } catch(e) {
        // write the error in  el for debugging
        var p = document.createElement("pre")
        p.innerText = e;
        el.appendChild(p);
    }
    */
  },

  resize: function(el, width, height, instance) {
    
  }
  

});
