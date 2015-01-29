HTMLWidgets.widget({

  name: 'DiagrammeR',

  type: 'output',

  initialize: function(el, width, height) {
    
    /* wait to initialize until renderValue
        since x not provided until then
        and mermaid will try to build the diagram
        as soon as class of the div is set to "mermaid"
    */
    
    /* to prevent auto init() by mermaid
        not documented but
        see lines https://github.com/knsv/mermaid/blob/master/src/main.js#L100-L109
          mermaid_config in global with mermaid_config.startOnLoad = false
        appears to turn off the auto init behavior
        allowing us to callback after manually init and then callback
        after complete
    */
   window.mermaid.startOnLoad = false;

    return {
      // TODO: add instance fields as required
    }

  },

  renderValue: function(el, x, instance) {
    
    // if no diagram provided then assume
    // that the diagrams are provided through htmltools tags
    // and DiagrammeR was just used for dependencies 
    if ( x.diagram != "" ) {
      el.innerHTML = x.diagram;
      //if dynamic such as shiny remove data-processed
      // so mermaid will reprocess and redraw
      el.removeAttribute("data-processed");
      el.classList.add('mermaid');
      //make sure if shiny that we turn display back on
      el.style.display = "";
      //again if dynamic such as shiny
      //  explicitly run mermaid.init()
    } else {
      // set display to none
      // should we remove instead??
      el.style.display = "none";
    }
    
    
    // use this to sort of make our diagram responsive
    //  or at a minimum fit within the bounds set by htmlwidgets
    //  for the parent container
    function makeResponsive(el){
       var svg = el.getElementsByTagName("svg")[0];
       if(svg){
        if(svg.width) {svg.removeAttribute("width")};
        if(svg.height) {svg.removeAttribute("height")};
        svg.style.width = "100%";
        svg.style.height = "100%";
       }
    };

    // set up a container for tasks to perform after completion
    //  one example would be add callbacks for event handling
    //  styling
    if (typeof mermaid.tasks === "undefined"){
      mermaid.tasks = [];
    }
      
    if (!(typeof x.tasks === "undefined") ){
      if ( (typeof x.tasks.length === "undefined") ||
       (typeof x.tasks === "function" ) ) {
         // handle a function not enclosed in array
         // should be able to remove once using jsonlite
         x.tasks = [x.tasks];
      }
      x.tasks.map(function(t){
        // for each tasks add it to the mermaid.tasks with el
        mermaid.tasks.push(
          {"task": t, "el": el}
        );
      })
    }

  
    // get all DiagrammeR mermaids widgets
    dg = document.getElementsByClassName("DiagrammeR");
    // run mermaid.init
    //  but use try catch block
    //  to send error to the htmlwidget for display
    try{
      mermaid.init();
      
      // sort of make our diagram responsive
      //   should we make this an option?
      //   if so, then could easily add to list of post process tasks
      makeResponsive( el );
      
      // change the id of our SVG assigned by mermaid to prevent conflict
      //   mermaid.init has a counter that will reset to 0
      //   and cause duplication of SVG id if multiple
      d3.select(el).select("svg")
        .attr("id", "mermaidChart-" + el.id);
      // now we have to change the styling assigned by mermaid
      //   to point to our new id that we have assigned
      d3.select(el).select("svg").select("style")[0][0].innerHTML = d3.select(el).select("svg")
        .select("style")[0][0].innerHTML
        .replace(/mermaidChart[0-9]*/gi, "mermaidChart-" + el.id);

      /*
      //difficult way to change the stylesheet
      //     thought it would be more robust but the above
      //     works better
      for( i = 0; i <  document.styleSheets.length; i++ ) {
        var s = document.styleSheets[i];
        // find the stylesheet for this widget
        if(s.ownerNode.parentNode.parentNode.id === el.id){
          // use http://davidwalsh.name/add-rules-stylesheets
          //  to change the rules to use our new svg id in css
          var howManyRules = s.cssRules.length;
          for ( rule = 0; rule < howManyRules; rule++){
            s.insertRule(
      */
      //        s.cssRules.item(rule).cssText.replace(/mermaidChart[0-9]*/gi, "mermaidChart-" + el.id),
      /*        howManyRules
            );
          }
          // now delete the original rules          
          for ( rule = 0; rule < howManyRules ; rule++){
            s.deleteRule(rule);
          }
        }
      }
      */
    } catch(e) {
      // if error look for last processed DiagrammeR
      //  and send error to the container div
      //  with pre containing the errors
      var processedDg = d3.selectAll(".DiagrammeR[data-processed=true]");
      // select the last
      processedDg = d3.select(processedDg[0][processedDg[0].length - 1])
      // remove the svg
      processedDg.select("svg").remove();
      
      //if dynamic such as shiny remove data-processed
      // so mermaid will reprocess and redraw
      if (HTMLWidgets.shinyMode) {
        el.removeAttribute("data-processed")
      }
      
      processedDg.append("pre").html( ["parse error with " + x.diagram, e.message].join("\n") )
    }
    
    // makeResponsive needs to be run again with shiny
    //  since asynchrony sometimes results in not run
    if( HTMLWidgets.shinyMode ){
      for ( i = 0 ; i < dg.length; i++ ){
        makeResponsive( dg[i] )
      }
    }

    // will this ensure synchronous order of execution
    //  the first set of tests seem to indicate they will
    //  but it should be more robustly tested
    if(!(typeof mermaid.tasks.length === "undefined" ) ) {
      mermaid.tasks.forEach(function(t) { 
        //add some error checking here
        if ( typeof t.task === "function" ){
          t.task.call(null, t.el);
        } else {
          console.log("task not a function so skipped");
        }
      });
    }
  },

  resize: function(el, width, height, instance) {

  }
  

});
