HTMLWidgets.widget({

  name: 'DiagrammeR',

  type: 'output',

  initialize: function(el, width, height) {
    
    /* wait to initialize until renderValue
        since x not provided until then
        and mermaid will try to build the diagram
        as soon as class of the div is set to "mermaid"
    */
    
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
      mermaid.init();
    } else {
      // set display to none
      el.style.display = "none";
    }
    
  },

  resize: function(el, width, height, instance) {

  }

});
