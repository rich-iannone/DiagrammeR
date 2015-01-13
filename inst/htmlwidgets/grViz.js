HTMLWidgets.widget({

  name: 'grViz',

  type: 'output',

  initialize: function(el, width, height) {
        
    return {
      // TODO: add instance fields as required
    }

  },

  renderValue: function(el, x, instance) {

    if ( x.diagram != "" ) {
      el.innerHTML = Viz( x.diagram, format="svg", engine="dot", options=null );
    }
    
  },

  resize: function(el, width, height, instance) {
    
  }
  

});
