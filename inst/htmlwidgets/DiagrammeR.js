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
    
    this.manualRender( function(){
       var svg = el.getElementsByTagName("svg")[0]
       if(svg.width) {svg.removeAttribute("width")};
       if(svg.height) {svg.removeAttribute("height")};
       svg.style.width = "100%";
       svg.style.height = "100%";
    } );
  },

  resize: function(el, width, height, instance) {

  },
  
  manualRender: function( callback ){
    /* not optimal way to manually run and wait for
        mermaid.init() before other steps
    */
    mermaid.init();
    // currently just using to make "responsive" svg
    // but could apply to a list of callbacks / behaviors
    callback();
  }
  

});
