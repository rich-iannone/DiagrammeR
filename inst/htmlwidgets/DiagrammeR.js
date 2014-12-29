HTMLWidgets.widget({

  name: 'DiagrammeR',

  type: 'output',

  initialize: function(el, width, height) {

    return {
      // TODO: add instance fields as required
    }

  },

  renderValue: function(el, x, instance) {

    el.innerText = x.message;

  },

  resize: function(el, width, height, instance) {

  }

});
