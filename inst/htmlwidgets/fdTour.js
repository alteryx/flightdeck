HTMLWidgets.widget({

  name: 'fdTour',

  type: 'output',

  initialize: function(el, width, height) {

    return {
      // TODO: add instance fields as required
    }

  },

  renderValue: function(el, x, instance) {
    var intro = introJs().setOptions(x.options)
    intro.setOptions({steps: x.steps})
    if (x.button){
      /*
      var btn = document.createElement("button");
      btn.className = 'btn btn-success';
      var t = document.createTextNode("Start Tour")
      btn.appendChild(t)
      el.appendChild(btn)

      var t = document.createTextNode("Start Tour")
      el.appendChild(t)
      */
      document.querySelector(x.button).addEventListener("click", function(){
        intro.start()
      })
      el.style.display = 'none';
    } else {
      el.style.display = 'none';
      intro.start()
    }
  },

  resize: function(el, width, height, instance) {

  }

});
