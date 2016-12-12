HTMLWidgets.widget({

  name: 'plotlyLite',

  type: 'output',

  factory: function(el, width, height) {

    // TODO: define shared variables for this instance
    var instance = {}

    return {

      renderValue: function(x) {

        var graphDiv = document.getElementById(el.id);
        // if no plot exists yet, create one with a particular configuration
        if (!instance.plotly) {
          Plotly.plot(graphDiv, x.data, x.layout, x.config);
          instance.plotly = true;
          instance.autosize = x.layout.autosize;
        } else {
          Plotly.newPlot(graphDiv, x.data, x.layout);
        }

      },

      resize: function(width, height) {
        if (instance.autosize) {
          Plotly.relayout(el.id, {width: width, height: height});
        }
      }

    };
  }
});
