HTMLWidgets.widget({

  name: 'queryBuildR',

  type: 'output',

  initialize: function(el, width, height) {

  },

  renderValue: function(el, x, instance) {

    var changeInput = function(id, data) {
        Shiny.onInputChange(el.id + '_' + id, data);
    };

    var $el = $(el);
    $el.queryBuilder({
      sortable: true,
      filters:x.filters,
      rules: x.rules,
      allow_empty: true
    });

  },

  resize: function(el, width, height, instance) {

  }

});
