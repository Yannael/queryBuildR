HTMLWidgets.widget({

  name: 'queryBuildR',

  type: 'output',

  initialize: function(el, width, height) {

  },

  renderValue: function(el, x, instance) {

  //  var changeInput = function(id, data) {
  //      Shiny.onInputChange(el.id + '_' + id, data);
  //  };

    var $el = $(el);
    $el.queryBuilder({
      filters:x.filters,
      rules: x.rules,
      allow_empty:'true'
    });

  //  changeInput('rules',$el.queryBuilder('getRules'))
  //  changeInput('sqlQuery',$el.queryBuilder('getSQL', false))
  },

  resize: function(el, width, height, instance) {

  }

});
