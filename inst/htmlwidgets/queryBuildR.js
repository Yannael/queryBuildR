HTMLWidgets.widget({

  name: 'queryBuildR',

  type: 'output',

  initialize: function(el, width, height) {

  },

  renderValue: function(el, x, instance) {

   /*
   mobileConsole.show();

      mobileConsole.options({
    		showOnError: false,
    		proxyConsole: true,
    		isCollapsed: true,
    		catchErrors: true
    	});
    */

    var changeInput = function(id, data) {
        Shiny.onInputChange(el.id + '_' + id, data);
    };

    var $el = $(el);

   $el.on('afterCreateRuleInput.queryBuilder', function(e, rule) {
      if (rule.filter.plugin == 'selectize') {
        rule.$el.find('.rule-value-container').css('min-width', '200px')
          .find('.selectize-control').removeClass('form-control');
      }
    });

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
