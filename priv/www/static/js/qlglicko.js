create = function(map) {
  if ($('#' + map).length == 0) {
    var clone = $('.template-map').clone();
    clone.find('.map-header').text(map);
    clone.attr('id', map);
    clone.removeClass('template-map');
    clone.addClass('map-entry');
    $('#maps').append(clone.show())
  }
};

add_player = function(player, data) {
  $.each(data, function(map, values) {
    create(map);
    
    var clone = $('.template-player').clone();
    clone.removeClass('template-player');
    clone.attr('id', player + '-' + map);
    clone.find('.name').text(player);
    var ranks = values['rank'];
    var r = Math.round(ranks[ranks.length - 1]['rank']);
    var rd = Math.round(ranks[ranks.length - 1]['rank_deviation']);
    clone.find('.rank').text(r);
    $('#' + map).find('.players').append(clone.fadeIn());
    clone.find('.sparkline').sparkline([0, r - 2*rd, r - rd, r, r + rd, r + 2*rd, 3000],
      { type: 'box', raw: true, width: '140px' } );
    clone.find('.streak').sparkline(values['streak'],
      { type: 'tristate' } );
    var devel = jQuery.map(ranks, function(o) { return o['rank'] });
    clone.find('.development').sparkline(devel, {type: 'line' });
    
  });
};

get_and_add_player = function(player) {
	$.getJSON('/player/' + player, function(data) {
		add_player(player, data);
	});
};
