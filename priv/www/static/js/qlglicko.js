create = function(map) {
  if ($('#' + map).length == 0) {
    var clone = $('.template-map').clone();
    clone.find('.map-header').text(map);
    clone.attr('id', map);
    clone.removeClass('template-map');
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
    clone.find('.rank').text(values['rank']);
    var r = values['rank'];
    var rd = values['rank_deviation'];
    $('#' + map).find('.players').append(clone.fadeIn());
    clone.find('.sparkline').sparkline([0, r - 2*rd, r - rd, r, r + rd, r + 2*rd, 3000],
      { type: 'box', raw: true, width: '140px' } );
    clone.find('.streak').sparkline(values['streak'],
      { type: 'tristate' } );
    
  });
};

get_and_add_player = function(player) {
	$.getJSON('/player/' + player, function(data) {
		add_player(player, data);
	});
};
