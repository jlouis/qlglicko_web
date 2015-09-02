create = function(map) {
    if ($('#' + map).length == 0) {
        var clone = $('.template-map').clone();
        clone.find('.map-header').text(map);
        clone.attr('id', map);
        clone.removeClass('template-map');
        clone.addClass('map-entry');
        $('#maps').append(clone.show());
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
        clone.find('.sparkline').sparkline(
            [r, r-2*rd, 2600, 2400, 2000],
            { type: 'bullet', raw: true, width: '220px' } );
        clone.find('.streak').sparkline(values['streak'],
                                        { type: 'tristate' } );
        var devel = jQuery.map(ranks, function(o) { return o['rank']; });
        clone.find('.development').sparkline(
            devel,
            {type: 'line', width: '220px',
             maxSpotColor: 'green', minSpotColor: 'red'});
        
    });
};

get_and_add_player = function(player) {
    $.getJSON('/player/' + player, function(data) {
	add_player(player, data);
    });
};
