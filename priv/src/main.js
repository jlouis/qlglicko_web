var d3 = require('d3');

console.log('Ready.');
var map_count = 'http://qlglicko.org/stats/global/map_count';

var width = 420,
    barHeight = 20;

var x = d3.scale.linear()
    .range([0, width]);

var chart = d3.select('.map_count_chart')
    .attr('width', width);

console.log('reading tsv');
d3.tsv(map_count, type, function(error, data) {
  x.domain([0, d3.max(data, function(d) { return d.Count; })]);

  chart.attr('height', barHeight * data.length);

  var bar = chart.selectAll('g')
      .data(data)
    .enter().append('g')
      .attr('transform', function(d, i) { return 'translate(0,' + i * barHeight + ')'; });

  bar.append('rect')
      .attr('width', function(d) { return x(d.Count); })
      .attr('height', barHeight - 1);

  bar.append('text')
      .attr('x', function(d) { return x(d.Count) - 3; })
      .attr('y', barHeight / 2)
      .attr('dy', '.35em')
      .text(function(d) { return d.Count; });
});

function type(d) {
  d.value = +d.Count; // coerce to number
  return d;
}
