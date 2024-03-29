var d3 = require('d3');

var map_count = 'http://qlglicko.org/stats/global/map_count';

var margin = {top: 20, right: 30, bottom: 150, left: 40},
    width = 960 - margin.left - margin.right,
    height = 600 - margin.top - margin.bottom;

var x = d3.scale.ordinal()
	.rangeRoundBands([0, width], .1);

var y = d3.scale.linear()
	.range([height, 0]);

var xAxis = d3.svg.axis()
	.scale(x)
	.orient('bottom');

var yAxis = d3.svg.axis()
	.scale(y)
	.orient('left');

var chart = d3.select('.chart')
	.attr('width', width + margin.left + margin.right)
	.attr('height', height + margin.top + margin.bottom)
  .append('g')
    .attr('transform', 'translate(' + margin.left + ',' + margin.top + ')');

d3.tsv(map_count, type, function(error, data) {
  // Only pick those maps which are of a certain popularity
  data = data.filter(function(d) { return (d.Count > 15000) });

  x.domain(data.map(function(d) { return d.Map; }));
  y.domain([0, d3.max(data, function(d) { return (d.Count / 1000); })]);


  chart.append('g')
  	.attr('class', 'x axis')
  	.attr('transform', 'translate(0,' + height + ')')
  	.call(xAxis)
    .selectAll('text')
        .style('text-anchor', 'end')
        .attr('dx', '-.8em')
        .attr('dy', '.15em')
        .attr('transform', function(d) {
            return 'rotate(-65)';
        });

  chart.append('g')
    .attr('class', 'y axis')
    .call(yAxis)
   .append('text')
    .attr('transform', 'rotate(-90)')
    .attr('y', 6)
    .attr('dy', '.71em')
    .style('text-anchor', 'end')
    .text('Kilomatches');

  chart.selectAll('.bar')
      .data(data)
    .enter().append('rect')
      .attr('class', 'bar')
      .attr('x', function(d) { return x(d.Map); })
      .attr('y', function(d) { return y(d.Count / 1000); })
      .attr('height', function(d) { return height - y(d.Count / 1000); })
      .attr('width', x.rangeBand());
});

function type(d) {
  d.value = +d.Count; // coerce to number
  return d;
}



