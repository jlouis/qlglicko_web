var d3 = require('d3');

var map_count = 'http://qlglicko.org/stats/global/map_count';

var margin = {top: 20, right: 30, bottom: 100, left: 40},
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
  data = data.filter(function(d) { return (d.Count > 20000) });

  x.domain(data.map(function(d) { return d.Map; }));
  y.domain([0, d3.max(data, function(d) { return (d.Count / 1000); })]);


  chart.append('g')
  	.attr('class', 'x axis')
  	.attr('transform', 'translate(0,' + height + ')')
  	.call(xAxis)
    .selectAll("text")  
        .style("text-anchor", "end")
        .attr("dx", "-.8em")
        .attr("dy", ".15em")
        .attr("transform", function(d) {
            return "rotate(-65)" 
        });

  chart.append('g')
    .attr('class', 'y axis')
    .call(yAxis)
   .append("text")
    .attr("transform", "rotate(-90)")
    .attr("y", 6)
    .attr("dy", ".71em")
    .style("text-anchor", "end")
    .text("Kilomatches");

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

var sparkLine = function () {
  var width = 100;
  var height = 25;
  
  var x = d3.scale.linear().range([0, width - 2]);
  var y = d3.scale.linear().range([height - 4, 0]);
  
  var parseDate = d3.time.format("%b-%d-%Y");
  var line = d3.svg.line()
    .interpolate('basis')
    .x(function (d) { return x(d.date); })
    .y(function (d) { return y(d.rank); })
    
  function draw(elemID, data) {
    data.forEach(function (d) {
        d.date = parseDate(d.Date);
        d.rank = +d.Rank
    });
    
    x.domain(d3.extent(data, function (d) { return d.date; });
    y.domain(d3.extent(data, function (d) { return d.rank; });
    
    var svg = d3.select(elemId)
      .append('svg')
      .attr('width', width)
      .attr('height', height)
      .append('g')
      .attr('transform', 'translate(0, 2)');
    
    svg.append('path')
      .datum(data)
      .attr('class', 'sparkline')
      .attr('d', line)
      
    svg.append('circle')
      .attr('class', 'sparkcircle')
      .attr('cx', x(data[0].date))
      .attr('cy', y(data[0].rank))
      .attr('r', 1.5);
      
  };
  
  this.draw = draw;
  
}();
