var d3 = require('d3');

(function() {

d3.sparkline = function() {
  var width = 100;
  var height = 25;

  var x = d3.scale.linear().range([0, width - 2]);
  var y = d3.scale.linear().range([height - 4, 0]);

  var format = d3.time.format('%Y-%-m-%-d');
  var line = d3.svg.line()
    .interpolate('basis')
    .x(function(d) { return x(d.date); })
    .y(function(d) { return y(d.rank); });

  function sparkline(elemID, data) {
    data.forEach(function(d) {
        d.date = format.parse(d.Date);
        d.rank = +d.Rank;
    });

    console.log(data.rank);

    x.domain(d3.extent(data, function(d) { return d.date; }));
    y.domain(d3.extent(data, function(d) { return d.rank; }));

    var svg = d3.select(elemID)
      .append('svg')
      .attr('width', width)
      .attr('height', height)
      .append('g')
      .attr('transform', 'translate(0, 2)');

    svg.append('path')
      .datum(data)
      .attr('class', 'sparkline')
      .attr('d', line);

    svg.append('circle')
      .attr('class', 'sparkcircle')
      .attr('cx', x(data[0].date))
      .attr('cy', y(data[0].rank))
      .attr('r', 1.5);

  console.log(data[0].date);
  };

  return sparkline;

};

// Chart design based on the recommendations of Stephen Few. Implementation
// based on the work of Clint Ivy, Jamie Love, and Jason Davies.
// http://projects.instantcognition.com/protovis/bulletchart/
d3.bullet = function() {
  var orient = 'left', // TODO top & bottom
      reverse = false,
      duration = 0,
      ranges = bulletRanges,
      markers = bulletMarkers,
      measures = bulletMeasures,
      width = 380,
      height = 30,
      tickFormat = null;

  // For each small multiple…
  function bullet(g) {
    g.each(function(d, i) {
      var rangez = ranges.call(this, d, i).slice().sort(d3.descending),
          markerz = markers.call(this, d, i).slice().sort(d3.descending),
          measurez = measures.call(this, d, i).slice().sort(d3.descending),
          g = d3.select(this);

      // Compute the new x-scale.
      var x1 = d3.scale.linear()
          .domain([0, Math.max(rangez[0], markerz[0], measurez[0])])
          .range(reverse ? [width, 0] : [0, width]);

      // Retrieve the old x-scale, if this is an update.
      var x0 = this.__chart__ || d3.scale.linear()
          .domain([0, Infinity])
          .range(x1.range());

      // Stash the new scale.
      this.__chart__ = x1;

      // Derive width-scales from the x-scales.
      var w0 = bulletWidth(x0),
          w1 = bulletWidth(x1);

      // Update the range rects.
      var range = g.selectAll('rect.range')
          .data(rangez);

      range.enter().append('rect')
          .attr('class', function(d, i) { return 'range s' + i; })
          .attr('width', w0)
          .attr('height', height)
          .attr('x', reverse ? x0 : 0)
        .transition()
          .duration(duration)
          .attr('width', w1)
          .attr('x', reverse ? x1 : 0);

      range.transition()
          .duration(duration)
          .attr('x', reverse ? x1 : 0)
          .attr('width', w1)
          .attr('height', height);

      // Update the measure rects.
      var measure = g.selectAll('rect.measure')
          .data(measurez);

      measure.enter().append('rect')
          .attr('class', function(d, i) { return 'measure s' + i; })
          .attr('width', w0)
          .attr('height', height / 3)
          .attr('x', reverse ? x0 : 0)
          .attr('y', height / 3)
        .transition()
          .duration(duration)
          .attr('width', w1)
          .attr('x', reverse ? x1 : 0);

      measure.transition()
          .duration(duration)
          .attr('width', w1)
          .attr('height', height / 3)
          .attr('x', reverse ? x1 : 0)
          .attr('y', height / 3);

      // Update the marker lines.
      var marker = g.selectAll('line.marker')
          .data(markerz);

      marker.enter().append('line')
          .attr('class', 'marker')
          .attr('x1', x0)
          .attr('x2', x0)
          .attr('y1', height / 6)
          .attr('y2', height * 5 / 6)
        .transition()
          .duration(duration)
          .attr('x1', x1)
          .attr('x2', x1);

      marker.transition()
          .duration(duration)
          .attr('x1', x1)
          .attr('x2', x1)
          .attr('y1', height / 6)
          .attr('y2', height * 5 / 6);

      // Compute the tick format.
      var format = tickFormat || x1.tickFormat(8);

      // Update the tick groups.
      var tick = g.selectAll('g.tick')
          .data(x1.ticks(8), function(d) {
            return this.textContent || format(d);
          });

      // Initialize the ticks with the old scale, x0.
      var tickEnter = tick.enter().append('g')
          .attr('class', 'tick')
          .attr('transform', bulletTranslate(x0))
          .style('opacity', 1e-6);

      tickEnter.append('line')
          .attr('y1', height)
          .attr('y2', height * 7 / 6);

      tickEnter.append('text')
          .attr('text-anchor', 'middle')
          .attr('dy', '1em')
          .attr('y', height * 7 / 6)
          .text(format);

      // Transition the entering ticks to the new scale, x1.
      tickEnter.transition()
          .duration(duration)
          .attr('transform', bulletTranslate(x1))
          .style('opacity', 1);

      // Transition the updating ticks to the new scale, x1.
      var tickUpdate = tick.transition()
          .duration(duration)
          .attr('transform', bulletTranslate(x1))
          .style('opacity', 1);

      tickUpdate.select('line')
          .attr('y1', height)
          .attr('y2', height * 7 / 6);

      tickUpdate.select('text')
          .attr('y', height * 7 / 6);

      // Transition the exiting ticks to the new scale, x1.
      tick.exit().transition()
          .duration(duration)
          .attr('transform', bulletTranslate(x1))
          .style('opacity', 1e-6)
          .remove();
    });
    d3.timer.flush();
  }

  // left, right, top, bottom
  bullet.orient = function(x) {
    if (!arguments.length) return orient;
    orient = x;
    reverse = orient == 'right' || orient == 'bottom';
    return bullet;
  };

  // ranges (bad, satisfactory, good)
  bullet.ranges = function(x) {
    if (!arguments.length) return ranges;
    ranges = x;
    return bullet;
  };

  // markers (previous, goal)
  bullet.markers = function(x) {
    if (!arguments.length) return markers;
    markers = x;
    return bullet;
  };

  // measures (actual, forecast)
  bullet.measures = function(x) {
    if (!arguments.length) return measures;
    measures = x;
    return bullet;
  };

  bullet.width = function(x) {
    if (!arguments.length) return width;
    width = x;
    return bullet;
  };

  bullet.height = function(x) {
    if (!arguments.length) return height;
    height = x;
    return bullet;
  };

  bullet.tickFormat = function(x) {
    if (!arguments.length) return tickFormat;
    tickFormat = x;
    return bullet;
  };

  bullet.duration = function(x) {
    if (!arguments.length) return duration;
    duration = x;
    return bullet;
  };

  return bullet;
};

function bulletRanges(d) {
  return d.ranges;
}

function bulletMarkers(d) {
  return d.markers;
}

function bulletMeasures(d) {
  return d.measures;
}

function bulletTranslate(x) {
  return function(d) {
    return 'translate(' + x(d) + ',0)';
  };
}

function bulletWidth(x) {
  var x0 = x(0);
  return function(d) {
    return Math.abs(x(d) - x0);
  };
}

function addAxisAndLegend(svg, xAxis, yAxis, margin, chartWidth, chartHeight) {
	var legendWidth = 200,
	    legendHeight = 100;
	    
	svg.append('clipPath')
	  .attr('id', 'axes-clip')
	  .append('polygon')
        .attr('points',
          (-margin.left)                 + ',' + (-margin.top)                 + ' ' +
          (chartWidth - legendWidth - 1) + ',' + (-margin.top)                 + ' ' +
          (chartWidth - legendWidth - 1) + ',' + legendHeight                  + ' ' +
          (chartWidth + margin.right)    + ',' + legendHeight                  + ' ' +
          (chartWidth + margin.right)    + ',' + (chartHeight + margin.bottom) + ' ' +
          (-margin.left)                 + ',' + (chartHeight + margin.bottom));

	var axes = svg.append('g')
      .attr('clip-path', 'url(#axes-clip)');
      
	axes.append('g')
      .attr('class', 'x axis')
      .attr('transform', 'translate(0,' + chartHeight + ')')
      .call(xAxis);

	axes.append('g')
      .attr('class', 'y axis')
      .call(yAxis)
      .append('text')
        .attr('transform', 'rotate(-90)')
        .attr('y', 6)
        .attr('dy', '.71em')
        .style('text-anchor', 'end')
        .text('Time (s)');

	var legend = svg.append('g')
      .attr('class', 'legend')
      .attr('transform', 'translate(' + (chartWidth - legendWidth) + ', 0)');

	legend.append('rect')
      .attr('class', 'legend-bg')
      .attr('width',  legendWidth)
      .attr('height', legendHeight);

	legend.append('rect')
      .attr('class', 'outer')
      .attr('width',  75)
      .attr('height', 20)
      .attr('x', 10)
      .attr('y', 10);

	legend.append('text')
      .attr('x', 115)
      .attr('y', 25)
      .text('5% - 95%');

	legend.append('rect')
      .attr('class', 'inner')
      .attr('width',  75)
      .attr('height', 20)
      .attr('x', 10)
      .attr('y', 40);

	legend.append('text')
      .attr('x', 115)
      .attr('y', 55)
      .text('25% - 75%');

	legend.append('path')
      .attr('class', 'median-line')
      .attr('d', 'M10,80L85,80');

	legend.append('text')
      .attr('x', 115)
      .attr('y', 85)
      .text('Median');
}    	  

function drawPaths (svg, data, x, y) {
	var upperOuterArea = d3.svg.area()
      .interpolate('basis')
      .x (function (d) { return x(d.date) || 1; })
      .y0(function (d) { return y(d.pct95); })
      .y1(function (d) { return y(d.pct75); });

	var upperInnerArea = d3.svg.area()
      .interpolate('basis')
      .x (function (d) { return x(d.date) || 1; })
      .y0(function (d) { return y(d.pct75); })
      .y1(function (d) { return y(d.pct50); });

	var medianLine = d3.svg.line()
      .interpolate('basis')
      .x(function (d) { return x(d.date); })
      .y(function (d) { return y(d.pct50); });

	var lowerInnerArea = d3.svg.area()
      .interpolate('basis')
      .x (function (d) { return x(d.date) || 1; })
      .y0(function (d) { return y(d.pct50); })
      .y1(function (d) { return y(d.pct25); });

	var lowerOuterArea = d3.svg.area()
      .interpolate('basis')
      .x (function (d) { return x(d.date) || 1; })
      .y0(function (d) { return y(d.pct25); })
      .y1(function (d) { return y(d.pct05); });

	svg.datum(data);

	svg.append('path')
      .attr('class', 'area upper outer')
      .attr('d', upperOuterArea)
      .attr('clip-path', 'url(#rect-clip)');

	svg.append('path')
      .attr('class', 'area lower outer')
      .attr('d', lowerOuterArea)
      .attr('clip-path', 'url(#rect-clip)');

	svg.append('path')
      .attr('class', 'area upper inner')
      .attr('d', upperInnerArea)
      .attr('clip-path', 'url(#rect-clip)');

	svg.append('path')
      .attr('class', 'area lower inner')
      .attr('d', lowerInnerArea)
      .attr('clip-path', 'url(#rect-clip)');

	svg.append('path')
      .attr('class', 'median-line')
      .attr('d', medianLine)
      .attr('clip-path', 'url(#rect-clip)');
}

function addMarker (marker, svg, chartHeight, x) {
	var radius = 32,
        xPos = x(marker.date) - radius - 3,
        yPosStart = chartHeight - radius - 3,
        yPosEnd = (marker.type === 'Client' ? 80 : 160) + radius - 3;

	var markerG = svg.append('g')
      .attr('class', 'marker '+marker.type.toLowerCase())
      .attr('transform', 'translate(' + xPos + ', ' + yPosStart + ')')
      .attr('opacity', 0);

	markerG.transition()
      .duration(1000)
      .attr('transform', 'translate(' + xPos + ', ' + yPosEnd + ')')
      .attr('opacity', 1);

	markerG.append('path')
      .attr('d', 'M' + radius + ',' +
              (chartHeight-yPosStart) + 'L' + radius + ',' + (chartHeight-yPosStart))
      .transition()
        .duration(1000)
        .attr('d', 'M' + radius + ',' +
              (chartHeight-yPosEnd) + 'L' + radius + ',' + (radius*2));

	markerG.append('circle')
      .attr('class', 'marker-bg')
      .attr('cx', radius)
      .attr('cy', radius)
      .attr('r', radius);

	markerG.append('text')
      .attr('x', radius)
      .attr('y', radius*0.9)
      .text(marker.type);

	markerG.append('text')
      .attr('x', radius)
      .attr('y', radius*1.5)
      .text(marker.version);
}

function startTransitions (svg, chartWidth, chartHeight, rectClip, markers, x) {
	rectClip.transition()
      .duration(1000*markers.length)
      .attr('width', chartWidth);

	markers.forEach(function (marker, i) {
      setTimeout(function () {
        addMarker(marker, svg, chartHeight, x);
      }, 1000 + 500*i);
    });
}

function makeChart (data, markers) {
	var svgWidth  = 960,
        svgHeight = 500,
        margin = { top: 20, right: 20, bottom: 40, left: 40 },
        chartWidth  = svgWidth  - margin.left - margin.right,
        chartHeight = svgHeight - margin.top  - margin.bottom;

	var x = d3.time.scale().range([0, chartWidth])
              .domain(d3.extent(data, function (d) { return d.date; })),
        y = d3.scale.linear().range([chartHeight, 0])
              .domain([0, d3.max(data, function (d) { return d.pct95; })]);

	var xAxis = d3.svg.axis().scale(x).orient('bottom')
                  .innerTickSize(-chartHeight).outerTickSize(0).tickPadding(10),
        yAxis = d3.svg.axis().scale(y).orient('left')
                  .innerTickSize(-chartWidth).outerTickSize(0).tickPadding(10);

	var svg = d3.select('body').append('svg')
      .attr('width',  svgWidth)
      .attr('height', svgHeight)
      .append('g')
        .attr('transform', 'translate(' + margin.left + ',' + margin.top + ')');

	// clipping to start chart hidden and slide it in later
	var rectClip = svg.append('clipPath')
      .attr('id', 'rect-clip')
      .append('rect')
        .attr('width', 0)
        .attr('height', chartHeight);

	addAxesAndLegend(svg, xAxis, yAxis, margin, chartWidth, chartHeight);
	drawPaths(svg, data, x, y);
	startTransitions(svg, chartWidth, chartHeight, rectClip, markers, x);
}

})();
