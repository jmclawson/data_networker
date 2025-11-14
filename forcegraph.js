// !preview r2d3 data = jsonlite::read_json("miserables.json"), d3_version = 4

// Based on: https://bl.ocks.org/mbostock/4063570

var radius = 5;

r2d3.onRender(function(graph, svg, width, height, options) {
  var colorScale = d3.scaleOrdinal(
  options.default_color
    ? [options.default_color].concat(d3.schemeCategory20)
    : d3.schemeCategory20
);

  var simulation = svg._sim || (svg._sim = d3.forceSimulation()
    .force("link", d3.forceLink().id(function(d){ return d.id; }).distance(40))
    .force("charge", d3.forceManyBody().strength(-80))
    .force("center", d3.forceCenter(width/2, height/2))
    .force("x", d3.forceX(width / 2).strength(0.04))
  .force("y", d3.forceY(height / 2).strength(0.04))
  .force("collide", d3.forceCollide().radius(function(d){ return d.size + 2; }).iterations(2))
);

  var prev = new Map((simulation.nodes() || []).map(function(n){ return [n.id, n]; }));

  var jitter = 40;
  graph.nodes.forEach(function(n){
    var o = prev.get(n.id);
    if (o) { n.x = o.x; n.y = o.y; n.vx = o.vx; n.vy = o.vy; }
    else   { n.x = width/2 + (Math.random()-0.5)*jitter;
             n.y = height/2 + (Math.random()-0.5)*jitter;
             n.vx = 0; n.vy = 0; }
  });

  if (options && options.reset === true) {
    graph.nodes.forEach(function(n){
      n.x = width/2 + (Math.random()-0.5)*jitter;
      n.y = height/2 + (Math.random()-0.5)*jitter;
      n.vx = n.vy = 0;
    });
  }

  svg.selectAll("g").remove();

  var link = svg.append("g")
      .attr("class", "links")
    .selectAll("line")
    .data(graph.links, function(d){
      var s = d.source.id || d.source, t = d.target.id || d.target;
      return s + "-" + t; // key links
    })
    .enter().append("line")
      .attr("stroke-width", function(d) { return Math.sqrt(d.value); });

  var node = svg.append("g")
      .attr("class", "nodes")
    .selectAll("circle")
    .data(graph.nodes, function(d){ return d.id; }) // key nodes
    .enter().append("circle")
      .attr("r", function(d) { return d.size; })
      .attr("fill", function(d) {
        var g = d.group;
        return (g && d3.color(g)) ? g : colorScale(g);
      })
      .attr("cx", function(d){ return d.x; })
      .attr("cy", function(d){ return d.y; })
      .call(d3.drag()
          .on("start", dragstarted)
          .on("drag", dragged)
          .on("end", dragended));

  node.append("title").text(function(d) { return d.id; });

  var labels = svg.append("g")
    .attr("class", "labels")
    .selectAll("text")
    .data(graph.nodes, function(d){ return d.id; })
    .enter().append("text")
    .attr("class", "node-label")
    .attr("x", function(d){ return d.x; })
    .attr("y", function(d){ return d.y; })
    .attr("dy", "-0.9em")        // offset slightly above circle
    .attr("text-anchor", "middle")
    .style("font-size", "10px")
    .style("pointer-events", "none")  // so circles can still drag
    .text(function(d){ return d.label; });

  labels.style("display", options.show_labels ? null : "none");

  simulation.nodes(graph.nodes).on("tick", ticked);
  simulation.force("link").links(graph.links);

  simulation.alpha(1).stop();
  for (var i = 0; i < 30; ++i) simulation.tick();

  simulation.alpha(0.25).restart();
  setTimeout(function(){ simulation.alphaTarget(0); }, 200);

  function ticked() {
    node
      .attr("cx", function(d) { return d.x = Math.max(radius, Math.min(width - radius, d.x)); })
      .attr("cy", function(d) { return d.y = Math.max(radius, Math.min(height - radius, d.y)); });

    link
      .attr("x1", function(d) { return d.source.x; })
      .attr("y1", function(d) { return d.source.y; })
      .attr("x2", function(d) { return d.target.x; })
      .attr("y2", function(d) { return d.target.y; });

    labels
      .attr("x", function(d){ return d.x; })
      .attr("y", function(d){ return d.y; });
  }

  function dragstarted(d) {
  if (!d3.event.active) simulation.alphaTarget(0.3).restart();
  d.fx = d.x; d.fy = d.y;
}
function dragged(d) {
  d.fx = d3.event.x; d.fy = d3.event.y;
}
function dragended(d) {
  if (!d3.event.active) simulation.alphaTarget(0);
  d.fx = null; d.fy = null;
}

node.call(d3.drag()
  .on("start", dragstarted)
  .on("drag", dragged)
  .on("end", dragended));
});

//
// r2d3::r2d3(data = jsonlite::read_json("miserables.json"), d3_version = 4, script = "forcegraph.js")
