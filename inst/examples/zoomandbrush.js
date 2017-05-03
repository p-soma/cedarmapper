// THIS IS A SAMPLE FORCE LAYOUT THAT IMPLEMENTS FOLLOWING FEATURES:
// semantic zoom:  when using the wheel to zoom, does not change pixels or nodes (e.g svg transform) but
// zoom can move entire layout with mouse clicks
// shift click (not on a node) will start a  rectangular brush that selects nodes not previously selected
//   this brush compensates for current node position in space (e.g. if zoomed still selects correctly)
// click on a nodes can be selected by clicking, clicking a node will de-selected all of the others currently selected
// shift+click on a node will add to currently selected nodes
// click and drag on a node will drag it and other layout will move out of the way
// click and drag on a node will move ALL selected nodes in the same way
// nodes have encloded test (are g with circles and text
// CURRENTLY NODES ARE NOT 'FIXED'  ; FORCE LAYOUT WILL MOVE THEM INTO POSITION


function selectableForceDirectedGraph(el,w,h) {
    var force, nodes, circles, texts, width,height,
    shiftKey, ctrlKey;
    var colorscale = d3.scale.category20b();

    width = w;
    height = h;

    var nodeGraph = null;

    // SCALES used by zoom
    var xScale = d3.scale.linear().domain([0,width]).range([0,width]);
    var yScale = d3.scale.linear().domain([0,height]).range([0, height]);

    var svg = d3.select(el)
    .attr("tabindex", 1)
    .on("keydown.brush", keydown)
    .on("keyup.brush", keyup)
    .each(function() { this.focus(); })
    .append("svg")
    .attr("width", width)
    .attr("height", height);

    /*** Configure zoom behaviour ***/

    // GLOBALS used by semantic zoom
    var scaleFactor = 1, translation = [0,0];

    var zoomer = d3.behavior.zoom()
        .scaleExtent([0.1,10])
        .x(xScale)
        .y(yScale)
        .on("zoom", semanticzoom);

    function semanticzoom() {
        console.log("zoom", d3.event.translate, d3.event.scale);
        scaleFactor = d3.event.scale;
        translation = d3.event.translate;
        tick(); //update positions
    }

    function redraw() {
        // vis.attr("transform",
        //          "translate(" + d3.event.translate + ")" + " scale(" + d3.event.scale + ")");
    }

    var svg_graph = svg.append('svg:g')
    .call(zoomer)
    //.call(brusher)

    var rect = svg_graph.append('svg:rect')
    .attr('width', width)
    .attr('height', height)
    .attr('fill', 'transparent')
    //.attr('opacity', 0.5)
    .attr('stroke', 'transparent')
    .attr('stroke-width', 1)
    //.attr("pointer-events", "all")
    .attr("id", "zrect")

    // BRUSHING ********
    var brusher = d3.svg.brush()
      .x(d3.scale.identity().domain([0, width]))
      .y(d3.scale.identity().domain([0, height]))
      .on("brushstart", function(d) {
        nodes.each(function(d) {
            d.previouslySelected = shiftKey && d.selected; });
          })
      .on("brush", function() {
        var extent = d3.event.target.extent();
        console.log("**********extent x",extent[0][0],extent[1][0],"y",extent[0][1],extent[1][1]);

        nodes.classed("selected", function(d) {
              return( d.selected = d.previouslySelected ||
            (   xScale.invert(extent[0][0]) <= d.x  &&  d.x < xScale.invert(extent[1][0])
             && yScale.invert(extent[0][1]) <= d.y  &&  d.y < yScale.invert(extent[1][1])  )
           );

        });
    })
    .on("brushend", function() {
        d3.event.target.clear();
        d3.select(this).call(d3.event.target);
    });

    var brush = svg_graph.append("g")
    .datum(function() { return {selected: false, previouslySelected: false}; })
    .attr("class", "brush");

    brush.call(brusher)
    .on("mousedown.brush", null)
    .on("touchstart.brush", null)
    .on("touchmove.brush", null)
    .on("touchend.brush", null);

    brush.select('.background').style('cursor', 'auto');


    var vis = svg_graph.append("svg:g");

    vis.attr('fill', 'red')
    .attr('stroke', 'black')
    .attr('stroke-width', 1)
    .attr('opacity', 0.5)
    .attr('id', 'vis')

    var link = vis.append("g")
    .attr("class", "link")
    .selectAll("line");

    var node = vis.append("g")
    .attr("class", "node")
    .selectAll("circle");

    center_view = function() {
        //  THIS USING SVG TRANSFORMS, which we are trying to avoid
        // Center the view on the molecule(s) and scale it so that everything
        // fits in the window

        if (nodeGraph === null)
            return;

        var nodes = nodeGraph.nodes;

        //no molecules, nothing to do
        if (nodes.length === 0)
            return;

        // Get the bounding box
        min_x = d3.min(nodes.map(function(d) {return d.x;}));
        min_y = d3.min(nodes.map(function(d) {return d.y;}));

        max_x = d3.max(nodes.map(function(d) {return d.x;}));
        max_y = d3.max(nodes.map(function(d) {return d.y;}));


        // The width and the height of the graph
        mol_width = max_x - min_x;
        mol_height = max_y - min_y;

        // how much larger the drawing area is than the width and the height
        width_ratio = width / mol_width;
        height_ratio = height / mol_height;

        // we need to fit it in both directions, so we scale according to
        // the direction in which we need to shrink the most
        min_ratio = Math.min(width_ratio, height_ratio) * 0.8;

        // the new dimensions of the molecule
        new_mol_width = mol_width * min_ratio;
        new_mol_height = mol_height * min_ratio;

        // translate so that it's in the center of the window
        x_trans = -(min_x) * min_ratio + (width - new_mol_width) / 2;
        y_trans = -(min_y) * min_ratio + (height - new_mol_height) / 2;


        // do the actual TRANSLATION
        vis.attr("transform",
                 "translate(" + [x_trans, y_trans] + ")" + " scale(" + min_ratio + ")");

                 // tell the zoomer what we did so that next we zoom, it uses the
                 // transformation we entered here
                 zoomer.translate([x_trans, y_trans ]);
                 zoomer.scale(min_ratio);

    };


    function tick() {
      // uses globals translation and scaleFactor, set by semantic Zoom fn

      link.attr("x1", function (d) {
              return translation[0] + scaleFactor*d.source.x;
          })
          .attr("y1", function (d) {
              return translation[1] + scaleFactor*d.source.y;
          })
          .attr("x2", function (d) {
              return translation[0] + scaleFactor*d.target.x;
          })
          .attr("y2", function (d) {
              return translation[1] + scaleFactor*d.target.y;
          });


      circles.attr("cx", function (d) { return translation[0] + scaleFactor*d.x;
          })
          .attr("cy", function (d) { return translation[1] + scaleFactor*d.y;
          });

      texts.attr("x", function (d) { return translation[0] + scaleFactor*d.x;
          })
          .attr("y", function (d) { return translation[1] + scaleFactor*d.y;
          });
    };



   // RESIZING
   /* Set the display size based on the SVG size and re-draw */
  function setSize() {
    var svgStyles = window.getComputedStyle(svg.node());
    var svgW = parseInt(svgStyles["width"]);
    var svgH = parseInt(svgStyles["height"]);

    //Set the output range of the scales
    xScale.range([0, svgW]);
    yScale.range([0, svgH]);

    //re-attach the scales to the zoom behaviour
    zoomer.x(xScale)
          .y(yScale);

    //resize the background
    rect.attr("width", svgW)
            .attr("height", svgH);

    console.log(xScale.range(), yScale.range());
    tick();//re-draw

    }




    function keydown() {
        shiftKey = d3.event.shiftKey || d3.event.metaKey;
        ctrlKey = d3.event.ctrlKey;

        console.log('d3.event', d3.event)

        if (d3.event.keyCode == 67) {   //the 'c' key
            center_view();
        }

        if (shiftKey) {
            svg_graph.call(zoomer)
            .on("mousedown.zoom", null)
            .on("touchstart.zoom", null)
            .on("touchmove.zoom", null)
            .on("touchend.zoom", null);

            //svg_graph.on('zoom', null);
            vis.selectAll('g.gnode')
            .on('mousedown.drag', null);

            brush.select('.background').style('cursor', 'crosshair')
            brush.call(brusher);
        }
    }

    function keyup() {
        shiftKey = d3.event.shiftKey || d3.event.metaKey;
        ctrlKey = d3.event.ctrlKey;

        brush.call(brusher)
        .on("mousedown.brush", null)
        .on("touchstart.brush", null)
        .on("touchmove.brush", null)
        .on("touchend.brush", null);

        brush.select('.background').style('cursor', 'auto')
        svg_graph.call(zoomer);
    }


    function dragstarted(d) {
        d3.event.sourceEvent.stopPropagation();
        if (!d.selected && !shiftKey) {
            // if this node isn't selected, then we have to unselect every other node
            nodes.classed("selected", function(p) { return p.selected =  p.previouslySelected = false; });
        }

        d3.select(this).classed("selected", function(p) { d.previouslySelected = d.selected; return d.selected = true; });

        nodes.filter(function(d) { return d.selected; })
        .each(function(d) { d.fixed |= 2; })
    }

    function dragended(d) {
        //d3.select(self).classed("dragging", false);
        nodes.filter(function(d) { return d.selected; })
        .each(function(d) { d.fixed &= ~6; })

    }


    function dragged(d) {
        nodes.filter(function(d) { return d.selected; })
        .each(function(d) {
            d.x += d3.event.dx;
            d.y += d3.event.dy;

            d.px += d3.event.dx;
            d.py += d3.event.dy;
        })

        force.resume();
    }

    // LOAD DATA, SETUP NODES AND LINKS,  AND START FORCE layout
    d3.json("diabetesmapper.json", function(error, graph) {
        nodeGraph = graph;

        graph.links.forEach(function(d) {
            d.source = graph.nodes[d.source];
            d.target = graph.nodes[d.target];
        });

        link = link.data(graph.links).enter().append("line")
        .attr("x1", function(d) { return d.source.x; })
        .attr("y1", function(d) { return d.source.y; })
        .attr("x2", function(d) { return d.target.x; })
        .attr("y2", function(d) { return d.target.y; });


        force = d3.layout.force()
        .charge(-500)
        .linkDistance(75)
        .nodes(graph.nodes)
        .links(graph.links)
        .size([width, height])
        .start();


        nodes = node.data(graph.nodes)
              .enter().append("g")
              .attr("class", "node");


        titles = nodes.append("svg:title")
                .text(function (d) {
                    return d.y;
                });


        circles = nodes.append("circle")
                .attr("r", function(d) { return( Math.sqrt(d.x)); })
                .attr("cx", function(d) { return d.x; })
                .attr("cy", function(d) { return d.y; })
                .style("fill",function(d) { return(colorscale(Math.floor(Math.random() * 20)));  })
                .on("dblclick", function(d) { d3.event.stopPropagation(); })
                .on("click", function(d) {
                          if (d3.event.defaultPrevented) return;

                          if (!shiftKey) {
                              //if the shift key isn't down, unselect everything
                              node.classed("selected", function(p) { return p.selected =  p.previouslySelected = false; })
                          }

                          // always select this node
                          d3.select(this).classed("selected", d.selected = !d.previouslySelected);
                      })
                .on("mouseup", function(d) {
                    //if (d.selected && shiftKey) d3.select(this).classed("selected", d.selected = false);
                    })
                .call(d3.behavior.drag()
                      .on("dragstart", dragstarted)
                      .on("drag", dragged)
                      .on("dragend", dragended));


        texts = nodes.append("text")
            .attr("pointer-events","none")
            .attr("class", "nodetext")
            .attr("dy","0.1em")
            .attr("x",  function(d) { return d.x; })
            .attr("y",  function(d) { return d.y; })
            .attr("text-anchor","middle")
            .text(function(d) { return Math.floor(d.x)});

        force.on("tick", tick);

    });



}
