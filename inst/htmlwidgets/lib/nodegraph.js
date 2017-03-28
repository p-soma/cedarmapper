//var shinyMode = True;

// NodeGraph module
//var shinyMode = window.HTMLWidgets.shinyMode =
//      typeof(window.Shiny) !== "undefined" && !!window.Shiny.outputBindings;


cedar = {};
var selectedNodes;

cedar.NodeGraph = function module() {

    var margin = {
        top: 20,
        right: 20,
        bottom: 40,
        left: 40
    };

    var w = '100%';
    var h = window.innerHeight;

    var opacity_percent = 0.98,
        node_area_percent = 0.2,
        maxLinkWidth = 8,
        minLinkWidth = 1,
        nodecolors = ['white', 'darkgreen'],
        ForceCharge = -3000,
        LinkDistance = 30,
        linkdistanceFactor = 1,
        nudgefactor = 10;

    // functions called by public API

    var resetzoom, manualzoom, force, graph, svg, nodeSizeScale, setFillColor,
    getSelected, clearSelected, getValues, setValues, forceresize, nodevalues,
    nudge, changeforcecharge,changeLinkDistance,changeRotation,setTransform;
    var setGroupID, clearGroupID, removeGroupID; // function used by external API
    var nodes;

    function nodegraph(_selection) {

        var width =  _selection.innerWidth;
        var height = _selection.innerHeight;

        _selection.attr("tabindex", 1);

        _selection.each(function(graphdata) {


            // ********* ZOOM FUNCTIONS
            // currentlymouse wheel sets scale only, no panning

            // zoomable scales
            var x_scale = d3.scale.linear().domain([0, w]).range([0, w]);
            var y_scale = d3.scale.linear().domain([0, h]).range([0, h]);

            // initial values
            var tx = 0,ty = 0,scale = 1,rotation=0;


            setTransform = function(){
                
              // uses globals tx,tx,scale and rotation
              var rotationy = h/2;
              var rotationx = w/2;        
              var graphtransform = `translate(${tx}, ${ty}) scale(${scale}) rotate(${rotation} ${rotationx} ${rotationy})`;
                            
              graph.attr('transform', graphtransform );
              
              nodegroup.each(function(d,i) {
                  d3.selectAll('text')
                    .attr('transform', `rotate(${-1*rotation})`); 
              });
              // brush.attr('transform', `rotate(${antirotation}  ${rotationx} ${rotationy})`);
            
 
            }; 
            
            resetzoom = function() {
              // this is really a "re-center" function
                scale = 1;
                tx = 0;
                ty = 0;
                setTransform();
              };

            manualzoom = function(z){
              scale = scale + z;
              center = [w/2,h/2];
              doZoom(z,center);
              setTransform();
      
            };
            
             changeRotation = function(deg){
              rotation = rotation + deg;
              setTransform();
            };


            // ZOOM FUNCTION does not use d3.zoom;
            // overrwrides mouse wheel event and translates
            function mousezoom() {
              center = d3.mouse(document.querySelector('svg'));
              zoomdir = d3.event.deltaY;
              d3.event.preventDefault();
               // prevent default event behaviour
              doZoom(zoomdir,center);
            }

            function doZoom(zoomdir,center){
                console.log(scale);
                // set zooming; should be global constant
                var factor = 1.05;
                // var center = d3.mouse(document.querySelector('svg'));
                var zY, zX, zScale;

                // calculate new scale
                if (zoomdir > 0) {
                    zScale = scale * factor;
                } else {
                    zScale = scale / factor;
                }

                // calculate new translate position
                // [current mouse position] - 
                //      ([current mouse position] - [current translate]) * magnification
                zX = center[1] - (center[1] - tx) * zScale / scale;
                zY = center[0] - (center[0] - ty) * zScale / scale;

                // set new scale and translate position
                scale = zScale;
                console.log("new scale: ");
                console.log(scale);
                tx = zX;
                ty = zY;

                setTransform();
        
            }


            // *** MAIN SVG ELEMENT
            svg = d3.select(this)
                .on("keydown", keydown)
                .on("keyup", keyup)
                .classed("svg-container", true)
                .each(function() {
                    this.focus();
                })
                .append("svg")
                .attr("id", "nodegraph")
                .attr("height", h)
                .attr("width", w);
                // .on('dblclick.zoom', resetzoom)
                // .on('dblclick', resetzoom);
                
              svg.append("g")
                .attr("class", "canvas")
                .style("cursor","move")
                .on('wheel.zoom', mousezoom)

            
//                .on("keydown.brush", keydown)
//               .on("keyup.brush", keyup);

            // brush is a seperate svg 'layer' for rectangle selection


            // this holds the nodes and links
            var graph = svg.append('g')
                .attr("id","graph");
                            
            var brush = graph.append("g")
                .attr("class", "brush")
                .datum(function() {
                    return {selected: false, previouslySelected: false};
                });
            var nodeSizes = graphdata.nodes.map(
                function(node, i) {
                    return (node.size+1);
                }
            );

            var linkWeights = graphdata.links.map(
                function(link, i) {
                    return (link.weight);
                });

            var linkWeightScale = d3.scale.linear()
                .domain(d3.extent(linkWeights))
                .range([minLinkWidth, maxLinkWidth]);

            
           getWindowArea  = function(){
                console.log("window area h, w");
                var A = (h - margin.top - margin.bottom) *
                        (w - margin.left - margin.right);
                console.log("window area=" + A);
                return(A);
            };

            // **** NODE SIZE CALCULATIONS
            // determine the largest node size based on n nodes and window area
            maxNodeSize = function(){
              var ncount = graphdata.nodes.length;
              var A = getWindowArea();
              // node_area_percent constant set above
              ns = Math.sqrt(( A * node_area_percent)/(ncount * Math.PI ) );
              console.log("maxnodesize="+ns );
              return(  ns  );
            };

            minNodeSize = function(){
                var t =0.2; // coefficient determining node size variation
                noderange = d3.extent(nodeSizes);
                m = (
                      ((1-t) * noderange[0] + t * noderange[1])  
                              / noderange[1]
                   );
                  
              console.log("minnodesize = " + (m * maxNodeSize()));
              return(maxNodeSize() *m);
            };
            
            nodeSizeScale = d3.scale.log().base(10)
                .domain(d3.extent(nodeSizes))
                .range([minNodeSize(), maxNodeSize()]);

            maxLinkWidth = function(){
                return(10);
            };

            minLinkWidth = function(){
                return(maxLinkWidth() * 0.1);
            };


            // holder for shiftkey detection
            var shiftKey;

            // dispatch is D3's event model, this function is wired to any functions
            // that alter node selection (click, brush, etc)
            dispatch = d3.dispatch("nodeselected");

            dispatch.on("nodeselected", function() {
                nodelist = getSelected();
                // TODO: detect if Shiny is loaded first, then and add shiny callback
                // Rstudio specific, will error when used outside of Shiny
                if (typeof Shiny != "undefined") {
                    Shiny.onInputChange("nodelist", nodelist);
                }
            });

            // create force layout
            // TODO  make link distance a function of number of nodes and size
            force = d3.layout.force()
                .linkDistance(LinkDistance)
                .charge(ForceCharge)
                .size([w, h])
                .on("tick", do_tick)
                .nodes(graphdata.nodes)
                .links(graphdata.links)
                // .chargeDistance(100);
                // other paramters to consider :  or 

            // added for Shiny HTMLWidget; need to determine if useful
            d3.select(window).on('resize', function() {
                window_w = parseInt(_selection.style('width'), 10);
                window_h = parseInt(_selection.style('height'), 10);
                if (window_w < w) {
                    forceresize(window_w, window_h);
                }
            });

            changeLinkDistance = function(z){
              var LinkDistance = force.linkDistance();
              LinkDistance  = LinkDistance + z;
              force.linkDistance(LinkDistance);
              force.resume();

            };
            
            
            // currently unused
            changeforcecharge = function(z){
              forcecharge = force.charge();
              forcecharge  = forcecharge + z;
              force.charge(forcecharge);
              force.alpha(0.25).start();
              console.log(force.charge());
            };

            // added for Shiny HTMLWidget, called when window is resized above
            forceresize = function(_w, _h) {
                w = _w, h = _h;
                svg.attr("width", _w).attr("height", _h);
                force.size([w, h]).resume();
            };


            // graph moving UI: allow for arrow keys to slightly move all selected (fixed) nodes
            function keydown() {
                console.log('keydown');
                if (!d3.event.metaKey) switch (d3.event.keyCode) {
                        case 38:
                            nudge(0, -1);
                            break; // UP
                        case 40:
                            nudge(0, +1);
                            break; // DOWN
                        case 37:
                            nudge(-1, 0);
                            break; // LEFT
                        case 39:
                            nudge(+1, 0);
                            break; // RIGHT
                    }
                    // detect shiftkey event and set shiftkey var
                shiftKey = d3.event.shiftKey || d3.event.metaKey;
            }

            // detect shiftkey event and set shiftkey var
            function keyup() {
                shiftKey = d3.event.shiftKey || d3.event.metaKey;
            }

           nudge= function(dx, dy) {
                //. nudgefactor global config var
                tx = tx + dx * nudgefactor;
                ty = ty + dy * nudgefactor;
                setTransform();
                // d3.event.preventDefault();
            };


            // fixed nodes don't move when layout is redrawn
            function setFixed(d) {
                d3.select(this).classed("fixed", d.fixed = true);
            }

            function releaseFixed(d) {
                d3.select(this).classed("fixed", d.fixed = false);
            }

           function do_tick() {
                // Translate the groups
                nodegroup.attr("transform", function(d) {
                    return 'translate(' + [d.x, d.y] + ')';
                });
                
                link.attr("x1", function(d) {
                        return d.source.x;
                    })
                    .attr("y1", function(d) {
                        return d.source.y;
                    })
                    .attr("x2", function(d) {
                        return d.target.x;
                    })
                    .attr("y2", function(d) {
                        return d.target.y;
                    });

            }


            // Glow Filter Code.  Add a filter in defs element of SVG
            var defs = svg.append("defs");           
            // SourceAlpha refers to opacity of graphic that this filter will be applied to
            // convolve that with a Gaussian with standard deviation 3 and store result
            // in  blur
            
            var blurfilter = function(filterid,blurcolor) {
            filterstr =  '<filter id="' + filterid +'" height="130%"> ' + 
             '<feGaussianBlur in="SourceAlpha" stdDeviation="3"/>' + 
             '  <feOffset dx="2" dy="2" result="offsetblur"/>' +
             '  <feMerge><feMergeNode/><feMergeNode in="SourceGraphic"/></feMerge></filter>';
            };     
            
            glowfilter = '<filter id="glow">'+
                      '    <feGaussianBlur stdDeviation="2.5" result="coloredBlur"/>'+
                      '    <feMerge>'+
                      '        <feMergeNode in="coloredBlur"/>'+
                      '        <feMergeNode in="SourceGraphic"/>'+
                      '    </feMerge>'+
                      '</filter>';
            
            var filter = defs.append("filter").attr("id","colorglow");
            filter.append("feGaussianBlur")
                  .attr("stdDeviation","3.5")
	                .attr("result","coloredBlur");
                  var feMerge = filter.append("feMerge");
                  feMerge.append("feMergeNode")
	                        .attr("in","coloredBlur");
                  feMerge.append("feMergeNode")
	                        .attr("in","SourceGraphic");
          
            // <polygon points="58.263,0.056 100,41.85" 
            //         filter="url(#dropshadow)"/>
            //var filter1 = defs.append("filter").attr("id", "group1filter").attr("height", "130%");
            //filter1.append("feGaussianBlur")
            //    .attr("in", "SourceAlpha")
            //    .attr("stdDeviation", 10) //THIS IS THE SIZE OF THE GLOW
            //    .attr("result", "blur");

            // original code from example that offets the blur for shadow effect
            // we don't want an offset, so need to find a way to remove this
            //filter1.append("feOffset")
            //    .attr("in", "blur")
            //    .attr("dx", 0)
            //    .attr("dy", 0)
            //    .attr("result", "offsetBlur");

            // overlay original SourceGraphic over translated blurred opacity by using
            // feMerge filter. Order of specifying inputs is important!
            //var feMerge = filter1.append("feMerge");
            //feMerge.append("feMergeNode")
            //feMerge.append("feMergeNode").attr("in", "SourceGraphic");
            // end of filter

            var link = graph.selectAll(".link")
                .data(graphdata.links)
                .enter().append("line")
                .attr("class", "link")
                .style("stroke-width", function(d) {
                    return (linkWeightScale(d.weight));
                });


            var nodegroup = graph.selectAll("g.nodegroup")
                .data(graphdata.nodes)
                .enter().append("g")
                .attr("nodevalue", function(d) {
                    return d.values;
                })
                .attr("nodeid", function(d) {
                    return d.name;
                })
                .attr("cx", function(d) {
                    return d.x;
                })
                .attr("cy", function(d) {
                    return d.y;
                })
                .classed('nodegroup', true)
                .on("dblclick", releaseFixed)
                .on("click", function() {
                    d3.select(this).classed("selected", !d3.select(this).classed("selected"));
                    dispatch.nodeselected();
                })
                .call(force.drag().on("dragstart", setFixed));


           nodes = nodegroup
                .append("circle")
                .attr("class", "node")
                .attr("id", function(d) {
                    return "node_" + d.name;
                })
                .attr("r", function(d) {
                    console.log("d size=" + d.size);
                    r = nodeSizeScale(d.size);
                    console.log("r = " + r);
                    return r;
                })
                .attr("nodevalue", function(d) {
                    return d.values;
                })
                .attr("size", function(d) {
                    return d.size;
                });
 
            nodegroup.append("text")
                .attr("text-anchor", "middle")
                .text(function(d) {
                    return d.size;
                });


            // ***** BRUSH FUNCTIONS
            // note this brush layer in the svg element must be defined before the
            // nodegroup elements, or it covers the nodes and they can't be selected
            // possible alternative  to explore is to set the z-layers

            function brush_start(d) {
                // first, save previously selected IF shift+mouse
                nodegroup.each(function(d) {
                    d.previouslySelected = shiftKey && d.selected;
                });
            }

            function do_brush(d) {
                var extent = d3.event.target.extent();
                nodegroup.classed("selected", function(d) {
                    return d.selected = d.previouslySelected ||
                        (extent[0][0] <= d.x && d.x < extent[1][0] &&
                            extent[0][1] <= d.y && d.y < extent[1][1]);
                });
                // update selection during brush
                dispatch.nodeselected();
            }

            function brush_end() {
                d3.event.target.clear();
                dispatch.nodeselected();
                d3.select(this).call(d3.event.target);
            }



          brush.call(d3.svg.brush()
              .x(x_scale)
                .y(y_scale)
                .on("brushstart", brush_start)
                .on("brush", do_brush)
                .on("brushend", brush_end)
                );

            //var labels = nodegroup.append("text")
            //      .attr("dx", 12)
            //      .attr("dy", "1em")
            //      .text(function(d) { return d.values; })
            //      .attr("style","display:none;");

            getValues = function() {
                // create a new array of the nodevalue attribute on all nodes
                // why can't we just nodes.map()
                _v = [];
                _selection.selectAll(".node").each(function(d, i) {
                    _v.push(d3.select(this).attr('nodevalue'));
                });
                return (_v);
            };

            setValues = function(valuearray) {
                if (valuearray.constructor === Array && valuearray.length === getValues().length) {
                    // check if array was sent, and if so, set Values
                    // debug console.log(valuearray);
                    // loop over all items class node
                    _selection.selectAll(".node").each(
                        function(d, i) {
                            d3.select(this).attr('nodevalue', valuearray[i]);
                            //.select("title").text(valuearray[i]);
                        }
                    );
                    setFillColor();
                }
            };

            // standard css class name for groups
            // css file currently assumes groupId is 1 or 2
            groupClass = function(groupId) {
                return ("group_" + groupId);
            };

            // ### need to use this to toggle filter by detecting current group
            setGroupAppearance = function(n,groupId){
                var className = groupClass(groupId);    
                n.classed(className, true);
                n.style("filter", "url(#colorglow)");
                // n.attr('filter',"url(#colorglow)");                
            };
            
            removeGroupAppearance = function(n,groupID){
              var className = groupClass(groupId);  
              n.classed(className, false);
              var c = n.attr("class")  // need a string here
             
              console.log(/[Gg]roup/.test(c));
              if (/[Gg]roup/.test(c)) {
                  n.style("filter",null);
              }
            };
            
            // add group css class to specific nodes
            setGroupID = function(groupId, nodeArray) {
                // if(nodeArray.constructor === Array){
                var arrayLength = nodeArray.length;
                for (var i = 0; i < arrayLength; i++) { 
                    var n = d3.select("#node_" + nodeArray[i]); 
                    // instead of d3.select, should be grap select to avoid whole DOM search?
                    setGroupAppearance(n,groupId);
                }
            };

            // remove group css class from specific nodes
            removeGroupID = function(groupId, nodeArray) {
                // nodeArray = nodeArray instanceof Array ? nodeArray : [nodeArray]
                var arrayLength = nodeArray.length;
                for (var i = 0; i < arrayLength; i++) {
                    var n = d3.select("#node_" + nodeArray[i]);
                    removeGroupAppearance(n,groupID);
                }

            };

            // remove group_x css call from ALL nodes that have it
            clearGroupID = function(groupId) {
                // get standard class name for groups
                var c = groupClass(groupId);
                // select all nodes with this class, and remove it with D3
                d3.selectAll("." + c).classed(c, false);
                
            };


            setFillColor = function() {
                // set fill color based on value attribute of nodes
                // call this function after setting up the viz, e.g. in render()
                var v = getValues().map(Number); // gets the array of the values as numbers
                var vrange = [d3.min(v), d3.max(v)];
                colorScale = d3.scale.linear()
                    .domain(vrange)
                    .range(nodecolors);
                    
                nodegroup.each(
                    function(d, i) {
                        n = d3.select(this).select(".node");
                        n.style('fill', colorScale(n.attr('nodevalue')));
                        n.style('opacity',opacity_percent);
                    });
            };

            // *** NODE SELECTION FUNCTIONS

            getSelected = function() {
                // get ids OR values of selected nodes;
                _sel = [];
                nodegroup.filter(".selected").each(
                    function(d, i) {
                        _sel.push(d3.select(this).attr('nodeid'));
                    });
                selectedNodes = _sel;
                return (_sel);

            };

            clearSelected = function() {
                nodegroup.classed("selected", false);
                dispatch.nodeselected();
            };

        }); // end of inner function



    } // end of main nodegraph function






    //********* API starts here***************
    nodegraph.render = function() {
        setFillColor();
        force.start();
    };

    nodegraph.w = function(_x) {
        if (!arguments.length) return w;
        w = _x;
        return this;
    };

    nodegraph.h = function(_x) {
        if (!arguments.length) return h;
        h = _x;
        return this;
    };

    nodegraph.resize = function(_w, _h) {
        console.log("resizing from JS with w=" + _w + " and height=" + _h);
        if (!arguments.length) return 0;
        this.w(_w);
        this.h(_h);
        forceresize(_w, _h);
    };

    nodegraph.values = function(newvals) {
        if (!arguments.length) return (getValues());
        // TODO ensure newvals must have same length as nodes, or recycle
        // TODO ensure or convert newvals to array here?
        setValues(newvals);
        return this;
    };

    nodegraph.selected = function() {
        return (getSelected());

    };

    nodegraph.unSelectAll = function() {
        return (clearSelected());
    };

    // may need
    //d3.rebind(nodegraph, this.dispatch, "on");

    nodegraph.selecteddispatch = function() {
        // currently unused
        return ("");
    };

    // GROUP SETTING API
    // THIS SHOULD BE SIMPLIFIED
    // INTERNAL FUNCTIONS ARE setGroupID, clearGroupID, removeGroupID

    nodegraph.group = function(groupId, nodeIds, remove = false) {
        // adds attribute of group ID to nodes if not currently selected
        // clear all previous nodes with attr
       
        if (remove === true) {
            // remove true means remove the group
            if (nodeIds.constructor === Array) {
                // an array of nodes sent, remove just for those
                removeGroupID(groupId, nodeIds);
            } else {
                // no array sent, remove grup from all nodes
                // TODO: allow single node id to be sent
                // as sending a single node id (non-array) will cause group to be removed from all
                clearGroupID(groupId);
            }
            // remove not true so add group
        } else {
            setGroupID(groupId, nodeIds);
        }
    };

    nodegraph.reforce = function() {
        force.nodes(this.nodes.data).links(this.links.data);

    };

    nodegraph.recolor = function() {
        setFillColor();
    };

    nodegraph.reset = function() {
        resetzoom();
    };

    nodegraph.moveleft = function(){
      nudge(-1,0);
    };

    nodegraph.moveright = function(){
      nudge(1,0);
    };


    // origin at top of div, subtract to move up
    nodegraph.moveup = function(){
      nudge(0,-1);
    };

    nodegraph.movedown = function(){
      nudge(0,1);
    };

    nodegraph.zoomin = function(){
      manualzoom(-0.1);
    };

    nodegraph.zoomout = function(){
      manualzoom(0.1);
    };

    nodegraph.shrink = function(){
      changeLinkDistance(-10);
    };

    nodegraph.expand = function(){
      changeLinkDistance(10);
    };
    
    nodegraph.rotate = function(direction){
      direction = (typeof direction !== 'undefined') ?  direction : 1;
      changeRotation(10*direction); // 10 degrees
    };

//    nodegraph.shrinknodes = function(){
//      shrinkNodeSize();
//    }

    dispatch = d3.dispatch("nodeselected");

    add_resetbtn = function(ngname,sel){
        resetbtn = sel.append("div").append("button").text("reset").attr("class","btn");
        resetbtn.attr("onclick",ngname + "reset();");

    };

    return( nodegraph);

};




// example use on an html page with element <div id="container1"></div>
// var nodgeGraph1 = cedar.NodeGraph().w(300).h(300);
// d3.select("#container1")
//    .datum(data1)
//    .call(nodgeGraph1);

// example render in HTMLWidget land
// var renderGraph =  function(el, x, nodegraph) {
//         // nodegraph already initialized on el? might as well re draw
//         var links = HTMLWidgets.dataframeToD3(x.links);
//         var  nodes = HTMLWidgets.dataframeToD3(x.nodes);
//         ng = d3.select(el).call(nodegraph);
//     }
