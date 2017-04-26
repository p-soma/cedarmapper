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
        bottom: 20,
        left: 20
    };

    // starting values, overridden when plot is created
    // eg ng = nodegraph(el).h(500).w(800)
    var w = $(window).width()-margin.left - margin.right;
    var h = $(window).height()-margin.top - margin.bottom;

    var opacity_percent = 0.98,
        node_area_percent = 0.3,
        maxLinkWidth = 8,
        minLinkWidth = 1,
        nodecolors = ['white', 'darkgreen'],
        ForceCharge = -1000,
        LinkDistance = 100,
        linkdistanceFactor = 1,
        nudgefactor = 10,
        scaleFactor = 1,
        translation = [0,0];
        option_textvisible = true;
    
    
    // functions called by public API
    var resetzoom, manualzoom, force, graph, svg, nodeSizeScale, setFillColor,
        getSelected, clearSelected, getValues, setValues, forceresize, nodevalues,
        nudge, rotate, setTransform,
        setGroupID, clearGroupID, removeGroupID; 
    
        var  changeforcecharge,changeLinkDistance; // deprecated and will be removed

    var nodes,nodegroup, lasso;
    
    function nodegraph(_selection) {

        var shiftKey;
        
        _selection.attr("tabindex", 1);

        _selection.each(function(graphdata) {

            // DEBUG
            //console.log(JSON.stringify(graphdata));

            // initial values
            var tx = 0,ty = 0,scale = 1,rotation=0;


            setTransform = function(){ // TO DO USE PARAMETERS h and w
              // for ROTATION only currently; let zoom handle translate and scale
              // uses globals tx,tx,scale and rotation
              var rotationy = h/2;
              var rotationx = w/2;
              var graphtransform = `translate(${tx}, ${ty}) scale(${scale}) rotate(${rotation} ${rotationx} ${rotationy})`;
              var bbox, thisnode;

              // add svg transform to whole graph
              graph.attr('transform', graphtransform );

              // since whole graph is rotated, now counter rotate the text inside nodes
              // but with the bbox center as center of rotation (not graph center)
              if(option_textvisible){
                  nodegroup.each(function(d,i) {
                      thisnode = d3.select(this)
                      bbox = thisnode.node().getBBox();
                           //bounding box of rotated nodegroup, to find center x,y
                      thisnode.attr('transform', `rotate(${-1*rotation}  ${bbox.x+ bbox.width/2} ${bbox.y + bbox.height/2} )`);
                  });
              }
              
              // brush.attr('transform', `rotate(${antirotation}  ${rotationx} ${rotationy})`);
            };


            rotate = function(deg){
              rotation = rotation + deg;
              setTransform();
            };


            /*** Configure zoom behaviour ***/

            // zoomable scales
            var x_scale = d3.scale.linear().domain([0, w ]).range([0, w]);
            var y_scale = d3.scale.linear().domain([0, h ]).range([0, h]);

            var zoombehavior = d3.behavior.zoom()
                .scaleExtent([0.1,10]) //allow 10 times zoom in or out
                .x(x_scale)
                .y(y_scale)
                .on('zoom', semanticzoom);

            function semanticzoom() {
                // debug console.log("zoom", d3.event.translate, d3.event.scale);
                scaleFactor = d3.event.scale;  // set global
                translation = d3.event.translate;  //  set global
                do_tick(); //update positions
            }
            
            function zoom_disable(){
              graph.call(zoombehavior)
                .on("zoom", null)            
                .on("mousedown.zoom", null)
                .on("touchstart.zoom", null)
                .on("touchmove.zoom", null)
                .on("touchend.zoom", null);
                
            }
            
            function zoom_enable(){
                graph.call(zoombehavior).on("zoom",semanticzoom);
            }
            
            resetzoom = function(){
                tx = ty = 0 
                scale = 1
                zoombehavior.scale(1).translate([0, 0])
                zoombehavior.event(graph)
                setTransform()
            }
            



         var drag  = d3.behavior.drag()

         function drag_enable(){
            drag.on("dragstart", dragstarted)
            .on("drag", dragged)
            .on("dragend", dragended);
        }
  
        function drag_disable(){
            drag.on("dragstart", null)
            .on("drag", null)
            .on("dragend", null);
        }
        
        drag_enable();
  

        function dragstarted(d) {
          d3.event.sourceEvent.stopPropagation();
          if (!d.selected && !shiftKey) {
            // if this node isn't selected, then we have to unselect every other node
            nodegroup.classed("selected", function(p) {
              return p.selected = p.previouslySelected = false;
            });
          }

          d3.select(this).classed("selected", function(p) {
            d.previouslySelected = d.selected;
            return d.selected = true;
          });

          nodegroup.filter(function(d) {
              return d.selected;
            })
            .each(function(d) {
              d.fixed |= 2;
            })
        }


        function dragged(d) {
          nodegroup.filter(function(d) {
              return d.selected;
            })
            .each(function(d) {
              d.x += d3.event.dx;
              d.y += d3.event.dy;

              d.px += d3.event.dx;
              d.py += d3.event.dy;
            })

          force.resume();
        }
        
        function dragended(d) {
          //d3.select(self).classed("dragging", false);
          nodegroup.filter(function(d) {
              return d.selected;
            })
            .each(function(d) {
              d.fixed &= ~6;
            })
            dispatch.nodeselected();
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


        // this holds the nodes and links
        var graph = svg.append('g')
            .attr("class", "canvas")
            .attr("id","graph")
            .call(zoombehavior);


        // not sure if this rect is needed but seems to add a glue layer for the lasso
        var rect = graph.append('svg:rect')
            .attr('width', w)
            .attr('height', h)
            .attr('fill', 'transparent')
            .attr('opacity', 0.5)
            .attr('stroke', 'transparent')
            .attr('stroke-width', 1)
            .attr("id", "zrect");
            // .datum(function() {
            //     return {selected: false, previouslySelected: false};
            // });
          // lassoing
          // Create the area where the lasso event can be triggered
          var lasso_area = graph.append("rect")
            .attr("id", "lasso_area")
            .attr("x",-(w)/2)
            .attr("y",-(w)/2)
            .attr("width", 0)
            .attr("height", 0)
            .style('cursor', 'crosshair')
            .style('background', "grey")
            .style("opacity", 0.05);

          function lasso_enable(){
           	lasso_area.attr("width", w*4).attr("height", h*4);
          }

          function lasso_disable() {
          	lasso_area.attr("width", 0).attr("height", 0);
          }

          // Lasso functions to execute while lassoing
          function lasso_start() {
            lasso.items()
              .classed({
                "not_possible": true,
                "selected": false
              }); // style as not possible
          }

          function lasso_draw() {
            // Style the possible nodes
            lasso.items()
              .filter(function(d) {
                return d.possible === true
              })
              .classed({
                "not_possible": false,
                "possible": true
              });

            // Style the not possible nodes
            lasso.items().filter(function(d) {
                return d.possible === false
              })
              .classed({
                "not_possible": true,
                "possible": false
              });
          }

          function lasso_end() {
            // Style the selected dots
            lasso.items().filter(function(d) {
                return d.selected === true
              })
              .classed({
                "not_possible": false,
                "possible": false
              })
              .classed("selected", true);

            // Reset the style of the not selected dots
            lasso.items().filter(function(d) {
                return d.selected === false;
              })
              .classed({
                "not_possible": false,
                "possible": false
              });

              dispatch.nodeselected();
              lasso_disable();

          }

          // Define the lasso GLOBAL
          lasso = d3.lasso()
            .closePathDistance(300) // max distance for the lasso loop to be closed
            .closePathSelect(true) // can items be selected by closing the path?
            .hoverSelect(true) // can items by selected by hovering over them?
            .area(lasso_area)
            .on("start", lasso_start) // lasso start function
            .on("draw", lasso_draw) // lasso draw function
            .on("end", lasso_end); // lasso end function

            // NODE INFORMATIONAL FUNCTIONS
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
                var t =0.3; // coefficient determining node size variation
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
                .linkDistance(maxNodeSize()*2.5)
                .gravity(0.05)
                .charge(ForceCharge)
                .size([w, h])
                .on("tick", do_tick)
                .nodes(graphdata.nodes)
                .links(graphdata.links);

                // other paramters to consider :  or
                  //.friction(0.1)
                //

                      
            graph.call(lasso);
            

            // added for Shiny HTMLWidget; need to determine if useful
            d3.select(window).on('resize', function() {
                window_w = parseInt(_selection.style('width'), 10);
                window_h = parseInt(_selection.style('height'), 10);
                if (window_w < w) {
                    forceresize(window_w, window_h);
                }
            });

            // currently unused
            changeForceCharge = function(z){
              force.charge(force.charge()+z);
              force.start();
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

              if (!d3.event.metaKey) switch (d3.event.keyCode) {
                case 82:  // r
                  rotation = rotation + 10;
                  setTransform();
                  break;
                case 76:  // l
                  rotation = rotation - 10;
                  setTransform();
                  break;
                case 84: // t
                    //  TOGGLE TEXT 
                    if (option_textvisible){
                        option_textvisible = false;
                        removeNodeText();
                    } else {
                        option_textvisible = true
                        addNodeText();
                    };
                    break;
                case 88: // x
                  resetzoom();
                  break;
                case 38:// UP
                  nudge(0, -1);
                  break;
                case 40: // DOWN
                  nudge(0, +1);
                  break;
                case 37: // LEFT
                  nudge(-1, 0);
                  break;
                case 39: // RIGHT
                  nudge(+1, 0);
                  break;
                case 67:  // C KEY triggers 'CENTERING'
                  center_view();
                  break;
                }

              // just shift key
              shiftKey = d3.event.shiftKey || d3.event.metaKey;

              if (shiftKey) {
                  zoom_disable();
                  // drag_disable();
                  lasso_enable();
              }
            }
            
            function keyup() {
              shiftKey = d3.event.shiftKey || d3.event.metaKey;
              if (d3.event.keyCode == 16) { // || d3.event.metaKey;
                // ctrlKey = d3.event.ctrlKey;
                lasso_disable();
                drag_enable();
                zoom_enable();
              }

            }

           nudge= function(dx, dy) {
                //. nudgefactor global config var
                // tx = tx + dx * nudgefactor;
                // ty = ty + dy * nudgefactor;
                // setTransform();
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


                // NODES are circles and texts in nodegroup (g).  Both must be moved around at each tick
                nodes.attr("cx", function (d) {
               //return(d.x)
                    return translation[0] + scaleFactor*d.x;
                    })
                    .attr("cy", function (d) {
                        return translation[1] + scaleFactor*d.y;
                    });
                    
                    
                if (option_textvisible){
                    nodetexts
                    .attr("x", function (d) { 
                        return translation[0] + scaleFactor*d.x;
                    })
                    .attr("y", function (d) {
                        return translation[1] + scaleFactor*d.y;
                    });
                    
                }
                
                setTransform();

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

            // attempt to reduce color saturation to indicate selection
            // currently not used
             var greyFilter = defs.append("filter").attr("id","greyfilter")
              .html(  '    <feColorMatrix type="matrix" values="0.3333 0.3333 0.3333 0 0'+
                      '                                         0.3333 0.3333 0.3333 0 0'+
                      '                                         0.3333 0.3333 0.3333 0 0'+
                      '                                         0      0      0      1 0"/>');

            // filter helps to indicate group affiliation; glows color or stroke only

            var glowFilter = defs.append("filter").attr("id","colorglow");
            glowFilter.append("feGaussianBlur")
                  .attr("stdDeviation","3.5")
	                .attr("result","coloredBlur");
            var feMerge = glowFilter.append("feMerge");
            feMerge.append("feMergeNode")
                      .attr("in","coloredBlur");
            feMerge.append("feMergeNode")
                      .attr("in","SourceGraphic");

            var grayScaleFilter = defs.append("filter")
                .attr("id","grayscale")
                .append("feColorMatrix")
                  .attr("type","saturate")
                  .attr("values","5");


            // example use
            //         <something filter="url(#dropshadow)"/>
            // or <something style="filter: url(#dropshadow)">
            //var filter1 = defs.append("filter").attr("id", "group1filter").attr("height", "130%");
            //filter1.append("feGaussianBlur")
            //    .attr("in", "SourceAlpha")
            //    .attr("stdDeviation", 10) //THIS IS THE SIZE OF THE GLOW
            //    .attr("result", "blur");

            // STYLES ADDED INSIDE THIS SVG ELEMENT
            // COULD BE INSIDE THE CONTAINING  HTML PAGE ALSO
            // OR COULD BE REF'D IN THE CSS AS url("nodegraph.html#grayscale")

            var styles = svg.append('style')
              .html('.selected { filter: url("#colorglow");}');


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
                .call(drag);
                // .on("click", function() {
                //     d3.select(this).classed("selected", !d3.select(this).classed("selected"));
                //     dispatch.nodeselected();
                // })
               


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

                if( option_textvisible) {
                    addNodeText();
                };

                function addNodeText(){
                    nodetexts = nodegroup.append("text")
                        .filter(function(d) { return d.size > 1 })
                        .attr("class", "nodetext")
                        .attr("text-anchor", "middle")
                        .attr("pointer-events","none")                        
                        .text(function(d) {return d.size; });
                    do_tick();                    
                }
                
                function removeNodeText(){
                    graph.selectAll('.nodetext').remove();
                    nodegroup.each(function(d,i) { d3.select(this).attr('transform', null); });
                    do_tick();
                }

           lasso.items(nodegroup);
           
          // DISABLE BRUSH
           
            // ***** BRUSH FUNCTIONS
            // note this brush layer in the svg element must be defined before the
            // nodegroup elements, or it covers the nodes and they can't be selected
            // possible alternative  to explore is to set the z-layers
            // TODO delete these brush functions as they are not used by lasso
            // function brush_start(d) {
//                 // first, save previously selected IF shift+mouse
//                 nodegroup.each(function(d) {
//                     d.previouslySelected = shiftKey && d.selected;
//                 });
//                 graph.selectAll(".node").classed({'unbrushed':false});
//
//             }
//
//             function do_brush(d) {
//                 var extent = d3.event.target.extent();
//                 nodegroup.classed("selected", function(d) {
//                     return d.selected = d.previouslySelected ||
//                         (extent[0][0] <= d.x && d.x < extent[1][0] &&
//                             extent[0][1] <= d.y && d.y < extent[1][1]);
//                 });
//                 // update selection during brush
//                 dispatch.nodeselected();
//             }
//
//             function brush_end() {
//                 d3.event.target.clear();
//                 dispatch.nodeselected();
//                 d3.select(this).call(d3.event.target);
//             }


          // TODO delete this section
          // brush.call(d3.svg.brush()
          //     .x(x_scale)
          //       .y(y_scale)
          //       .on("brushstart", brush_start)
          //       .on("brush", do_brush)
          //       .on("brushend", brush_end)
          //       );


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

            otherGroup = function(groupId){
              // very naive function to return other group
              if(groupId === 1) return 2;
              return 1;

            };

            // ### need to use this to toggle filter by detecting current group
            setGroupAppearance = function(n,groupId){
                var className = groupClass(groupId);
                n.classed(className, true);
                n.style("filter", "url(#colorglow)");
                // n.attr('filter',"url(#colorglow)");
            };

            removeGroupAppearance = function(n,groupID){
              n.classed(groupClass(groupId), false);
              var c = n.attr("class") ; // need a string here

              // loop through n (node list) and remove filter
              var otherClassname =  groupClass(otherGroup(groupId));

              n.style("filter",function(d,i){
                return ( this.classed(otherClassname) ? "" : this.style("filter"));
              });

            };

            // add group css class to specific nodes
            setGroupID = function(groupId, nodeArray) {
                // if(nodeArray.constructor === Array){
                var arrayLength = nodeArray.length;
                for (var i = 0; i < arrayLength; i++) {
                    var n = d3.select("#node_" + nodeArray[i]);
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

    nodegraph.lasso = function() { return lasso;};
    
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

    nodegraph.group = function(groupId, nodeIds, remove) {
        // adds attribute of group ID to nodes if not currently selected
        // clear all previous nodes with attr

        if (arguments.length < 3) { remove = false; }


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
        // doesn't work
        force.nodes(nodes.data).links(links.data);

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

    nodegraph.rotate = function(direction){
      direction = (typeof direction !== 'undefined') ?  direction : 1;
      rotate(10*direction); // 10 degrees
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
