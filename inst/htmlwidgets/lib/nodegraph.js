cedar = {};
var selectedNodes;

cedar.NodeGraph = function module() {
    // starting values, overridden when plot is created
    var margin = 10,
        w = $(window).width()-margin,
        h = $(window).height()-margin,
        opacity_percent = 0.98,
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

    // functions called by API
    var resetzoom, manualzoom, force, graph, svg, nodeSizeScale, setFillColor,
        getSelected, clearSelected, getValues, setValues, forceresize, nodevalues,
        nudge, rotate, setTransform, setGroupID, clearGroupID, removeGroupID, 
        nodes,nodegroup, lasso;
    
    function nodegraph(_selection) { 
     _selection.attr("tabindex", 1);
     _selection.each(function(graphdata) { 
         // this structure allows d3.select(el).datum(nodedata).call(ng);
         
            // initial values
        var shiftKey;
        var tx = 0,ty = 0,scale = 1,rotation=0;
        
        // *** MAIN SVG ELEMENT ADDED TO _selection
        svg = d3.select(this)  // or _selection ?
            .on("keydown", keydown)
            .on("keyup", keyup)
            .classed("svg-container", true)
            .each(function() { this.focus(); })
            .append("svg")
                .attr("id", "nodegraph")
                .attr("height", h)
                .attr("width", w);


        //////// graph holds lasso, zoom, nodes and links
        var graph = svg.append('g')
            .attr("class", "canvas")
            .attr("id","graph");


        setTransform = function(){ // TO DO USE PARAMETERS h and w
          // for ROTATION only currently; let zoom handle translate and scale
          // uses globals tx,tx,scale and rotation
          var rotationy = h/2, rotationx = w/2;
          var graphtransform = `translate(${tx}, ${ty}) scale(${scale}) rotate(${rotation} ${rotationx} ${rotationy})`;
          var bbox, thisnode;
          // add svg transform to whole graph
          graph.attr('transform', graphtransform );

          // since whole graph is rotated, now counter rotate the text inside nodes
          // but with the bbox center as center of rotation (not graph center)
          // this is expensive, so only do when text is showing
          if(option_textvisible){
              nodegroup.each(function(d,i) {
                  thisnode = d3.select(this)
                  bbox = thisnode.node().getBBox();
                       //bounding box of rotated nodegroup, to find center x,y
                  thisnode.attr('transform', `rotate(${-1*rotation}  ${bbox.x+ bbox.width/2} ${bbox.y + bbox.height/2} )`);
              });
          }
        };

        //  rotation api is this simple:  ng.rotate(10)
        rotate = function(deg){
          rotation = rotation + deg;
          setTransform();
        };

        ////// D3 ZOOOOOOOM

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
            // redraw all text on zoom, so that small nodes text is not displayed
            if( option_textvisible ){
                removeNodeText();
                addNodeText();
            }
            
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
            tx = ty = 0 ;
            scale = 1;
            zoombehavior.scale(1).translate([0, 0]);
            zoombehavior.event(graph);
            setTransform();
        };
        
        graph.call(zoombehavior);  // add zoom to graph

        ////////////////// DRAG
        var drag  = d3.behavior.drag();

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
          
        function dragstarted(d) {
           // when dragging node, don't zoom (drag whole graph)
           d3.event.sourceEvent.stopPropagation();
            
          // click to drag, so do click behavior when dragging
          // TODO move to click()
          d3.select(this).classed("fixed", d.fixed = true);
          
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

          // once selecting one, review all the rest
          // currently disabled
          nodegroup
              .filter(function(d) {return d.selected; })
              .each(function(d) {
                  // d.fixed |= 2;
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

        /// START WITH DRAG ENABLED
        drag_enable();
        
        /////////////// END DRAG

        //////////////////////// LASSO
        // staging area for lasso (but doesn't contain lasso)
        var rect = graph.append('svg:rect')
            .attr('width', w)
            .attr('height', h)
            .attr('fill', 'transparent')
            .attr('opacity', 0.5)
            .attr('stroke', 'transparent')
            .attr('stroke-width', 1)
            .attr("id", "zrect");

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

        function lasso_start() {
            lasso.items()
              .classed({
                "not_possible": true,
              }); 
          }

          function lasso_draw() {
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
              }).each(function(d){
                  d3.select(this).classed({
                    "not_possible": false,
                    "possible": false
                  })
                  .classed("selected", true)
                  .classed("fixed", d.fixed = true)
            });

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

        ///////// END LASSO

        ////// **** NODE SIZE CALCULATIONS
        getWindowArea  = function(){
             return((h -margin) * (w - margin));
         };
                
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

        ///////// SELECTION EVENT MANAGEMENT
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
            .gravity(0.1)
            .charge(ForceCharge)
            .size([w, h])
            .on("tick", do_tick)
            .nodes(graphdata.nodes)
            .links(graphdata.links);

        // TODO why is this in this point, after force?
        graph.call(lasso);

        ///////// HTMLWIDGET INTERFACE
        d3.select(window).on('resize', function() {
            window_w = parseInt(_selection.style('width'), 10);
            window_h = parseInt(_selection.style('height'), 10);
            if (window_w < w) {
                forceresize(window_w, window_h);
            }
        });

        forceresize = function(_w, _h) {
            w = _w, h = _h;
            svg.attr("width", _w).attr("height", _h);
            force.size([w, h]).resume();
        };


        /////// KEYBOARD INTERFACE
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
                    showNodeText();
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
                nodes
                    .attr("cx", function (d) {
                      return translation[0] + scaleFactor*d.x; })
                    .attr("cy", function (d) {
                        return translation[1] + scaleFactor*d.y;})
                    .attr("r",function(d){
                        if (scaleFactor < 1) {
                          // console.log( d.size * scaleFactor);
                          // console.log('r orig', nodeSizeScale(d.size * scaleFactor));
                          r = nodeSizeScale(d.size) * scaleFactor;
                          // console.log( 'r', r);
                        }
                        else {
                          r = nodeSizeScale(d.size);
                        }
                        return r;                     
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


        /////////// STYLES AND FILTERS 
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
              .attr("stdDeviation","5")
                .attr("result","coloredBlur");
          var feOffset = glowFilter.append("feOffset");
          glowFilter.append("feBlend")
             .attr("in","SourceGraphic" )
             .attr("in2","offOut")
             .attr("mode","normal" );
          feOffset.attr("result","offOut").attr("in","SourceGraphic").attr("dx","10").attr("dy","10");
        
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

        // add the filters we use as styles  can be overriden, combined with other CSS
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
            .on("dblclick",function(d) {
                d3.event.stopPropagation();
                d.fixed = false;
                d3.select(this)
                  .classed("selected",d.selected = false )
                  .classed("fixed",false)
                  .style("filter", "");
              })
            .on("click", function(d) {
                if (d3.event.defaultPrevented) {return(false)};
                if (!shiftKey) {
                    //if the shift key isn't down, unselect everything
                    d3.select(this).classed("selected", function(p) { 
                      return p.selected =  p.previouslySelected = false; });
                }

                // always select this node
                d.fixed = true;
                d3.select(this)
                  .classed("selected", d.selected = !d.previouslySelected)
                  .classed("fixed",true)
                  .style("filter", "url(#colorglow)");
            });
            
            // .on("click", function() {
            //     d3.select(this).classed("selected", !d3.select(this).classed("selected"));
            //     dispatch.nodeselected();
            // })
            
       nodegroup.call(drag); // ADD DRAG BEHAVIOR TO NODEGROUP nodes+text


       nodes = nodegroup
            .append("circle")
            .attr("class", "node")
            .attr("id", function(d) {
                return "node_" + d.name;
            })
            .attr("r", function(d) {                    
                return nodeSizeScale(d.size);
            })
            .attr("nodevalue", function(d) {
                return d.values;
            })
            .attr("size", function(d) {
                return d.size;
            });

       // now that nodes are defined, can tell lasso to include them
       lasso.items(nodegroup);
            
       
        
        ////////////// NODE LABELS/TEXT
        // can be toggled as text in nodegroups requiring manual counter-rotation
        // is expensive
        var nodesize_threshold = 12;
        
        function addNodeText(){
            nodetexts = nodegroup
                .filter(function(d){ // exclude nodes below certain size  
                    node_radius = d3.select(this).select('circle').attr('r');
                    return node_radius > nodesize_threshold  
                })
            .append("text")
                .filter(function(d) { return d.size > 1 })
            .attr("class", "nodetext")
            .attr("text-anchor", "middle")
            .attr("pointer-events","none")                        
            .text(function(d) {return d.size; });  ////////  use d.value here
        }
              
        function removeNodeText(){
        // triggered by UI, clears text from nodes
            graph.selectAll('.nodetext').remove();
            nodegroup.each(function(d,i) { d3.select(this).attr('transform', null); });
            do_tick();
        }
          
        function showNodeText(){ // triggered by UI (eg 't' key)
            // build text and tick 
                addNodeText()
                do_tick();
        }
                
        // add text when starting but don't tick
        addNodeText();


        //////////  NODE VALUES and GROUPING API 
        // used for coloring nodes
        getValues = function() {
            // create a new array of the nodevalue attribute on all nodes
            // why can't we just nodes.map()
            _v = [];
            _selection.selectAll(".node").each(function(d, i) {
                _v.push(d3.select(this).attr('nodevalue'));
            });
            return (_v);
        };

        // set values AND color, called by HTMLWidget
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
        };

        removeGroupAppearance = function(n,groupID){
          n.classed(groupClass(groupId), false);
          var c = n.attr("class") ; // need a string here

          // loop through n (node list) and remove filter
          var otherClassname =  groupClass(otherGroup(groupId));

          //n.style("filter",function(d,i){
          //  return ( this.classed(otherClassname) ? "" : this.style("filter"));
          //});

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


    }); /////// end of inner function
  
    } // end of main nodegraph function


    ///////////// API available to HTMLWidget, buttons, etc
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

    nodegraph.nodes = function(){
      return nodes;
    }

    nodegraph.nodeSizeScale = function(){
      return nodeSizeScale;
    
    }

    dispatch = d3.dispatch("nodeselected");

    // NOT USED
    add_resetbtn = function(ngname,sel){
        resetbtn = sel.append("div").append("button").text("reset").attr("class","btn");
        resetbtn.attr("onclick",ngname + "reset();");

    };

    return( nodegraph);
};



//var shinyMode = window.HTMLWidgets.shinyMode =
//      typeof(window.Shiny) !== "undefined" && !!window.Shiny.outputBindings;




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
