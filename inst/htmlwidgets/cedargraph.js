HTMLWidgets.widget({

  name: "cedarGraph",
  type: 'output',
  factory: function(el, w=500,h=500) {
    // build object on DOM element 'el' and set size
    ng = cedar.NodeGraph(el);
    ng.w(w);
    ng.h(h);
    // change element to have same size as parameters
    d3.select(el).style({"width":w,"height":h});


    // function buttons
      function btn(btnname,text, icon,action){
        b = `<button type="button" id = "${btnname}" class="btn btn-info btn-sm" onclick="${action};"><span class="glyphicon ${icon}"></span>${text}</button>`;
        return(b);
      }


      btngroup =  $( "<div class='btn-group' id='graph-btns'></div>" );


      $(el).before(btngroup);
     //  btngroup.addClass("btn-group");
     // recenter button
      // btngroup.append(btn('recenter','recenter','','ng.reset();'));
      // NUDGE BUTTONS
      // var a  = ["left", "right","up", "down"];     
      // for (var i = 0; i < a.length; i++) {
      //   btngroup.append(btn(`move${a[i]}`,'', `glyphicon-arrow-${a[i]}`,`ng.move${a[i]}()`));
      // }
      btngroup.append(
        btn('attractplus','','glyphicon-resize-full','ng.expand()')
        );
      btngroup.append(
        btn('attractmius','','glyphicon-resize-small','ng.shrink()')
          );

      btngroup.append(
        btn('rotate','','glyphicon-repeat','ng.rotate(1)')
        );
        
      btngroup.append(
        btn('rotateleft','','flipped glyphicon-repeat','ng.rotate(-1)')
        );


    // TODO: test if SHINY shinyMode
    // requires this in your shiny app:
    // session$sendCustomMessage(type='nodevalues',message=newdata )}
    Shiny.addCustomMessageHandler("nodevalues",
      function(message) {
        console.log(message);
        ng.values(message); }
    );

    Shiny.addCustomMessageHandler("setgroup1",
    // message is an array of node IDs, set in R/Shiny code
      function(message){
        ng.group(1,message);
        ng.unSelectAll();
      }
    );

    Shiny.addCustomMessageHandler("removefromgroup1",
    // message is an array of node IDs, set in R/Shiny code
      function(message){
        ng.group(1,message, remove=true);
        ng.unSelectAll();
      }
    );

    Shiny.addCustomMessageHandler("setgroup2",
    // message is an array of node IDs, set in R/Shiny code
      function(message){
        ng.group(2,message);
        ng.unSelectAll();
      }
    );

    Shiny.addCustomMessageHandler("removefromgroup2",
    // message is an array of node IDs, set in R/Shiny code
      function(message){
        ng.group(2,message, remove=true);
        ng.unSelectAll();
      }
    );

    Shiny.addCustomMessageHandler("unsetgroup", function(message){
      // message is the group ID
      // remove all group from all nodes
        ng.group(message,"",remove=true);
      }
    );

    return {
      renderValue: function(x) {

          // hook to shiny, update node values

          // first, remove any svg on this element so ng can add one
          // TODO: remove SVG with ID of this type
          // d3.select(el).select("svg").select("#nodegraph").remove();
          // OR let the ng remove itself with some new method ng.clear()
          d3.select(el).select("svg").remove();

          var  nodedata = {};
          nodedata.links = HTMLWidgets.dataframeToD3(x.links);
          nodedata.nodes = HTMLWidgets.dataframeToD3(x.nodes);

          // add data to window for debugging
          window.nodedata = nodedata;
          // TODO options = x.options


          d3.select(el).datum(nodedata).call(ng);

          ng.render();
          // add d3 graph object to window for debugging
          window.ng = ng;
      },

      resize: function(w,h) {
        console.log("resizing from R with w,="+w +","+h);
        // element size - needed?
        d3.select(el).attr("width", w).attr("height", h);
        // svg size - this is also done in the ng instance - needed?
        d3.select(el).select("svg").attr("width", w).attr("height", h);
        // instance size method
        ng.resize(w,h);
      },

      //WIP
      setValues: function(newvalues){
          ng.values(newvalues);
      },
      cg:ng

    };
  }
});



// note
// the nodegraph.js javascript object includes this line:
//    Shiny.onInputChange("nodelist", nodelist);
// that is captured by the Shiny server.R as input$nodelist
// the goal is to move that out of nodegraph.js to keep it shiny-agnostic,
// and in this widget JS code only
// by accessing the D3 dispatch attached to the ng object
//if (HTMLWidgets.shinyMode){
   // could also add code to accept custom data from shiny app
   // e.g. color options
   // Shiny.addCustomMessageHandler('someShinyData', function(data) {
   //   console.log("got this from shiny");
   //   console.log(data);
   // });
// }
