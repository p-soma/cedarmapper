// add buttons to nodegraph ; SAVED - not currently used in CEDAR
// requires jquery and nodegraph.js

function nodegraph_buttons(_el) {
    var btngroup = $("<div class='btn-group' id='graph-btns'></div>");
    btngroup.addClass("btn-group");
    
    $(_el).before(btngroup);

    var btn = function(btnname, text, icon, action) {
        b = `<button type="button" id = "${btnname}" class="btn btn-info btn-sm" onclick="${action};"><span class="glyphicon ${icon}"></span>${text}</button>`;
        return (b);
    }

    var addRecenter = function(e) {
        e.append(btn('recenter', 'recenter', '', 'ng.reset();'));

    }

    var addNudges = function(e) {
        var a = ["left", "right", "up", "down"];
        for (var i = 0; i < a.length; i++) {
            e.append(btn(`move${a[i]}`, '', `glyphicon-arrow-${a[i]}`, `ng.move${a[i]}()`));
        }
    }

    var addExppand = function(e) {
        e.append(
            btn('attractplus', '', 'glyphicon-resize-full', 'ng.expand()'));
    }

    var addShrink = function(e) {
        e.btngroup.append(
            btn('attractmius', '', 'glyphicon-resize-small', 'ng.shrink()')
        );
    }

    var addRotateRight = function(e) {
        e.append(
            btn('rotate', '', 'glyphicon-repeat', 'ng.rotate(1)')
        );
    }

    var addRotateLeft = function(e) {
        e.append(
            btn('rotateleft', '', 'flipped glyphicon-repeat', 'ng.rotate(-1)')
        );
    }


}

 
var nb = nodegraph_buttons(el);
nb.addR