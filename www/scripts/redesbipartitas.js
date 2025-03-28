//-----------------------------------------------------------------------------
// 
// Module       : redesbipartitas.js
// Authors      : Juan Manuel Garcia Santi
//                Javer Garcia Algarra
// Description  : User interaction with ziggurat and bipartite plots
//
//-----------------------------------------------------------------------------

// Function called on window load
function windowLoad() {
    
    // indica al servidor que el cliente esta listo
    Shiny.onInputChange("windowLoad", new Date());
       
    // Enable zoom buttons on load
    enableZoomButtons();

    // actualiza los tooltips de ayuda
    //updateHelpTooltips();
}

var plotData;
var fp;
var incfont = 1.3;      // Increase font size of marked nodes
// establece los tooltips de ayuda de todos los elementos
var helpTooltips=[
    {id: "zoomin",      value: "Ziggurat zoom in"},
    {id: "zoomout",     value: "Ziggurat zoom out"},
    {id: "zoomfit",     value: "Ajustar el ziggurat a la ventana de visualización"},
    {id: "zoomreset",   value: "Visualizar el ziggurat en su tamaño real"}
];
function updateHelpTooltips() {
    for (var i=0;i<helpTooltips.length;++i) {
        $("#" + helpTooltips[i].id).each(function() {
            $(this).qtip({
                content:    {text: helpTooltips[i].value},
                style:      {classes: "qtip-bootstrap rbtooltiphelp"},
                position:   {my: "bottom right", at: "top center"}
            });
        });
    }
}

//actualiza los eventos asociados a todos los elementos del SVG
function updateSVGEvents(plottype) {
    let plotData;
    let idsvg;
    if (plottype=="ziggurat")
        idsvg = "#ziggurat";
    else
        idsvg = "#bipartite"
    if (plottype == "ziggurat")
        plotData = zigguratData;
    else // if (plottype == "bipartite")
        plotData = bipartiteData;
    // actualiza los eventos asociados a las etiquetas
    updateNodeEvents(plottype,plotData);
    // actualiza los eventos asociados a los enlaces
    updateLinkEvents();
    // actualiza los tooltips
    updateNodeTooltips(plottype,plotData);
    // inicializa el scroll mediante "drag"
    $(idsvg).dragscrollable();
    // inicializa el scroll
    $(idsvg).perfectScrollbar({scrollXMarginOffset:4, scrollYMarginOffset:4});
    $(idsvg+"NodesDetail").perfectScrollbar({scrollXMarginOffset:16, scrollYMarginOffset:4});
    // almacena la informacion del tamaño del SVG
    //svgZoomStore(idsvg);

    // establece el SVG a su tamaño real
    //svgZoomFit(idsvg);
    let lstyle;
    if (plottype==='ziggurat')
        lstyle = 'ziggurat'
    else
        lstyle = 'bipartite'
    // Get the SVG element

    const svgplot = document.getElementById('svgplot'+lstyle);
    
    // Get the current transform attribute value (if any)
    let transform = svgplot.getAttribute('transform');
    let trv = getTranslateValues(transform);
    if (trv.vert)
        setTranslateValues(0,trv.y,svgplot,lstyle);
    transform = svgplot.getAttribute('transform');
    trv = getTranslateValues(transform);
}

function svgMoveHoriz(jump,style) {
    let lstyle;
    if (style==='ziggurat')
        lstyle = 'ziggurat'
    else
        lstyle = 'bipartite'
    svgplot = document.getElementById('svgplot'+lstyle);
    transform = svgplot.getAttribute('transform');
    trv = getTranslateValues(transform);
    console.log("Translate values",trv.x,trv.y,trv.vert)
    if (!trv.vert)
        trv.x = trv.x + jump;
    else{
        trv.y = trv.y - jump;
        trv.x = 0;
    }
    setTranslateValues(trv.x,trv.y,svgplot,lstyle);
    transform = svgplot.getAttribute('transform');
    trv = getTranslateValues(transform);
}


// actualiza el scroll de los detalles de los nodos del ziggurat
function updateZigguratNodesDetailScroll() {
    $("#zigguratNodesDetail").perfectScrollbar("update");
}

// actualiza los eventos asociados a los nodos del SVG
function updateNodeEvents(plottype,plotData) {

    for (var i=0;i<plotData.ids.length;++i) {
        var guild       = plotData.ids[i];
        var guildName   = plotData.names[i];
        var guildData   = plotData.data[guild];
        for (var kcore=1;kcore<=guildData.length;++kcore) {
            var pattern=plottype+"kcore" + kcore + "-" + guild;
            if (kcore==1)
                pattern=plottype+"edge-kcore" + kcore + "-" + guild;
            // estilo del cursor
            $("[id*=" + pattern + "]").css("pointer", "pointer");
            // eventos para resaltar un nodo y los asociados
            $("[id*=" + pattern + "]").click(function() {
                markRelatedNodes($(this).attr("id").replace("-text", "").replace("-rect",""),plottype,plotData);
            });
            // datos asociados al nodo
            $("rect[id*=" + pattern + "]").each(function() {
                var id=$(this).attr("id").replace("-rect", "");
                $(this).data("guild", guild);
                $(this).data("kcore", kcore);  
                $(this).data("nodeIds", getNodeIds($("#" + id + "-text").find("tspan").toArray()));
                $(this).data("marked", false);

            });
           
        }
    }
}

// actualiza los eventos asociados a los enlaces del SVG
function updateLinkEvents() {
    var pattern="link";
    // estilo del cursor
    $("g[id*=" + pattern + "]").css("pointer","pointer");

    // eventos
    $("g[id*=" + pattern + "]").mouseover(function() {
        var strokeWidth=parseFloat($(this).css("stroke-width"));
        $(this).css("stroke-width", strokeWidth*2);

    });
    $("g[id*=" + pattern + "]").mouseout(function() {
        var strokeWidth=parseFloat($(this).css("stroke-width"));
        $(this).css("stroke-width", strokeWidth/2);
    });
}

// actualiza los tooltips de los nodos
function updateNodeTooltips(plottype, plotData) {
    function textToolTipkcore1() {
        var nodeIds=$(this).data("nodeIds");
        var id=$(this).attr("id").replace("-rect", "");
        $("[id*=" + id + "]").each(function() {
            $(this).qtip("destroy", true);
            $(this).qtip({
                content:    {text: getTooltipContent(guildName, kcore, guildCoreData, nodeIds)},
                style:      {classes: "qtip-bootstrap rbtooltipinfo", width: 500},
                show:       {delay:50},
                hide:       {delay:0},
                position:   {my: "bottom left", at: "top left", target: "mouse"}
            });
        });
    }

    for (var i=0;i<plotData.ids.length;++i) {
        var guild       = plotData.ids[i];
        var guildName   = plotData.names[i];
        var guildData   = plotData.data[guild];
        for (var kcore=1;kcore<=guildData.length;++kcore) {
            // tooltips
            var pattern=plottype+"kcore" + kcore + "-" + guild;
            if (kcore==1)
                pattern = plottype+"edge-kcore" + kcore + "-" + guild;
            var guildCoreData=guildData[kcore-1];
            // Tail nodes
            if (guildCoreData!=null)
                $("rect[id*=" + pattern + "]").each(textToolTipkcore1);
                
        }
    }
}

// resalta el nodo indicado en el SVG
function markNode(nodeId,plottype) {
    // marca el texto

    if (nodeId.indexOf(plottype) == -1){
        console.log("bye")
        return;
    }
    $("#" + nodeId + "-text").each(function() {
        // incrementa la fuente
        var fontSize=parseInt($(this).css("font-size").replace("px",""));
        $(this).css("font-size", (fontSize*incfont) + "px");
        $(this).css("font-style", "italic");
        $(this).css("font-weight", "bold");
    });

    // marca el nodo
    $("#" + nodeId + "-rect").each(function() {
        // incrementa el borde
        var strokeWidth=parseFloat($(this).css("stroke-width"));
        $(this).css("stroke-width", strokeWidth+2);

        // indica que el nodo esta marcado
        $(this).data("marked", true);
    });
}

// elimina el resaltado del nodo indicado en el SVG
function unmarkNode(nodeId,plottype) {
    // desmarca el texto
    $("#" + nodeId + "-text").each(function() {
        // reduce la fuente
        var fontSize=parseInt($(this).css("font-size").replace("px",""));
        $(this).css("font-size", (fontSize/incfont) + "px");
        $(this).css("font-style", "normal");
        $(this).css("font-weight", "normal");
    });

    // desmarca el nodo
    $("#" + nodeId + "-rect").each(function() {
        // reduce el borde
        var strokeWidth=parseFloat($(this).css("stroke-width"));
        $(this).css("stroke-width", strokeWidth-2);

        // indica que el nodo no esta marcado
        $(this).data("marked", false);

    });
}

// resalta el enlace indicado en el SVG
function markLink(linkId,plottype) {
    $("g[id*=" + linkId + "]").each(function() {
        plottype === 'ziggurat' ? fp = 1 : fp = 50;
        // incrementa el ancho del enlace
        var strokeWidth=parseFloat($(this).css("stroke-width"));
        $(this).css("stroke-width", strokeWidth);//+2*fp);

        // indica que el enlace esta marcado
        $(this).data("marked", true);
        $(this).css("stroke-dasharray",`${3*fp},${1*fp}`);
    });
}

// elimina el resaltado del enlace indicado en el SVG
function unmarkLink(linkId,plottype) {
    plottype === 'ziggurat' ? fp = 1 : fp = 50;
    $("g[id*=" + linkId + "]").each(function() {
        // reduce el ancho del enlace
        var strokeWidth=parseFloat($(this).css("stroke-width"));
        $(this).css("stroke-width", strokeWidth-2*fp);

        // indica que el enlace no esta marcado
        $(this).data("marked", false);
        $(this).css("stroke-dasharray",`${5*fp},0`);
    });
}

// resalta el nodo, los nodos relacionados y los enlaces que les unen
function markRelatedNodes(nodeId,plottype,plotData) {

    var svg             = $("#"+plottype+" svg");
    var node            = $("rect[id*=" + nodeId + "]");
    var nodeIds         = node.data("nodeIds");
    var guild           = node.data("guild");
    var marked          = node.data("marked");
    var markedNodes     = [];
    var markedNodesData = [];
    var neighbors       = (plotData.neighbors[guild])[nodeIds[0]-1];
    if (!$.isArray(neighbors)) {
        neighbors=[neighbors];
    }
    // si el nodo estaba marcado solo deshace el marcado de todos los nodos, si no lo
    // estaba desmarca los nodos anteriores y marca los nuevos
    var nodes=$("rect[id*=kcore]");
    // desmarca todos los nodos y enlaces marcados
    nodes.each(function() {
        if ($(this).data("marked")) {
            unmarkNode($(this).attr("id").replace("-rect", ""),plottype);
        }
    });
    $("g[id*=link-]").each(function() {
        if ($(this).data("marked")) {
            unmarkLink($(this).attr("id"),plottype);
        }
    });

    // elimina los nodos marcados
    Shiny.onInputChange("markedNodesData", new Date());

    if (!marked) {
        // busca los nodos que son vecinos y los marca
        nodes.each(function() {
            // si es del guild contrario comprueba si el contenido es alguno de los vecinos
            if ((typeof $(this).data("guild")!="undefined") && $(this).data("guild")!=guild) {
                var i=0;
                var isNeighbor=false;
                var nodeIds2=$(this).data("nodeIds");
                while (i<neighbors.length && !isNeighbor) {
                    if ($.inArray(neighbors[i], nodeIds2)!=-1) {
                        isNeighbor=true;
                    }
                    ++i;
                }

                // si es vecino y del mismo tipo de gráfico lo marca
                if ((isNeighbor) && (($(this).attr("id")).indexOf(plottype)==0)){
                    markNode($(this).attr("id").replace("-rect", ""),plottype);
                    markedNodes.push($(this).attr("id").replace("-rect", ""));
                }
            }
        });

        // marca el nodo seleccionado
        markNode(node.attr("id").replace("-rect", ""),plottype);
        markedNodes.push(node.attr("id").replace("-rect", ""));

        // comprueba los enlaces que intersectan
        $("g[id*=link-] path").each(function() {
           var intersect=pathIntersectRect($(this)[0], node[0]);
           if (intersect) {
               markLink($(this).parent().attr("id"),plottype);
           }
        });

        // obtiene la informacion de los nodos marcados
        for (var i=0;i<markedNodes.length;++i) {
            var markedNode      = $("#" + markedNodes[i] + "-rect");
            var markedNodeData  = {};
            markedNodeData[(i+1)]={
                guild:      markedNode.data("guild"),
                kcore:      markedNode.data("kcore"),
                nodeIds:    markedNode.data("nodeIds"),
                plottype:   plottype
            };
            markedNodesData.push(markedNodeData);
        }
        // notifica los nodos marcados
        Shiny.onInputChange("markedNodesData", markedNodesData);
    }
}

// comprueba si un path del SVG intersecta con un rectangulo
function pathIntersectRect(path, rect) {
    var pp1     = path.getPointAtLength(0);
    var pp2     = path.getPointAtLength(path.getTotalLength())
    var margin  = 1;
    var pr1     = {x:rect.x.baseVal.value-margin, y:rect.y.baseVal.value-margin};
    var pr2     = {x:rect.x.baseVal.value+rect.width.baseVal.value+margin, y:rect.y.baseVal.value+rect.height.baseVal.value+margin};
    var result  = (pr1.x<pp1.x && pp1.x<pr2.x && pr1.y<pp1.y && pp1.y<pr2.y) || (pr1.x<pp2.x && pp2.x<pr2.x && pr1.y<pp2.y && pp2.y<pr2.y)
    return result;
}

// obtiene los identificadores de los elementos en un nodo del SVG
function getNodeIds(aNodes) {
    var result=[];
    for (var i=0;i<aNodes.length;++i) {
        var strNode=aNodes[i].innerHTML.trim();
        if (strNode.length>0) {
            var ids=strNode.split(" ");
            for (var j=0;j<ids.length;++j) {
                if ($.isNumeric(ids[j])) {
                    result.push(parseInt(ids[j]));
                }
            }
        }
    }
    //alert(JSON.stringify(result));
    return result;
}

// obtiene el contenido para un tooltip de informacion de un
// nodo del ziggurat
function getTooltipContent(guildName, kcore, guildCoreData, nodeIds) {
    var content="";

    // datos generales
    content+="<table class='rbtooltiptableinfo1'>";
    content+="<tr>";
    content+="<th>" + getMessage("LABEL_ZIGGURAT_INFO_DETAILS_TYPE") + "</th>";
    content+="<td>" + guildName + "</td>";
    content+="</tr>";
    content+="</table>";
    // datos de cada elemento
    content+="<table class='rbtooltiptableinfo2'>";
    content+="<tr>";
    content+="<th>" + getMessage("LABEL_ZIGGURAT_INFO_DETAILS_ID") + "&nbsp;</th>";
    content+="<th align='center'>" + getMessage("LABEL_ZIGGURAT_INFO_DETAILS_KCORE") + "</th>";
    content+="<th>" + getMessage("LABEL_ZIGGURAT_INFO_DETAILS_NAME") + "</th>";
    content+="<th>" + getMessage("LABEL_ZIGGURAT_INFO_DETAILS_KRADIUS") + "</th>";
    content+="<th>" + getMessage("LABEL_ZIGGURAT_INFO_DETAILS_KDEGREE") + "</th>";
    content+="</tr>";
    if (typeof nodeIds !== 'undefined'){
        for (var i=0;i<nodeIds.length;++i) {
            var id=nodeIds[i];
            var node=getNodeTooltipContent(guildCoreData, id);
            content+="<tr>";
            content+="<td>" +  id + "&nbsp;</td>";
            content+="<td align='center'>" + node.kcore + "</td>";
            content+="<td>" + node.name + "</td>";
            if (node.kradius>0) 
                content+="<td>" + node.kradius + "</td>";
            else
                content+="<td></td>";
            if (node.kdegree>0)
                content+="<td>" + node.kdegree + "</td>";
            else
                content+="<td></td>";
            content+="</tr>";
        }
    }
    content+="</table>";
    return content;
}

// obtiene los datos concretos asociados a un elemento
// de un nodo del ziggurat
function getNodeTooltipContent(guildCoreData, id) {
    var bFound=false;
    var i=0;
    while (!bFound && i<guildCoreData.label.length) {
        if (guildCoreData.label[i]==id) {      
            bFound=true;
        } else {
            ++i;
        }
    }
    var name="(error)";
    var kdegree="(error)";
    var kradius="(error)";
    var kcore="(error)";

    if (bFound) {
        kcore=guildCoreData.kcorelabel[i];
        name=guildCoreData.name_species[i];
        kdegree=Math.round(guildCoreData.kdegree[i]*100)/100;;
        kradius=Math.round(guildCoreData.kradius[i]*100)/100;;
    }
    value={id:id, kcore: kcore, name:name, kdegree:kdegree, kradius:kradius};
    return value;
}

// muestra la informacion obtenida de la wikipedia para un nodo concreto
function linktoWiki(type, id, name,wsubdomain) {
  window.open('https://'+wsubdomain+'.wikipedia.org/wiki/'+name, "wikipedia", "directories=no,titlebar=no,toolbar=no,location=no,status=no,menubar=no,scrollbars=yes,resizable=yes");
}



// Function to extract translate values from the transform attribute
function getTranslateValues(transform) {
    let regex = /translate\((-?\d+\.?\d*),?\s*(-?\d+\.?\d*)\)/;
    let matches = transform.match(regex);
    regex = /rotate/;
    let matchesv = transform.match(regex);
    let vertical = matchesv;
    if (matches) {
        return {
          x: parseFloat(matches[1]),
          y: matches[2] ? parseFloat(matches[2]) : 0, 
          vert : vertical!=null,// Default to 0 if no y value is found
        };
      }
    return { x: 0, y: 0, vert:false }; // Return default values if no translate found
  }

  // Function to set new translate values
  function setTranslateValues(x, y, svgplot, lstyle) {
    const currentTransform = svgplot.getAttribute('transform') || '';
    const newTransform = `translate(${x}, ${y})`;
    // If there's already a translate, replace it; otherwise, just set the new one
    if (currentTransform.includes('translate')) {
        svgplot.setAttribute('transform', currentTransform.replace(/translate\([-?\d\.]+\s*,?\s*-?\d*\.?\d*\)/, newTransform));
    } else {
        svgplot.setAttribute('transform', currentTransform + ' ' + newTransform);
    }
  }


let zoomStep = 0.05;

// amplia el SVG
function svgZoomIn(plottype) {
    var svg = document.querySelector("#" + plottype + " svg");
    var viewBox = svg.getAttribute("viewBox").split(" ");
    var x = parseFloat(viewBox[0]);
    var y = parseFloat(viewBox[1]);
    var width = parseFloat(viewBox[2]);
    var height = parseFloat(viewBox[3]);

    // Check if the SVG is rotated by 90 degrees
    var transform = svg.getAttribute("transform") || "";
    var isRotated90 = transform.includes("rotate(90");

    // Scale the viewBox dimensions
    var newWidth = width / (1 + zoomStep);
    var newHeight = height / (1 + zoomStep);

    // Move the left corner by zoomStep percent of the current width and height
    var newX = x + (width - newWidth) * zoomStep;
    var newY = y + (height - newHeight) * zoomStep;

    svg.setAttribute("viewBox", `${newX} ${newY} ${newWidth} ${newHeight}`);

    // Increase the HTML width value of the SVG plot
    var currentHtmlWidth = parseFloat(svg.getAttribute("width"));
    svg.setAttribute("width", currentHtmlWidth * (1 + zoomStep));
}

// reduce el SVG
function svgZoomOut(plottype) {
    var svg = document.querySelector("#" + plottype + " svg");
    var viewBox = svg.getAttribute("viewBox").split(" ");
    var x = parseFloat(viewBox[0]);
    var y = parseFloat(viewBox[1]);
    var width = parseFloat(viewBox[2]);
    var height = parseFloat(viewBox[3]);

    // Check if the SVG is rotated by 90 degrees
    var transform = svg.getAttribute("transform") || "";
    var isRotated90 = transform.includes("rotate(90)") || transform.includes("rotate(-90)");


    // Scale the viewBox dimensions
    var newWidth = width / (1 - zoomStep);
    var newHeight = height / (1 - zoomStep);

    var newX = x + (width - newWidth) * zoomStep;
    var newY = y + (height - newHeight) * zoomStep;

    svg.setAttribute("viewBox", `${newX} ${newY} ${newWidth} ${newHeight}`);

    // Decrease the HTML width value of the SVG plot
    var currentHtmlWidth = parseFloat(svg.getAttribute("width"));
    svg.setAttribute("width", currentHtmlWidth * (1 - zoomStep));
}
// ajusta el SVG del ziggurat al marco que lo contiene
function svgZoomFit(idsvg) {

    var ziggurat    = $(idsvg);
    var svg         = $(idsvg+" svg");
    var _width      = ziggurat.width();
    var _height     = ziggurat.height();

    svg[0].setAttribute("width", _width);
    svg[0].setAttribute("height", _height);

    // restablece el scroll
    ziggurat.scrollTop(0);
    ziggurat.scrollLeft(0);
    //ziggurat.perfectScrollbar("update");
}

// establece el tamaño SVG del ziggurat a su tamaño real
function svgZoomReset(idsvg) {
    var ziggurat    = $(idsvg);
    var svg         = $(idsvg+" svg");
    var size        = svg.data("size");
    svg[0].setAttribute("width", size.width);
    svg[0].setAttribute("height", size.height);

    // restablece el scroll
    ziggurat.scrollTop(0);
    ziggurat.scrollLeft(0);
    ziggurat.perfectScrollbar("update");
}

// almacena la informacion sobre el tamaño original del ziggurat
function svgZoomStore(idsvg) {
    var svg         = $(idsvg+" svg");
    var _viewBox    = svg[0].getAttribute("viewBox");
    var _width      = _viewBox.split(" ")[2];
    var _height     = _viewBox.split(" ")[3];

    localStorage.setItem(idsvg+"width",parseFloat(_width));
    localStorage.setItem(idsvg+"height",parseFloat(_height));
    
    svg.data("size", {width:parseFloat(_width), height:parseFloat(_height)});
}

// registra la funcion que recorre la tabla en pantalla
// y envia el evento al servidor para el borrado de los ficheros
Shiny.addCustomMessageHandler(
    "deleteFilesHandler",
    function(tableId) {
        var deleteFilesList=[];
        $("div[id=" + tableId + "] tbody tr td:nth-child(1)").each(
            function() {
                //alert("this=" + $(this).html());
                deleteFilesList.push($(this).html())
            }
        );

        if (deleteFilesList.length>0) {
            var message=getMessage("MESSAGE_CONFIRM_DELETE_FILES") + ":\n";
            for (var i=0;i<deleteFilesList.length;++i) {
                message=message + "  - " + deleteFilesList[i] + "\n";
            }
            var bDelete=confirm(message);
            if (bDelete) {
                // envia el evento de borrado al servidor incluyendo la lista de ficheros
                // y un timestamp
                Shiny.onInputChange("deleteFilesData", {timestamp: new Date(), fileList: deleteFilesList});
            }
        }
    }
);

// registra la funcion que se encarga de deshabilitar un div
// de un diagrama mientras se esta generando
Shiny.addCustomMessageHandler(
    "disableDivHandler",
    function(divData) {
        //alert("disableDivHandler(divData=" + JSON.stringify(divData) + ")");
        if (divData.disable) {
            $("#" + divData.id).fadeOut(500);
        } else {
            $("#" + divData.id).fadeIn(500);
        }
    }
);


//registra la funcion que se usa para mostrar los textos de los mensajes
//en el lenguage seleccionado
var zigguratData=null;
Shiny.addCustomMessageHandler(
    "zigguratDataHandler",
    function(data) {
        zigguratData=data;
    }
);
var bipartiteData=null;
Shiny.addCustomMessageHandler(
    "bipartiteDataHandler",
    function(data) {
        bipartiteData=data;
    }
);

// registra la funcion que se usa para mostrar los textos de los mensajes
// en el lenguage seleccionado
var messagesMap=null;
Shiny.addCustomMessageHandler(
    "messagesHandler",
    function(messages) {
        messagesMap=messages;
    }
);

function getMessage(key) {
  return messagesMap[key];
}

function openZigguratReport() {
    // Open a new blank tab
    const newTab = window.open('', '_blank');

    // Check if the tab was created
    if (newTab) {
        // Write "Hello, World!" to the new tab
        newTab.document.write('<h1>Hello, World!</h1>');
        newTab.document.close(); // Close the document stream
    } else {
        alert('Please allow popups for this website.');
    }
}

