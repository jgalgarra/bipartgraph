//-----------------------------------------------------------------------------
// Universidad Politecnica de Madrid - EUITT
//  PFC
//  Representacion grafica de redes bipartitas basadas en descomposicion k-core
//
// Autor         : Juan Manuel Garcia Santi
// Módulo        : redesbipartitas.js
// Descricpción  : Funciones javascript que permiten la interaccion del usuario
//                 con el diagrama ziggurat y la presentacion de la informacion
//                 relativa a nodos y elementos
//-----------------------------------------------------------------------------

// funcion que se llama cuando la pagina esta cargada
function windowLoad() {
    // actualiza los tooltips de ayuda
    updateHelpTooltips();

    // indica al servidor que el cliente esta listo
    Shiny.onInputChange("windowLoad", new Date());
}

// establece los tooltips de ayuda de todos los elementos
var helpTooltips=[
    {id: "zoomin",      value: "Incrementar el nivel de zoom del ziggurat"},
    {id: "zoomout",     value: "Disminuir el nivel de zoom del ziggurat"},
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
function updateSVGEvents() {
    // actualiza los eventos asociados a las etiquetas
    updateNodeEvents();

    // actualiza los eventos asociados a los enlaces
    updateLinkEvents();

    // actualiza los tooltips
    updateNodeTooltips();

    // inicializa el scroll mediante "drag"
    $("#ziggurat").dragscrollable();

    // inicializa el scroll
    $("#ziggurat").perfectScrollbar({scrollXMarginOffset:4, scrollYMarginOffset:4});
    $("#zigguratNodesDetail").perfectScrollbar({scrollXMarginOffset:16, scrollYMarginOffset:4});

    // almacena la informacion del tamaño del SVG
    svgZoomStore();

    // establece el SVG a su tamaño real
    svgZoomFit();
}

// actualiza el scroll de los detalles de los nodos del ziggurat
function updateZigguratNodesDetailScroll() {
    $("#zigguratNodesDetail").perfectScrollbar("update");
}

// actualiza los eventos asociados a los nodos del SVG
function updateNodeEvents() {
    for (var i=0;i<zigguratData.ids.length;++i) {
        var guild       = zigguratData.ids[i];
        var guildName   = zigguratData.names[i];
        var guildData   = zigguratData.data[guild];
        for (var kcore=1;kcore<=guildData.length;++kcore) {
            var pattern="kcore" + kcore + "-" + guild;

            // estilo del cursor
            $("[id*=" + pattern + "]").css("pointer", "pointer");

            // eventos para resaltar un nodo y los asociados
            $("[id*=" + pattern + "]").click(function() {
                markRelatedNodes($(this).attr("id").replace("-text", "").replace("-rect",""));
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
    $("g[id*=" + pattern + "]").css("pointer", "pointer");

    // eventos
    $("g[id*=" + pattern + "]").mouseover(function() {
        var strokeWidth=parseFloat($(this).css("stroke-width"));
        $(this).css("stroke-width", strokeWidth+2);
    });
    $("g[id*=" + pattern + "]").mouseout(function() {
        var strokeWidth=parseFloat($(this).css("stroke-width"));
        $(this).css("stroke-width", strokeWidth-2);
    });
}

// actualiza los tooltips de los nodos
function updateNodeTooltips() {
    for (var i=0;i<zigguratData.ids.length;++i) {
        var guild       = zigguratData.ids[i];
        var guildName   = zigguratData.names[i];
        var guildData   = zigguratData.data[guild];
        for (var kcore=1;kcore<=guildData.length;++kcore) {
            // tooltips
            var pattern="kcore" + kcore + "-" + guild;
            var guildCoreData=guildData[kcore-1];
            if (guildCoreData!=null) {
                $("rect[id*=" + pattern + "]").each(function() {
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
                });
            }
        }
    }
}

// resalta el nodo indicado en el SVG
function markNode(nodeId) {
    // marca el texto
    $("#" + nodeId + "-text").each(function() {
        // incrementa la fuente
        var fontSize=parseInt($(this).css("font-size").replace("px",""));
        $(this).css("font-size", (fontSize+4) + "px");
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
function unmarkNode(nodeId) {
    // desmarca el texto
    $("#" + nodeId + "-text").each(function() {
        // reduce la fuente
        var fontSize=parseInt($(this).css("font-size").replace("px",""));
        $(this).css("font-size", (fontSize-4) + "px");
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
function markLink(linkId) {
    $("g[id*=" + linkId + "]").each(function() {
        // incrementa el ancho del enlace
        var strokeWidth=parseFloat($(this).css("stroke-width"));
        $(this).css("stroke-width", strokeWidth+2);

        // indica que el enlace esta marcado
        $(this).data("marked", true);

        $(this).css("stroke-dasharray","5,5");
    });
}

// elimina el resaltado del enlace indicado en el SVG
function unmarkLink(linkId) {
    $("g[id*=" + linkId + "]").each(function() {
        // reduce el ancho del enlace
        var strokeWidth=parseFloat($(this).css("stroke-width"));
        $(this).css("stroke-width", strokeWidth-2);

        // indica que el enlace no esta marcado
        $(this).data("marked", false);


        $(this).css("stroke-dasharray","5,0");
    });
}

// resalta el nodo, los nodos relacionados y los enlaces que les unen
function markRelatedNodes(nodeId) {
    var svg             = $("#ziggurat svg");
    var node            = $("rect[id*=" + nodeId + "]");
    var nodeIds         = node.data("nodeIds");
    var guild           = node.data("guild");
    var marked          = node.data("marked");
    var markedNodes     = [];
    var markedNodesData = [];
    var neighbors       = (zigguratData.neighbors[guild])[nodeIds[0]-1];
    if (!$.isArray(neighbors)) {
        neighbors=[neighbors];
    }

    // si el nodo estaba marcado solo deshace el marcado de todos los nodos, si no lo
    // estaba desmarca los nodos anteriores y marca los nuevos
    var nodes=$("rect[id*=kcore]");

    // desmarca todos los nodos y enlaces marcados
    nodes.each(function() {
        if ($(this).data("marked")) {
            unmarkNode($(this).attr("id").replace("-rect", ""));
        }
    });
    $("g[id*=link-]").each(function() {
        if ($(this).data("marked")) {
            unmarkLink($(this).attr("id"));
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

                // si es vecino lo marca
                if (isNeighbor) {
                    markNode($(this).attr("id").replace("-rect", ""));
                    markedNodes.push($(this).attr("id").replace("-rect", ""));
                }
            }
        });

        // marca el nodo seleccionado
        markNode(node.attr("id").replace("-rect", ""));
        markedNodes.push(node.attr("id").replace("-rect", ""));

        // comprueba los enlaces que intersectan
        $("g[id*=link-] path").each(function() {
           var intersect=pathIntersectRect($(this)[0], node[0]);
           if (intersect) {
               markLink($(this).parent().attr("id"));
           }
        });

        // obtiene la informacion de los nodos marcados
        for (var i=0;i<markedNodes.length;++i) {
            var markedNode      = $("#" + markedNodes[i] + "-rect");
            var markedNodeData  = {};
            markedNodeData[(i+1)]={
                guild:      markedNode.data("guild"),
                kcore:      markedNode.data("kcore"),
                nodeIds:    markedNode.data("nodeIds")
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
    content+="<tr>";
    content+="<th>" + getMessage("LABEL_ZIGGURAT_INFO_DETAILS_KCORE") + "</th>";
    content+="<td>" + kcore + "</td>";
    content+="</tr>";
    content+="</table>";

    // datos de cada elemento
    content+="<table class='rbtooltiptableinfo2'>";
    content+="<tr>";
    content+="<th>" + getMessage("LABEL_ZIGGURAT_INFO_DETAILS_ID") + "</th>";
    content+="<th>" + getMessage("LABEL_ZIGGURAT_INFO_DETAILS_NAME") + "</th>";
    content+="<th>" + getMessage("LABEL_ZIGGURAT_INFO_DETAILS_KRADIUS") + "</th>";
    content+="<th>" + getMessage("LABEL_ZIGGURAT_INFO_DETAILS_KDEGREE") + "</th>";
    content+="</tr>";
    for (var i=0;i<nodeIds.length;++i) {
        var id=nodeIds[i];
        var node=getNodeTooltipContent(guildCoreData, id);
        content+="<tr>";
        content+="<td>" +  id + "</td>";
        content+="<td>" + node.name + "</td>";
        content+="<td>" + node.kradius + "</td>";
        content+="<td>" + node.kdegree + "</td>";
        content+="</tr>";
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
    if (bFound) {
        name=guildCoreData.name_species[i];
        kdegree=Math.round(guildCoreData.kdegree[i]*100)/100;;
        kradius=Math.round(guildCoreData.kradius[i]*100)/100;;
    }
    value={id:id, name:name, kdegree:kdegree, kradius:kradius};
    return value;
}

// muestra la informacion obtenida de la wikipedia para un nodo concreto
function linktoWiki(type, id, name,wsubdomain) {
  window.open('https://'+wsubdomain+'.wikipedia.org/wiki/'+name, "wikipedia", "directories=no,titlebar=no,toolbar=no,location=no,status=no,menubar=no,scrollbars=yes,resizable=yes");
}


// muestra la informacion obtenida de la wikipedia para un nodo concreto
function showWiki(type, id, name) {
    //alert("showWiki(id=" + id + ", name=" + name + ")");
    var wikiPanelId     = "wikiDetail-" + type + "-" + id;
    var wikiPanelName   = type + " [#" + id + "]";
    var wikiPanel       = $("#" + wikiPanelId);
    var wikiLoaded      = wikiPanel.data("loaded");

    // selecciona el tab concreto
    Shiny.onInputChange("wikiPanelName", wikiPanelName);

    // muestra el "tab" y el contenido
    $("#zigguratWikiDetail ul.nav-pills a").each(function() {
        if ($(this).html()==wikiPanelName) {
            $(this).css("display", "block");
        }
    });
    $("#" + wikiPanelId).css("display", "block");

    // carga los datos si no estan cargados
    if (typeof wikiLoaded=="undefined" || !wikiLoaded) {
        // http://en.wikipedia.org/w/api.php?action=query&prop=revisions&format=json&rvprop=content&rvparse=&titles=Bombus%20dahlbomii
        // solo funciona si se usa jquery.getJSON y "callback=?" como parametro
        // si no da error de CORS 'Access-Control-Allow-Origin
        var wikiBase        = "https://en.wikipedia.org";
        var wikiApi         = "/w/api.php";
        var wikiParameters  = {
             cache:         false,
             format:        "json",
             action:        "query",
             prop:          "revisions",
             rvprop:        "content",
             rvparse:       "",
             titles:        name
        }
        var wikiUrl         = wikiBase + wikiApi + "?" + $.param(wikiParameters) + "&callback=?";
        //alert("Consultando wikipedia: " + wikiUrl);
        var jqXHR=$.getJSON(wikiUrl);
        jqXHR.done(function(data) {
            //alert("data=" + JSON.stringify(data));
            var pages=data.query.pages;
            for (var property in pages) {
                // establece el contenido a partir de la primera revision
                //alert("name=" + property  + ", value=" + pages[property]);
                var page=pages[property];
                if (typeof page.revisions=="undefined") {
                    wikiPanel.html(getMessage("MESSAGE_WIKIPEDIA_NO_INFO_ERROR") + " " + name);
                } else {
                    var revision=page.revisions[0];
                    var content=revision["*"];
                    wikiPanel.html(content);

                    // modifica todos los enlaces para que se abran en una nueva ventana
                    // y los relativos para que apunten a wikipedia
                    $("#" + wikiPanelId + " a").each(function() {
                        var _href=$(this).attr("href");
                        // nueva ventana
                        if (_href.substring(0,1)!="#") {
                            $(this).attr("target", "_blank");

                            // completa los enlaces relativos
                            if (_href.substring(0,1)=="/") {
                                $(this).attr("href", wikiBase + _href);
                            }
                        }
                    });

                    // inicializa el scroll
                    wikiPanel.perfectScrollbar({scrollXMarginOffset:10, scrollYMarginOffset:4});
                    wikiPanel.perfectScrollbar("update");
                }

                // marca el panel como cargado
                wikiPanel.data("loaded", true);
            }
        });

        // no funciona el "fail" con callback, pero lo dejo por si acaso algún día....
        jqXHR.fail(function(jqXHR, textStatus, errorThrown) {
            alert(getMessage("MESSAGE_WIKIPEDIA_DOWNLOAD_ERROR") + " [status=" + textStatus+ ", error=" + errorThrown + "]");
        });
    }
}

// amplia el SVG del ziggurat
function svgZoomIn() {
    var svg         = $("#ziggurat svg");
    var ziggurat    = $("#ziggurat");
    var _width      = ziggurat.width();
    var _height     = ziggurat.height();
    
    svg[0].setAttribute("width", _width);
    svg[0].setAttribute("height", _height);
    
    svg[0].setAttribute("width", Math.floor(_width*1.1));
    svg[0].setAttribute("height", Math.floor(_height*1.1));
    ziggurat.scrollTop(0);
    ziggurat.scrollLeft(0);
}

// reduce el SVG del ziggurat
function svgZoomOut() {
    var svg         = $("#ziggurat svg");
    var ziggurat    = $("#ziggurat");
    var _width      = ziggurat.width();
    var _height     = ziggurat.height();
    
    svg[0].setAttribute("width", _width);
    svg[0].setAttribute("height", _height);
    svg[0].setAttribute("width", Math.floor(_width/1.1));
    svg[0].setAttribute("height", Math.floor(_height/1.1));
}

// ajusta el SVG del ziggurat al marco que lo contiene
function svgZoomFit() {
    var ziggurat    = $("#ziggurat");
    var svg         = $("#ziggurat svg");
    var _width      = ziggurat.width();
    var _height     = ziggurat.height();
    svg[0].setAttribute("width", _width);
    svg[0].setAttribute("height", _height);

    // restablece el scroll
    ziggurat.scrollTop(0);
    ziggurat.scrollLeft(0);
    ziggurat.perfectScrollbar("update");
}

// establece el tamaño SVG del ziggurat a su tamaño real
function svgZoomReset() {
    var ziggurat    = $("#ziggurat");
    var svg         = $("#ziggurat svg");
    var size        = svg.data("size");
    svg[0].setAttribute("width", size.width);
    svg[0].setAttribute("height", size.height);


    // restablece el scroll
    ziggurat.scrollTop(0);
    ziggurat.scrollLeft(0);
    ziggurat.perfectScrollbar("update");
}

// almacena la informacion sobre el tamaño original del ziggurat
function svgZoomStore() {
    var svg         = $("#ziggurat svg");
    var _viewBox    = svg[0].getAttribute("viewBox");
    var _width      = _viewBox.split(" ")[2];
    var _height     = _viewBox.split(" ")[3];
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

//registra la funcion que se usa para mostrar los textos de los mensajes
//en el lenguage seleccionado
var zigguratData=null;
Shiny.addCustomMessageHandler(
    "zigguratDataHandler",
    function(data) {
        zigguratData=data;
        //alert("zigguratData=" + JSON.stringify(zigguratData));
    }
);
