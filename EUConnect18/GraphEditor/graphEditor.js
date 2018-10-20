/*-----------------------------------------------------------------------------
FILE: /LinkedDataDev/GraphEditor/GraphEditor.js
DESC: Called from GraphEditor.html
REQ :
VIEW: http://localhost:8000/GraphEditor/GraphEditor.html
SRC :
IN  :
OUT :
DEV:
NOTE: Basing node addition on this: http://jsfiddle.net/Nivaldo/tUKh3/
      validate TTL file using RIOT: riot --validate "WhiteBoardTriples.ttl"
TODO: Task list:  https://kanbanflow.com/board/5d2eb8e3f370395a0ab2fff3c9cc65c6
      Discussion: https://kanbanflow.com/board/53c6d9a2c742c52254825aca6aabd85d
-----------------------------------------------------------------------------*/
"use strict";

// Initializations.
let w          = 1200,
    h          = 890,
    nodeWidth  = 150,
    nodeHeight = 30;
let editActive = false;  // opacity flag for editing box

// Start/end nodes when constructing a link
// last...= count of edge and node Ids tracking last in array
let startNode   = null,
    endNode     = null,
    lastEdgeId  = null,
    lastNodeId  = null;

// Mouse event settings as per Kirsling.
let selected_node  = null,
    selected_edge  = null,
    mousedown_edge = null,
    mousedown_node = null;

let svg=d3.select("#whiteboard")
          .append("svg")
          .attr({"width": w,
                 "height": h
               });

//Legend creation
//TODO : Change to creation using a loop over the array of values in the legend.
let legendDiv = d3.select("#legend").append("svg").attr("id","legendsvg");
let legendNodeHeight = 15,
    legendNodeWidth  = 20,
    rectStartX = 5,
    textStartX = 35;

var legendData = [
      {"rectClass" : "legendIri",    "rectLabel" : "IRI (links to/from)"},
      {"rectClass" : "legendUnspec", "rectLabel" : "New Node: edit to set properties"},
      {"rectClass" : "legendEdit",   "rectLabel" : "Editing node"},
      {"rectClass" : "legendSubjectLink", "rectLabel" : "Source node for new link"},
      {"rectClass" : "legendString", "rectLabel" : "String: No outgoing links!"},
      {"rectClass" : "legendInt",    "rectLabel" : "Integer: No outgoing links!"},
      {"rectClass" : "legendIriont", "rectLabel" : "External Ontology IRI"}
    ];
console.log("-----------------------");
console.log(legendData);
console.log(legendData[0].rectClass);
for(var i = 0; i < legendData.length; i++) {
    legendDiv.append("rect")
        .attr({"class" : legendData[i].rectClass,
               "width" : legendNodeWidth,
               "height" : legendNodeHeight,
               "x" : 5,
               "y" : i*25
        });
    legendDiv.append("text")
        .attr({"dx" : 35,
               "dy" : i*25+15
        })
        .text( legendData[i].rectLabel);
}
d3.select("#saveText").append("text").attr('id','currentGraphText').text("Not saved")
// Tooltip defn for Nodes
let tooltip = d3.select("body").append("div")
//let tooltip = svg.append("div")
      .attr("class", "tooltip")
      .style("opacity", 0);

// Tooltip defn for Nodes
let edgeTooltip = d3.select("body").append("div")
    .attr("class", "edgeTooltip")
    .style("opacity", 0);

let force     = null;

if (localStorage.reloadFromLocalStorage === "true") {
    localStorage.reloadFromLocalStorage = false
    console.log("Loading from local storage")
    let graph = {}
    graph.nodesData = JSON.parse(localStorage.nodes)
    graph.edgesData = JSON.parse(localStorage.edges)
    console.log(graph.edgesData)
    localStorage.currentGraph = "Current graph based on last save state"
    initializeGraph(graph);
} else if (localStorage.loadFile !== undefined) {
    console.log("Loading my selected file: "+localStorage.loadFile)
    localStorage.currentGraph = "Current graph loaded from file: "+localStorage.loadFile
    d3.queue()
       .defer(d3.json, '/graphEditor/data/'+localStorage.loadFile+'.json')
       .await(processData);

    function processData (error, graph) {
        if(error) { console.log(error); }
        console.log(graph.nodesData[0]);
        console.log(graph.edgesData[0]);
        initializeGraph(graph);
    ;}
    localStorage.removeItem("loadFile")
} else {
    console.log("Loading from file")
    localStorage.currentGraph = "Current graph loaded from file: graph.json"
    // Read source data
    d3.queue()
       .defer(d3.json, '/graphEditor/data/graph.json')
       .await(processData);

    function processData (error, graph) {
        if(error) { console.log(error); }
        console.log(graph.nodesData[0]);
        console.log(graph.edgesData[0]);
        initializeGraph(graph);
    ;}
}

// Initialize the graph components ---------------------------------------------
function initializeGraph(graph){
    // Find the max Node and Edge Id values based on array length. Used when
    // creating IDs for new nodes (increment counter)
    lastEdgeId = graph.edgesData.length - 1;
    lastNodeId = graph.nodesData.length - 1;

    // Initialize D3 force layout
    force = d3.layout.force()
            .nodes(graph.nodesData)
            .links(graph.edgesData)
            .size([w, h])
            .on("tick", tick);
    // Edge arrow
    svg.append('defs').append('marker')
        .attr({'id'          :'arrowhead',
               'viewBox'     : '-0 -5 10 10',
               'refX'        : 60,
               'refY'        : 0,
               'fill'        : '#C7C7C7',
               'stroke'      : '#9C9C9C',
               'orient'      : 'auto',
               'markerWidth' : 5,
               'markerHeight': 5,
               'xoverflow'   :'visible',
             })
        .append('path')
        .attr("d", "M 0,-5 L 10 ,0 L 0,5");

    // Parent groups sets order so nodes always on top
    svg.append("g").attr("id", "links");
    svg.append('g').attr("id", "edgepaths");
    svg.append('g').attr("id", "edgelabels");
    svg.append("g").attr("id", "nodes");

    // Add node icon. Within initiallizeGraph() for access to data
    svg.append("image")
        .attr({'x'         : 5,
               'y'         : 5,
               'width'     : 20,
               'height'    : 24,
               'xlink:href': '/GraphEditor/img/AddIcon.png'
             })
        .on('mouseover', function(d){
                let addIcon = d3.select(this)
                               .attr({'width' :25,
                                      'height':29
                               });
        })
        .on('mouseout', function(d){
                let addIcon = d3.select(this)
                               .attr({'width' :20,
                                      'height':24
                               });
        })
        .on('click', function(d){ addNode(graph);});

    // Setup and display div for buttons
    let buttonsDiv = d3.select("#buttons");
    buttonsDiv.append("button")
      .text("Create TTL")
      .on("click", function(d){ createTTL(graph);});

    buttonsDiv.append("button")
      .text("Save State")
      .on("click", function(d){ saveState(graph);});
    buttonsDiv.append("button")
      .text("Restore")
      .on("click", function(d){ restoreSaveState();});
    setLoadFiles();
    update(graph);  // Update graph for the first time
}  // End initializeGraph

function update(graph){
    //---- LINKS ---------------------------------------------------------------
    let link_update = svg.select("#links").selectAll('.link').data(
        graph.edgesData,
        function(d) { return d.id; }
    );
    link_update.enter()
        .append("path")
        .attr({"class" : "link",
               "id" : function(d,i){return 'edge'+ d.id},
               'marker-end' : 'url(#arrowhead)'
        })
        .style("stroke", "#ccc");
    link_update.exit().remove();

    // Path for the Edge Label (link) Text
    let edgepath_update = svg.select("#edgepaths").selectAll('.edgepath').data(
        graph.edgesData,
        //build the edge path id as "ep"+sourceid+"-"+targetid
        function(d){return d.id;}
    );

    edgepath_update.enter()
        .append('path')
        .attr({'class':'edgepath',
            'fill-opacity':0,
            'stroke-opacity':0,
            'id':function(d,i) { return 'edgepath'+d.id}
        })
        .style("pointer-events", "none");

    edgepath_update.exit().remove();

    // Edge Label (link) text --------------------------------------------------
    let edgelabel_update = svg.select("#edgelabels").selectAll('.edgelabel').data(
        graph.edgesData,
        function(d){return d.id;}
    );

    edgelabel_update.enter()
        .append('text')
            .attr("id", function(d,i){
                return "edgetext" + d.id;
            })
            .attr("class", function(d,i){
                if (d.prefix == "schema" || d.prefix == "ncit"){ return "edgelabel extont";}
                else if (d.prefix == "rdf" || d.prefix == "rdfs"){ return "edgelabel rdf";}
                else {return "edgelabel unspec";}
            })
            .attr("dy", -1) // place above line. 5 for inline
            .append('textPath')
            .style("text-anchor", "middle")
            .attr({"startOffset" : "50%",
                   'xlink:href' : function(d,i) {return '#edgepath'+d.id;},
                   'id' : function(d,i){return "edgelabel" + d.id}
                 })
            .text(function(d,i){
                let displayLabel=d.label;
                // Shorten the label and add elipse if it is over 12 char
                if (d.label.length > 12) {
                    displayLabel = d.label.substr(0,12) + "...";
                }
                return d.prefix + ":" + displayLabel;
            })
            //---- Double click edgelabel to edit ----------------------
            .on("dblclick", function(d, i){
                edit(d,i, "edge", graph);
            })
            .on('mouseover', function(d){
                edgeTooltip.transition()
                    .duration(200)
                    .style("opacity", 0.95);
                edgeTooltip.html(d.label)
                    .style("left", (d3.event.pageX + 6) + "px")
                    .style("top", (d3.event.pageY - 10) + "px");
            })
            .on('mouseout', function(d){
              edgeTooltip.transition()
                  .duration(500)
                  .style("opacity", 0);
            })
            ;
   edgelabel_update.exit().remove();

    // NODES -------------------------------------------------------------------
    let node_update = svg.select("#nodes").selectAll('.node').data(
    // let node_update = svg.selectAll('.node').data(
        graph.nodesData,
        function(d) {return d.id;}
    );

    // Append the rect shape to the node data
    node_update.enter().append("rect")
        .attr({"width" : function(d){ return nodeWidth; },
               "height" : nodeHeight,
               "rx" : 5,
               "ry" : 5,
               "id" : function(d, i) {return("rect"+d.id) ; }
        })  // ID used to update class
        .call(force.drag)
        //---- Double click node to edit -----------------------------------------
        // For new nodes, this should allow the entry of label, type, and prefix...
        .on("dblclick", function(d, i){
            edit(d,i, "node", graph);
        })
        .on("click", function(d, i){
            if (d3.event.shiftKey)  {
                // In this case you are trying to START a link from a literal,
                // which is not allowed. STRING/INT not allowed as startnode.
                clearCurrent();
                if(startNode === null && (d.type === "STRING" || d.type === "INT")){
                    console.log("STart node is: " + startNode);
                    window.confirm("Links from " + d.type + " nodes are not allowed.");
                    return;
                }
                if (startNode!==null && startNode.id === d.id){
                    console.log("Deselecting link: ", startNode.id);
                    let selected_rect = d3.select(this);
                    selected_rect.classed("subjectLink", false); // add type class
                    startNode= null;
                    return;
                }
                if (startNode===null){
                    let selected_rect = d3.select(this);
                    console.log("SELECTED FOR LINK: ", d3.select(this))
                    selected_rect.classed("subjectLink", true); // add type class
                    startNode = d;
                    console.log("Setting Start Node as node ID: " + startNode.id);
                }
                // Only set endNode if it is not the same as the startNode or a node which already has a link.
                else {
                    let linkExist = graph.edgesData.filter(function(l) {
                        return (l.source.id === startNode.id && l.target.id === d.id) || (l.source.id === d.id && l.target.id === startNode.id);
                    });
                    if (linkExist.length !== 0) {
                        window.confirm("Cannot make more than one link between the same two nodes")
                        return
                    }
                    endNode= d;
                    console.log("Start Node: " + startNode.id + " End Node: " + endNode.id);
                    addEdge(graph);
                    d3.selectAll(".subjectLink")
                        .classed("subjectLink", false); // add type class
                }
           }
        }) // end mouse click
        .on('mouseover', function(d){
            //DEBUG  console.log("NODE MOUSEOVER");
            let nodeSelection = d3.select(this).attr({
              'width':nodeWidth+5,
              'height':nodeHeight+5
            });

            console.log("Node Mouseover is happening");
            // Add tooltip here
            tooltip.transition()
                .duration(200)
                .style("opacity", 0.95);
            //tooltip.html(d.props)
            tooltip.html(d.label)
                .style("left", (d3.event.pageX + 6) + "px")
                .style("top", (d3.event.pageY - 10) + "px");
          })
        //Mouseout Node  - bring node back to full colour
        .on('mouseout', function(d){
            let nodeSelection = d3.select(this).attr({
              'width':nodeWidth,
              'height': nodeHeight
            });
            tooltip.transition()
                .duration(500)
                .style("opacity", 0);
        }); // end mouseout

    node_update.exit().remove();

    // Find the currently edited node, so it can be set current again (if adding a new node or making a link)
    let currentEditNode = d3.select(".current") //.classed("current",true)
    node_update
        .attr("class", function(d){
            if (d.type == "STRING"){ return "node string";}
            else if (d.type == "INT"){ return "node int"; }

            else if (d.prefix == "schema"   ||
                     d.prefix  == "ncit"    ||
                     d.prefix == "cto"){ return "node iriont"; }
            else if (d.prefix == "eg"){ return "node iri"; }
            else {return "node unspec";}
        });
    // Highlight currently edited node again
    if (currentEditNode) {
        currentEditNode.classed("current",true)
    }

    // Data for node text
    let nodeText_update = svg.selectAll(".nodeText").data(
        graph.nodesData,
        function(d){ return "nodeText"+d.id;}
    );

    // Add id (nodeTextn) and the text
    nodeText_update.enter()
        .append("text")
        .attr("pointer-events","none");

     // Remove nodeText
     nodeText_update.exit().remove();

    nodeText_update
        .attr({
            // id links to editing window
            'id':    function(d, i) {return "nodeText"+d.id ; },
            'class': 'nodeText'
        })
        .text(function(d,i) {
            let displayLabel="";
            // Shorten the label and add elipse if it is over 12 char
            if (d.label.length > 12) {
                displayLabel = d.label.substr(0,12) + "...";
            }else{
                displayLabel = d.label;
            }
            // No prefix for INT, STRING. Prefix for all others
            if (d.type ==='INT' || d.type ==='STRING') {
                  return displayLabel;
            }else{
                return d.prefix + ":" + displayLabel;
            }
        });
    force.start();
}  // End of update(graph)

function tick() {
    svg.selectAll(".link")
        .attr('d', function(d) {
            let xposSource = d.source.x + nodeWidth/2,
                xposTarget = d.target.x + nodeWidth/2,
                yposSource = d.source.y + nodeHeight/2,
                yposTarget = d.target.y + nodeHeight/2;
            return 'M' + xposSource + ',' + yposSource + 'L' + xposTarget + ',' + yposTarget;
        });

   svg.selectAll(".edgepath")
    .attr('d', function(d) {
        //Adjust for rectangle shape
        let xposSource = d.source.x + nodeWidth/2,
            xposTarget = d.target.x + nodeWidth/2,
            yposSource = d.source.y + nodeHeight/2,
            yposTarget = d.target.y + nodeHeight/2;
        let path='M '+ xposSource + ' ' + yposSource+' L '+ xposTarget + ' ' + yposTarget;
        return path;
    });

    svg.selectAll(".edgelabel")
        .attr('transform',function(d,i){
            if (d.target.x<d.source.x){
                let bbox = this.getBBox();
                let rx = bbox.x+bbox.width/2;
                let ry = bbox.y+bbox.height/2;
                return 'rotate(180 '+rx+' '+ry+')';
            }
        else { return 'rotate(0)'; }
    });

    // NODES
    // Prevent node movement off the editing canvas by adj. each d.x, d.y
    svg.selectAll(".node")
        .attr("transform", function(d) {
            if (d.x > w ) {d.x = w-nodeWidth;}  // right
            if (d.x < -40 ) {d.x = nodeWidth;}  // left
            if (d.y < 0 ) {d.y = nodeWidth;}    // top
            if (d.y > h ) {d.y = h-nodeWidth;}  // bottom
            return "translate(" + d.x + "," + d.y + ")";
    });

    // Node Text label
    svg.selectAll(".nodeText")
        .attr("x", function(d){ return d.x+5;})
        .attr("y", function(d){ return d.y+17;});
};  // End tick

//------------------------------------------------------------------------------
//---- Additional Functions ----------------------------------------------------

function nodeIsSource(nodeId,edgesData){
    let isSourceList = edgesData.filter(function(l) {
      return l.source.id === nodeId;
    });
    return isSourceList.length !== 0;
}
function nodeIsTarget(nodeId,edgesData){
    let isTargetList = edgesData.filter(function(l) {
      return l.target.id === nodeId;
    });
    return isTargetList.length !== 0;
}
function doit(){
    let found = document.querySelectorAll('[currentlabel="me"]');
    console.log("I found: "+found)
    console.log(found)
}

//   Edit either a "node" or an "edge"
function edit(d, i, source, graph){
    // upsource used in editor display to match exercises text
    let upSource = source.charAt(0).toUpperCase() + source.slice(1);

    // If another node or edge was already selected (edit window already present,
    //   then made another dbl click, you must purge the existing info to allow
    //   display of info from the second dbl clicked item to replace the first.
    if (editActive === true) {
        // clicked a node or edge while previous edit div displayed
        // d3.selectAll("input").remove();
        d3.select("#edit").selectAll("*").remove();
        d3.select("#edit").style("opacity", 0);
    }
    // If a node has been selected (shift+click) then it will be deselected
    if (startNode != null) {
        console.log("I'll deselect the selected node for you:"+startNode)
        startNode = null
        d3.selectAll(".subjectLink")
            .classed("subjectLink", false);
    }
    clearCurrent();
    if (source === "node") {
      d3.select("#rect" + d.id).classed("current",true)
    }

    let self = this; //Necessary?
    editActive = true;
    d3.select("#edit").style("opacity", 1);
    d3.select("#buttons").style("opacity", 0);
    let div = d3.select("#edit");

    div.append("p")
        .text(function() {
          // Use 'Link values' for edges to match exercises (for comprehension)
          if (source=="edge"){return("Link values"); }
          else {return (upSource + " values");}
        });  // Select div for appending


    //TYPE - NODES only
    let typeText    = "";
    let typeInput   = "";
    let typeSelect  = "";


    if(source=="node"){
        typeText     = div.append("p")
                            .text("Type: ");
        // Set default types for nodes
        let typeData = ["IRI","STRING", "INT"]
        // Check if the node is the source in any relation
        let isSource = nodeIsSource(d.id,graph.edgesData)
        // If yes, you cannot change it to a literal
        if (isSource) {
            typeData = ["IRI"]
        }
        typeInput    = typeText.append("select")
                            .attr('class','select')
                            .attr('onchange', "doit()");
        typeSelect   = typeInput.selectAll('option')
                            .data(typeData).enter()
                            .append('option')
                            .text(function (d) { return d; })
                            .property("selected", function(g){ return g === d.type; });

    }

    // PREFIX - nodes
    let prefixText = "";
    let prefixData = "";
    let prefixInput = "";
    let prefixSelect = "";
    if(source=="node"){
        prefixText = div.append("p")
                            .text("Prefix: ");
        prefixData = ["eg","ncit","rdf", "rdfs", "schema" ]
        prefixInput = prefixText.append("select")
                            .attr('class','select');
        prefixSelect = prefixInput.selectAll('option')
                            .data(prefixData).enter()
                            .append('option')
                            .text(function (d) { return d; })
                            .property("selected", function(g){ return g === d.prefix; });
    }
    // Label
    let labelInput  = "";
    let labelSelect = "";
    let labelText   = "";


    // Label  - for nodes
    if(source=="node"){
    // Label  - for edges
        labelText = div.append("p")
                        .text("Label: ");
        labelInput = labelText.append("input")
                            .attr({
                              'size': '15',
                              'type': 'text',
                              'value': d.label
                            });
    } else {

        labelText = div.append("p")
                        .attr("currentlabel","me")
                        .text("Predicate: ");
        let labelData = [
                            "eg:age",
                            "eg:drugName",
                            "eg:LDExpert",
                            "eg:participatesIn",
                            "eg:randomizedTo",
                            "eg:trtArm",
                            "eg:trtArmType",
                            "ncit:gender",
                            "ncit:phase",
                            "ncit:study",
                            "schema:givenName"
                        ];
        labelInput    = labelText.append("select")
                            .attr('class','select')
        labelSelect   = labelInput.selectAll('option')
                            .data(labelData).enter()
                            .append('option')
                            .text(function (d) { return d; })
                            .property("selected", function(g){ return g === d.prefix+":"+d.label; });
                            // .property("selected", function(g){ console.log("nu:"+g+"="+d.prefix+":"+d.label);console.log("true:"+(g === d.prefix+":"+d.label)); return g === d.prefix+":"+d.label; });


    }
console.log("labelText: "+labelText)


    //---- UPDATE BUTTON -------------------------------------------------------
    let button =  div.append("button")
                      .text("Update/Hide")
                      .on("click", function() {
                          console.log("Prefix is: " + d.prefix);

                          //---- NODE ------------------------------------------
                          if(source=="node"){
                              console.log("Update on Node: "+ d.id)
                              if (typeInput.node().value == "INT") {
                                let inputValue = labelInput.node().value
                                if (!/^\d+$/.test(inputValue)) {  // Allows integers
                                    window.confirm("Incorrect value for INT: " + inputValue);
                                    return
                                }
                              }
                              // Prevent creation of node with same label
                              let nodeLabelExist = graph.nodesData.filter(function(l) {
                                  return l.label === labelInput.node().value && l.prefix === prefixInput.node().value && l.id !== d.id;
                              });
                              if (nodeLabelExist.length !== 0) {
                                  window.confirm("A node with label: "+labelInput.node().value+" already exists")
                                  return
                              }
                              d.label=labelInput.node().value;
                              d.prefix = prefixInput.node().value;
                              d.type = typeInput.node().value;
                              clearCurrent();
                          } // end of node UPDATE

                          //---- EDGE -----------------------------------------
                          if(source=="edge"){
                            console.log("Updating Edge")
                            // 1. Values for the data array
                            let splitInfo = labelInput.node().value.split(":");
                            d.prefix = splitInfo[0];
                            d.label = splitInfo[1];
                            d3.select("#edgetext" + d.id)
                              .attr("class", "")  // Remove all classes (node, iri, string, int)
                              .attr("class", "edgelabel") // Add the node class back in.
                              .classed(d.prefix.toLowerCase(), true)
                              .attr("class", function(d,i){
                                  if (d.prefix == "schema" ||
                                      d.prefix == "ncit") { return "edgelabel extont";}
                                  else if (d.prefix == "rdf" || d.prefix == "rdfs"){ return "edgelabel rdf";}
                                  else {return "edgelabel unspec";}
                              });
                            // Edge labels forced to lowercase for exercises.
                            d3.select("#edgelabel" + d.id)
                                .text(function(d)  {
                                    // 2. prefix + label to display in the SVG
                                    let displayLabel=d.label;
                                    // Shorten the label and add elipse if it is over 12 char
                                    if (d.label.length > 10) {
                                        displayLabel = d.label.substr(0,10) + "...";
                                    }
                                    return d.prefix + ":" + displayLabel;
                                })
                          } // End of Edge update
                          // Clean up the edit window after click of Hide/Update
                          d3.select("#edit").selectAll("*").remove();
                          d3.select("#edit").style("opacity", 0);
                          editActive = false;  // Turn off the edit area
                          d3.select("#buttons").style("opacity", 1);  // Redisplay buttons
                          update(graph);
                      }); // End of click on update button
    if(source=="edge" && d.target.type !== "STRING" && d.target.type !== "INT"){
        let reverseButton = div.append("button")
                            .text("Reverse")
                            .on("click", function() {
                                console.log(d)
                                let tmpSource = d.source
                                d.source = d.target
                                d.target = tmpSource
                                update(graph)
                            });
    }
    let delButton = div.append("button")
                        .text("Delete")
                        .on("click", function() {
                            if(source=="node"){
                                mousedown_node = d; // Captures the node Initialized to null as per Kirsling
                                selected_node = mousedown_node ;
                                deleteNode(graph, selected_node);
                            }
                            if(source=="edge"){
                                console.log("So you want to DELETE an Edge!")
                                mousedown_edge = d; // Captures the edge.
                                selected_edge = mousedown_edge;
                                console.log("Selected_edge: " , selected_edge)
                                graph.edgesData.splice(graph.edgesData.indexOf(selected_edge), 1); // Delete selected edge from array
                            }
                            d3.select("#edit").selectAll("*").remove();
                            d3.select("#edit").style("opacity", 0);
                            editActive = false;  // Turn off the edit area
                            d3.select("#buttons").style("opacity", 1);  // Redisplay buttons
                            force.start();
                            update(graph);
                        });
}

function clearCurrent() {
    d3.selectAll(".node").classed("current",false)
    d3.selectAll(".edgelabel").classed("current",false)
}

function addNode(graph){
    console.log("A node you wish to add! lastNodeId before"+lastNodeId)
    let newNode = {
        id: ++lastNodeId,
        label: 'NEW'+lastNodeId,
        prefix: 'new',
        type: 'UNSPEC',
        x:200,
        y:200,
        fixed:true
    };
    let n = graph.nodesData.push(newNode);
    console.log(newNode)
    console.log(graph.nodesData)
    update(graph);
}

function deleteNode(graph, selected_node){
    clearCurrent();
    console.log("A node you will delete!")
    console.log("id number: ", )
    console.log("Selected_node: " , selected_node)
    console.log("Selected_node Label: " , selected_node.label)

    //TODO: Add deletion of any edge attached to the selected node
    graph.nodesData.splice(graph.nodesData.indexOf(selected_node), 1); // Delete selected node from array
    console.log("Nodes data post-deletion:");
    console.log(graph.nodesData);
    console.log("Edges data pre-deletion:");
    console.log(graph.edgesData);
    graph.edgesData = graph.edgesData.filter(function(l) {
        //console.log(l)
        //console.log(selected_node)
        return l.source.id !== selected_node.id && l.target.id !== selected_node.id;
    });
    console.log("Edges data post-deletion:");
    console.log(graph.edgesData);
    // Remove the text in the SVG that is associated with this node. [KLUDGE]
    d3.select("#nodeText" + selected_node.id).remove();
    d3.select("#buttons").style("opacity", 1);  // Redisplay buttons
    update(graph)
}

function addEdge(graph){
    let newEdge = {
        id: ++lastEdgeId,
        source: startNode,
        target: endNode,
        label: 'NEW'+lastEdgeId,
        prefix: 'eg'
    };
    let n = graph.edgesData.push(newEdge);
    // Reset flags
    startNode = null,
    endNode   = null;
    update(graph);
}
// Just saving JSON for debugging createTTL
function download(text, name, type) {
    var a = document.createElement("a");
    // var file = new Blob([JSON.stringify(text)], {type: type});
    var file = new Blob([JSON.stringify(text, null, 2)], {type: type});
    a.href = URL.createObjectURL(file);
    a.download = name;
    a.click();
}
function compareEdges(a,b) {
  // First compare source.prefix
  if (a.source.prefix < b.source.prefix) {
      return -1
  }
  if (a.source.prefix > b.source.prefix) {
      return 1
  }
  // Then source.label
  if (a.source.label < b.source.label) {
      return -1
  }
  if (a.source.label > b.source.label) {
      return 1
  }
  // Then edge.prefix
  if (a.prefix < b.prefix) {
      return -1
  }
  if (a.prefix > b.prefix) {
      return 1
  }
  // Then edge.label
  if (a.label < b.label) {
      return -1
  }
  if (a.label > b.label) {
      return 1
  }
  // If here, everything is equal. Which should never occur as you cannot have two edges between the same nodes
  return 0
}

function cleanJson(graph) {
    let cleanedGraph = JSON.parse(JSON.stringify(graph));

    // Remove properties created by the force() function
    cleanedGraph.nodesData.forEach(n => {
        delete n.index;
        delete n.px;
        delete n.py;
        delete n.weight;
    });

    // Replace node array values with their index reference
    cleanedGraph.edgesData.forEach(e => {
        e.source = e.source.id;
        e.target = e.target.id;
    });
    return cleanedGraph;
}

function createTTL(jsonData) {
    alert("You will now create the TTL file. Click OK to confirm.");
    // Sort edges
    let sortedEdges = jsonData.edgesData
    sortedEdges.sort(compareEdges)
    // Clean up JSON and save
    let graphClone = cleanJson(jsonData);
    download(graphClone, 'whiteboard.json', 'application/json');

    // Set the prefixes
    let writer = N3.Writer({ prefixes: { eg: 'http://example.org/LDWorkshop#',
                                         ncit: 'http://ncicb.nci.nih.gov/xml/owl/EVS/Thesaurus.owl#',
                                         rdf:'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
                                         rdfs:'http://www.w3.org/2000/01/rdf-schema#',
                                         schema:'http://schema.org/',
                                         xsd:'http://www.w3.org/2001/XMLSchema#'
                                        }
                          });
    // loop through the edges to create triples
    //   This version ignores unattached nodes.
    for(var i = 0; i < sortedEdges.length; i++) {
        let raw       = sortedEdges[i];  // create object for shorter references
        let subject   = raw.source.prefix + ":" + raw.source.label;
        let predicate = raw.prefix + ":" + raw.label;
        let object    = null;
        //console.log("S-P: " + subject + " --" + predicate)

        // Create Object based on their type
        // IRI and IRIONT are treated the same
        if (raw.target.type ==='IRI' || raw.target.type ==='IRIONT') {
            object = raw.target.prefix + ":" + raw.target.label;
        } else {
            // Literal values are enquoted with use of '"'
            if (raw.target.type =='INT') {
                object = '"' + raw.target.label + '"^^xsd:integer' ;
            } else if (raw.target.type =='STRING') {
                object = '"' + raw.target.label + '"^^xsd:string' ;
            }
        } // end of Object creation
        console.log("TRIPLE: " + subject + " --" + predicate + "--> " + object);
        writer.addTriple(subject, predicate, object); // Add triple for each edge
    } // end looping over edges
    // Write out to file
    writer.end(function (error, result) {
        console.log(result);
        //utf-8 causes error when reading TTL with R redland, so use ascii
        let blob = new Blob([result], {type: "text/plain;charset=ascii"});
        saveAs(blob, "WhiteBoardTriples.ttl");
    });
} // End createTTL()

function setLoadFiles(){
    let loadFileDiv = d3.select("#buttons")
    let loadText = loadFileDiv.append("p")
        .text("Select file: ");
    let graphFiles = ["graph","graph1", "graph2"]
    let fileSelect = loadText.append("select")
        .attr('class','select')
    let fileOption = fileSelect.selectAll('option')
        .data(graphFiles).enter()
        .append('option')
        .text(function (d) { return d; });
    // let loadButton = loadFileDiv.append("button")
    let loadButton = loadText.append("button")
      .text("Load")
      .on("click", function(d){
                            let selectedFile = fileSelect.node().value
                            console.log("Selected file: "+selectedFile)
                            localStorage.loadFile = selectedFile
                            window.location.reload(false);
                            });
    loadFileDiv.append('text')
        .attr('x',w/2)
        .attr('y',10)
        .style("font-size", "16px")
        .style("text-decoration", "underline")
        .text(localStorage.currentGraph);

} // End setLoadFiles

function saveState(graph){
    // Clone the original graph to allow pre-export cleansing
    let graphClone = cleanJson(graph);
    localStorage.nodes = JSON.stringify(graphClone.nodesData)
    localStorage.edges = JSON.stringify(graphClone.edgesData)
    d3.select("#currentGraphText").text("Last saved "+new Date())
} // end saveState()
function restoreSaveState(){
    localStorage.reloadFromLocalStorage = true
    window.location.reload(false);
} // end restoreSaveState()
