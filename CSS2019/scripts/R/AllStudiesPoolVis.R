###############################################################################
# FILE: /scripts/r/AllStudiesPoolVis.R
# DESC: Force network graph of the pooled studies
# IN  : Stardog graph AllStudies  (Stardog instance running, pool populated)
# OUT : visnetwork graph
# REQ : 
# NOTE: For EUConnect18 Hands-on Workshop. Colours match original whiteboard
# TODO: 
#       Clean up the code. 
#       
###############################################################################
library(plyr)     #  rename
library(reshape)  #  melt
library(SPARQL)
library(visNetwork)

# Query StardogTriple Store ----
endpoint <- "http://localhost:5820/AllStudies/query"


queryOnt = paste0("
SELECT ?s ?p ?o
WHERE{
  ?s ?p ?o
}")

qd <- SPARQL(endpoint, queryOnt)
triples <- qd$results

# Convert IRI to qnam 
# TODO: Change to lapply over list of iris and qnam replacements
iriToQnam <- function(elem)
{
  elem <- gsub("<http://example.org/LDWorkshop#", "eg:", elem)
  elem <- gsub("<http://www.example.org/LDWorkshop/", "eg:", elem)  # Kludge for ontology error </wink>
  elem <- gsub("<http://ncicb.nci.nih.gov/xml/owl/EVS/Thesaurus.owl#", "ncit:",elem)
  elem <- gsub("<http://schema.org/", "schema:", elem)

  # From the Study.TTL ontology
  # owl
  elem <- gsub("<http://www.w3.org/2002/07/owl#", "owl:", elem)
  elem <- gsub("<http://www.w3.org/1999/02/22-rdf-syntax-ns#", "rdf:", elem)
  elem <- gsub("<http://www.w3.org/2000/01/rdf-schema#", "rdfs:", elem)
  
  # Remove the trailing >
  elem <- gsub(">", "", elem)

  # Object literals require removal of quotes and type to get value only
  elem <- gsub("^\"", "", elem)  # quote at start of value
  elem <- gsub("\"\\S+", "", elem)  # quote value end and type
}  

#TODO  Change this code/function to something more efficient.
triples$s <- iriToQnam(triples$s) # Subjects
triples$p <- iriToQnam(triples$p) # Predicates
triples$o <- iriToQnam(triples$o) # Objects

#---- Nodes Construction ------------------------------------------------------
# Get the unique list of nodes by combine Subject and Object into 
# single column.
# "id.vars" = list of columns to keep untouched whil the unamed (s,o) are 
# melted into the "value" column.
nodeList <- melt(triples, id.vars=c("p" ))

# A node can be both a Subject and a Predicate so ensure a unique list of node names
#  by dropping duplicate values.
nodeList <- nodeList[!duplicated(nodeList$value),]

# Rename to ID for use in visNetwork and keep only that column
nodeList <- rename(nodeList, c("value" = "id" ))
nodes<- as.data.frame(nodeList[c("id")])

# Assign node color based on content (int, string) then based on prefixes
nodes$group <- 'iri'
nodes$group[grepl("^\\w+", nodes$id, perl=TRUE)]         <- "string"
nodes$group[grepl("^\\d+", nodes$id, perl=TRUE)]         <- "int"
nodes$group[grepl("ncit:|schema:", nodes$id, perl=TRUE)] <- "iriont"
nodes$group[grepl("eg:", nodes$id, perl=TRUE)]           <- "iri"

nodes$shape <- "box"
nodes$title <-  nodes$id  # mouseover. 
nodes$label <- nodes$title # label on node (always displayed)

#---- Edges -------------------------------------------------------------------
# Create list of edges with from, to for visNetwork 
edges<-as.data.frame(rename(triples, c("s" = "from", "o" = "to")))

# Edge values
#   edges$label : always displayed, so not set in current vis.
#   edges$title : only displayed on mouseover. Used in current vis.
edges$title <- edges$p

#---- Visualize ---------------------------------------------------------------
        
visNetwork(nodes, edges, height = "1200px", width = "100%") %>%
  visOptions(selectedBy    = "group", 
          highlightNearest = TRUE, 
          nodesIdSelection = TRUE) %>%
  
  visEdges(arrows = list(to = list(enabled = TRUE, scaleFactor = 0.5)),
           color  = "gray",
           smooth = list(enabled = FALSE, type = "cubicBezier", roundness=.8)) %>%
  
  visGroups(groupname = "iri",    color = list(background     = "#BCF5BC", 
                                                   border     = "#CCCCCC",
                                                   highlight  = "#FFFF33")) %>%

  visGroups(groupname = "iriont", color = list(background     = "#FFC862",
                                                   border     = "#CCCCCC", 
                                                   highlight  = "#FFFF33")) %>%

  visGroups(groupname = "string", color = list(background     = "#E4E4E4", 
                                                   border     = "#CCCCCC", 
                                                   highlight  = "#FFFF33")) %>%
  visGroups(groupname = "int",    color = list(background     = "#C1E1EC", 
                                                   border     = "#CCCCCC",
                                                   highlight  = "#FFFF33" )) %>%

  visIgraphLayout(layout  = "layout_nicely",
                    physics = FALSE) %>%  
    
  visIgraphLayout(avoidOverlap = 1)
