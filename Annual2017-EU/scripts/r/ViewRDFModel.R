###############################################################################
# FILE : Person-MultLevel-VisNetwork-ForceNetwork.R
# DESCR: Visualization of the nodes connected to Person_1 as a FN graph
# SRC  : 
# KEYS : 
# NOTES: Docs:  https://cran.r-project.org/web/packages/visNetwork/visNetwork.pdf 
#         
#
# INPUT: cdiscpilot01.TTL  (OR) local endpoint graph SDTMTORDF
#      : 
# OUT  : 
# REQ  : 
# TODO :  Convert from use of rrdf to redland package 
#        Clean up the RDF:TYPE edge label tl 'a'
#        
#        
#        
###############################################################################
library(plyr)     #  rename
library(reshape)  #  melt
# library(redland)
library(rrdf)
library(visNetwork)

setwd("C:/_sandbox/PhUSE/Annual/2017/Workshop/Exercises")

# Select all triples. 
#   REGEX eliminates unnecessary RDF and RDFS triples
query = 'PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX phuse: <http://www.example.org/phuse/workshop/>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>      
PREFIX x: <http://example.org/bogus>

SELECT ?s ?p ?o 
WHERE { ?s ?p ?o . 
    FILTER (REGEX(STR(?s), "phuse"))
}'

#-- Local TTL file
rdfSource = load.rdf("data/RDFModel.TTL", format="N3")
RDFTriples = as.data.frame(sparql.rdf(rdfSource, query))

# Remove any rows that have a blank Object. 
RDFTriples<-RDFTriples[!(RDFTriples$o==""),]

# Remove duplicates from the query
RDFTriples <- RDFTriples[!duplicated(RDFTriples),]


#---- Nodes Construction
# Get the unique list of nodes 
# Combine Subject and Object into a single column
# "id.vars" is the list of columns to keep untouched. The unamed ones (s,o) are 
# melted into the "value" column.
nodeList <- melt(RDFTriples, id.vars=c("p" ))

# A node can be both a Subject and a Predicate so ensure a unique list of node names
#  by dropping duplicate values.
nodeList <- nodeList[!duplicated(nodeList$value),]

# Rename to ID for use in visNetwork and keep only that column
nodeList <- rename(nodeList, c("value" = "id" ))
nodes<- as.data.frame(nodeList[c("id")])

# Assign groups used for icon types and colours
# Order is important.
# NOTE: Groups you define in your data must be specfied here in in additional 
#      statements.
nodes$group[grepl("Protocol", nodes$id, perl=TRUE)] <- "Protocol"  
nodes$group[grepl("Study", nodes$id, perl=TRUE)] <- "Study"  
nodes$group[grepl("Treatment", nodes$id, perl=TRUE)] <- "Treatment"  
nodes$group[grepl("Person", nodes$id, perl=TRUE)] <- "Person"  
nodes$group[! grepl(":", nodes$id, perl=TRUE)] <- "Literal"
# Kludge that fails if a literal has a colon. Close enough for exercise.
nodes$shape <- ifelse(grepl(":", nodes$id), "ellipse", "box")

# Assign labels used for mouseover and for label
nodes$title <- nodes$id
nodes$label <- gsub("\\S+:", "", nodes$id)

#---- Edges
# Create list of edges by keeping the Subject and Predicate from query result.
edges<-rename(RDFTriples, c("s" = "from", "o" = "to"))

# Edge values
#   use edges$label for values always displayed
#   use edges$title for values only displayed on mouseover
edges$title <-gsub("\\S+:", "", edges$p)   # label : text always present

# Graph selectible by ID or Group. 
# NOTE: Groups you define in your data must be specfied here in in additional 
#     visGroups statements.
visNetwork(nodes, edges, height = "500px", width = "100%") %>%
  visOptions(selectedBy = "group", 
             highlightNearest = TRUE, 
             nodesIdSelection = TRUE) %>%
    visEdges(arrows = list(to = list(enabled = TRUE, scaleFactor = 0.5)),
             smooth = list(enabled = FALSE, type = "cubicBezier", roundness=.8)) %>%
    visGroups(groupname = "Person",    color = "#ffff33") %>%
    visGroups(groupname = "Protocol",  color = "#99C2C2") %>%
    visGroups(groupname = "Study",     color = "#A3A3C2") %>%
    visGroups(groupname = "Treatment", color = "#FFB280") %>%
    visGroups(groupname = "Literal",   color = list(background="white", border="black")) %>%
    visPhysics(stabilization=FALSE, barnesHut = list(
                                       avoidOverlap=1,
                                       gravitationalConstant = -3000,
                                       springConstant = 0.0004,
                                       damping = 0.9,
                                       springLength = 40
                                       ))  