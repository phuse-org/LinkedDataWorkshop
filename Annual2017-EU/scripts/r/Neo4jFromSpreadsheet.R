###############################################################################
# FILE: /scripts/r/Neo4jFromSpreadsheet.R
# DESC: Import data from the Neo4j Model spreadsheet, upload to Neo4j
#       Insert new data into existing RDFModel.xlsx 
# SRC : http://stackoverflow.com/questions/25295590/how-to-create-nodes-in-rneo4j-using-vectors-or-dataframes      
# IN  : /data/Neo4jModel.xlsx
# OUT : Direct Upload to Neo4j
#       /data/RDFModel.xlsx (update with new data)
# REQ : Neo4j instance running, database present for the user
#       .../data/Neo4jDB/Exercise
# SRC : 
# NOTE: ! CAUTION Young Padawan !
#         Script automagically deletes existing Neo4j graph without confirmation!
#         Remove option input=FALSE to provide confirmation prompt.
###############################################################################
library(RNeo4j)
library(readxl)
library(plyr)
library(reshape2)
library(XLConnect)

# setwd("C:/_gitHub/LinkedDataWorkshop/Annual2017-EU")  #TODO: DELETE.Local dev only
setwd("C:/LinkedDataWorkshop")

# Read spreadsheet ----
NeoModel<- read_excel("data/Neo4jModel.xlsx", 
  sheet = 'Neo4jModel',
  skip = 1,
  col_names = TRUE)

# QC Checking 1 ---------------------------------------------------------------
# Detect node names that contain spaces; illegal for R Script.
nodes <- melt (NeoModel, id.vars=c("Relation", "X__1", "Property","Value"))
nodes <- data.frame(nodes[,"value"])
# Remove NA and duplicates, rename column.
nodes <- na.omit(data.frame(nodes[,1]))
nodes <- data.frame(nodes[!duplicated(nodes),1])
colnames(nodes) <- "node"
nodesWithSpaces <- data.frame(nodes[grep("^\\S+\\s+\\S+", nodes$node), ])
# if (! is.null(dim(nodesWithSpaces))){
if (nrow(nodesWithSpaces) > 0){
  message("ERROR: Spaces in node names not permitted in this exercise!" )
  message("ERROR: Fix node names, then re-run script.")
  nodesWithSpaces
  stop()
}

# DF to create nodes and relations
NeoNodRel <- (NeoModel[,c("StartNode", "Relation", "EndNode")])
NeoNodRel <- NeoNodRel[complete.cases(NeoNodRel),] # Remove extra NA rows
NeoNodRel$nodeId <- 1:(nrow(NeoNodRel))

# DF create node property value pairs
NeoNPV <- NeoModel[,c("Node", "Property", "Value")]
NeoNPV <- NeoNPV[complete.cases(NeoNPV),] # Remove extra NA rows
NeoNPV$NPVId <- 1:(nrow(NeoNPV))

# Assign node types ----
NeoNPV$type <- "undefined"
NeoNPV$type[grepl("Person", NeoNPV$Node)] <- "person" 
NeoNPV$type[grepl("Treat", NeoNPV$Node)]  <- "treatment" 
NeoNPV$type[grepl("Study", NeoNPV$Node)]  <- "study" 

# QC Checking 2 ---------------------------------------------------------------
# Case 1:  Node specified in a relation is not defined in the Nodes sxn of 
#          spreadsheet. Script execution terminates.
NeoNodRel$StartNodeMatch <- NeoNodRel$StartNode %in% NeoNPV$Node
NeoNodRel$EndNodeMatch <- NeoNodRel$EndNode %in% NeoNPV$Node

err_crit <- FALSE  # Flag for critical error script termination
ddply(NeoNodRel, .(nodeId), function(NeoNodRel){
  if (NeoNodRel$StartNodeMatch == FALSE) {
    message("ERROR: Node found in relation is not a defined node." )
    message(paste0("Node name:", NeoNodRel$StartNode))    
    err_crit <<- TRUE
  }else if (NeoNodRel$EndNodeMatch == FALSE) {
    message("ERROR: Node found in relation is not a defined node." )
    message(paste0("Node name:", NeoNodRel$EndNode))    
    err_crit <<- TRUE
  }
})

if (err_crit == TRUE){
  message ("Script Terminated due to errors in source data.")
  stop()
}    

# Case 2:  A defined node does not participate in a relation.
#          Issue a warning. Script execution continues.
NeoNPV$NodeMatchStartNode <- NeoNPV$Node %in% NeoNodRel$StartNode
NeoNPV$NodeMatchEndNode <- NeoNPV$Node %in% NeoNodRel$EndNode

ddply(NeoNPV, .(NPVId), function(NeoNPV){
  if ( ! NeoNPV$NodeMatchStartNode & ! NeoNPV$NodeMatchEndNode) {
    message(paste0("WARNING: Node not used in any relation: ", NeoNPV$Node))
  }
})

# END QC Checks ---------------------------------------------------------------

# RDF Spreadsheet Insert ------------------------------------------------------
#   Insert user-created values from the Ne04j Spreadsheet into the  RDF 
#     Spreadsheet for use in the RDF exercises.  
#     "Shhhhh! Do not tell anyone or you will ruin the surprise later."

# Table 1 : Nodes and relations
NeoNodRelNew <-NeoNodRel[,c("StartNode", "Relation", "EndNode")]
# Omit first 3 rows of original data to get new nodes only.
NeoNodRelNew <- tail(NeoNodRelNew, -3)  
colnames(NeoNodRelNew) <- c("subject", "predicate", "object")

# Table 2: Node P:V Pairs
NeoNPVNew <-NeoNPV[,c("Node", "Property", "Value")]
# Omit first 5 rows of original data to get new nodes only.
NeoNPVNew <- tail(NeoNPVNew, -3)  
colnames(NeoNPVNew) <- c("subject", "predicate", "object")

# Combine the dataframes
spoDf <-  rbind(NeoNodRelNew, NeoNPVNew)

# Load RDF workbook; DO NOT create workbook if it does not exist
wb <- loadWorkbook("data/RDFModel.xlsx", create= FALSE)

# Insert into the workbook at startRow, startCol with  no header row
writeWorksheet(wb, spoDf, sheet="RDFModel",
  startRow=10, startCol=1,
  header=FALSE)

# Save workbook
saveWorkbook(wb)

# Neo4j -----------------------------------------------------------------------
graph = startGraph("http://localhost:7474/db/data")
# Clear existing graph from previous uploads
#  CAUTION: input=FALSE removes confirmation prompt!
clear(graph, input = FALSE)  

# 1. Nodes and their P:V pairs
query = "
MERGE (node:NODETYPE { name:{node_name}})
WITH node
MATCH (node:NODETYPE { name:{node_name}})
SET node.PROPERTYNAME={property_value}
"

t = newTransaction(graph)

ddply(NeoNPV, .(NPVId), function(NeoNPV){
  # Trickery here to set property name dynamically, since use of 
  #  {} only appears to work for values of properties, not names
  query <- gsub("PROPERTYNAME", NeoNPV$Property, query)
  query <- gsub("NODETYPE", NeoNPV$type, query)
  foo <<- query
  
  node_name      = NeoNPV$Node
  property_value = NeoNPV$Value
  node_type      = NeoNPV$type
  
  appendCypher(t, 
    query, 
    node_name      = node_name, 
    property_value = property_value
  )
})
commit(t)

# Step 2: Relations
query = "
MATCH (startnode {name:{startnode_name}}), (endnode {name:{endnode_name}})
CREATE (startnode)-[:RELATIONNAME]->(endnode)
"

t = newTransaction(graph)

ddply(NeoNodRel, .(nodeId), function(NeoNodRel){
  query <- gsub("RELATIONNAME", NeoNodRel$Relation, query)

  startnode_name = NeoNodRel$StartNode
  endnode_name   = NeoNodRel$EndNode

  appendCypher(t, 
    query, 
    startnode_name = startnode_name, 
    endnode_name   = endnode_name 
  )
})
commit(t)

message("Success! Neo4j data available at http://localhost:7474/browser/")