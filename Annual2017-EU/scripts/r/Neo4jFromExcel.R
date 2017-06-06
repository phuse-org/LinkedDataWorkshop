###############################################################################
# FILE: /scripts/R/Neo4jFromExcel.R
# DESC: Import data from the Neo4j Model spreadsheet, upload to Neo4j
# SRC : http://stackoverflow.com/questions/25295590/how-to-create-nodes-in-rneo4j-using-vectors-or-dataframes      
# IN  : /data/Neo4jModel.xlsx
# OUT : Upload to Neo4j
# REQ : Neo4j instance running, database present for the user
#       .../data/Neo4jDB/Exercise
# SRC : 
# NOTE: CAUTION: Code will delete existing Neo4j graph without confirmation!
#                Remove option input=FALSE to provide confirmation prompt.
# TODO: 
#       Add data checks: a) Allnodes in both dataframes See code for details
#                        b) No spaces in node names.
#       Add TYPE to the nodes for coloration (pretty-pretty)
###############################################################################
library(RNeo4j)
library(readxl)
library(plyr)

setwd("C:/_gitHub/LinkedDataWorkshop/Annual2017-EU/")

# Read in the spreadsheet
NeoModel<- read_excel("data/Neo4jModel.xlsx", 
                      sheet = 'Neo4jModel',
                      skip = 1,
                      col_names = TRUE)

# Dataframes for nodes and relations
#-- To create nodes and relations
NeoNodRel <- (NeoModel[,c("Node1", "Relation", "Node2")])
NeoNodRel <- NeoNodRel[complete.cases(NeoNodRel),]   # Remove extra rows (NA values)
NeoNodRel$nodeId <- 1:(nrow(NeoNodRel))


#-- To create node property value pairs
NeoNPV <- NeoModel[,c("Node", "Property", "Value")]
NeoNPV <- NeoNPV[complete.cases(NeoNPV),]   # Remove extra rows (NA values)
NeoNPV$NPVId <- 1:(nrow(NeoNPV))

#TODO: Write this error section...
# Detect error where all nodes not present in one of the dataframes
#   Throw an error and stop if all nodes not present in both.



#Neo4j
graph = startGraph("http://localhost:7474/db/data")
# Clear existing graph from previous uploads
#  CAUTION: input=FALSE removed confirmation prompt!
clear(graph, input = FALSE)  


# Step 1: Create nodes and their P:V pairs
query = "
MERGE (node:Node{ name:{node_name}})
WITH node
MATCH (node:Node{ name:{node_name}})
SET node.PROPERTYNAME={property_value}
"
t = newTransaction(graph)

ddply(NeoNPV, .(NPVId), function(NeoNPV)
{
    # Trickery here to set property name dynamically, since use of 
    #  {} only appears to work for values of properties, not names
    query <- sub("PROPERTYNAME", NeoNPV$Property, query)
    
    node_name      = NeoNPV$Node
    property_value = NeoNPV$Value
    
    appendCypher(t, 
        query, 
        node_name      = node_name, 
        property_value = property_value)
})

commit(t)

#----- ORIGINAL STEPS HERE
# Step 2: Relations
query = "
MATCH (node1 {name:{node1_name}}), (node2 {name:{node2_name}})
CREATE (node1)-[:RELATIONNAME]->(node2)
"

t = newTransaction(graph)

ddply(NeoNodRel, .(nodeId), function(NeoNodRel)
{
    
    # Trickery here to set property name dynamically, since use of 
    #  {} only appears to work for values of properties, not names of Reln's?
    query <- sub("RELATIONNAME", NeoNodRel$Relation, query)

    node1_name = NeoNodRel$Node1
    node2_name = NeoNodRel$Node2

    appendCypher(t, 
        query, 
        node1_name = node1_name, 
        node2_name = node2_name 
        )
})

commit(t)
