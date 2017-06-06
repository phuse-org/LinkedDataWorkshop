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
#       Add data check: Allnodes in both dataframes See code for details
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


# Step 1: Create nodes and their P:V
query = "
MERGE (node:Node{ name:{node_name}})
WITH node
MATCH (node:Node{ name:{node_name}})
SET node.PROPERTYNAME={property_value}
"
t = newTransaction(graph)

ddply(NeoNPV, .(NPVId), function(NeoNPV)
{
    # some trickery here to set property name dynamically, since use of 
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
# Step 2: Create Nodes and relations
query = "
MERGE (node1:Node1{name:{node1_name}})
MERGE (node2:Node2{name:{node2_name}})
MERGE (node1)-[:JOINEDTO]->(node2)
"

#MERGE (origin:Airport {name:{origin_name}})
#MERGE (destination:Airport {name:{dest_name}})
#CREATE (origin)<-[:ORIGIN]-(:Flight {number:{flight_num}})-[:DESTINATION]->(destination)


t = newTransaction(graph)

ddply(NeoNodRel, .(nodeId), function(NeoNodRel)
{
    node1_name = NeoNodRel$Node1
    node2_name = NeoNodRel$Node2
    rel_name   = NeoNodRel$Relation
    
    appendCypher(t, 
        query, 
        node1_name = node1_name, 
        node2_name = node2_name, 
        rel_name   = rel_name)
})

commit(t)

#------------------------------------------------------------------------------
# Step 2: Add Property:Value Pairs to the nodes created in Step 1.
# TODO: Add section!

# Query for node-relation creation

#data = data.frame(Origin = c("SFO", "AUS", "MCI"),
#                  FlightNum = c(1, 2, 3),
#                  Destination = c("PDX", "MCI", "LGA"))


#query = "
#MERGE (origin:Airport {name:{origin_name}})
#MERGE (destination:Airport {name:{dest_name}})
#CREATE (origin)<-[:ORIGIN]-(:Flight {number:{flight_num}})-[:DESTINATION]->(destination)
#"
#
#t = newTransaction(graph)
#
#ddply(data, .(FlightNum), function(data)
#{
#    origin_name = data$Origin
#    dest_name = data$Dest
#    flight_num = data$FlightNum
#    
#    appendCypher(t, 
#        query, 
#        origin_name = origin_name, 
#        dest_name = dest_name, 
#        flight_num = flight_num)
#})

#commit(t)

#cypher(graph, "MATCH (o:Airport)<-[:ORIGIN]-(f:Flight)-[:DESTINATION]->(d:Airport)
#       RETURN o.name, f.number, d.name")
