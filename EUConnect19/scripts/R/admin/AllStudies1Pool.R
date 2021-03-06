###############################################################################
# FILE: /scripts/r/AllStudiesPool.R
# DESC: Create JSON for read by D3JS FN graph
# IN  : 
# OUT : C:/_gitHub/LinkedDataWorkshop/CSS2018/presentation/AllStudiesPool.JSON
# REQ : Stardog running with data in AllStudies database, loaded from LDW workshop
#       exercises.
# NOTE: 
# Sauce: CTDasRDF\vis\r\Person-FNGRAPH.R - data manip post query
# TODO: 1. Reload of TTL does not reset QC Check data. Need to reset dataframes
###############################################################################
library(SPARQL)
library(reshape2)
library(plyr)
library(jsonlite)

ep = "http://localhost:5820/AllStudies/query"

# Define the namespaces
namespaces <- c('eg', '<http://example.org/LDWorkshop#>',
'ncit', '<http://ncicb.nci.nih.gov/xml/owl/EVS/Thesaurus.owl#>',
'rdf', '<http://www.w3.org/1999/02/22-rdf-syntax-ns#>',
'rdfs', '<http://www.w3.org/2000/01/rdf-schema#>',
'schema', '<http://schema.org/>',
'xsd', '<http://www.w3.org/2001/XMLSchema#>'
)

query = '
SELECT ?s ?p ?o
WHERE 
{
   ?s ?p ?o
}'

qd <- SPARQL(url=ep, query=query, ns=namespaces)
triples <- as.data.frame(qd$results)

#---- Nodes -------------------------------------------------------------------
# Get the unique list of nodes as needed by the JSON file:
# Combine Subject and Object into a single column
# "id.vars" is the list of columns to keep untouched. The unamed ones are 
# melted into the "value" column.
nodeList <- melt(triples, id.vars=c("p" ))   # subject, object into 1 column.

# A node can be both a Subject and an Object so ensure a unique list of node names
#  by dropping duplicate values.
nodeList <- nodeList[!duplicated(nodeList$value),]

# Rename column value to 'name' for use in nodes list for JSON
colnames(nodeList)<-c("p", "var", "name")  # column name should be name, not value.
nodeList <-arrange(nodeList,name)  # sort prior to adding ID value. (not necessary, of course)

# Create the node ID values starting at 0 (as req. by D3JS)
id<-0:(nrow(nodeList)-1) 
nodeList<-data.frame(id, nodeList)  

# discard columns left over from teh merge.
nodeList <- nodeList[,c("id", "name")]
head(nodeList)

# nodeCategory used for grouping in the FN graph. Assign grouping based on type
#   Make this smarter later: sort on unique type and assign index value.
#   Must now be updated manually when a new node type appears. Boo. Bad code.Bad!

# Node Type ----
# Assign node color based on content (int, string) then based on prefixes
nodeList$type <- 'iri'

# string
nodeList$type[!grepl(":", nodeList$name, perl=TRUE)] <- "string"  # will include Int, but see next.


# int
nodeList$type[grepl("^\\d+", nodeList$name, perl=TRUE)] <- "int"
# person
nodeList$type[grepl("^eg:Person\\d+", nodeList$name, perl=TRUE)] <- "person"

# study
nodeList$type[grepl("^eg:Study\\d+", nodeList$name, perl=TRUE)] <- "study"
# Study: special:  99 that has subjects from all the different studies. 
nodeList$type[grepl("^eg:Study99", nodeList$name, perl=TRUE)] <- "crossstudy"


# TrtType
nodeList$type[grepl("^eg:ActiveArm", nodeList$name, perl=TRUE)] <- "trtAct"
nodeList$type[grepl("^eg:PlaceboArm", nodeList$name, perl=TRUE)] <- "trtPlc"



# ncit
nodeList$type[grepl("^ncit:", nodeList$name, perl=TRUE)] <- "ncit"
nodeList$type[grepl("^ncit:Male", nodeList$name, perl=TRUE)] <- "male"
nodeList$type[grepl("^ncit:Female", nodeList$name, perl=TRUE)] <- "female"


# schema
nodeList$type[grepl("^schema:", nodeList$name, perl=TRUE)] <- "schema"


# Drug 1 product
nodeList$type[grepl("eg:AllStudies", nodeList$name, perl=TRUE)] <- "product"


# nodeCategory used for grouping in the FN graph. Assign grouping based on type
#   Make this smarter later: sort on unique type and assign index value.
#   Must now be updated manually when a new node type appears. Boo. Bad code.Bad!
# Types that are NOT clustered: literal
nodeList$nodeCategory <- '6'   # Assign to IRI by default, then reassign based on tyep
nodeList$nodeCategory[grepl('string', nodeList$type)]  <- '1'      
nodeList$nodeCategory[grepl('int',    nodeList$type)]  <- '2'
nodeList$nodeCategory[grepl('person',   nodeList$type)]  <- '3'      
nodeList$nodeCategory[grepl('study',  nodeList$type)]  <- '4'
nodeList$nodeCategory[grepl('ncit',    nodeList$type)]  <- '5'      
nodeList$nodeCategory[grepl('schema',   nodeList$type)]  <- '5'
nodeList$nodeCategory[grepl('iri',    nodeList$type)]  <- '6'      

nodes<-data.frame(id=nodeList$id,
                  type=nodeList$type,
                  label=nodeList$name,
                  nodeCategory=nodeList$nodeCategory)

#---- Edges -------------------------------------------------------------------
# Now assign the node ID numbers to the Subject and Object nodes
#-- Subject Nodes, ID becomes the subject ID node
#   Assign node ID values to the Subject nodes
edgesList <- merge (triples, nodeList, by.x="s", by.y="name")

# Merge by 's', so 'id' becomes 'subjectID' 
#    'p' becomes 'predicate'
edgesList<-rename(edgesList, c("id" = "subjectID", "p" = "predicate"))

# Clean it up
edgesList<-edgesList[c("s", "subjectID", "predicate", "o")] 

# Merge by 'o' to assign node ID values to the Object nodes
edgesList <- merge (edgesList, nodeList, by.x="o", by.y="name")

# p is renamed to "value" for use in LINKS dataframe. "value" is needed above here.
edgesList<-rename(edgesList, c("id"="objectID"))   #TW was just 'id' not id.x here.
edgesList<-edgesList[c("s", "subjectID", "predicate", "o", "objectID")] 


#---- Edge types
edgesList$edgeType <- 'other'  # Default/unassigned as Other

# class
edgesList$edgeType[grepl('rdf:type|rdfs:subClassOf', edgesList$predicate, perl=TRUE)] <- 'class'  # Present? 

# schema ontology
edgesList$edgeType[grepl('schema:', edgesList$predicate, perl=TRUE)] <- 'schema'

# ncit ontology
edgesList$edgeType[grepl('ncit:', edgesList$predicate, perl=TRUE)] <- 'ncit'

# study
edgesList$edgeType[grepl('eg:', edgesList$predicate, perl=TRUE)] <- 'study'


# Create edges dataframe that contains: source, target, value, and type columns
edgesList<-rename(edgesList, c("subjectID"="source", "objectID"="target", "predicate"="value", "edgeType"="edgeType"))
edges<- as.data.frame(edgesList[c("source", "target", "value", "edgeType")])

#-- Combine the nodes and edges into a single dataframe for conversion to JSON
all <- list(nodes=nodes,
            edges=edges)
# Write out to JSON
# fileConn<-file("./vis/d3/data/Person-FNGraph.JSON") # for CTDasRDF Project
fileConn<-file("C:/_gitHub/LinkedDataWorkshop/CSS2018/presentation/AllStudies.JSON") # for CTDasRDF Project

writeLines(toJSON(all, pretty=TRUE), fileConn)
close(fileConn)