###############################################################################
# FILE: /scripts/r/Drug1Pool.R
# DESC: Create JSON for read by D3JS FN graph
# IN  : 
# OUT : 
# REQ : 
# NOTE: 
# Sauce: CTDasRDF\vis\r\Person-FNGRAPH.R - data manip post query
# TODO: 1. Reload of TTL does not reset QC Check data. Need to reset dataframes
###############################################################################
library(SPARQL)
ep = "http://localhost:5820/Drug1Pool/query" # not working as of 2017-01-16
# ep = "http://localhost:5820/#/databases/Drug1Pool/query"
#METHOD 2: Service

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
triples

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

# ncit
nodeList$type[grepl("^ncit:", nodeList$name, perl=TRUE)] <- "ncit"

# schema
nodeList$type[grepl("^schema:", nodeList$name, perl=TRUE)] <- "schema"

nodeList




nodes$type[grepl("ncit:|schema:", nodes$id, perl=TRUE)] <- "iriont"
nodes$type[grepl("eg:", nodes$id, perl=TRUE)] <- "iri"
        

