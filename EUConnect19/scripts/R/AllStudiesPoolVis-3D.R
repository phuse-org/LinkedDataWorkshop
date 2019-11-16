###############################################################################
# FILE: /scripts/r/AllStudiesPoolVis-Condensed.R
# DESC: 3D Network  graph of the pooled studies
# IN  : Stardog graph AllStudies  (Stardog instance running, pool populated)
# OUT : network3d graph
# REQ : 
# NOTE: Creates an HTML widget that must be viewed in Chrome or IE (a browser,
#       not the view pane)
#       edges dataframe can only have two variables: source, target, else 
#       vis. will fail.
#       Background color for RevealJS theme is : #002b36
#       Node colors copied from the orignal forcenetwork graph/ graph editor
# TODO: 
#       
#       
###############################################################################
library(network3d)
library(plyr)     #  rename
library(reshape)  #  melt
library(SPARQL)
library(tidyverse)

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
  
  # ct:
  elem <- gsub("<http://bio2rdf.org/clinicaltrials:", "ct:", elem)
  
  #dbpedia
  elem <- gsub("<http://dbpedia.org/resource/", "dbpedia:", elem)
  
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
#   A good palette for a very dark background.
# Green:         #2ECC40
# Bright Green:  #01FD6F
# Mid Green:     #3D9970
# Dk Blue:       #0071D3
# Bright Blue:   #39CCCC
# Pastel Blue:   #7EDBFE
# Yellow:        #FEDC00
# Orange:        #E77818
# Bright Orange: #DD382E
# Bright Pink:   #F011BE
# Dark Pink:     #85144B
# Purple:        #990BAE



nodes$color                                              <- '#BCF5BC'
#nodes$color[grepl("^\\w+", nodes$id, perl=TRUE)]         <- "#E4E4E4"
#nodes$color[grepl("^\\d+", nodes$id, perl=TRUE)]         <- "#C1E1EC"
##nodes$color[grepl("ncit:|schema:", nodes$id, perl=TRUE)] <- "#FFC862"
##nodes$color[grepl("ct:|dbpedia:", nodes$id, perl=TRUE)]  <- "#BCBDDC"
#nodes$color[grepl("eg:", nodes$id, perl=TRUE)]           <- "#BCF5BC"


#TODO Fix color assignment logics. currently incorrect regex and order on some.

# New scheme. Start Generic (string) and progress to more specific

# String
#nodes$color[grepl("^\\w+", nodes$id, perl=TRUE)]         <- "#7EDBFE"

#Integer
nodes$color[grepl("^\\d+", nodes$id, perl=TRUE)]         <- "#39CCCC" 

# Person
nodes$color[grepl("eg:Person", nodes$id, perl=TRUE)]     <- "#F011BE" 

# Items in the ontology
#nodes$color[grepl("HumanStudySubject|eg:Person|Expert|Male|Female|owl:|eg:TrtArmType|eg:randomizedTo", 
#                               nodes$id, perl=TRUE)]     <- "#E77818"

nodes$color[grepl("eg:randomized|eg:trtarm$|eg:trtarmtype|owl:Class|schema:Person|Expert|owl:|ncit:study|subject|ncit", 
                               nodes$id, perl = TRUE, ignore.case = TRUE)]     <- "#E77818"



# Related to Treatment arms
#nodes$color[grepl("Arm", nodes$id, perl=TRUE)]           <- "#FEDC00"   

# NCT ID
nodes$color[grepl("ct:NCT", nodes$id, perl=TRUE)]        <- "#01FD6F"   


# Study
nodes$color[grepl("eg:Study\\d+", nodes$id, perl=TRUE)]  <- "#85144B"


# Set size based on type of node. Studynn is large.
nodes$size <- .07 
nodes$size[grepl("eg:Study\\d+", nodes$id, perl=TRUE)]  <- 0.2



nodes$name <-  nodes$id  # mouseover. 

#---- Edges -------------------------------------------------------------------
# Create list of edges source, targetwith from, to for visNetwork 
edges<-as.data.frame(rename(triples, c("s" = "source", "o" = "target")))
edges <- edges[,c("source", "target")]



network3d(nodes, edges,
          background_color  = '#002b36',
          edge_opacity      = 0.2,
          force_explorer = TRUE, 
          link_strength     = 0.5 , 
          manybody_strength = -4.4, 
          max_iterations    = 100,
          static_length_strength = TRUE
)


