###############################################################################
# FILE: ValidateTTLScript.R
# DESC: Devleop the validation steps outside of the Shiny App, then apply them
#       to the Shiny App.
# SRC : hard coded .TTL file
# IN  : Study1-QCTest.TTL - Manually updated file to test Node and Relation 
#       exceptions
# OUT : 
# REQ : redland to read TTL
# SRC : 
# NOTE: Hardcoded list of standardNodes and standardRelation.
#       Currently not checking that the proper prefixes were assigned!
# TODO: 
###############################################################################
library(reshape2) # melt
library(redland)
setwd("C:/_github/LinkedDataWorkshop/CSS2018")

#---- Values to check ---------------------------------------------------------
#    Nodes that should be present in all graphs
standardNodes <- c("eg:ActiveArm", "eg:Drug1", "eg:PlaceboArm", "eg:Serum114", "ncit:Female", "ncit:Male")

#    Relations that should be present in all graphs
standardRelations <- c("eg:age", "eg:LDExpert", "eg:participatesIn", "eg:randomizedTo",
  "eg:trtArm", "eg:trtArmType", "ncit:drugname", "ncit:gender", "ncit:phase", "ncit:study",
  "schema:givenName")

#---- Read the source TTL file ---------------------------------------------
# Setup the file read
world <- new("World")
storage <- new("Storage", world, "hashes", name="", options="hash-type='memory'")
model <- new("Model", world=world, storage, options="")
parser <- new("Parser", world, name = 'turtle', mimeType = 'text/turtle')
redland::parseFileIntoModel(parser, world, "data/Study1-QCTest.ttl", model)

# Construct and execute the query
queryString <- '
    PREFIX eg: <http://example.org/LDWorkshop#>
    PREFIX ncit: <http://ncicb.nci.nih.gov/xml/owl/EVS/Thesaurus.owl#>
    PREFIX schema: <http://schema.org/>
    SELECT ?s ?p ?o
    WHERE { ?s ?p ?o . } '

query <- new("Query", world, queryString, base_uri=NULL, query_language="sparql", query_uri=NULL)
queryResult <- executeQuery(query, model)

# Wrap getNextResult in a loop that runs until NULL is returned.
queryResults = NULL;
repeat{
  nextResult <- getNextResult(queryResult)
  queryResults <- rbind(queryResults, data.frame(nextResult))
  if(is.null(nextResult)){
    break
  }
}

#---- Process the data --------------------------------------------------------
#---- RELATIONS
# Change IRI to use prefixes  TODO: Create function for this step
# eg:
ttlRelations <- gsub("<http://example.org/LDWorkshop#", "eg:", queryResults$p)
# ncit:
ttlRelations <- gsub("<http://ncicb.nci.nih.gov/xml/owl/EVS/Thesaurus.owl#", "ncit:", ttlRelations)
# schema
ttlRelations <- gsub("<http://schema.org/", "schema:", ttlRelations)
# Remove the trailing >
ttlRelations <- gsub(">", "", ttlRelations)

ttlRelations <- sort(unique(ttlRelations))
# ttlRelations

#---- Compare and flag values
flaggedRelations <-setdiff(ttlRelations, standardRelations)

# ---- NODES
nodeList <- melt(queryResults, id.vars=c("p" ))
nodes <- nodeList$value
nodes <-sort(unique(nodes))

# Change IRI to use prefixes  TODO: Create function for this step for Nodes,Relations
# eg:
processNodes <- gsub("<http://example.org/LDWorkshop#", "eg:", nodes)
# ncit:
processNodes <- gsub("<http://ncicb.nci.nih.gov/xml/owl/EVS/Thesaurus.owl#", "ncit:", processNodes)
# schema
processNodes <- gsub("<http://schema.org/", "schema:", processNodes)
# Remove the trailing >
processNodes <- gsub(">", "", processNodes)

# Remove integer and string. Not used in comparisons.
cleanNodes<-processNodes[!grepl("integer|string", processNodes)] 
# Extract values from the IRIs.
cleanNodes <- gsub("\\S*/(?:LDWorkshop#|Thesaurus.owl#)*(\\S+)>", "\\1", cleanNodes)
cleanNodes

# Parse Study node obtain attendee number used to check 
#    other nodes: Person<n>, TrtArm<n-n>.
studyNode <-cleanNodes[grepl("(S|s)tudy", cleanNodes)] 

#Get Attendee Num from Study Node. Must assume it is correct!
attendeeNum <-gsub("eg:Study", "", studyNode)

# Phase Node
phaseNode <-cleanNodes[grepl("Phase", cleanNodes)] 

#TODO Now use attendeeNum to check Person<n> and TrtArm<n> values.

# Person Nodes
personNodes <-cleanNodes[grepl("Person", cleanNodes)] 

# TrtArm Nodes
armNodes <-cleanNodes[grepl("TrtArm", cleanNodes)] 

# Nodes that should be in all studies
ttlNodes<-cleanNodes[!grepl("Study|Person|TrtArm|Phase", cleanNodes)] 

flaggedNodes <- setdiff(ttlNodes, standardNodes)

# Check Nodes unique to each Attendee and flag any that do not fit required patter
# Study<n> is NOT checked because Study<n> is used to extract the attendeeNum, so it
#   will always be correct according to this code. 
# Phase : Check that it is "Phase" then: Phase3, PhaseIIb, Phase2b all acceptable.
if (grepl("ncit:Phase\\S{1-3}", phaseNode)) {
  print ("----Phase Pattern Match")
}else{
  print ("----ERROR: Phase Pattern fail.")
   append(flaggedNodes, phaseNode)
}

# Person : Check : "Person" + "attendeeNum" + <n>
personRegex <- paste0("eg:Person", attendeeNum, "\\d+")
sapply(personNodes, function(person){
  if (grepl(personRegex, person)) {
    print ("----Person Pattern Match")
  }else{
    print ("----ERROR: Person Node fail.")
    flaggedNodes<<-append(flaggedNodes, person)
  }
}) 

# TrtArm : Check: "TrtArm" + attendeeNum+ "-"+ number 
armRegex <- paste0("eg:TrtArm", attendeeNum, "-", "\\d")
sapply(armNodes, function(arm){
  if (grepl(armRegex, arm)) {
    print ("----Arm Pattern Match")
  }else{
    print ("----ERROR: Arm Node fail.")
    flaggedNodes<<-append(flaggedNodes, arm)
  }
}) 

# Add logic to test length() of vectors and display based on that.

flaggedRelations

flaggedNodes
