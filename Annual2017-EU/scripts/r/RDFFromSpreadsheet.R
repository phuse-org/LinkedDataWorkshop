###############################################################################
# FILE: /scripts/r/RDFFromSpreadsheet.R
# DESC: LDWorkshop: Create RDF TTL file from spreadsheet.
# IN  : data/RDFModel.xlsx
# OUT : data/RDFModel.TTL
# REQ : Apache riot in system path for validation step       
# NOTE: Use of redland package could be replaced by rrdf package.
#       Creates only URI, INT, STRING types. 
###############################################################################
library(readxl)
library(redland)
library(plyr)

setwd("C:/LinkedDataWorkshop")

# Read Spreadsheet ----
RDFModel<- read_excel("data/RDFModel.xlsx", sheet = 'RDFModel')

err_crit <- FALSE  # Flag for critical error script termination
# QC Checks -------------------------------------------------------------------
# Missing ObjectType
if (sum(is.na(RDFModel[,"ObjectType"]))){
  message("ERROR: At least 1 ObjectType value is missing." )
  message("Make corrections in the source spreadsheet")    
  err_crit <<- TRUE
}
# Object defined as more than one type
# Kludgy, but gets the job done. Change to case when() ?
# Sort by Object for comparisons
RDFModel <- RDFModel[with(RDFModel, order(Object)), ]

for (i in 1:nrow(RDFModel)){
  if (i>1){
    if( (RDFModel[i,"Object"] == RDFModel[i-1, "Object"])
        &
        (RDFModel[i,"ObjectType"] != RDFModel[i-1, "ObjectType"])
      ){
      message ("Error! Same Object defined as different ObjectTypes")
      message (paste0("Object:", RDFModel[i,"Object"], 
        "    ObjectTypes: ", RDFModel[i,"ObjectType"], ",", RDFModel[i-1, "ObjectType"]))
      
      message("Correct in spreadsheet then re-run this script.")
      err_crit <<- TRUE
    }
  }
  
}
if (err_crit == TRUE){
  message ("Script Terminated due to errors in source data.")
  message ("Review console messages and ask for assistance if needed.")
  stop()
} 

RDFModel$rowID <- 1:nrow(RDFModel) # Row index, used in ddply
# Setup values needed by the redland pkg
# World is the redland mechanism for scoping models
world <- new("World")

# Storage provides a mechanism to store models; in-memory hashes are convenient for small models
storage <- new("Storage", world, "hashes", name="", options="hash-type='memory'")

# A model is a set of Statements, and is associated with a particular Storage instance
model <- new("Model", world=world, storage, options="")

# A prefixes for the exercises
phuse <- "http://www.example.org/phuse/workshop/"
xsd <-  " http://www.w3.org/2001/XMLSchema#"
# Loop through the data source to create triples from each row in the source XLSX

ddply(RDFModel, .(rowID), function(RDFModel)
{
    if(RDFModel$ObjectType == "string"){
        stmt <- new("Statement", world=world, 
            subject   = paste0(phuse, RDFModel$Subject), 
            predicate = paste0(phuse, RDFModel$Predicate),
            object    = paste0( RDFModel$Object),
            objectType = "literal",
            datatype_uri="http://www.w3.org/2001/XMLSchema#string" )
    }
    else if(RDFModel$ObjectType == "int"){
        stmt <- new("Statement", world=world, 
            subject   = paste0(phuse, RDFModel$Subject), 
            predicate = paste0(phuse, RDFModel$Predicate),
            object    = paste0( RDFModel$Object),
            objectType = "literal",
            datatype_uri="http://www.w3.org/2001/XMLSchema#int" )
    }
    else{
        stmt <- new("Statement", world=world, 
            subject   = paste0(phuse, RDFModel$Subject), 
            predicate = paste0(phuse, RDFModel$Predicate),
            object    = paste0(phuse, RDFModel$Object) )
        }    
    addStatement(model, stmt)
})  # Triple building completed. 

# Serialize the model to a TTL file
serializer <- new("Serializer", world, name="turtle", mimeType="text/turtle")
status <- setNameSpace(serializer, world, namespace="http://www.example.org/phuse/workshop/", prefix="phuse")  
status <- setNameSpace(serializer, world, namespace="http://www.w3.org/2001/XMLSchema#", prefix="xsd")  

outFileTTL <- "data/RDFModel.TTL"
status <- serializeToFile(serializer, world, model, outFileTTL)

# If no message to console, file passed validation. Be happy.
system(paste('riot --validate ', outFileTTL),
    show.output.on.console = TRUE)