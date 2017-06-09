###############################################################################
# $HeadURL: file:///C:/SVNLocalRepos/PhUSE/Annual/2017/workshop/Exercises/r/RDFFromExcel.R $
# $Rev: 228 $
# $Date: 2017-06-01 15:17:13 -0400 (Thu, 01 Jun 2017) $
# $Author: U041939 $
# -----------------------------------------------------------------------------
# DESCR: Create RDF from Excel spreadsheet for RDF modeling exercise 
# INPUT: 
# OUT  : 
# REQ  :  Apache riot in system path for validation step       
# NOTES: 
# TODO: 
###############################################################################
library(readxl)
library(redland)
library(plyr)
#DEL library(dplyr)
# setwd("C:/_sandbox/PhUSE/Annual/2017/Workshop/Exercises")
setwd("C:/_gitHubShared/LinkedDataWorkshop/Annual2017-EU")

# Read in the source Excel file
RDFModel<- read_excel("data/RDFModel.xlsx", sheet = 'RDFModel')

RDFModel$rowID <- 1:nrow(RDFModel) # row index

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
        # subject=paste0(phuse, RDFModel$Subject), 
        # predicate=paste0(phuse, RDFModel$Predicate), 

ddply(RDFModel, .(rowID), function(RDFModel)
{
#    stmt <- new("Statement", world=world, 
#        subject   = paste0(phuse, RDFModel$Subject), 
#        predicate = paste0(phuse, RDFModel$Predicate),
#        object    = paste0(phuse, RDFModel$Object) 
#    )
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
    
    
    
    #object="slaughter", objectType="literal", datatype_uri="http://www.w3.org/2001/XMLSchema#string")
    #
    addStatement(model, stmt)
    
})  # Triple building completed. 


#stmt <- new("Statement", world=world, 
#        subject="http://ropensci.org/", predicate=paste0(dc, "language"), object="en")


# Serialize the model to a TTL file
serializer <- new("Serializer", world, name="turtle", mimeType="text/turtle")
status <- setNameSpace(serializer, world, namespace="http://www.example.org/phuse/workshop/", prefix="phuse")  
status <- setNameSpace(serializer, world, namespace="http://www.w3.org/2001/XMLSchema#", prefix="xsd")  

outFileTTL <- "data/RDFModel.TTL"
status <- serializeToFile(serializer, world, model, outFileTTL)

# If no message to console, file passed validation. Be happy.
system(paste('riot --validate ', outFileTTL),
    show.output.on.console = TRUE)

