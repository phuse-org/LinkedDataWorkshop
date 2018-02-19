###############################################################################
# FILE: CreateSPARQLForPooling.R
# DESC: Create the SPARQL script used to pool data from all the Drug1 studies 
# IN  : ClassInfo.xlsx - has IP address of each study for each attendee
# OUT : XRX-PoolAllStudies.rq 
# REQ : 
# NOTE: 
# TODO: 
###############################################################################
library(readxl)
servers <- as.data.frame(read_excel("C:/_gitHub/LinkedDataWorkshop/CSS2018/data/ClassInfo.xlsx", sheet = "Servers", col_names=TRUE))


# sink("C:/_gitHub/LinkedDataWorkshop/CSS2018/scripts/SPARQL/XRX-PoolAllStudies.rq")
rqFile <-file("C:/_gitHub/LinkedDataWorkshop/CSS2018/scripts/SPARQL/XRX-PoolAllStudies.rq", "w")

scriptP1 <-"# xRx-PoolAllStudies.rq  - Pool all Drug1 Studies. 
#   Script created by CreatePoolScript.R based on list of servers
INSERT {?s ?p ?o}
WHERE
{  
"

writeLines(scriptP1, con=rqFile)

for (i in 1:nrow(servers)){
    
    graphNum <- paste("    # Graph", i)
    writeLines(graphNum, con=rqFile)
    writeLines("    {", con=rqFile)

    service <- paste0("        SERVICE <http://",servers[i,2],":5820/LDWStudy/query>\n")
    writeLines(service, con=rqFile, sep="")

    selState <-paste(
    "        {
            SELECT ?s ?p ?o
            WHERE { ?s ?p ?o .} 
        }")
    writeLines(selState, con=rqFile)  
    writeLines("    }", con=rqFile)
    
    # UNION statement for all but last query
    if(i < nrow(servers)){
      writeLines("    UNION", con=rqFile) 
    }
}
# Close the WHERE

writeLines("}", con=rqFile)
close(rqFile)