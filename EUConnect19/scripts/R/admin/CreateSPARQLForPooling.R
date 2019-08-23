###############################################################################
# FILE: CreateSPARQLForPooling.R
# DESC: Create the SPARQL script used to pool data from all the studies 
# IN  : ClassInfo.xlsx - has IP address of each study for each attendee
# OUT : 410-PoolAllStudies.rq 
# REQ : 
# NOTE: 
# TODO: 
###############################################################################
library(readxl)
servers <- as.data.frame(read_excel("C:/_gitHub/LinkedDataWorkshop/CSS2019/data/admin/CourseInfo.xlsx", sheet = "Servers", col_names=TRUE))
servers <- servers[ order(servers[,'server'], servers[,'ip']), ]


rqFile <-file("C:/_gitHub/LinkedDataWorkshop/CSS2019/scripts/SPARQL/510-PoolAllStudies.rq", "w")

scriptP1 <-paste("# 510-PoolAllStudies.rq  - Pool all Studies. 
#   Script created", Sys.time(), "by CreateSPARQLForPooling.R 
#       based on list of servers in ClassInfo.xlsx
INSERT {?s ?p ?o}
WHERE
{"  
)

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