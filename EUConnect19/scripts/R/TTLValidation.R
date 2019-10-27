###############################################################################
# FILE: /scripts/r/TTLValidation.R
# DESC: LDWorkshop: Select, query, validate, and visuzalize a TTL file.
# IN  : 
# OUT : N/A
# REQ : 
# NOTE: Visualize requires query to return s,p,o. 
# 
# TODO: 1. Reload of TTL does not reset QC Check data. Need to reset dataframes
#       2. Convert from use of Redland to rdflib
###############################################################################
library(DT)
library(plyr)     #  rename
library(reshape)  #  melt
library(shiny)
library(redland)
library(visNetwork)

#---- Values to check ---------------------------------------------------------
#  Default nodes present when Graph Editor is started. 
defaultNodes <- c(
  "dbpedia:Aspirin",
  "dbpedia:Sugar_pill",
  "eg:ActiveArm", 
  "eg:PlaceboArm", 
  "ncit:Female", 
  "ncit:Male")

#    Relations that should be present in all graphs
standardRelations <- c(
  "ct:nct-id",  
  "eg:age",
  "eg:drugName",
  "eg:LDExpert",
  "eg:randomizedTo",
  "eg:trtArm", 
  "eg:trtArmType",
  "ncit:gender",
  "schema:givenName"
  )

# List of valid NCTID's for the workshop.  Source: CourseInfo.xlsx
nctidList <- c("ct:NCT02301286",
  "ct:NCT02467582",
  "ct:NCT01973205",
  "ct:NCT02270242",
  "ct:NCT02521285",
  "ct:NCT02348203",
  "ct:NCT00565708",
  "ct:NCT02578706",
  "ct:NCT02415400",
  "ct:NCT01248468",
  "ct:NCT02325466",
  "ct:NCT02239120",
  "ct:NCT02237365",
  "ct:NCT02183220",
  "ct:NCT02183688",
  "ct:NCT02158806",
  "ct:NCT02155985",
  "ct:NCT02123849",
  "ct:NCT02090413",
  "ct:NCT01361399",
  "ct:NCT02049762",
  "ct:NCT01902498")


#------------------------------------------------------------------------------
# UI 
#------------------------------------------------------------------------------
ui <- navbarPage("TTL File Query",
    theme = "spacelab.css",
    tabPanel("Query",
        wellPanel(
            column(6, fileInput('fileTTL', '.TTL File', accept=c('.ttl'))),
            column(6, fileInput('fileRQ',   'OPTIONAL: .RQ Query File')),
            fluidRow(
                textAreaInput(inputId="query", "SPARQL Query", rows=10, width='90%', 
"# To Visualize, results must be ?s ?p ?o
SELECT ?s ?p ?o 
WHERE{
    ?s ?p ?o
}"),
            actionButton(inputId = "runQuery", label="Run query")
        )
    ),
    fluidRow(
        HTML('<br><label for="endpoint">Query Result:</label>'),
        dataTableOutput("queryresult")
    )
    ),
    tabPanel("QC Check",
        DT::dataTableOutput("qcresult")
    ),
    tabPanel("Visualize",
        visNetworkOutput("network",height = '900px')
    )
)

#------------------------------------------------------------------------------
# SERVER 
#------------------------------------------------------------------------------
server <- function(input, output, session) {

    queryText <- observeEvent(input$fileRQ, {
        filePath <- input$fileRQ$datapath
        queryText <- paste(readLines(filePath), collapse = "\n")
        # Update text area with file content
        updateTextAreaInput(session, "query", value = queryText)
        # return the text to be displayed in text Outputs
        return(queryText)
    })
    output$query <- renderPrint({ queryText() })    

    data <- eventReactive(input$runQuery, {

        # Setup the file read for redland
        world   <- new("World")
        storage <- new("Storage", world, "hashes", name="", options="hash-type='memory'")
        model   <- new("Model", world=world, storage, options="")
        parser  <- new("Parser", world, name = 'turtle', mimeType = 'text/turtle')

        inFileTTL <- input$fileTTL
            redland::parseFileIntoModel(parser, world, inFileTTL$datapath, model)
       
            queryResults = c(); 
            query <- new("Query", world, input$query, base_uri=NULL, query_language="sparql", query_uri=NULL)
            queryResult <- executeQuery(query, model)
            
            # getNextResult in a loop until NULL is returned.
            repeat{
                nextResult <- getNextResult(queryResult)
                queryResults <- rbind(queryResults, data.frame(nextResult))
                if(is.null(nextResult)){ break }
            }
        triples<-queryResults
    })

    # Query Result
    output$queryresult= renderDataTable({ data() });

    #---- Data Massage --------------------------------------------------------
    #   Massage data for both QC Check and Visualization
    #   prefData = prefixes instead of IRIs
    prefData = reactive ({
        # Replace IRI with prefixes for both plotting and data QC
        toPref <- as.data.frame(data())
        
        # Convert IRI to use of prefixes
        iriToPref <- function(elem)
        {
            
            # eg:
            elem <- gsub("<http://example.org/LDWorkshop#", "eg:", elem)
            
            # ncit:
            elem <- gsub("<http://ncicb.nci.nih.gov/xml/owl/EVS/Thesaurus.owl#", "ncit:",elem)
            
            # schema:
            elem <- gsub("<http://schema.org/", "schema:", elem)
            
            # ct:
            elem <- gsub("<http://bio2rdf.org/clinicaltrials:", "ct:", elem)
            
            #dbpedia
            elem <- gsub("<http://dbpedia.org/resource/", "dbpedia:", elem)
            
            # Remove the trailing >
            elem <- gsub(">", "", elem)
            
            # Object literals require removal of quotes and type to get value only
            elem <- gsub("^\"", "", elem)  # quote at start of value
            elem <- gsub("\"\\S+", "", elem)  # quote value end and type
        }  
        toPref$s <- iriToPref(toPref$s) # Subjects
        toPref$p <- iriToPref(toPref$p) # Predicates
        toPref$o <- iriToPref(toPref$o) # Objects
        toPref  # return the dataframe
    })
  
    # Initialize empty dataframe for QC observations and messages
    # Examine = Node or Relation, to differentiate between types of errors.
    #  Initialization must occur here, outside of the qcData section below.
    qcObs <- data.frame(Examine = character(), 
                        Value   = character(), 
                        Message = character())


    #--------------------------------------------------------------------------
    #---- Data QC -------------------------------------------------------------
    
    qcData  = reactive ({
        
       #---- Nodes -----------------------------------------------------------
       nodeList <- melt(prefData(), id.vars=c("p"))
       nodes <- nodeList$value
       uValues <- sort(unique(nodes))
       
       #--- Create groups of the different types of nodes for specific testing
       # Only QC Check iriNodes. String and Int are not critical.
       iriNodes <- uValues[grepl("^\\S+:", uValues)]
       
       # as.character conversion needed here due to coercion within Shiny 
       iriNodes <- as.character(iriNodes)
       
       # Parse Study node to obtain attendee number used to check 
       #    other nodes: Person<n>, TrtArm<n-x>.
       studyNode <- iriNodes[grep("(S|s)tudy", iriNodes, ignore.case = TRUE)] 

       # Extract attendee number, used in Study and other nodes
       attendeeNum <- gsub("eg:Study", "", studyNode)
       
       # Person Nodes. Person11, Person12, etc.
       personNodes <- iriNodes[grepl("Person", iriNodes, ignore.case = TRUE)] 
       
       # TrtArm Nodes, TrtArm1-1, TrtArm1-2, etc.
       armNodes <- iriNodes[grepl("TrtArm", iriNodes, ignore.case = TRUE)] 
       
       # NCT ID node
       nctidNode <- iriNodes[grepl("\\D+\\d{4,}", iriNodes, ignore.case = TRUE)] 

       # ---------------------------------------------------------------------
       # Nodes unique to each Attendee. Flag those not fitting req pattern
       # NB: Study<n> NOT checked: is used to extract the attendeeNum, so it
       #   is always correct in this code logic. 
       #----------------------------------------------------------------------

       #--- Study ------------------------------------------------------------
       #    Checks: prefix, "Study" + attendeeNum
       #    Even though the Study number is used for other checks, check to 
       #      ensure the prefix was not changed, Study to "study", etc.
       studyRegex <- paste0("eg:Study", attendeeNum)
       sapply(studyNode, function(testVal){
         if (length(testVal > 0) && !grepl(studyRegex, testVal)) {
           qcCurrVal <- data.frame(
             Examine = "Node",
             Value   = paste(testVal), 
             Message = "Check: 1. Prefix = eg:   
               2. Capital 'S' in 'Study' " 
           )
           qcObs <<- rbind(qcObs, qcCurrVal)  # global assign ;)
         }
       }) 
                 
       #--- Person -----------------------------------------------------------
       #   Checks: prefix, "Person" + "attendeeNum" + <n>
       # Working 2019-10-24
       personRegex <- paste0("eg:Person", attendeeNum, "\\d+")
       sapply(personNodes, function(testVal){
         if (length(testVal > 0) && !grepl(personRegex, testVal)) {
           qcCurrVal <- data.frame(
             Examine = "Node",
             Value   = paste(testVal), 
             Message = "Check: 1. Prefix = eg:   
               2. Capital 'P' in 'Person'   
               3. Number starts with same digit as 'Study<n>' node   
               4. Multiple errors? Check Study<n> is correct ")
           qcObs <<- rbind(qcObs, qcCurrVal)  # global assign ;)
         }
       })
       
       #--- TrtArm -----------------------------------------------------------
       #   Checks: prefix, "TrtArm" + attendeeNum+ "-"+ number 
       armRegex <- paste0("eg:TrtArm", attendeeNum, "-", "\\d")
       sapply(armNodes, function(testVal){
         if (length(testVal > 0) && !grepl(armRegex, testVal)) {
           qcCurrVal <- data.frame(
             Examine = "Node",
             Value   = paste(testVal), 
             Message = "Check: 1. Prefix = eg:   
               2. Capital 'A','T' in 'TrtArm'  
               3. TrtArm<n> , where <n> is same digit as 'Study<n>' node   
               4. Dash between numbers:  TrtArm<n>-<n>   
               5. Multiple errors? Check Study<n> is correct"
           )
           qcObs <<- rbind(qcObs, qcCurrVal)  # global assign ;)
         }
       }) 

       #--- NCTID ------------------------------------------------------------
       #   Checks: prefix, "NCTnnnnnn" against list used for the session
       #       (Value set earlier)
       nctidRegex <- paste0("ct:NCT\\d+")  # Pefix and proper NCT characters, then digits
       sapply(nctidNode, function(testVal){
         if (length(testVal > 0) && !grepl(nctidRegex, testVal)) {
           qcCurrVal <- data.frame(
             Examine = "Node",
             Value   = paste(testVal), 
             Message = "Check: 1. Prefix = ct:   
               2. Capital 'NCT' before digits"
           )
          qcObs <<- rbind(qcObs, qcCurrVal)  # global assign ;)
         }
         # Part 2: Check value against list of NCT ID used in the workshop
         if (length(testVal > 0) && (! testVal %in% nctidList)) {
           qcCurrVal <- data.frame(
             Examine = "Node",
             Value   = paste(testVal), 
             Message = "NCT ID is not in approved list.
               1. Check digits against the values in your InfoSheet   
               2. Capital 'NCT' before digits"
           )
           qcObs <<- rbind(qcObs, qcCurrVal)  # global assign ;)
         }
       }) 

       #---- Relations -------------------------------------------------------     
       # Only check the relations most often incorrectly entered by the 
       #   students.  eg:trtArm and eg:trtArmType are easily confused and can be
       #   difficult to spot.
       spo <- prefData()
       
       for (i in 1:nrow(spo)){
         
         # Study to TrtArm<n>-<n> by the eg:trtArm predicate
         # Study --  eg:trtArm -- -TrtArm 
         if(  (grepl("Study\\d", spo[i,'s'] )) &&
              (grepl("TrtArm\\d", spo[i,'o'])) && 
              (! spo[i,'p'] == 'eg:trtArm' )) 
         {
           qcCurrVal <- data.frame(
             Examine = "Relation",
             Value   = paste0( spo[i,'s'], " ", spo[i,'p'], " " , spo[i,'o']),
             Message = "Problem in Study to TrtArm relation/triple. Ensure predicate is eg:trtArm."
               
           )
           qcObs <<- rbind(qcObs, qcCurrVal)  # global assign ;)    
         }
         
         # Arm to ArmType is not predicate eg:trtArmType
         if(  (grepl("TrtArm\\d", spo[i,'s'])) &&
              (spo[i,'o'] %in% c("eg:ActiveArm", "eg:PlaceboArm") ) && 
              (! spo[i,'p'] == 'eg:trtArmType' )) 
         {
           qcCurrVal <- data.frame(
             Examine = "Relation",
             Value   = paste0( spo[i,'s'], " ", spo[i,'p'], " " , spo[i,'o']),
             Message = "Problem in TrtArm to TrtArmType relation/triple. Ensure predicate is eg:trtArmType."
           )
           qcObs <<- rbind(qcObs, qcCurrVal)  # global assign ;)    
         }
       }
       
       
      #--- END OF CHECKS  -----------------------------------------------------
       
      # If no QC findings added to dataset, then add a row stating all checks passed. 
      if (dim(qcObs)[1] == 0) {
        qcCurrVal <- c("", "All QC Checks Passed")
        qcObs <- rbind(qcObs, qcCurrVal)
      }
      else{
        print(qcObs)  # ERROR:  The relation errors are NOT showing up here! 
                      # ONly the NODE errors are here.
         qcObs 
      }
    })

    # Query Result
    # output$qcresult = DT::renderDataTable({qcData() })
    output$qcresult = DT::renderDataTable(datatable(qcData(), 
          colnames = c("Examine", "Value", "Message"),
          rownames = FALSE,
          options = list(pageLength=20,
                         paging = FALSE,
                         dom = 't')))

    #--------------------------------------------------------------------------
    #-- Visualize -------------------------------------------------------------
    output$network <- renderVisNetwork({
        RDFTriples <<- as.data.frame(prefData())
        
        RDFTriples<-RDFTriples[!(RDFTriples$o==""),]
        
        # Remove duplicates from the query
        RDFTriples <- RDFTriples[!duplicated(RDFTriples),]

        #---- Nodes Construction
        # Get the unique list of nodes by combine Subject and Object into 
        # single column.
        # "id.vars" = list of columns to keep untouched whil the unamed (s,o) are 
        # melted into the "value" column.
        nodeList <- melt(RDFTriples, id.vars=c("p" ))

        # A node can be both a Subject and a Predicate so ensure a unique list of node names
        #  by dropping duplicate values.
        nodeList <- nodeList[!duplicated(nodeList$value),]

        # Rename to ID for use in visNetwork and keep only that column
        nodeList <- rename(nodeList, c("value" = "id" ))
        nodes<- as.data.frame(nodeList[c("id")])

        # Assign node color based on content (int, string) then based on prefixes
        nodes$group <- 'iri'
        nodes$group[grepl("^\\w+", nodes$id, perl=TRUE)]         <- "string"
        nodes$group[grepl("^\\d+", nodes$id, perl=TRUE)]         <- "int"
        nodes$group[grepl("ncit:|schema:", nodes$id, perl=TRUE)] <- "iriont"
        nodes$group[grepl("ct:|dbpedia:", nodes$id, perl=TRUE)]  <- "iriext"
        nodes$group[grepl("eg:", nodes$id, perl=TRUE)]           <- "iri"
        
        nodes$shape <- "box"
        nodes$title <-  nodes$id  # mouseover. 
        nodes$label <- nodes$title # label on node (always displayed)

        
        #---- Edges
        # Create list of edges with from, to for visNetwork 
        edges<-as.data.frame(rename(RDFTriples, c("s" = "from", "o" = "to")))
        
        # Edge values
        #   edges$label : always displayed, so not set in current vis.
        #   edges$title : only displayed on mouseover. Used in current vis.
        edges$title <- edges$p

        visNetwork(nodes, edges, height = "1200px", width = "100%") %>%
            visOptions(selectedBy = "group", 
                highlightNearest = TRUE, 
                nodesIdSelection = TRUE) %>%
            visEdges(arrows = list(to = list(enabled = TRUE, scaleFactor = 0.5)),
                     color  = "gray",
                     smooth = list(enabled = FALSE, type = "cubicBezier", roundness=.8)) %>%
            visGroups(groupname = "iri",    color = list(background = "#BCF5BC", 
                                                         border     = "#CCCCCC",
                                                         highlight  = "#FFFF33")) %>%

          visGroups(groupname = "iriext", color = list(background = "#BCBDDC",
                                                       border     = "#CCCCCC", 
                                                       highlight  = "#FFFF33")) %>%
          
          
            visGroups(groupname = "iriont", color = list(background = "#FFC862",
                                                         border     = "#CCCCCC", 
                                                         highlight  = "#FFFF33")) %>%

            visGroups(groupname = "string", color = list(background = "#E4E4E4", 
                                                         border     = "#CCCCCC", 
                                                         highlight  = "#FFFF33")) %>%
            visGroups(groupname = "int",    color = list(background = "#C1E1EC", 
                                                         border     = "#CCCCCC",
                                                         highlight  = "#FFFF33" )) %>%
            visPhysics(stabilization=FALSE, 
                barnesHut = list(
                    avoidOverlap=1,
                    gravitationalConstant = -2000,
                    springConstant = 0.01,
                    damping = 0.2,
                    springLength = 50
            ))  
    })
}
shinyApp(ui = ui, server = server)