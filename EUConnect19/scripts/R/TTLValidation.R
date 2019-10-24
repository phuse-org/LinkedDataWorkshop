###############################################################################
# FILE: /scripts/r/TTLValidation.R
# DESC: LDWorkshop: Select, query, validate, and visuzalize a TTL file.
# IN  : 
# OUT : N/A
# REQ : 
# NOTE: Visualize requires query to return s,p,o. 
# 
# TODO:   # Re-writing error handling so errors page shows a Dataframe.
#    Start will NULL dataframe
#    If dim(DF) ==0, then rbind: value: No errors Msg: All Passed.
#     Else each error found keeps appending to the dataframe, which is displayed at the end.
#            1. Reload of TTL does not reset QC Check data. Need to reset dataframes
#       Convert from use of Redland to rrdf
#  !!! library DT may need installed on server!      
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
  
    #--------------------------------------------------------------------------
    #---- Data QC -------------------------------------------------------------
    qcData  = reactive ({
        
       # initialize empty dataframe for QC results/messaging
       qcObs <- data.frame(value=character(), message=character())
        # Initialize exceptions to message for success. It will be reset if 
#TW         # exceptions are found. 
#TW         # item="" row is later removed when passed message displayed
#TW         dataExceptions <- as.data.frame(list(type="All QC Checks Passed", item=""))
#TW 
#TW         #---- Nodes -----------------------------------------------------------
#TW         nodeList <- melt(prefData(), id.vars=c("p"))
#TW         nodes <- nodeList$value
#TW         uValues <-sort(unique(nodes))
#TW 
#TW         # Only QC Check iriNodes. String and Int are not critical.
#TW         iriNodes <- uValues[grepl("^\\S+:", uValues)]
#TW         # as.character conversion needed here due to coercion within Shiny 
#TW         iriNodes <-as.character(iriNodes)
#TW 
#TW         # Parse Study node to obtain attendee number used to check 
#TW         #    other nodes: Person<n>, TrtArm<n-x>.
#TW         studyNode <-iriNodes[grep("(S|s)tudy", iriNodes)] 
#TW 
#TW         # Extr. Attendee Number. Assumes Study Number is correct!
#TW         attendeeNum <-gsub("eg:Study", "", studyNode)
#TW 
#TW         # Person Nodes. Person11, Person12, etc.
#TW         personNodes <-iriNodes[grepl("Person", iriNodes)] 
#TW 
#TW         # TrtArm Nodes, TrtArm1-1, TrtArm1-2, etc.
#TW         armNodes <-iriNodes[grepl("TrtArm", iriNodes)] 
#TW 
#TW         # NCT ID node
#TW         nctidNode <<-iriNodes[grepl("\\D+\\d{4,}", iriNodes)] 
#TW         
#TW                 
#TW         # Default that should be in all studies. Check for accidental changes to 
#TW         #   default nodes.
#TW         ttlNodes<-iriNodes[!grepl("Study|Person|TrtArm|NCT", iriNodes)] 
#TW         
#TW         flaggedNodes <- setdiff(ttlNodes, defaultNodes)
#TW 
#TW         # ---------------------------------------------------------------------
#TW         # Nodes unique to each Attendee. Flag those not fitting req pattern
#TW         # NB: Study<n> NOT checked: is used to extract the attendeeNum, so it
#TW         #   is always correct in this code logic. 
#TW         #----------------------------------------------------------------------
#TW 
#TW         #--- Study ------------------------------------------------------------
#TW         #    Checks: prefix, "Study" + attendeeNum
#TW         #    Even though the Study number is used for other checks, check to 
#TW         #      ensure the prefix was not changed, etc.
#TW         studyRegex <- paste0("eg:Study", attendeeNum)
#TW         sapply(studyNode, function(study){
#TW             if (length(study > 0) && !grepl(studyRegex, study)) {
#TW                 print ("----ERROR: Study Node fail.")
#TW                 print (c("----------: ", study))
#TW                 flaggedNodes<<-append(flaggedNodes, study)
#TW             }
#TW         }) 
#TW                 
#TW         #--- Person -----------------------------------------------------------
#TW         #   Checks: prefix, "Person" + "attendeeNum" + <n>
#TW         personRegex <- paste0("eg:Person", attendeeNum, "\\d+")
#TW         sapply(personNodes, function(person){
#TW             if (length(person > 0) && !grepl(personRegex, person)) {
#TW                 print ("----ERROR: Person Node fail.")
#TW                 print (c("----------: ", person))
#TW                 flaggedNodes<<-append(flaggedNodes, person)
#TW             }
#TW         }) 
#TW 
#TW         #--- TrtArm -----------------------------------------------------------
#TW         #   Checks: prefix, "TrtArm" + attendeeNum+ "-"+ number 
#TW         armRegex <- paste0("eg:TrtArm", attendeeNum, "-", "\\d")
#TW         sapply(armNodes, function(arm){
#TW             if (length(arm > 0) && !grepl(armRegex, arm)) {
#TW                 print ("----ERROR: Arm Node fail.")
#TW                 print (c("----------: ", arm))
#TW                 flaggedNodes<<-append(flaggedNodes, arm)
#TW           }
#TW         }) 
#TW 
#TW 
#TW         #--- NCTID ------------------------------------------------------------
#TW         #   Checks: prefix, "NCTIDnnnnnn" against list used for the session
#TW         #       (Value set earlier)
#TW         nctidRegex <- paste0("ct:NCT")  # Pefix and proper NCT characters
#TW         sapply(nctidNode, function(nctid){
#TW           if (length(nctid > 0) && !grepl(nctidRegex, nctid)) {
#TW                 print ("ERROR: NCT ID Node fail. Prefix or NCT character issue.")
#TW                 print (c("----------: ", nctid))
#TW                 flaggedNodes<<-append(flaggedNodes, nctid)
#TW           }
#TW           # Part 2: Check value against list of NCT ID used in the workshop
#TW           if (length(nctid > 0) && (! nctid %in% nctidList)) {
#TW                 print ("ERROR: NCT ID Node fail. Number not found in approved list.")
#TW                 print (c("----------: ", nctid))
#TW                 flaggedNodes<<-append(flaggedNodes, nctid)
#TW           }
#TW         }) 
#TW         # if any node exceptions are found, add them to dataExceptions
#TW         if (length(flaggedNodes > 0)){
#TW             qcNode <- as.data.frame(list(item=flaggedNodes))
#TW             qcNode$type <-"Node"
#TW             qcNode <- qcNode[c("type", "item")]  # Order columns
#TW             # Append to qcData 
#TW             dataExceptions <- rbind(dataExceptions, qcNode)
#TW         }
#TW         
#TW         #---- Relations -------------------------------------------------------      
#TW         ttlRelations <- prefData()$p
#TW         ttlRelations <- sort(unique(ttlRelations))
#TW         flaggedRelations <- setdiff(ttlRelations, standardRelations)
#TW 
#TW         if (length(flaggedNodes > 0)){
#TW             qcRel <- as.data.frame(list(item=flaggedRelations))
#TW             qcRel$type <- "Relation"
#TW             qcRel <- qcRel[c("type", "item")]  # Order columns
#TW         
#TW             # Append to exceptions 
#TW             dataExceptions <- rbind(dataExceptions, qcRel)
#TW         }
#TW         
#TW         
#TW         #--- Checks complete. Create Messaging.
#TW         print(c("---- dataExceptions = ", dataExceptions))
#TW         print(c("---- nrow(dataExceptions) = ", nrow(dataExceptions)))
#TW         dataExceptions     # Return the exceptions set, with values or with default OK message.
      # If no QC findings added to dataset, then add a row stating all checks passed. 
      if (dim(qcObs)[1] == 0) {
        qcCurrVal <- c("", "All QC Checks Passed")
        qcObs <- rbind(qcObs, qcCurrVal)
      }
     
    
    })
    output$ui = renderUI({ 
      qcReport <- qcData();
      names(qcReport) <- c("value", "message")
#TW      qcReport <- qcReport[!(qcReport$item==""),]  # Remove the default row for no items
#TW      
#TW      if (nrow(qcReport) < 1)
#TW          return("All QC Checks Passed") # Message if no findings
#TW          tableOutput("table") # Otherwise, return the data as a table         
    })
    
    
    # Table must be defined separately
## HERE!!!!     
        # Query Result
    # output$qcresult = DT::renderDataTable({qcData() })
    output$qcresult = DT::renderDataTable(datatable(qcData(), 
          colnames = c("Value", "Message"),
          rownames = FALSE,
          options = list(pageLength=20,
                         paging = FALSE,
                         dom = 't')))

#TW    output$table <- renderTable({
#TW      qcReport <-qcData();
#TW      
#TW      qcReport
#TW    })
  
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