###############################################################################
# FILE: /scripts/r/SelectTTLToQuery.R
# DESC: LDWorkshop: Select, query, visuzalize a TTL file.
# IN  : data/RDFModel.TTL
# OUT : N/A
# REQ : 
# NOTE: Visualize requires query to return s,p,o. 
# TODO: 1. Improve parsing of the nodes and relations in the Visualization.
#         See code in the ValidateTTLScript.R for a nice Regex.
#       2. visnetwork styles to match GraphEditor : Shapes, colours
#       0. TTL validation and output.
###############################################################################
library(plyr)     #  rename
library(reshape)  #  melt
library(shiny)
library(redland)
library(visNetwork)


# Setup the file read for redland
world   <- new("World")
storage <- new("Storage", world, "hashes", name="", options="hash-type='memory'")
model   <- new("Model", world=world, storage, options="")
parser  <- new("Parser", world, name = 'turtle', mimeType = 'text/turtle')


#---- Values to check ---------------------------------------------------------
#    Nodes that should be present in all graphs
standardNodes <- c("eg:ActiveArm", "eg:Drug1", "eg:PlaceboArm", "eg:Serum114", "ncit:Female", "ncit:Male")

#    Relations that should be present in all graphs
standardRelations <- c("eg:age", "eg:LDExpert", "eg:participatesIn", "eg:randomizedTo",
  "eg:trtArm", "eg:trtArmType", "ncit:drugname", "ncit:gender", "ncit:phase", "ncit:study",
  "schema:givenName")

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
        h2("QC Results"),
        p("Any values listed should be reviewed in the exercise steps."),
        h3("Nodes"),
        verbatimTextOutput("flaggedNodes"),
        br(),
        h3("Relations"),
        verbatimTextOutput("flaggedRelations")
    ),
    tabPanel("Old Vis",
        visNetworkOutput("networkOld",height = '400px')
    ),
    tabPanel("Visualize",
        visNetworkOutput("network",height = '400px')
    ),
    tabPanel("DEV",
        dataTableOutput("dev")
    )
)

#------------------------------------------------------------------------------
# SERVER 
#------------------------------------------------------------------------------
server <- function(input, output, session) {

    queryText <- observeEvent(input$fileRQ, {
        filePath <- input$fileRQ$datapath
        queryText <- paste(readLines(filePath), collapse = "\n")
        # update text area with file content
        updateTextAreaInput(session, "query", value = queryText)
        # return the text to be displayed in text Outputs
        return(queryText)
    })
    output$query <- renderPrint({ queryText() })    

    data <- eventReactive(input$runQuery, {
        inFileTTL <- input$fileTTL
            redland::parseFileIntoModel(parser, world, inFileTTL$datapath, model)
            queryResults = NULL;
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
    #   prefData - prefixes instead of IRIs
    prefData = reactive ({
        # Replace IRI with prefixes for both plotting and data QC
        toPref <- as.data.frame(data())
        
        # Convert IRI to us prefixes
        # TODO: change into apply loop within fnt
        iriToPref <- function(elem)
        {
            elem <- gsub("<http://example.org/LDWorkshop#", "eg:", elem)
            # ncit:
            elem <- gsub("<http://ncicb.nci.nih.gov/xml/owl/EVS/Thesaurus.owl#", "ncit:",elem)
            # schema
            elem <- gsub("<http://schema.org/", "schema:", elem)
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
     
        # Initialize qcData to null. It will later either have values picked up 
        # in QC checks or an "all passed" message.
        qcData <- data.frame()
        
        
        #---- Nodes -----------------------------------------------------------
        nodeList <- melt(prefData(), id.vars=c("p"))
        nodes <- nodeList$value
        uValues <-sort(unique(nodes))

        # Only iriNodes will be used for QC Checking
        iriNodes <- uValues[grepl("^\\S+:", uValues)]
        # as.character conversion needed here due to coercion within Shiny 
        #   that was not in external code dev
        iriNodes <-as.character(iriNodes)

        # Parse Study node to obtain attendee number used to check 
        #    other nodes: Person<n>, TrtArm<n-n>.
        studyNode <-iriNodes[grep("(S|s)tudy", iriNodes)] 

        # Extr. Attendee Num, assuming Study Num is correct!
        attendeeNum <-gsub("eg:Study", "", studyNode)

        # Phase Node
        phaseNode <-iriNodes[grepl("Phase", iriNodes)] 

        # Person Nodes
        personNodes <-iriNodes[grepl("Person", iriNodes)] 

        # TrtArm Nodes
        armNodes <-iriNodes[grepl("TrtArm", iriNodes)] 
        
        # Nodes that should be in all studies
        ttlNodes<-iriNodes[!grepl("Study|Person|TrtArm|Phase", iriNodes)] 
        
        flaggedNodes <- setdiff(ttlNodes, standardNodes)

        # Nodes unique to each Attendee. Flag those not fitting req pattern
        # NB: Study<n> NOT checked: is used to extract the attendeeNum, so it
        #   is always correct in this code logic. 
        
        # Phase : Check that it is "Phase" then: Phase3, PhaseIIb, Phase2b etc.
        if (!grepl("ncit:Phase\\S{1-4}", phaseNode)) {
            print ("----ERROR: Phase Pattern fail.")
            print (c("----------: ", phaseNode))
            flaggedNodes<-append(flaggedNodes, phaseNode)
        }
        
        # Person : Check : "Person" + "attendeeNum" + <n>
        #     use of << within sapply
        personRegex <- paste0("eg:Person", attendeeNum, "\\d+")
        sapply(personNodes, function(person){
            if (!grepl(personRegex, person)) {
                print ("----ERROR: Person Node fail.")
                print (c("----------: ", person))
                flaggedNodes<<-append(flaggedNodes, person)
            }
        }) 

        # TrtArm : Check: "TrtArm" + attendeeNum+ "-"+ number 
        #     use of << within sapply
        armRegex <- paste0("eg:TrtArm", attendeeNum, "-", "\\d")
        sapply(armNodes, function(arm){
            if (!grepl(armRegex, arm)) {
                print ("----ERROR: Arm Node fail.")
                print (c("----------: ", arm))
                flaggedNodes<<-append(flaggedNodes, arm)
          }
        }) 
        
        qcData <- as.data.frame(list(item=flaggedNodes))
        qcData$type <-"Node"
      
        #---- Relations -------------------------------------------------------      
      
      
      
        #---- Pretty-Pretty -----------------------------------------------------
        # Re-order for display
        qcData <- qcData[c("type", "item")]
     
        print(c("---- qcData = ", qcData))
        ## After all checks complete (Nodes and Relaions) 
        ## if (nrow(qcData) == 0){
            # Set the returned value is a single column name "Message" with value:
          # "All QC Checks Passed."
        ## }
        qcData     # return the qc set, with values or with OK message.
    })

    output$dev = renderDataTable({ qcData() });
    
    #--------------------------------------------------------------------------
    #-- Visualize -------------------------------------------------------------
    output$networkOld <- renderVisNetwork({
        RDFTriples <<- as.data.frame(data())
        
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

        # Kludgy grouping of nodes to assign color values based on if they are
        # uri, int, or string, similar to the spreadsheet source
        # NOTE: Will not scale to custom entries by students. 
        #nodeMatch <- c("^Person", "^Study", "^Treat", "^Protocol")
        #nodes$group[grepl(paste(nodeMatch, collapse="|"), nodes$id, perl=TRUE)]  <- "uri"  
        
        # Default to type =uri, then reassign for best guess at int and string.
        nodes$group <- 'uri'
        nodes$group[grepl("#int", nodes$id, perl=TRUE)] <- "int"
        nodes$group[grepl("#string", nodes$id, perl=TRUE)] <- "string"
        
        # Kludge that fails if a literal has a colon. Close enough for exercise.
        nodes$shape <- ifelse(grepl("int|string", nodes$group, perl=TRUE), "box", "ellipse")

        # Assign labels used for mouseover and for label
        # NEW PATTERNS HERE
        # foo <-sub(".*/", "", foo)  # Everything ahead of the value
        # foo <-sub(">.*", "", foo)  # Everything after the value

        nodes$title <- nodes$id
        
        # process string and int values first
        nodes$title <- sub('^"', "", nodes$title)  # Remove leading quote if there is one. Keep the rest.
        nodes$title <- sub('".*', "",  nodes$title)  # Everything after the next quote is deleted
        
        # At this point the URI's are unaffected, so now play with them.
        nodes$title <- sub(".*/", "", nodes$title)  # Everything ahead of the value
        nodes$title <- sub(">.*", "",  nodes$title)  # Everything ahead of the value
        # foo <-sub(">.*", "", foo)  # Everything after the value
        
        
        nodes$label <- nodes$title
        # nodes$label <- gsub("\\S+:", "", nodes$id)

        #---- Edges
        # Create list of edges by keeping the Subject and Predicate from query result.
        edges<-as.data.frame(rename(RDFTriples, c("s" = "from", "o" = "to")))
        
        # Edge values
        #   use edges$label for values always displayed
        #   use edges$title for values only displayed on mouseover
        # edges$title <-gsub("\\S+:", "", edges$p)   # label : text always present
        edges$title <- sub(".*/", "", edges$p)  # Everything ahead of the value
        edges$title <- sub(">.*", "", edges$title)  # Everything ahead of the value  
          
        # ORIGINAL TESTING visNetwork(nodes, edges)
        
        visNetwork(nodes, edges, height = "800px", width = "100%") %>%
            visOptions(selectedBy = "group", 
                highlightNearest = TRUE, 
                nodesIdSelection = TRUE) %>%
            visEdges(arrows = list(to = list(enabled = TRUE, scaleFactor = 0.5)),
                     color  = "black",
                     smooth = list(enabled = FALSE, type = "cubicBezier", roundness=.8)) %>%
            visGroups(groupname = "uri",    color = list(background = "white", 
                                                         border     = "#b30000", 
                                                         highlight  = "#cc4400")) %>%
            visGroups(groupname = "string", color = list(background = "white", 
                                                         border     = "#008000", 
                                                         highlight  = "#00e600")) %>%
            visGroups(groupname = "int",    color = list(background = "white", 
                                                         border     = "#0000cc",
                                                         highlight  = "#668cff" )) %>%
            visPhysics(stabilization=FALSE, barnesHut = list(
                avoidOverlap=.8,
                gravitationalConstant = -100,
                springConstant = 0.004,
                damping = 0.9,
                springLength = 10
            ))  
    })
    
    # NEW VIS 
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

        # Kludgy grouping of nodes to assign color values based on if they are
        # uri, int, or string, similar to the spreadsheet source
        # NOTE: Will not scale to custom entries by students. 
        #nodeMatch <- c("^Person", "^Study", "^Treat", "^Protocol")
        #nodes$group[grepl(paste(nodeMatch, collapse="|"), nodes$id, perl=TRUE)]  <- "uri"  
        # Default to type =uri, then reassign for best guess at int and string.
        nodes$group <- 'iri'
        nodes$group[grepl("^\\w+", nodes$id, perl=TRUE)] <- "string"
        nodes$group[grepl("^\\d+", nodes$id, perl=TRUE)] <- "int"
        nodes$group[grepl("ncit:|schema:", nodes$id, perl=TRUE)] <- "iriont"
        nodes$group[grepl("eg:", nodes$id, perl=TRUE)] <- "iri"
        
        nodes$shape <- "box"

        # remove prefixes if present
        # nodes$title <- sub("^\\S+:", "", nodes$id)
        nodes$title <-  nodes$id
        # Label includes prefix
        nodes$label <- nodes$title

        
        #---- Edges
        # Create list of edges by keeping the Subject and Predicate from query result.
        edges<-as.data.frame(rename(RDFTriples, c("s" = "from", "o" = "to")))
        
        # Edge values
        #   use edges$label for values always displayed
        #   use edges$title for values only displayed on mouseover
        # edges$title <-gsub("\\S+:", "", edges$p)   # label : text always present
        edges$title <- sub(".*/", "", edges$p)  # Everything ahead of the value
        edges$title <- sub(">.*", "", edges$title)  # Everything ahead of the value  
          
        # ORIGINAL TESTING visNetwork(nodes, edges)
        
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