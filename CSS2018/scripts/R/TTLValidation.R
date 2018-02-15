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
    prefData = reactive ({
        # Replace IRI with prefixes for both plotting and data QC
        temp <- as.data.frame(data())
        
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
        
        temp$s <- iriToPref(temp$s) # Subjects
        temp$p <- iriToPref(temp$p) # Predicates
        temp$o <- iriToPref(temp$o) # Objects

        temp  # return the dataframe
        })
  
  
    output$dev = renderDataTable({ prefData() });

  
          

    #---- Data QC -------------------------------------------------------------
    
##    #---- Nodes
##    flaggedNodes <- vector(mode='character', length=0);
##    flaggedNodes <- c("eg:One", "eg:Two")
##    if (length(flaggedNodes)<1){
##      flaggedNodes <-c("All QC checks passed.")
##    }
##    output$flaggedNodes <- renderPrint({
##       flaggedNodes
##    })
##
##    #---- Relations
##    flaggedRelations <- vector(mode='character', length=0);
##    # flaggedRelations <- c("eg:oneone", "eg:twotwo")
##    if (length(flaggedRelations)<1){
##      flaggedRelations <-c("All QC checks passed.")
##    }
##    output$flaggedRelations <- renderPrint({
##       flaggedRelations
##    })

    
    

    
    #-- Graph -----------------------------------------------------------------
    output$network <- renderVisNetwork({
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
}
shinyApp(ui = ui, server = server)