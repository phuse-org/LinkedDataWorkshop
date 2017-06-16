###############################################################################
# $HeadURL: file:///C:/SVNLocalRepos/Applications/r/CodeEg/Shiny/SelectTTLToQuery.R $
# $Rev: 79 $
# $Date: 2017-06-16 14:06:58 -0400 (Fri, 16 Jun 2017) $
# $Author: U041939 $
# -----------------------------------------------------------------------------
# DESC: Select a TTL file and execute a query on it. Return result to the window.
# SRC :
# IN  :
# OUT :
# REQ :
# NOTE: 
# SAUCE/REF: https://stackoverflow.com/questions/44572622/rshiny-how-to-update-textareainput-with-contents-of-a-file-via-fileinput
# RElATED: textAreaInputTest.R 
# TODO:  
###############################################################################
library(plyr)     #  rename
library(reshape)  #  melt
library(shiny)
library(rrdf)
library(visNetwork)
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
         "# Default Query
PREFIX phuse: <http://www.example.org/phuse/workshop/>
SELECT * 
WHERE {
  ?s ?p ?o
  FILTER (REGEX(STR(?s), 'phuse'))
}limit 10"),
            actionButton(inputId = "runQuery", label="Run query")
        )
    ),
    fluidRow(
         HTML('<br><label for="endpoint">Query Result:</label>'),
         tableOutput("queryresult")
    )
    ),
    tabPanel("Visualize",
        visNetworkOutput("network",height = '400px')
    )
)

#------------------------------------------------------------------------------
# SERVER 
#------------------------------------------------------------------------------
server <- function(input, output, session) {
    data <- eventReactive(input$runQuery, {
        inFileTTL <- input$fileTTL
        sourceTTL = load.rdf(inFileTTL$datapath, format="N3")
        triples <- sparql.rdf(sourceTTL, input$query)
        triples    
    })
    queryText <- observeEvent(input$fileRQ, {
        filePath <- input$fileRQ$datapath
        queryText <- paste(readLines(filePath), collapse = "\n")
        # update text area with file content
        updateTextAreaInput(session, "query", value = queryText)
        # return the text to be displayed in text Outputs
        return(queryText)
    })
    output$query <- renderPrint({ queryText() })    

    # Query Result
    output$queryresult= renderTable({ head(data()) });

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!        
    #-- Graph -----------------------------------------------------------------
    # Make a graph
    # TEsting a bogus plot. later add the FN graph
    # see here for interactive graph too
    output$network <- renderVisNetwork({
        RDFTriples <- as.data.frame(data())
        
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

        # Assign groups used for icon types and colours
        # Order is important.
        # NOTE: Groups you define in your data must be specfied here in in additional 
        #      statements.
        nodes$group[grepl("Protocol", nodes$id, perl=TRUE)]  <- "Protocol"  
        nodes$group[grepl("Study", nodes$id, perl=TRUE)]     <- "Study"  
        nodes$group[grepl("Treatment", nodes$id, perl=TRUE)] <- "Treatment"  
        nodes$group[grepl("Person", nodes$id, perl=TRUE)]    <- "Person"  
        nodes$group[! grepl(":", nodes$id, perl=TRUE)]       <- "Literal"
        # Kludge that fails if a literal has a colon. Close enough for exercise.
        nodes$shape <- ifelse(grepl(":", nodes$id), "ellipse", "box")

        # Assign labels used for mouseover and for label
        nodes$title <- nodes$id
        nodes$label <- gsub("\\S+:", "", nodes$id)

        #---- Edges
        # Create list of edges by keeping the Subject and Predicate from query result.
        edges<-as.data.frame(rename(RDFTriples, c("s" = "from", "o" = "to")))
        
        # Edge values
        #   use edges$label for values always displayed
        #   use edges$title for values only displayed on mouseover
        edges$title <-gsub("\\S+:", "", edges$p)   # label : text always present
  
        # ORIGINAL TESTING visNetwork(nodes, edges)
        
        visNetwork(nodes, edges, height = "500px", width = "100%") %>%
            visOptions(selectedBy = "group", 
                highlightNearest = TRUE, 
                nodesIdSelection = TRUE) %>%
            visEdges(arrows = list(to = list(enabled = TRUE, scaleFactor = 0.5)),
                smooth = list(enabled = FALSE, type = "cubicBezier", roundness=.8)) %>%
            visGroups(groupname = "Person",    color = "#ffff33") %>%
            visGroups(groupname = "Protocol",  color = "#99C2C2") %>%
            visGroups(groupname = "Study",     color = "#A3A3C2") %>%
            visGroups(groupname = "Treatment", color = "#FFB280") %>%
            visGroups(groupname = "Literal",   color = list(background="white", border="black")) %>%
            visPhysics(stabilization=FALSE, barnesHut = list(
                avoidOverlap=1,
                gravitationalConstant = -3000,
                springConstant = 0.0004,
                damping = 0.9,
                springLength = 40
            ))  
     })
}
shinyApp(ui = ui, server = server)