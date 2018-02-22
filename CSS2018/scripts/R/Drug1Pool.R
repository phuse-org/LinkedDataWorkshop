###############################################################################
# FILE: /scripts/r/Drug1Pool.R
# DESC: LDWorkshop: Select, query, visuzalize a TTL file.
# IN  : data/RDFModel.TTL
# OUT : N/A
# REQ : 
# NOTE: Visualize requires query to return s,p,o. 
# TODO: 1. Reload of TTL does not reset QC Check data. Need to reset dataframes
###############################################################################
library(SPARQL)
ep = "http://localhost:5820/Drug1Pool/query" # not working as of 2017-01-16
# ep = "http://localhost:5820/#/databases/Drug1Pool/query"
#METHOD 2: Service

# Define the namespaces
namespaces <- c('eg', '<http://example.org/LDWorkshop#>',
'ncit', '<http://ncicb.nci.nih.gov/xml/owl/EVS/Thesaurus.owl#>',
'rdf', '<http://www.w3.org/1999/02/22-rdf-syntax-ns#>',
'rdfs', '<http://www.w3.org/2000/01/rdf-schema#>',
'schema', '<http://schema.org/>',
'xsd', '<http://www.w3.org/2001/XMLSchema#>'
)

query = '
SELECT ?s ?p ?o
WHERE 
{
   ?s ?p ?o
}'

triples <- SPARQL(url=ep, query=query, ns=namespaces)
triples

