// xxx-DeleteGraph.cql
// Careful you must be, my young padawan.
MATCH (n)
OPTIONAL MATCH (n)-[r]-()
DELETE n,r