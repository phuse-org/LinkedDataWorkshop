//---- Delete all nodes and relations in a graph
// Be careful young Graph Jedi - when its gone, its gone!
MATCH (n)
OPTIONAL MATCH (n)-[r]-()
DELETE n,r
