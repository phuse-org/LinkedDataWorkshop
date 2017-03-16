//111-BartSister-Graph.cql
MATCH a = (brother)-[:hasSon]-(parent)-[:hasDaughter]-(daughter)
WHERE brother.firstName='Bart'
RETURN a