//109-LisaBrother-Graph.cql
MATCH a = (brother)-[:hasSon]-(parent)-[:hasDaughter]-(daughter)
WHERE daughter.firstName='Lisa'
RETURN a
