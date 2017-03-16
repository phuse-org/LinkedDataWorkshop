//110-LisaBrother-Name.cql
MATCH a = (brother)-[:hasSon]-(parent)-[:hasDaughter]-(daughter)
WHERE daughter.firstName='Lisa'
RETURN DISTINCT brother.firstName
