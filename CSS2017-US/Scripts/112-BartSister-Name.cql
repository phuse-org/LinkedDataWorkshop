//112-BartSister-Name.cql
MATCH a = (brother)-[:hasSon]-(parent)-[:hasDaughter]-(daughter)
WHERE brother.firstName='Bart'
RETURN DISTINCT daughter.firstName