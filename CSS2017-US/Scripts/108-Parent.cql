//108-Parent.cql
MATCH (person1)-[r]->(person2)
WHERE (type(r) = "hasSon" OR 
       type(r) = "hasDaughter")
RETURN DISTINCT person1.firstName