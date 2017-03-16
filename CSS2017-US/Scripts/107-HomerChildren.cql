//107-HomerChildren.cql
MATCH (person1)-[r]->(person2)
WHERE (person1.firstName ='Homer' AND
       (type(r) = 'hasSon' OR 
        type(r) = 'hasDaughter'))
RETURN DISTINCT person2.firstName