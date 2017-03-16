//101-LisaGender.cql
MATCH (a:Person)
WHERE a.firstName = "Lisa"
RETURN a.gender