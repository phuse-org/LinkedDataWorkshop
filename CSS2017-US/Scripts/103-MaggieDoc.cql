//103-MaggieDoc.cql
MATCH (pers1)-[r:hasDaughter]-(pers2)
WHERE pers2.firstName='Maggie'
RETURN r.DeliveredBy as DeliveryDoctor