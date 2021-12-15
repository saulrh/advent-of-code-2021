insert or replace into costs (r, c, v, neighbor)
select
risks.r as r,
risks.c as c,
min(coalesce(prev.v, 100000), min(risks.v + adj.v)) as v,
neighbors.rowid as neighbor
from
risks
cross join neighbors
left outer join costs as prev ON (risks.r = prev.r AND risks.c = prev.c)
inner join costs as adj ON (risks.r + neighbors.r = adj.r AND risks.c + neighbors.c = adj.c)
group by risks.r, risks.c;
