insert into route (r, c)
select
route.r + neighbors.r as r,
route.c + neighbors.c as c
from
route
inner join costs on route.r = costs.r and route.c = costs.c
inner join neighbors on neighbors.rowid = costs.neighbor
on conflict do nothing;
