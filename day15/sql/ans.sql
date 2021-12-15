.bail on
.mode table

select
costs.v
from costs
where costs.r = (select max(costs.r) from costs)
and costs.c = (select max(costs.c) from costs);
