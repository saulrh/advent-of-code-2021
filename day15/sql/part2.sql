.bail on


insert into risks (r, c, v)
select
risks.r + (select max(r) from risks) * (megarow.n - 1) as r,
risks.c + (select max(c) from risks) * (megacol.n - 1) as c,
CASE WHEN (risks.v + megarow.n + megacol.n - 2) > 9
THEN (risks.v + megarow.n + megacol.n - 2) - 9
ELSE (risks.v + megarow.n + megacol.n - 2) END as v
from risks
cross join ints as megarow
cross join ints as megacol
where
megarow.n <= 5
and megacol.n <= 5
and not (megarow.n == 1 and megacol.n == 1);
