.bail on
.mode table

drop table if exists ints;
create table ints (
n int not null
);

.mode csv
.import "ints.txt" ints

drop table if exists raw;
create table raw (
line text not null
);
.mode csv
-- .import "../example.txt" raw
.import "../input.txt" raw

drop table if exists risks;
create table risks (
r int not null,
c int not null,
v int not null
);
create unique index risks_rc on risks(r, c);

insert into risks (r, c, v)
select
raw.rowid as r,
rowidx.n as c,
substr(raw.line, rowidx.n, 1) as v
from raw
cross join ints as rowidx
where rowidx.n <= length(raw.line);

drop table if exists neighbors;
create table neighbors (
r int not null,
c int not null,
disp text not null
);
insert into neighbors (r, c, disp)
values
(-1, 0, '^'),
(1, 0, 'v'),
(0, -1, '<'),
(0, 1, '>');
