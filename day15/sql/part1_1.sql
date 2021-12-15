.bail on
.mode table

drop table if exists costs;
create table costs (
r int not null,
c int not null,
v int not null,
-- If there's a known cost, this will be a foreign key to the
-- neighbors table indicating the direction OR null to indicate this
-- is the start
neighbor int
);
create unique index costs_rc on costs(r, c);

insert into costs (r, c, v, neighbor)
values (1, 1, 0, null);


drop table if exists route;
create table route (
r int not null,
c int not null
);
create unique index route_rc on route(r, c);

insert into route (r, c)
select max(risks.r), max(risks.c) from risks;
