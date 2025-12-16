create table intervals as
select
    cast(substr(line, 0, instr(line, '-')) as number) as lo,
    cast(substr(line, instr(line, '-') + 1) as number) as hi
from input
where _rowid_ < (select _rowid_ from input where line = '');

create table ids as
select cast(line as number) as id
from input
where _rowid_ > (select _rowid_ from input where line = '');

PART1(
    select count(*)
    from ids
    where exists (
        select 1
        from intervals
        where id >= lo and id <= hi
    )
);

-- DEBUG(
--     select '' || _rowid_ || ' => ' || start
--     from (
--         select
--             _rowid_,
--             lo,
--             hi,
--             (
--                 select group_concat('[' || i2._rowid_ || ':' || i2.hi || ']')
--                 from intervals i2
--                 where (
--                     -- This row is not the current row
--                     i2._rowid_ != i1._rowid_
--                 ) and (
--                     -- This row overlaps at least some part of the beginning of the current row
--                     i2.lo <= i1.lo and i2.hi >= i1.lo
--                 )
--             ) as start
--         from intervals i1
--     )
--     where start is not null
-- );

-- TODO: If two intervals are the same, how to prevent them from choosing each other?

-- PART2(
--     select sum(hi - min(hi, max(start, lo - 1)))
--     from (
--         select
--             lo,
--             hi,
--             (
--                 select coalesce(max(i2.hi), 0)
--                 from intervals i2
--                 where (
--                     -- This row is not the current row
--                     i2._rowid_ != i1._rowid_
--                 ) and (
--                     -- This row overlaps at least some part of the beginning of the current row
--                     i2.lo <= i1.lo and i2.hi >= i1.lo
--                 )
--             ) as start
--         from intervals i1
--     )
-- );
