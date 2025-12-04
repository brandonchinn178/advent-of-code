/*
11|22
565653|565659
*/
create table intervals as
with recursive cte(v1, v2, rest) as (
    select null, null, line || ',' from input
    union all
    select
        cast(substr(rest, 1, INSTR(rest, '-') - 1) as int) as v1,
        cast(substr(rest, INSTR(rest, '-') + 1, INSTR(rest, ',') - INSTR(rest, '-') - 1) as int) as v2,
        substr(rest, INSTR(rest, ',') + 1) as rest
    from cte
    where rest != ''
)
select v1, v2
from cte
where v1 is not null and v2 is not null;

/*
-- ranges expanded out to numbers, converted to text
11
...
22
565653
...
565659
*/
create table all_numbers as
with recursive cte(val, end) as (
    select v1, v2 from intervals
    union all
    select val + 1, end
    from cte
    where val < end
)
select distinct cast(val as text) as num from cte order by val;

/* 1 to 10, assuming 10 is the max number of digits we'll see */
create table cycle_sizes as
with recursive cte(val, end) as (
    select 1, 10
    union all
    select val + 1, end
    from cte
    where val < end
)
select val as size from cte order by val;

/*
-- original number + number of cycles + resulting number
11|2|11
...
22|2|22
565653|2|565565
565653|3|565656
565653|6|555555
...
565659|2|565565
565659|3|565656
565659|6|555555
*/
create table trials as
with recursive cycle_parts as (
    select
        num as val,
        length(num) / size as num_cycles,
        substr(num, 1, size) as cycle_part
    from all_numbers
    cross join cycle_sizes
    where size < length(num)
    and length(num) % size = 0
),
cte(val, num_cycles, cycle_part, cycled_val) as (
    select val, num_cycles, cycle_part, cycle_part from cycle_parts
    union all
    select
        val,
        num_cycles,
        cycle_part,
        cycled_val || cycle_part
    from cte
    where length(cycled_val) < length(val)
)
select * from cte
where length(val) = length(cycled_val);

/*
-- numbers that have a cycle, with the number of cycles present
11|2
22|2
565656|3
*/
create table vals_with_cycles as
select val, num_cycles from trials where val = cycled_val;

-- part 1
insert into output (part, result)
select
    1 as part,
    sum(val) as result
from vals_with_cycles
where num_cycles = 2;

-- part 2
insert into output (part, result)
select
    2 as part,
    sum(distinct val) as result
from vals_with_cycles;
