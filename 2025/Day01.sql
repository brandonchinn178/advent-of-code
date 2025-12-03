create temporary table vals (val int);

insert into vals
select
    case substr(line, 1, 1)
        when 'R' then cast(substr(line, 2) as int)
        when 'L' then -1 * cast(substr(line, 2) as int)
    end as val
from input;

-- part 1
select '---------- Part 1 ----------';
with raw_states as (
    select 50 + sum(val) over (rows unbounded preceding) AS raw_pos
    from vals
), states as (
    select *, (raw_pos % 100 + 100) % 100 AS pos from raw_states
)
select count(*) from states where pos = 0;

-- part 2
select '---------- Part 2 ----------';
