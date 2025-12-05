create table vals as
select
    _rowid_ as idx,
    case substr(line, 1, 1)
        when 'R' then cast(substr(line, 2) as int)
        when 'L' then -1 * cast(substr(line, 2) as int)
    end as val
from input;

create table states as
with raw_states as (
    select
        idx,
        ((sum(val) over (order by idx rows between unbounded preceding and 1 preceding))
            % 100 + 100) % 100 AS prev_pos,
        val
    from (select * from vals union select 0, 50)
)
select
    idx,
    prev_pos,
    val,
    val + prev_pos as raw_pos,
    ((val + prev_pos) % 100 + 100) % 100 as pos
from raw_states;

PART1(
    select count(*)
    from states
    where pos = 0
);

PART2(
    select sum(
        abs(raw_pos) / 100
        + case when prev_pos > 0 and raw_pos < 0 then 1 else 0 end
        + case when raw_pos == 0 then 1 else 0 end
    )
    from states
    where idx > 0
);
