create table coords as
with recursive cte(c, x, y, rest) as (
    select null, -1, _rowid_ - 1, line from input
    union all
    select
        substr(rest, 1, 1),
        x + 1,
        y,
        substr(rest, 2)
    from cte
    where rest != ''
)
select row_number() over () as id, x, y from cte where c == '@';

create index idx_coords
on coords (id);

create table rounds as
with recursive
    neighbor_offsets(dx, dy) as (
        select -1, -1 union all
        select -1,  0 union all
        select -1,  1 union all
        select  0, -1 union all
        select  0,  1 union all
        select  1, -1 union all
        select  1,  0 union all
        select  1,  1
    ),
    round_simulation(round, coords) as (
        select 0, (select group_concat(id || ',-1', ';') from coords)
        union all
        select
            sim.round + 1,
            (
                with recursive
                    parsed_coords(id, removed_at, rest) as (
                        select null, null, sim.coords || ';'
                        union all
                        select
                            cast(substr(rest, 1, INSTR(rest, ',') - 1) as int),
                            cast(substr(rest, INSTR(rest, ',') + 1, INSTR(rest, ';') - INSTR(rest, ',') - 1) as int),
                            substr(rest, INSTR(rest, ';') + 1)
                        from parsed_coords
                        where rest != ''
                    ),
                    curr_coords(id, x, y, removed_at) as (
                        select *
                        from parsed_coords
                        inner join coords on parsed_coords.id = coords.id
                    ),
                    curr_coords_with_neighbors(id, x, y, removed_at, num_neighbors) as (
                        select
                            *,
                            (
                                select count(*)
                                from neighbor_offsets
                                where exists (
                                    select 1
                                    from curr_coords as neighbor
                                    where neighbor.x = c.x + dx
                                    and neighbor.y = c.y + dy
                                    and neighbor.removed_at != -1
                                )
                            )
                        from curr_coords as c
                    ),
                    new_coords(id, removed_at) as (
                        select
                            id,
                            case when removed_at != -1 then
                                removed_at
                            when num_neighbors > 4 then
                                sim.round + 1
                            else
                                -1
                            end
                        from curr_coords_with_neighbors
                    )
                select group_concat(id || ',' || removed_at, ';') from parsed_coords where id is not null
            )
        from round_simulation as sim
        where
            sim.round < 2 -- DEBUG
            -- sim.round < 80 -- max rounds in final data is 74
    ),
    parsed_rounds(round, id, removed_at, rest) as (
        select round, null, null, coords || ';' from round_simulation
        union all
        select
            round,
            cast(substr(rest, 1, INSTR(rest, ',') - 1) as int),
            cast(substr(rest, INSTR(rest, ',') + 1, INSTR(rest, ';') - INSTR(rest, ',') - 1) as int),
            substr(rest, INSTR(rest, ';') + 1)
        from parsed_rounds
        where rest != ''
    )
select
    round,
    'id=' || id || ', removed_at=' || removed_at as num_removed -- DEBUG
    -- count(case when removed_at = round then 1 else 0 end) as num_removed
from parsed_rounds
order by round -- DEBUG
-- group by round
-- having round >= 1
;

-- DEBUG(
--     select round || ': ' || num_removed from rounds
-- );

PART1(
    select num_removed from rounds where round = 1
);

PART2(
    select sum(num_removed) from rounds
);
