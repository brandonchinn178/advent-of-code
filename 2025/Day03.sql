create table batteries as
with recursive
    cte(bank, col, char, remaining_string) AS (
        select
            _rowid_ as bank,
            0 as col,
            substr(line, 1, 1) as char,
            substr(line, 2) as remaining_string
        from input

        union all

        select
            bank,
            col + 1 as col,
            substr(remaining_string, 1, 1) as char,
            substr(remaining_string, 2) as remaining_string
        from cte
        where remaining_string != ''
    )
select bank, col, cast(char as int) as val from cte order by bank, col;

create index idx_batteries
on batteries (bank, col, val DESC);

-- part 1
select '---------- Part 1 ----------';
with batteries_with_top_digit as (
    select
        *,
        row_number() over (partition by bank order by val desc) AS rank
    from batteries
    where batteries.col < (select max(col) from batteries)
), first_digits as (
    select bank, col, val
    from batteries_with_top_digit
    where rank = 1
), batteries_with_next_top_digit as (
    select
        batteries.*,
        row_number() over (partition by batteries.bank order by batteries.val desc) AS rank
    from batteries inner join first_digits on batteries.bank = first_digits.bank
    where batteries.col > first_digits.col
), digits as (
    select * from first_digits
    union all
    select bank, col, val from batteries_with_next_top_digit where rank = 1
), joltages as (
    select
        cast(group_concat(cast(val as string), '' order by col) as int) as joltage
    from digits
    group by bank
)
select sum(joltage) from joltages;

-- part 2
select '---------- Part 2 ----------';
create table possibilities as
with recursive
    cte(bank, digits_left, pos, number) AS (
        select
            bank,
            12 as digits_left,
            -1 as pos,
            '' as number
        from batteries
        group by bank

        union all

        select
            cte.bank,
            cte.digits_left - 1,
            batteries.col,
            cte.number || batteries.val
        from cte
        cross join (select max(col) as max_col from batteries) as m
        inner join batteries on cte.bank = batteries.bank
        where cte.digits_left > 0
        and batteries.col > cte.pos
        and batteries.col <= m.max_col - cte.digits_left + 1
        and batteries.val = (
            select max(val)
            from batteries as batteries2
            where batteries2.bank = batteries.bank
            and batteries2.col > cte.pos
            and batteries2.col <= m.max_col - cte.digits_left + 1
        )
    )
select bank, cast(number as int) as number from cte where digits_left = 0 order by bank;

select sum(joltage) from (
    select max(number) as joltage
    from possibilities
    group by bank
);
