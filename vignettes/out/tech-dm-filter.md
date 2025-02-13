<!-- Generated by galley: do not edit by hand -->

The {dm} package offers functions to work with relational data models in
R.

This document introduces you to filtering functions, and shows how to
apply them to the data that is separated into multiple tables.

Our example data is drawn from the
[{nycflights13}](https://github.com/tidyverse/nycflights13) package that
contains five inter-linked tables.

First, we will load the packages that we need:

``` r
library(tidyverse)
library(nycflights13)
library(dm)
```

## Data: nycflights13

To explore filtering with {dm}, we’ll use the {nycflights13} data with
its `flights`, `planes`, `airlines`, `airports` and `weather` tables.

This dataset contains information about the 336 776 flights that
departed from New York City in 2013, with 3322 different planes and 1458
airports involved. The data comes from the US Bureau of Transportation
Statistics, and is documented in `?nycflights13::flights`.

To start with our exploration, we have to create a `dm` object from the
{nycflights13} data. The built-in `dm::dm_nycflights13()` function takes
care of this.

By default it only uses a subset of the complete data though: only the
flights on the 10th of each month are considered, reducing the number of
rows in the `flights` table to 11 227.

A [data model
object](https://cynkra.github.io/dm/articles/tech-dm-class.html#class-dm)
contains data from the source tables, and metadata about the tables.

If you would like to create a `dm` object from tables other than the
example data, you can use the `new_dm()`, `dm()` or `as_dm()` functions.
See `vignette("howto-dm-df")` for details.

``` r
dm <- dm_nycflights13()
```

The console output of the ’dm\` object shows its data and metadata, and
is colored for clarity:

``` r
dm
#> ── Metadata ───────────────────────────────────────────────────────────────
#> Tables: `airlines`, `airports`, `flights`, `planes`, `weather`
#> Columns: 53
#> Primary keys: 4
#> Foreign keys: 4
```

Now we know that there are five tables in our `dm` object. But how are
they connected? These relations are best displayed as a visualization of
the entity-relationship model:

``` r
dm_draw(dm)
```

![](/home/kirill/git/R/dm/vignettes/out/tech-dm-filter_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

You can look at a single table with `tbl`. To print the `airports`
table, call

``` r
tbl(dm, "airports")
#> # A tibble: 1,458 x 8
#>    faa   name                    lat    lon   alt    tz dst   tzone        
#>    <chr> <chr>                 <dbl>  <dbl> <dbl> <dbl> <chr> <chr>        
#>  1 04G   Lansdowne Airport      41.1  -80.6  1044    -5 A     America/New_…
#>  2 06A   Moton Field Municipa…  32.5  -85.7   264    -6 A     America/Chic…
#>  3 06C   Schaumburg Regional    42.0  -88.1   801    -6 A     America/Chic…
#>  4 06N   Randall Airport        41.4  -74.4   523    -5 A     America/New_…
#>  5 09J   Jekyll Island Airport  31.1  -81.4    11    -5 A     America/New_…
#>  6 0A9   Elizabethton Municip…  36.4  -82.2  1593    -5 A     America/New_…
#>  7 0G6   Williams County Airp…  41.5  -84.5   730    -5 A     America/New_…
#>  8 0G7   Finger Lakes Regiona…  42.9  -76.8   492    -5 A     America/New_…
#>  9 0P2   Shoestring Aviation …  39.8  -76.6  1000    -5 U     America/New_…
#> 10 0S9   Jefferson County Intl  48.1 -123.    108    -8 A     America/Los_…
#> # … with 1,448 more rows
```

## Filtering a `dm` object

`dm_filter()` allows you to select a subset of a `dm` object.

### How it works

Filtering a `dm` object is not that different from filtering a dataframe
or tibble with `dplyr::filter()`.

The corresponding {dm} function is `dm::dm_filter()`. With this function
one or more filtering conditions can be set for one of the tables of the
`dm` object. These conditions are immediately evaluated for their
respective tables and in addition they are stored in the `dm`. There are
two ways in which a condition on one table could affect another table:

1.  Calling `dm_apply_filters_to_tbl()`, i.e. requesting a table from a
    `dm` after the filter conditions are applied. In this case, all
    tables that are connected to the requested table and have stored
    filter conditions associated with them are taken into account in the
    following way:
    -   filtering semi-joins are successively performed along the paths
        from each of the filtered tables to the requested table, each
        join reducing the left-hand side tables of the joins to only
        those of their rows with key values that have corresponding
        values in key columns of the right-hand side tables of the join.
    -   eventually the requested table is returned, containing only the
        the remaining rows after the filtering joins
2.  Calling `dm_apply_filters()` or `compute()` methods for `dm` objects
    on a `dm`: this results in a new `dm` that contains the same tables
    as before. Each table is the result of a `dm_apply_filters_to_tbl()`
    call, meaning that the effects of the filter conditions on each of
    the tables are taken into account.

Currently, this only works if the graph induced by the foreign key
relations is cycle free. Fortunately, this is the default for
`dm_nycflights13()`.

Keep in mind that several {dm} functions will refuse to work when
unevaluated filter conditions exist, such as,
e.g. `dm_enum_fk_candidates()`, `dm_enum_pk_candidates()`,
`dm_select_tbl()`, `dm_rename_tbl()`, `dm_select()`, `dm_rename()` and
`dm_nrow()`. In these cases consider applying the filters with
`dm_apply_filters()` first.

### Filtering Examples

Let’s see filtering in action:

**We only want the data that is related to John F. Kennedy International
Airport.**

``` r
filtered_dm <-
  dm %>%
  dm_filter(airports, name == "John F Kennedy Intl")
filtered_dm
#> ── Metadata ───────────────────────────────────────────────────────────────
#> Tables: `airlines`, `airports`, `flights`, `planes`, `weather`
#> Columns: 53
#> Primary keys: 4
#> Foreign keys: 4
#> ── Filters ────────────────────────────────────────────────────────────────
#> airports: name == "John F Kennedy Intl"
```

The filter expression is listed in the print output.

You can get the numbers of rows of each table with `dm_nrow()`. Before
doing that, you will need to apply the filters using
`dm_apply_filters()`:

``` r
rows_per_table <-
  filtered_dm %>%
  dm_apply_filters() %>%
  dm_nrow()
rows_per_table
#> airlines airports  flights   planes  weather 
#>       10        1     3661      783      228
sum(rows_per_table)
#> [1] 4683
```

``` r
sum_nrow <- sum(dm_nrow(dm))
sum_nrow_filtered <- sum(dm_nrow(dm_apply_filters(filtered_dm)))
```

The total number of rows in the `dm` drops from 16 884 to 4 683 (the
only unaffected table is the disconnected `weather` table).

Next example:

**Get a `dm` object containing data for flights from New York to the
Dulles International Airport in Washington D.C., abbreviated with
`IAD`.**

``` r
dm %>%
  dm_filter(flights, dest == "IAD") %>%
  dm_apply_filters() %>%
  dm_nrow()
#> airlines airports  flights   planes  weather 
#>        4        3      191       95      178
```

Chaining multiple filters on different tables is also supported.

An example:

**Get all flights from Delta Air Lines which didn’t depart from John F.
Kennedy International Airport in May 2013.**

``` r
dm_delta_may <-
  dm %>%
  dm_filter(airlines, name == "Delta Air Lines Inc.") %>%
  dm_filter(airports, name != "John F Kennedy Intl") %>%
  dm_filter(flights, month == 5)
dm_delta_may
#> ── Metadata ───────────────────────────────────────────────────────────────
#> Tables: `airlines`, `airports`, `flights`, `planes`, `weather`
#> Columns: 53
#> Primary keys: 4
#> Foreign keys: 4
#> ── Filters ────────────────────────────────────────────────────────────────
#> airlines: name == "Delta Air Lines Inc."
#> airports: name != "John F Kennedy Intl"
#> flights: month == 5
dm_delta_may %>%
  dm_apply_filters() %>%
  dm_nrow()
#> airlines airports  flights   planes  weather 
#>        1        2       79       61       27
```

You can inspect the filtered tables with `dm_apply_filters_to_tbl()`.

In the `airlines` table, Delta is the only remaining carrier:

``` r
dm_delta_may %>%
  dm_apply_filters_to_tbl("airlines")
#> # A tibble: 1 x 2
#>   carrier name                
#>   <chr>   <chr>               
#> 1 DL      Delta Air Lines Inc.
```

Which planes were used to service these flights?

``` r
dm_delta_may %>%
  dm_apply_filters_to_tbl("planes")
#> # A tibble: 61 x 9
#>    tailnum  year type       manufacturer  model  engines seats speed engine
#>    <chr>   <int> <chr>      <chr>         <chr>    <int> <int> <int> <chr> 
#>  1 N305DQ   2008 Fixed win… BOEING        737-7…       2   149    NA Turbo…
#>  2 N310DE   2009 Fixed win… BOEING        737-7…       2   149    NA Turbo…
#>  3 N312US   1990 Fixed win… AIRBUS INDUS… A320-…       2   182    NA Turbo…
#>  4 N313US   1990 Fixed win… AIRBUS INDUS… A320-…       2   182    NA Turbo…
#>  5 N314US   1991 Fixed win… AIRBUS INDUS… A320-…       2   182    NA Turbo…
#>  6 N318NB   2000 Fixed win… AIRBUS INDUS… A319-…       2   145    NA Turbo…
#>  7 N322NB   2001 Fixed win… AIRBUS INDUS… A319-…       2   145    NA Turbo…
#>  8 N325NB   2001 Fixed win… AIRBUS INDUS… A319-…       2   145    NA Turbo…
#>  9 N329NW   1992 Fixed win… AIRBUS INDUS… A320-…       2   182    NA Turbo…
#> 10 N332NW   1992 Fixed win… AIRBUS INDUS… A320-…       2   182    NA Turbo…
#> # … with 51 more rows
```

And indeed, all included flights departed in May (`month == 5`):

``` r
dm_delta_may %>%
  dm_apply_filters_to_tbl("flights")
#> # A tibble: 79 x 19
#>     year month   day dep_time sched_dep_time dep_delay arr_time
#>    <int> <int> <int>    <int>          <int>     <dbl>    <int>
#>  1  2013     5    10      554            600        -6      739
#>  2  2013     5    10      556            600        -4      825
#>  3  2013     5    10      606            610        -4      743
#>  4  2013     5    10      625            630        -5      843
#>  5  2013     5    10      653            700        -7      923
#>  6  2013     5    10      656            700        -4      911
#>  7  2013     5    10      700            700         0      958
#>  8  2013     5    10      701            705        -4      952
#>  9  2013     5    10      714            715        -1      908
#> 10  2013     5    10      743            745        -2      956
#> # … with 69 more rows, and 12 more variables: sched_arr_time <int>,
#> #   arr_delay <dbl>, carrier <chr>, flight <int>, tailnum <chr>,
#> #   origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>, hour <dbl>,
#> #   minute <dbl>, time_hour <dttm>
```

For comparison, let’s review the equivalent manual query for `flights`
in `dplyr` syntax:

``` r
airlines_filtered <- filter(airlines, name == "Delta Air Lines Inc.")
airports_filtered <- filter(airports, name != "John F Kennedy Intl")
flights %>%
  semi_join(airlines_filtered, by = "carrier") %>%
  semi_join(airports_filtered, by = c("origin" = "faa")) %>%
  filter(month == 5)
#> # A tibble: 2,340 x 19
#>     year month   day dep_time sched_dep_time dep_delay arr_time
#>    <int> <int> <int>    <int>          <int>     <dbl>    <int>
#>  1  2013     5     1      554            600        -6      731
#>  2  2013     5     1      555            600        -5      819
#>  3  2013     5     1      603            610        -7      754
#>  4  2013     5     1      622            630        -8      848
#>  5  2013     5     1      654            700        -6      931
#>  6  2013     5     1      655            700        -5      944
#>  7  2013     5     1      656            705        -9     1005
#>  8  2013     5     1      658            700        -2      925
#>  9  2013     5     1      743            745        -2     1014
#> 10  2013     5     1      755            800        -5      929
#> # … with 2,330 more rows, and 12 more variables: sched_arr_time <int>,
#> #   arr_delay <dbl>, carrier <chr>, flight <int>, tailnum <chr>,
#> #   origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>, hour <dbl>,
#> #   minute <dbl>, time_hour <dttm>
```

The {dm} code is leaner because the foreign key information is encoded
in the object.

## SQL statements behind filtering a `dm` object on a database

{dm} is meant to work with relational data models, locally as well as on
databases. In your project, the data is probably not stored locally but
in a remote [relational
database](https://cynkra.github.io/dm/articles/howto-dm-theory.html#relational-databases)
that can be queried with SQL statements.

You can check the queries by using `sql_render()` from the
[{dbplyr}](https://dbplyr.tidyverse.org/) package.

Example:

**Print the SQL statements for getting all flights from Delta Air Lines,
which did not depart from John F. Kennedy International Airport in May
2013, with the data stored in a sqlite database.**

To show the SQL query behind a `dm_filter()`, we copy the `flights`,
`airlines` and `airports` tables from the `nyflights13` dataset to a
temporary in-memory database using the built-in function `copy_dm_to()`
and `dbplyr::src_memdb`.

Then we filter the data, and print the corresponding SQL statement with
`dbplyr::sql_render()`.

``` r
dm %>%
  dm_select_tbl(flights, airlines, airports) %>%
  dm_filter(flights, month == 5) %>%
  copy_dm_to(dbplyr::src_memdb(), .) %>%
  dm_filter(airlines, name == "Delta Air Lines Inc.") %>%
  dm_filter(airports, name != "John F Kennedy Intl") %>%
  dm_apply_filters() %>%
  dm_get_tables() %>%
  map(dbplyr::sql_render)
#> $flights
#> <SQL> SELECT * FROM (SELECT * FROM `flights_2020_08_28_07_13_03_12345_1` AS `LHS`
#> WHERE EXISTS (
#>   SELECT 1 FROM (SELECT *
#> FROM `airlines_2020_08_28_07_13_03_12345_1`
#> WHERE (`name` = 'Delta Air Lines Inc.')) AS `RHS`
#>   WHERE (`LHS`.`carrier` = `RHS`.`carrier`)
#> )) AS `LHS`
#> WHERE EXISTS (
#>   SELECT 1 FROM (SELECT *
#> FROM `airports_2020_08_28_07_13_03_12345_1`
#> WHERE (`name` != 'John F Kennedy Intl')) AS `RHS`
#>   WHERE (`LHS`.`origin` = `RHS`.`faa`)
#> )
#> 
#> $airlines
#> <SQL> SELECT * FROM (SELECT *
#> FROM `airlines_2020_08_28_07_13_03_12345_1`
#> WHERE (`name` = 'Delta Air Lines Inc.')) AS `LHS`
#> WHERE EXISTS (
#>   SELECT 1 FROM (SELECT * FROM `flights_2020_08_28_07_13_03_12345_1` AS `LHS`
#> WHERE EXISTS (
#>   SELECT 1 FROM (SELECT *
#> FROM `airports_2020_08_28_07_13_03_12345_1`
#> WHERE (`name` != 'John F Kennedy Intl')) AS `RHS`
#>   WHERE (`LHS`.`origin` = `RHS`.`faa`)
#> )) AS `RHS`
#>   WHERE (`LHS`.`carrier` = `RHS`.`carrier`)
#> )
#> 
#> $airports
#> <SQL> SELECT * FROM (SELECT *
#> FROM `airports_2020_08_28_07_13_03_12345_1`
#> WHERE (`name` != 'John F Kennedy Intl')) AS `LHS`
#> WHERE EXISTS (
#>   SELECT 1 FROM (SELECT * FROM `flights_2020_08_28_07_13_03_12345_1` AS `LHS`
#> WHERE EXISTS (
#>   SELECT 1 FROM (SELECT *
#> FROM `airlines_2020_08_28_07_13_03_12345_1`
#> WHERE (`name` = 'Delta Air Lines Inc.')) AS `RHS`
#>   WHERE (`LHS`.`carrier` = `RHS`.`carrier`)
#> )) AS `RHS`
#>   WHERE (`LHS`.`faa` = `RHS`.`origin`)
#> )
```

Further reading: {dm}’s function for copying data [from and to
databases](https://cynkra.github.io/dm/articles/dm.html#copy).
