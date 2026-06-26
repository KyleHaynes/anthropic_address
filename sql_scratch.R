
require(data.table)

x <- data.table(DBI::dbGetQuery(
  con,
  "SELECT * FROM gnaf_addresses WHERE alias_type = 'street_only'"
))

x <- data.table(DBI::dbGetQuery(
  con,
  "SELECT * FROM gnaf_addresses WHERE address_label LIKE '%FIRST%'"
))


freq <- DBI::dbGetQuery(
  con,
  "
  SELECT
    alias_type,
    COUNT(*) AS n
  FROM gnaf_addresses
  GROUP BY alias_type
  ORDER BY n DESC
  "
)


gnaf_match("190 MUSGRAVE RD RED HILL 4059 QLD", con, alias_types = c("street_only"))
# Index: <matched>
#    input_id                         input_raw                   input_standardised                      address_label match_rank matched match_status total_score score_postcode score_suburb score_street_name score_street_type score_number score_flat in_postcode in_state
#       <int>                            <char>                               <char>                             <char>      <int>  <lgcl>       <char>       <int>          <int>        <int>             <int>             <int>        <int>      <int>       <int>   <char>
# 1:        1 190 MUSGRAVE RD RED HILL 4059 QLD 190 MUSGRAVE ROAD, RED HILL QLD 4059 190 MUSGRAVE RD, RED HILL QLD 4059          1    TRUE      matched         100             20           15                40                10           10          5        4059      QLD
#    in_locality in_street_name in_street_type in_street_suffix in_number_first in_number_last in_flat_type in_flat_number in_building_name address_detail_pid building_name flat_type flat_number number_first number_last street_name street_type street_suffix locality_name  state
#         <char>         <char>         <char>           <char>           <int>          <int>       <char>         <char>           <char>             <char>        <char>    <char>      <char>        <int>       <int>      <char>      <char>        <char>        <char> <char>
# 1:    RED HILL       MUSGRAVE           ROAD             <NA>             190             NA         <NA>           <NA>             <NA>     GAQLD155735246          <NA>      <NA>        <NA>          190          NA    MUSGRAVE        ROAD          <NA>      RED HILL    QLD
#    postcode longitude  latitude source alias_type
#       <int>     <num>     <num> <char>     <char>
# 1:     4059  153.0057 -27.45377   gnaf       <NA>
