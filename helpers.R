
# ---- データの読み込み ----

col_names <- c("id_muni", "name_muni", "year", "population", "pop_u15", "pop_15to64", "pop_o65")

df <- read_csv("data/data.csv", skip = 1) |> 
  select(1, 2, 3, 6:9) |> 
  set_names(col_names) |> 
  arrange(id_muni, year) |> 
  mutate(
    year         = as.integer(str_sub(year, 1, 4)), 
    ratio_u15    = if_else(population == 0, NA_real_, pop_u15 / population),
    ratio_15to64 = if_else(population == 0, NA_real_, pop_15to64 / population),
    ratio_o65    = if_else(population == 0, NA_real_, pop_o65 / population), 
    pct_u15      = round(ratio_u15 * 100, digits = 2), 
    pct_15to64   = round(ratio_15to64 * 100, digits = 2), 
    pct_o65      = round(ratio_o65 * 100, digits = 2)
  ) |> 
  mutate(
    inc_rate_pop    = if_else(lag(population) == 0, NA_real_, 
                              (population - lag(population)) / lag(population)), 
    inc_rate_u15    = if_else(lag(pop_u15) == 0, NA_real_, 
                              (pop_u15 - lag(pop_u15)) / lag(pop_u15)), 
    inc_rate_15to64 = if_else(lag(pop_15to64) == 0, NA_real_, 
                              (pop_15to64 - lag(pop_15to64)) / lag(pop_15to64)), 
    inc_rate_o65    = if_else(lag(pop_o65) == 0, NA_real_, 
                              (pop_o65 - lag(pop_o65)) / lag(pop_o65)), 
    pct_inc_pop     = round(inc_rate_pop * 100, digits = 2), 
    pct_inc_u15     = round(inc_rate_u15 * 100, digits = 2), 
    pct_inc_15to64  = round(inc_rate_15to64 * 100, digits = 2), 
    pct_inc_o65     = round(inc_rate_o65 * 100, digits = 2), 
    .by = id_muni
  )


# ---- geojsonファイルの読み込み ----

gis <- read_sf("data/jp_muni_simplified.geojson")


# ---- データフレームの作成 ----

df_gis <- df |> 
  left_join(gis |> select(-name_muni), 
            by = "id_muni") |> 
  st_as_sf()

