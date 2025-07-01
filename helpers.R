# ==========================================
#    Shiny高速化用ヘルパースクリプト
#    - 空間データ & 指標データを年ごとに分割
#    - 前処理済みデータをRDSキャッシュ
# ==========================================

library(tidyverse)
library(sf)

# ----- 設定 -----
csv_file <- "data/data.csv"
gis_file <- "data/jp_muni_simplified.gpkg"
rds_file <- "data/map_data_list.rds"

# ==========================================
# 1. RDSが存在すればロード、なければ前処理
# ==========================================

if (file.exists(rds_file)) {
  # ---- ロード ----
  map_data_list <- read_rds(rds_file)
  
} else {
  # ---- 人口データ読み込み・変数作成 ----
  col_names <- c("id_muni", "name_muni", "year", "population", "pop_u15", "pop_15to64", "pop_o65")
  df <- read_csv(csv_file, skip = 1) |> 
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
    group_by(id_muni) |> 
    mutate(
      inc_rate_pop    = if_else(lag(population) == 0, NA_real_, (population - lag(population)) / lag(population)), 
      inc_rate_u15    = if_else(lag(pop_u15) == 0, NA_real_, (pop_u15 - lag(pop_u15)) / lag(pop_u15)), 
      inc_rate_15to64 = if_else(lag(pop_15to64) == 0, NA_real_, (pop_15to64 - lag(pop_15to64)) / lag(pop_15to64)), 
      inc_rate_o65    = if_else(lag(pop_o65) == 0, NA_real_, (pop_o65 - lag(pop_o65)) / lag(pop_o65)), 
      pct_inc_pop     = round(inc_rate_pop * 100, digits = 2), 
      pct_inc_u15     = round(inc_rate_u15 * 100, digits = 2), 
      pct_inc_15to64  = round(inc_rate_15to64 * 100, digits = 2), 
      pct_inc_o65     = round(inc_rate_o65 * 100, digits = 2)
    ) |> 
    ungroup()
  
  # ---- 市町村ポリゴンを読み込み ----
  gis <- read_sf(gis_file)
  
  # ---- 人口データと空間データを結合 ----
  df_gis <- df |> 
    left_join(gis |> select(-name_muni, geometry = geom), by = "id_muni") |> 
    st_as_sf() |> 
    st_transform(4326)
  
  # ---- 年ごとにsfデータをリスト化し、必要な列だけ保存 ----
  year_list <- sort(unique(df_gis$year))
  map_data_list <- vector("list", length(year_list))
  names(map_data_list) <- as.character(year_list)
  
  for (i in seq_along(year_list)) {
    y <- year_list[i]
    dat <- df_gis |> 
      filter(year == y) |> 
      select(
        id_muni, name_muni, year, geometry,
        population, pop_u15, pop_15to64, pop_o65,
        pct_u15, pct_15to64, pct_o65,
        pct_inc_pop, pct_inc_u15, pct_inc_15to64, pct_inc_o65
      )
    map_data_list[[as.character(y)]] <- dat
  }
  
  # ---- RDSでキャッシュ ----
  write_rds(map_data_list, rds_file)
}
