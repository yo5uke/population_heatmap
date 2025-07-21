#-------------------------------------------------
# 作成：阿部洋輔
# 市町村パネル・空間データ 共通化・軽量化ヘルパー
# - 表示用データの作成
# - データの軽量化
#-------------------------------------------------

library(tidyverse)
library(sf)

csv_file   <- "data/data.csv"                   # 属性データ
gis_file   <- "data/jp_muni_simplified.gpkg"    # 市町村geometry
rds_geom   <- "data/muni_geometry.rds"
rds_attr   <- "data/pop_attr_df.rds"

#------------------------------
# 1. キャッシュがあればロード
#------------------------------
if (file.exists(rds_geom) && file.exists(rds_attr)) {
  muni_geometry <- read_rds(rds_geom)
  pop_attr_df   <- read_rds(rds_attr)
  
} else {
  #------------------------------
  # 2. geometry（市町村境界）は全期間を通じて一つだけ持つ
  #------------------------------
  muni_geometry <- read_sf(gis_file) |>
    select(id_muni, geom) |>
    st_transform(4326)
  
  #------------------------------
  # 3. 属性データ（年次人口など）はlong形式
  #------------------------------
  pop_attr_df <- read_csv(csv_file, skip = 1) |>
    select(1, 2, 3, 6:9) |>
    set_names(c("id_muni", "name_muni", "year", "population", "pop_u15", "pop_15to64", "pop_o65")) |>
    mutate(
      year = as.integer(str_sub(year, 1, 4)),
      pct_u15 = round(if_else(population == 0, NA_real_, pop_u15 / population * 100), 2),
      pct_15to64 = round(if_else(population == 0, NA_real_, pop_15to64 / population * 100), 2),
      pct_o65 = round(if_else(population == 0, NA_real_, pop_o65 / population * 100), 2)
    ) |>
    group_by(id_muni) |>
    arrange(id_muni, year) |>
    mutate(
      pct_inc_pop = round(if_else(lag(population) == 0, NA_real_, (population - lag(population)) / lag(population) * 100), 2),
      pct_inc_u15 = round(if_else(lag(pop_u15) == 0, NA_real_, (pop_u15 - lag(pop_u15)) / lag(pop_u15) * 100), 2),
      pct_inc_15to64 = round(if_else(lag(pop_15to64) == 0, NA_real_, (pop_15to64 - lag(pop_15to64)) / lag(pop_15to64) * 100), 2),
      pct_inc_o65 = round(if_else(lag(pop_o65) == 0, NA_real_, (pop_o65 - lag(pop_o65)) / lag(pop_o65) * 100), 2)
    ) |>
    ungroup()
  
  #------------------------------
  # 4. キャッシュ保存
  #------------------------------
  write_rds(muni_geometry, rds_geom)
  write_rds(pop_attr_df,   rds_attr)
}

#------------------------------
# 5. 指定年の属性をgeometryにjoin（Shiny等で使用）
#------------------------------
get_map_sf <- function(year) {
  left_join(muni_geometry, pop_attr_df |> filter(year == !!year), by = "id_muni") |> st_as_sf()
}

year_list <- sort(unique(pop_attr_df$year))
