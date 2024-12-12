library(tidyverse)
library(terra)
library(sf)

prefectures <- st_read(dsn = "Boundary2019.gdb", layer = "地级行政区划及省直辖县") %>%
  vect()

tiff_files <- c(
  M202301 = "20230101-20230131_global_vcmslcfg_v10_c202302080600",
  M202302 = "20230201-20230228_global_vcmslcfg_v10_c202303060900",
  M202303 = "20230301-20230331_global_vcmslcfg_v10_c202304101100",
  M202304 = "20230401-20230430_global_vcmslcfg_v10_c202305081600",
  M202305 = "20230501-20230531_global_vcmslcfg_v10_c202306121300",
  M202306 = "20230601-20230630_global_vcmslcfg_v10_c202307071000",
  M202307 = "20230701-20230726_global_vcmslcfg_v10_c202308111200",
  M202308 = "20230801-20230831_global_vcmslcfg_v10_c202309131200",
  M202309 = "20230901-20230930_global_vcmslcfg_v10_c202310101000",
  M202310 = "20231001-20231031_global_vcmslcfg_v10_c202311081400",
  M202311 = "20231101-20231130_global_vcmslcfg_v10_c202312080900",
  M202312 = "20231201-20231231_global_vcmslcfg_v10_c202401181200",
  M202401 = "20240101-20240131_global_vcmslcfg_v10_c202402062300",
  M202402 = "20240201-20240229_global_vcmslcfg_v10_c202403141100",
  M202403 = "20240301-20240331_global_vcmslcfg_v10_c202404121500",
  M202404 = "20240401-20240430_global_vcmslcfg_v10_c202405080700",
  M202405 = "20240501-20240528_global_vcmslcfg_v10_c202406122300",
  M202406 = "20240604-20240630_global_vcmslcfg_v10_c202407161300",
  M202407 = "20240701-20240731_global_vcmslcfg_v10_c202408091300"
)

merged_res <- tibble(
  region = integer(),
  year = integer(),
  month = integer(),
  zonal_mean = double(),
  zonal_max = double(),
  zonal_min = double(),
  zonal_sd = double(),
  .rows = 0
)

for (year in 2023:2024) {
  for (month in 1:12) {
    if (year == 2024 & month > 7) {
      break
    }
    
    tiff_file <- paste0("./global_vcmslcfg_v10/SVDNB_npp_",
                        tiff_files[[paste0("M", year, sprintf("%02d", month))]],
                        ".avg_rade9h.tif")
    
    raster_data <- rast(tiff_file)
    region_shp <- project(prefectures, crs(raster_data))
    
    cat(paste0("M", year, sprintf("%02d", month)), "mean\n")
    zonal_mean <- raster_data %>%
      terra::extract(region_shp, mean, na.rm = TRUE) %>%
      arrange(ID)
    
    cat(paste0("M", year, sprintf("%02d", month)), "min\n")
    zonal_min <- raster_data %>%
      terra::extract(region_shp, min, na.rm = TRUE) %>%
      arrange(ID)
    
    cat(paste0("M", year, sprintf("%02d", month)), "max\n")
    zonal_max <- raster_data %>%
      terra::extract(region_shp, max, na.rm = TRUE) %>%
      arrange(ID)
    
    cat(paste0("M", year, sprintf("%02d", month)), "sd\n")
    zonal_sd <- raster_data %>%
      terra::extract(region_shp, sd, na.rm = TRUE) %>%
      arrange(ID)
    
    merged_res <- tibble(
      region = region_shp$ID,
      year = year,
      month = month,
      zonal_mean = zonal_mean[[2]],
      zonal_max = zonal_max[[2]],
      zonal_min = zonal_min[[2]],
      zonal_sd = zonal_sd[[2]],
    ) %>%
      add_row(merged_res, .)
  }
}

write_rds(merged_res, "nightlight_zonal_stat.rds")
