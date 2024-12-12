library(tidyverse)
library(patchwork)
library(showtext)
library(sf)
library(plm)
library(splm)
library(spdep)
library(GWmodel)
library(GWPR.light)

ExportSptialPanel <- function (model, filename){
  model_summary <- summary(model)
  CoefTable <- model_summary[["CoefTable"]] %>% data.frame()
  ErrCompTable <- model_summary[["ErrCompTable"]] %>% data.frame()
  ARCoefTable <- model_summary[["ARCoefTable"]] %>% data.frame()
  result <- rbind(CoefTable, ErrCompTable, ARCoefTable)
  colnames(result) <- c("Estimate", "Std.Error", "t_value", "p_value")
  result[["Variable"]] <- rownames(result)
  result %>% dplyr::select(Variable, Estimate, Std.Error, t_value, p_value) %>%
    mutate(Estimate = Estimate %>% round(5) %>% format(nsmall = 5),
           Std.Error = Std.Error %>% round(5) %>% format(nsmall = 5),
           t_value = t_value %>% round(5) %>% format(nsmall = 5),
           p_value = p_value %>% round(5) %>% format(nsmall = 5)) -> result
  write_csv(result, filename)
  return(result)
}

baidu_migration <- read_rds("baidu_migration_topological_features.rds")
nightlight <- read_rds("nightlight_zonal_stat.rds")

prefectures <- st_read(dsn = "Boundary2019.gdb", layer = "地级行政区划及省直辖县")
provinces <- st_read(dsn = "Boundary2019.gdb", layer = "省级行政区划")
boundary <- st_read(dsn = "Boundary2019.gdb", layer = "国界九段线")

panel.data <- prefectures %>%
  left_join(baidu_migration, by = c(ID = "region")) %>%
  left_join(nightlight, by = c(ID = "region", year = "year", month = "month")) %>%
  mutate(in_strength = ifelse(ID %in% c(810000, 820000), NA, in_strength),
         total_strength = ifelse(ID %in% c(810000, 820000), NA, total_strength),
         year_month = zoo::as.yearmon(year + (month - 1) / 12),
         year_month_str = format(year_month, "%Y-%m")) %>%
  filter(!is.na(total_strength),
         !is.na(zonal_mean))

#====================================================
# Plot
#====================================================

showtext_auto()
font_add("Cons", "constan.ttf", "constanb.ttf", "constani.ttf", "constanz.ttf")

ggplot(panel.data) +
  geom_histogram(aes(x = total_strength)) +
  labs(x = "Strength", y = "Count") + 
  theme_bw() +
  theme(text = element_text(family = "Cons", size = 36)) -> p1.1

ggplot(panel.data) +
  geom_histogram(aes(x = log2(total_strength))) +
  labs(x = "Log2(Strength)", y = "Count") + 
  theme_bw() +
  theme(text = element_text(family = "Cons", size = 36)) -> p1.2

ggplot(panel.data) +
  geom_histogram(aes(x = pg_index)) +
  labs(x = "Page-rank", y = "Count") + 
  theme_bw() +
  theme(text = element_text(family = "Cons", size = 36)) -> p1.3

ggplot(panel.data) +
  geom_histogram(aes(x = log2(pg_index))) +
  labs(x = "Log2(Page-rank)", y = "Count") + 
  theme_bw() +
  theme(text = element_text(family = "Cons", size = 36)) -> p1.4

ggplot(panel.data) +
  geom_histogram(aes(x = zonal_mean)) +
  labs(x = "Nightlight", y = "Count") + 
  theme_bw() +
  theme(text = element_text(family = "Cons", size = 36)) -> p1.5

ggplot(panel.data) +
  geom_histogram(aes(x = log2(zonal_mean))) +
  labs(x = "Log2(Nightlight)", y = "Count") + 
  theme_bw() +
  theme(text = element_text(family = "Cons", size = 36)) -> p1.6

(p1.1 + p1.2) / (p1.3 + p1.4) / (p1.5 + p1.6) -> p1
p1

ggsave("p1.png", p1, width = 8, height = 6)

ggplot(panel.data) +
  geom_histogram(aes(x = log2(total_strength))) +
  facet_wrap(. ~ year_month_str, ncol = 4)

ggplot(panel.data) +
  geom_histogram(aes(x = log2(pg_index))) +
  facet_wrap(. ~ year_month_str, ncol = 4)

ggplot(panel.data) +
  geom_histogram(aes(x = log2(zonal_mean))) +
  facet_wrap(. ~ year_month_str, ncol = 4)

ggplot(panel.data) +
  geom_sf(aes(fill = total_strength), color = NA) +
  geom_sf(data = provinces, linewidth = 0.1, linetype = 2, fill = NA) +
  geom_sf(data = boundary) +
  facet_wrap(. ~ year_month_str, ncol = 5) +
  labs(fill = "Strength") +
  coord_sf(crs = st_crs(2381)) + 
  scale_fill_fermenter(palette = "OrRd", direction = 1) +
  theme_bw() +
  theme(text = element_text(family = "Cons", size = 36),
        axis.text.x = element_text(angle = 90),
        legend.position = "inside",
        legend.position.inside = c(0.9, 0.1)) -> p2.1.notlog
ggsave("p2.1.notlog.png", p2.1.notlog, width = 10, height = 10)

ggplot(panel.data) +
  geom_sf(aes(fill = log2(total_strength)), color = NA) +
  geom_sf(data = provinces, linewidth = 0.1, linetype = 2, fill = NA) +
  geom_sf(data = boundary) +
  facet_wrap(. ~ year_month_str, ncol = 5) +
  labs(fill = "Log2(Strength)") +
  coord_sf(crs = st_crs(2381)) + 
  scale_fill_fermenter(palette = "OrRd", direction = 1) +
  theme_bw() +
  theme(text = element_text(family = "Cons", size = 36),
        axis.text.x = element_text(angle = 90),
        legend.position = "inside",
        legend.position.inside = c(0.9, 0.1)) -> p2.1
ggsave("p2.1.png", p2.1, width = 10, height = 10)

ggplot(panel.data) +
  geom_sf(aes(fill = pg_index), color = NA) +
  geom_sf(data = provinces, linewidth = 0.1, linetype = 2, fill = NA) +
  geom_sf(data = boundary) +
  facet_wrap(. ~ year_month_str, ncol = 5) +
  labs(fill = "Page-rank") +
  coord_sf(crs = st_crs(2381)) + 
  scale_fill_fermenter(palette = "OrRd", direction = 1) +
  theme_bw() +
  theme(text = element_text(family = "Cons", size = 36),
        axis.text.x = element_text(angle = 90),
        legend.position = "inside",
        legend.position.inside = c(0.9, 0.1)) -> p2.2.notlog
ggsave("p2.2.notlog.png", p2.2.notlog, width = 10, height = 10)

ggplot(panel.data) +
  geom_sf(aes(fill = log2(pg_index)), color = NA) +
  geom_sf(data = provinces, linewidth = 0.1, linetype = 2, fill = NA) +
  geom_sf(data = boundary) +
  facet_wrap(. ~ year_month_str, ncol = 5) +
  labs(fill = "Log2(Page-rank)") +
  coord_sf(crs = st_crs(2381)) + 
  scale_fill_fermenter(palette = "OrRd", direction = 1) +
  theme_bw() +
  theme(text = element_text(family = "Cons", size = 36),
        axis.text.x = element_text(angle = 90),
        legend.position = "inside",
        legend.position.inside = c(0.9, 0.1)) -> p2.2
ggsave("p2.2.png", p2.2, width = 10, height = 10)

ggplot(panel.data) +
  geom_sf(aes(fill = zonal_mean), color = NA) +
  #geom_sf(data = provinces, linewidth = 0.1, linetype = 2, fill = NA) +
  geom_sf(data = boundary) +
  facet_wrap(. ~ year_month_str, ncol = 5) +
  labs(fill = "Nightlight") +
  coord_sf(crs = st_crs(2381)) + 
  scale_fill_gradient(low = "black", high = "white") +
  theme_bw() +
  theme(text = element_text(family = "Cons", size = 36),
        axis.text.x = element_text(angle = 90),
        legend.position = "inside",
        legend.position.inside = c(0.9, 0.1)) -> p2.3.notlog
ggsave("p2.3.notlog.png", p2.3.notlog, width = 10, height = 10)

ggplot(panel.data) +
  geom_sf(aes(fill = log2(zonal_mean)), color = NA) +
  #geom_sf(data = provinces, linewidth = 0.1, linetype = 2, fill = NA) +
  geom_sf(data = boundary) +
  facet_wrap(. ~ year_month_str, ncol = 5) +
  labs(fill = "Log2(Nightlight)") +
  coord_sf(crs = st_crs(2381)) + 
  scale_fill_gradient(low = "black", high = "white") +
  theme_bw() +
  theme(text = element_text(family = "Cons", size = 36),
        axis.text.x = element_text(angle = 90),
        legend.position = "inside",
        legend.position.inside = c(0.9, 0.1)) -> p2.3
ggsave("p2.3.png", p2.3, width = 10, height = 10)

#====================================================
# Corr
#====================================================

year_months <- unique(panel.data$year_month)

cor.total_strength <- lapply(year_months, function(ym) {
  sub_data <- filter(panel.data, year_month == ym)
  cor.test(~ log2(total_strength) + log2(zonal_mean), data = sub_data)
})

lapply(cor.total_strength, function(cor.result) {
  tibble(
    cor = cor.result[["estimate"]][["cor"]],
    lower = cor.result[["conf.int"]][1],
    upper = cor.result[["conf.int"]][2]
  )
}) %>%
  reduce(add_row) %>%
  mutate(year_month = year_months,
         year_month_str = format(year_month, "%Y-%m"),
         var = "Strength") ->
  cor.table.total_strength

cor.pg_index <- lapply(year_months, function(ym) {
  sub_data <- filter(panel.data, year_month == ym)
  cor.test(~ log2(pg_index) + log2(zonal_mean), data = sub_data)
})

lapply(cor.pg_index, function(cor.result) {
  tibble(
    cor = cor.result[["estimate"]][["cor"]],
    lower = cor.result[["conf.int"]][1],
    upper = cor.result[["conf.int"]][2]
  )
}) %>%
  reduce(add_row) %>%
  mutate(year_month = year_months,
         year_month_str = format(year_month, "%Y-%m"),
         var = "Page-Rank") ->
  cor.table.pg_index

add_row(cor.table.total_strength,
        cor.table.pg_index) ->
  cor.table

ggplot(cor.table, aes(x = year_month, y = cor, color = var)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, alpha = 0.5) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(limits = c(2022.9, 2024.6),
                     breaks = c(2023, 2023.5, 2024, 2024.5),
                     labels = c("2023-01", "2023-07", "2024-01", "2024-07")) + 
  labs(x = "Year-month", y = "Correlation coefficient", color = "Variable") +
  theme_bw() +
  theme(text = element_text(family = "Cons", size = 36),
        legend.position = "bottom") -> p3

ggsave("p3.png", p3, width = 8, height = 6)

ggplot(panel.data) +
  geom_point(aes(x = log2(total_strength), y = log2(zonal_mean)), alpha = 0.1) +
  geom_smooth(aes(x = log2(total_strength), y = log2(zonal_mean)), method = "lm", color = "red") +
  geom_smooth(aes(x = log2(total_strength), y = log2(zonal_mean)), color = "blue") +
  facet_wrap(. ~ year_month_str, ncol = 4)

ggplot(panel.data) +
  geom_point(aes(x = log2(pg_index), y = log2(zonal_mean)), alpha = 0.1) +
  geom_smooth(aes(x = log2(pg_index), y = log2(zonal_mean)), method = "lm", color = "red") +
  geom_smooth(aes(x = log2(pg_index), y = log2(zonal_mean)), color = "blue") +
  facet_wrap(. ~ year_month_str, ncol = 4)

#====================================================
# Panel
#====================================================

fm.no_insec <- log2(zonal_mean) ~ 
  log2(total_strength) + 
  log2(pg_index)

fm.total <- log2(zonal_mean) ~ 
  log2(total_strength) + 
  log2(pg_index) + 
  log2(total_strength) * log2(pg_index)

model.no_insec.plm <- plm(
  formula = fm.no_insec, 
  data = panel.data, 
  effects = "twoways",
  model = "pooling", 
  index = c("ID", "year_month")
)
summary(model.no_insec.plm)

model.total.plm <- plm(
  formula = fm.total, 
  data = panel.data, 
  effects = "twoways",
  model = "pooling", 
  index = c("ID", "year_month")
)
summary(model.total.plm)

#====================================================
# Spatial Panel
#====================================================

pref.shp <- panel.data %>%
  dplyr::filter(year == 2023, month == 1) %>%
  as("Spatial")
pref.reord <- pref.shp[order(pref.shp$ID),]
pref.listw <-nb2listw(graph2nb(soi.graph(tri2nb(coordinates(pref.reord)), coordinates(pref.reord))))

slmtest(fm.no_insec, data = panel.data, listw = pref.listw, index = c("ID", "year_month"), 
        model = "pooling", test = "lme")
slmtest(fm.no_insec, data = panel.data, listw = pref.listw, index = c("ID", "year_month"), 
        model = "pooling", test = "lml")
slmtest(fm.no_insec, data = panel.data, listw = pref.listw, index = c("ID", "year_month"), 
        model = "pooling", test = "rlme")
slmtest(fm.no_insec, data = panel.data, listw = pref.listw, index = c("ID", "year_month"), 
        model = "pooling", test = "rlml")

model.no_insec.spml <- spml(
  formula = fm.no_insec, 
  data = panel.data, 
  listw = pref.listw, 
  effects = "twoways",
  model = "pooling", 
  lag = T, 
  spatial.error = "kkp", 
  index = c("ID", "year_month")
)
summary(model.no_insec.spml)
ExportSptialPanel(model.no_insec.spml, "model.no_insec.spml.csv") -> result

slmtest(fm.total, data = panel.data, listw = pref.listw, index = c("ID", "year_month"), 
        model = "pooling", test = "lme")
slmtest(fm.total, data = panel.data, listw = pref.listw, index = c("ID", "year_month"), 
        model = "pooling", test = "lml")
slmtest(fm.total, data = panel.data, listw = pref.listw, index = c("ID", "year_month"), 
        model = "pooling", test = "rlme")
slmtest(fm.total, data = panel.data, listw = pref.listw, index = c("ID", "year_month"), 
        model = "pooling", test = "rlml")

model.total.spml <- spml(
  formula = fm.total, 
  data = panel.data, 
  listw = pref.listw, 
  effects = "twoways",
  model = "pooling", 
  lag = T, 
  spatial.error = "kkp", 
  index = c("ID", "year_month")
)
summary(model.total.spml)
ExportSptialPanel(model.total.spml, "model.total.spml.err.csv") -> result

#====================================================
# GWPR
#====================================================

pref.SDF <- panel.data %>%
  filter(year == 2023, month == 1) %>%
  select(ID) %>%
  as_Spatial()

GWPR.data <- panel.data %>%
  st_drop_geometry() %>%
  mutate(log2_zonal_mean = log2(zonal_mean),
         log2_total_strength = log2(total_strength),
         log2_pg_index = log2(pg_index),
         log2_intersect = log2_total_strength * log2_pg_index)

fm.GWPR <- log2_zonal_mean ~ log2_total_strength + log2_pg_index + log2_intersect

# 21.22243
bw <- bw.GWPR(formula = fm.GWPR, 
              data = GWPR.data, 
              index = c("ID", "year_month"), 
              SDF = pref.SDF,
              adaptive = F, 
              bigdata = F, 
              effect = "twoways",
              model = "pooling", 
              approach = "AIC", 
              kernel = "gaussian", 
              doParallel = T)

GWPR.moran.test(model.total.plm, 
                SDF = pref.SDF, 
                bw = bw, 
                kernel = "gaussian",
                adaptive = T)

GWPR.model <- GWPR(bw = bw, 
                   formula = fm.GWPR, 
                   data = GWPR.data, 
                   index = c("ID", "year_month"), 
                   SDF = pref.SDF, 
                   adaptive = F, 
                   effect = "twoways", 
                   model = "pooling",
                   kernel = "gaussian")

GWPR.sf <- st_as_sf(GWPR.model$SDF)

ggplot(GWPR.sf) +
  geom_sf(aes(fill = Local_R2), color = NA) +
  geom_sf(data = provinces, linewidth = 0.1, linetype = 2, fill = NA) +
  geom_sf(data = boundary) +
  labs(fill = "Local R-Square") +
  coord_sf(crs = st_crs(2381)) +
  scale_fill_gradient(low = "#fef0d9", high = "#d7301f") +
  theme_bw() +
  theme(text = element_text(family = "Cons", size = 36),
        legend.position = "inside",
        legend.position.inside = c(0.2, 0.2),
        legend.background = element_blank()) -> p4.1

ggplot(GWPR.sf) +
  geom_sf(aes(fill = log2_total_strength), color = NA) +
  geom_sf(data = provinces, linewidth = 0.1, linetype = 2, fill = NA) +
  geom_sf(data = boundary) +
  labs(fill = "Log2(Strength)") +
  coord_sf(crs = st_crs(2381)) +
  scale_fill_gradient(low = "#fef0d9", high = "#d7301f") +
  theme_bw() +
  theme(text = element_text(family = "Cons", size = 36),
        legend.position = "inside",
        legend.position.inside = c(0.2, 0.2),
        legend.background = element_blank()) -> p4.2

ggplot(GWPR.sf) +
  geom_sf(aes(fill = log2_pg_index), color = NA) +
  geom_sf(data = provinces, linewidth = 0.1, linetype = 2, fill = NA) +
  geom_sf(data = boundary) +
  labs(fill = "Log2(Page-rank)") +
  coord_sf(crs = st_crs(2381)) +
  scale_fill_gradient(low = "#fef0d9", high = "#d7301f") +
  theme_bw() +
  theme(text = element_text(family = "Cons", size = 36),
        legend.position = "inside",
        legend.position.inside = c(0.2, 0.2),
        legend.background = element_blank()) -> p4.3

ggplot(GWPR.sf) +
  geom_sf(aes(fill = log2_intersect), color = NA) +
  geom_sf(data = provinces, linewidth = 0.1, linetype = 2, fill = NA) +
  geom_sf(data = boundary) +
  labs(fill = "Log2(Interaction)") +
  coord_sf(crs = st_crs(2381)) +
  scale_fill_gradient(low = "#fef0d9", high = "#d7301f") +
  theme_bw() +
  theme(text = element_text(family = "Cons", size = 36),
        legend.position = "inside",
        legend.position.inside = c(0.2, 0.2),
        legend.background = element_blank()) -> p4.4

(p4.1 + p4.2) / (p4.3 + p4.4) -> p4

ggsave("p4.png", p4, width = 10, height = 11)
