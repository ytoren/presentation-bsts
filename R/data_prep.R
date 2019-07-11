library(tidyverse)

trans_var <- function(x, a, b) {as.integer(a + b * (x - min(x)) / (max(x) - min(x)))}
date_offset <- 1264
missing_date <- as.Date('2019-05-20')

d <- read_csv('~/Downloads/search_rank_impact_analysis-query_1-85226666896e-2019-06-25-15-34-19.csv') 
d <- d %>% 
  ## Missing values in search
  union_all(tibble(
    date =  as.POSIXct(missing_date), 
    type =  'searches', 
    count = d[(d$date >= missing_date - 1) & (d$date <= missing_date + 1) & d$type == 'searches', ] %>% 
      pull(count) %>% 
      mean()
  ))

d2 <- d %>% 
  spread(type, count) %>% 
  mutate(
    source1 = import_search + import_product_internal, 
    source2 = import_extension,
    source3 = cbind(import_module_discount, import_module_product_group, import_unknown) %>% 
      apply(X = .,MARGIN = 1, FUN = sum, na.rm = TRUE),
    source4 = import_override, 
    source5 = import_url
  ) %>% 
  mutate(
    source1 = trans_var(source1, a = 100, b = 1000),
    source2 = trans_var(source2, a = 150, b = 800),
    source5 = trans_var(source5, a = 0, b = 1500),
    searches = as.integer(searches / 10)
  ) %>% 
  mutate(date = date - 1264 * 3600 * 24 ) %>% 
  select(date, starts_with('source'), searches)


d2 %>% write_csv(path = 'data/generated_data.csv')

