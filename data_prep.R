library(tidyverse)

# load data --------------------------------------------------------------------

# Read in all NAICS 2-digit files
files <- list.files("data_raw/2016.annual.by_industry")
match <- str_match(files, "2016.annual [1-9]{2}((-[1-9]{2} NAICS)|( NAICS))")

files_to_read <- files[!is.na(match[,1])]

cols <- cols(
  area_fips = col_character(),
  own_code = col_integer(),
  industry_code = col_character(),
  agglvl_code = col_integer(),
  size_code = col_integer(),
  year = col_integer(),
  qtr = col_character(),
  disclosure_code = col_character(),
  area_title = col_character(),
  own_title = col_character(),
  industry_title = col_character(),
  agglvl_title = col_character(),
  size_title = col_character(),
  annual_avg_estabs_count = col_integer(),
  annual_avg_emplvl = col_integer(),
  total_annual_wages = col_double(),
  taxable_annual_wages = col_skip(),
  annual_contributions = col_skip(),
  annual_avg_wkly_wage = col_skip(),
  avg_annual_pay = col_skip(),
  lq_disclosure_code = col_skip(),
  lq_annual_avg_estabs_count = col_double(),
  lq_annual_avg_emplvl = col_double(),
  lq_total_annual_wages = col_double(),
  lq_taxable_annual_wages = col_skip(),
  lq_annual_contributions = col_skip(),
  lq_annual_avg_wkly_wage = col_skip(),
  lq_avg_annual_pay = col_skip(),
  oty_disclosure_code = col_skip(),
  oty_annual_avg_estabs_count_chg = col_skip(),
  oty_annual_avg_estabs_count_pct_chg = col_skip(),
  oty_annual_avg_emplvl_chg = col_skip(),
  oty_annual_avg_emplvl_pct_chg = col_skip(),
  oty_total_annual_wages_chg = col_skip(),
  oty_total_annual_wages_pct_chg = col_skip(),
  oty_taxable_annual_wages_chg = col_skip(),
  oty_taxable_annual_wages_pct_chg = col_skip(),
  oty_annual_contributions_chg = col_skip(),
  oty_annual_contributions_pct_chg = col_skip(),
  oty_annual_avg_wkly_wage_chg = col_skip(),
  oty_annual_avg_wkly_wage_pct_chg = col_skip(),
  oty_avg_annual_pay_chg = col_skip(),
  oty_avg_annual_pay_pct_chg = col_skip()
)

if(exists("sector_data")){rm("sector_data")}

for(i in 1:length(files_to_read)){
  
  x <- read_csv(paste0("data_raw/2016.annual.by_industry/",files_to_read[i]), 
                col_types = cols) %>% stop_for_problems()
  
  if(exists("sector_data")){
    sector_data <- bind_rows(sector_data, x)
  } else {
    sector_data <- x
  }

}

# census regions
# source: https://github.com/cphalpert/census-regions/blob/master/us%20census%20bureau%20regions%20and%20divisions.csv
regions <- read_csv("data_raw/census_regions.csv")

# process data -----------------------------------------------------------------

# check agg codes
table(sector_data$agglvl_code)

# select state-wide
state_sectors <- sector_data %>% filter(agglvl_code == 54)

# check ownership codes
table(state_sectors$own_code)

# select state-wide, private-industry
state_private <- state_sectors %>% 
  filter(agglvl_code == 54 & own_code == 5)

# check industry
table(state_private$industry_code)
table(state_private$industry_title) %>% View()

# check geography
table(state_private$area_title) %>% View()

# clean up industry & geography labels
state_private <- state_private %>% 
  mutate(industry_title_2 = str_replace(industry_title, "NAICS (([1-9]{2})|([1-9]{2}-[1-9]{2})) ", ""),
         area_title_2 = str_replace(area_title, " -- Statewide", ""))

# merge in region labels
state_private <- left_join(state_private, regions, by = c("area_title_2" = "State"))

# exclude Puerto Rico and Virgin Islands
state_private <- state_private[!is.na(state_private$Region),]

# fill-in missing industries
state_private <- state_private %>% 
  complete(nesting(area_fips, area_title, area_title_2), nesting(industry_code, industry_title, industry_title_2))

# check missing industries
state_private[is.na(state_private$annual_avg_emplvl),] %>% View()

# exclude unclassified; for DC, fill in missings with zeros
state_private <- state_private %>% 
  filter(industry_code != "99") %>% 
  mutate(annual_avg_estabs_count = ifelse(is.na(annual_avg_estabs_count) & area_title == "District of Columbia",
                                          0, annual_avg_estabs_count),
         annual_avg_emplvl = ifelse(is.na(annual_avg_emplvl) & area_title == "District of Columbia",
                                          0, annual_avg_emplvl),
         total_annual_wages = ifelse(is.na(total_annual_wages) & area_title == "District of Columbia",
                                          0, total_annual_wages),
         Region = ifelse(is.na(Region) & area_title == "District of Columbia",
                         "South", Region))

# merge in all-industry, all-ownership total
state_total <- state_sectors %>% 
  group_by(area_fips) %>% 
  summarise(total_annual_avg_estabs_count = sum(annual_avg_estabs_count),
            total_annual_avg_emplvl = sum(annual_avg_emplvl),
            total_total_annual_wages = sum(total_annual_wages))
  
state_private <- left_join(state_private, state_total, by = "area_fips")

# calculate US industry share
us_pct <- sector_data %>% 
  filter(agglvl_code == 14) %>% 
  mutate(us_pct_annual_avg_estabs_count = annual_avg_estabs_count/sum(annual_avg_estabs_count),
         us_pct_annual_avg_emplvl = annual_avg_emplvl/sum(annual_avg_emplvl),
         us_pct_total_annual_wages = total_annual_wages/sum(total_annual_wages)) %>% 
  filter(own_code == 5) %>% 
  select(industry_code,
         us_pct_annual_avg_estabs_count,
         us_pct_annual_avg_emplvl,
         us_pct_total_annual_wages)
  
state_private <- left_join(state_private, us_pct, by = c("industry_code"))

# check location quotient calculation
state_private <- state_private %>% 
  mutate(local_pct_annual_avg_estabs_count = annual_avg_estabs_count / total_annual_avg_estabs_count,
         local_pct_annual_avg_emplvl = annual_avg_emplvl / total_annual_avg_emplvl,
         local_pct_total_annual_wages = total_annual_wages / total_total_annual_wages,
         lq_annual_avg_estabs_count_2 = local_pct_annual_avg_estabs_count / us_pct_annual_avg_estabs_count,
         lq_annual_avg_emplvl_2 = local_pct_annual_avg_emplvl / us_pct_annual_avg_emplvl,
         lq_total_annual_wages_2 = local_pct_total_annual_wages / us_pct_total_annual_wages)

# numbers match! yay!
state_private %>% 
  select(area_title_2,
         industry_title_2,
         starts_with("lq_")) %>% 
  View()

# finalize and export
state_final <- state_private %>% 
  select(area_fips,
         area_title_2,
         industry_code,
         industry_title_2,
         local_pct_annual_avg_emplvl,
         local_pct_total_annual_wages,
         us_pct_annual_avg_emplvl,
         us_pct_total_annual_wages,
         Region) %>% 
  rename(area_title = area_title_2,
         industry_title = industry_title_2,
         region = Region)

write_csv(state_final, "data_cleaned/viz_data.csv")



# test visualizations
table(state_final$industry_title)

#us_pct <- state_private$us_pct_annual_avg_emplvl[state_private$industry_title == "Agriculture, forestry, fishing and hunting"] %>% unique()
state_final %>% 
  ggplot() +
  #geom_bar(aes(x = local_pct_annual_avg_emplvl, y = industry_title)) +
  geom_errorbarh(aes(xmin = local_pct_annual_avg_emplvl, 
                     xmax = local_pct_annual_avg_emplvl, 
                     y = reorder(industry_title, us_pct_annual_avg_emplvl),
                     color = region)) +
  geom_point(aes(x = us_pct_annual_avg_emplvl, y = reorder(industry_title, us_pct_annual_avg_emplvl)))
