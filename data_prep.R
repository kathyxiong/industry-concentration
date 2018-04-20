library(tidyverse)

# load data --------------------------------------------------------------------

files_2016 <- list.files("data_raw/2016.annual.by_industry")
files_2010 <- list.files("data_raw/2010.annual.by_industry")
files_2000 <- list.files("data_raw/2000.annual.by_industry") # no "unclassified" file
files_1990 <- list.files("data_raw/1990.annual.by_industry") # no "unclassified" file

x <- read_csv(paste0("data_raw/2016.annual.by_industry/",files_2016[1]))
spec(x)

# only read these cols
cols <- cols_only(
  area_fips = col_character(),
  own_code = col_integer(),
  industry_code = col_character(),
  agglvl_code = col_integer(),
  size_code = col_integer(),
  year = col_integer(),
  area_title = col_character(),
  own_title = col_character(),
  industry_title = col_character(),
  agglvl_title = col_character(),
  size_title = col_character(),
  annual_avg_emplvl = col_integer()
)

read_year <- function(year, files, data_out) {
  
  if(exists(data_out, envir = globalenv())){rm(data_out, envir = globalenv())}
  
  # read all files for that year
  for(i in 1:length(files)){
    
    x <- read_csv(paste0("data_raw/",year,".annual.by_industry/",files[i]), 
                  col_types = cols) %>% stop_for_problems()
    
    # assign results to global environment
    if(exists(data_out, envir = globalenv())){
      #print(paste0(data_out, " exists!"))
      y <- get(data_out, envir = globalenv())
      assign(data_out, bind_rows(y, x), envir = globalenv())
    } else {
      assign(data_out, x, envir = globalenv())
    }
    
  }
  
}

read_year("2016", files_2016, "data_2016")
read_year("2010", files_2010, "data_2010")
read_year("2000", files_2000, "data_2000")
read_year("1990", files_1990, "data_1990")

# census regions
# source: https://github.com/cphalpert/census-regions/blob/master/us%20census%20bureau%20regions%20and%20divisions.csv
regions <- read_csv("data_raw/census_regions.csv")

# process data -----------------------------------------------------------------

# combine
data_all <- bind_rows(data_1990, data_2000, data_2010, data_2016)

table(data_all$agglvl_code)
table(data_all$industry_code)
table(data_all$agglvl_code[data_all$industry_code == "10"]) 

# get national by NAICS sector (14) and state by NAICS sector (54)
data_all_1 <- data_all %>% 
  filter(agglvl_code %in% c(14, 54))

data_all_1 %>% 
  count(area_title, year) %>% 
  View()

data_all_1 %>% 
  count(area_title, industry_code) %>% 
  View()

# nationally, how many are employed by public sector?
data_all_1 %>% 
  filter(agglvl_code == 14) %>% 
  group_by(year, own_code) %>% 
  summarise(total_emplvl = sum(annual_avg_emplvl)) %>% 
  group_by(year) %>% 
  mutate(pct_by_ownership = total_emplvl/sum(total_emplvl)) %>% 
  View()
# about 15% is employed by public sector - let's include it

# calculate total employed in sector, all ownership types
data_state <- data_all_1 %>% 
  filter(agglvl_code == 54) %>% 
  group_by(year, area_fips, area_title, industry_code, industry_title) %>% 
  summarise(industry_emplvl = sum(annual_avg_emplvl)) %>% 
  group_by(year, area_fips) %>% 
  mutate(total_emplvl = sum(industry_emplvl),
         industry_emplvl_pct_total = industry_emplvl/total_emplvl)
View(data_state)

# how many people are in "unclassified" category?
data_state %>% 
  filter(industry_code == "99") %>% 
  View()
# in all cases it is <1% so I think we can exclude

# calculate national pct in industry
data_us <- data_all_1 %>% 
  filter(agglvl_code == 14) %>% 
  group_by(year, industry_code, industry_title) %>% 
  summarise(industry_emplvl = sum(annual_avg_emplvl)) %>% 
  group_by(year) %>% 
  mutate(total_emplvl = sum(industry_emplvl),
         industry_emplvl_pct_total = industry_emplvl/total_emplvl)
View(data_us)

# any patterns over time nationally?
data_us %>% 
  ungroup() %>% 
  ggplot(aes(x=year, y=industry_emplvl_pct_total, color = industry_code)) +
  geom_line()
# manufacturing decreased, health services increased, hospitality also increased
# seems like there's a story here!

# merge state + us
data_state_1 <- data_state %>% 
  left_join(data_us %>% select(year, 
                               industry_code, 
                               industry_emplvl_pct_total), 
            by = c("year", "industry_code")) %>% 
  rename(industry_emp_pct = industry_emplvl_pct_total.x,
         industry_emp_pct_us = industry_emplvl_pct_total.y) %>% 
  mutate(area_title = str_replace(area_title, " -- Statewide", "")) %>% 
  filter(industry_code != 99) # excludes unclassified

# use 2016 industry titles
ind_titles <- data_state_1 %>%
  ungroup() %>% 
  filter(year == 2016) %>% 
  select(industry_code, industry_title) %>% 
  filter(!duplicated(industry_code)) %>% 
  mutate(industry_title = str_replace(industry_title, "NAICS [1-9]{2}(-[1-9]{2})* ", ""))

data_state_2 <- data_state_1 %>% 
  select(-industry_title) %>% 
  left_join(ind_titles, by = "industry_code")

table(data_state_2$industry_title, useNA = "always")

# merge in region
data_state_3 <- data_state_2 %>% 
  left_join(regions %>% select(State, Region), by = c("area_title" = "State")) %>% 
  rename(region = Region) %>% 
  filter(!is.na(region)) # excludes Puerto Rico and Virgin Islands

# check region
table(data_state_3$region, useNA = "always")

# finalize
data_state_final <- data_state_3 %>% 
  mutate(id = paste(area_title, industry_code)) %>% 
  select(id, 
         year,
         area_title,
         industry_title,
         industry_emp_pct,
         industry_emp_pct_us)
