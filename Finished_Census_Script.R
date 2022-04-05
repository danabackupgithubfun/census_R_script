# Install API Key ----
# Get API key from Census
# Website: https://api.census.gov/data/key_signup.html
# Get key through email. Save to your notes file. Copy to insert here.

# Install and load tidycensus
install.packages("tidycensus")
library(tidycensus)

# Load API key into R Studio 
census_api_key("ff4a1f4e27a255d02727dd4c32ae57aa68719027", install = TRUE, overwrite = TRUE)

# Load API key permanently into R environment file by typing:
readRenviron("~/.Renviron")

# Test 
Sys.getenv("CENSUS_API_KEY")
# If all good, your lengthly API key should appear in Console

# Load tidyverse
library(tidyverse)


# Census Data ----
## 2000 & 2010 ----
# As of 25 March 2022, available for 2000 and 2010 censuses

# Get state population data for 2000 census
state_pop_00 <- get_decennial(
  geography = "state",
  variables = "P001001",
  year = 2000
)

# Get state population data for 2010 census
state_pop_10 <- get_decennial(
  geography = "state",
  variables = "P001001",
  year = 2010
)


## Census 2020 ----
# As of 25 March 2022, only redistricting data available
# Redistricting data have different variable names
# "pl" is "public law," for PL 94-171, the 1975 redistricting data law

# Load 2020 redistricting variables
census20rd_vars <- load_variables(2020, "pl")

# Inspect census20rd_vars to find total population field
View(census20rd_vars)
# Comparable population field is P1_001N

# Get state population data for 2020 census
state_pop_20 <- get_decennial(
  geography = "state",
  variables = "P1_001N",
  year = 2020
)


## 2020 vs 2010 ---- 

# In state_pop_10, rename value field as pop2010
state_pop_10 <- state_pop_10 %>% 
  rename(pop2010 = value)

# In state_pop_20, rename value field as pop2020
state_pop_20 <- state_pop_20 %>% 
  rename(pop2020 = value)

# Inspect each
glimpse(state_pop_10)
glimpse(state_pop_20)

# Join state_pop_10 and state_pop_20 as state_pop, by "GEOID"
state_pop <- inner_join(state_pop_10, state_pop_20, by="GEOID")

# In state_pop, add field to calculate percent change; sort by largest change
state_pop <- state_pop %>% 
  mutate(pct_chg = (pop2020-pop2010)/pop2010*100) %>% 
  arrange(desc(pct_chg))

# Graph percent change with theme_minimal()
state_pop %>% 
  ggplot(aes(pct_chg, reorder(NAME.x, pct_chg))) +
  geom_col() +
  theme_minimal()

# Add field "direction" for positive or negative as logic test (true/false)
state_pop <- state_pop %>% 
  mutate(direction = pct_chg>= 0)

# Graph percent change, with fill=direction 
state_pop %>% 
  ggplot(aes(pct_chg, reorder(NAME.x, pct_chg), fill=direction)) +
  geom_col() +
  theme_minimal() +
  scale_fill_manual(values = c("red", "blue"), guide = "none")

# Graph same, only for states with at least 5 million people in 2020
state_pop %>% 
  filter(pop2020 >= 5000000) %>% 
  ggplot(aes(pct_chg, reorder(NAME.x, pct_chg), fill=direction)) +
  geom_col() +
  theme_minimal() +
  scale_fill_manual(values = c("red", "blue"), guide = "none")


# Estimates ---- 
# Census Bureau Population Estimates Program (PEP)
# Annual estimates for birth, death, and migration rates

## Alachua PEP ----
# Import estimates for Alachua County as alachua_components
alachua_components <- get_estimates(
  geography = "county",
  product = "components",
  state = "FL",
  county = "Alachua",
)

# Interpret results:
## Births and deaths 
## Domestic migration: negative indicates more moved out than moved in
## International migration: positive indicates more moved in than moved out
## NATURALINC = births-deaths
## NETMIG = domestic migration + international migration
## R-values are residuals, or an indication of error margins

# Get Alachua County estimates for 2018
alachua_components_18 <- get_estimates(
  geography = "county",
  product = "components",
  state = "FL",
  county = "Alachua",
  year = "2018"
)

## Florida PEP ----
# Obtain population estimates for each county in Florida, county_fl_components
# Most recent year (default)
county_fl_components <- get_estimates(
  geography = "county",
  product = "components",
  state = "FL"
)

### Wide ----
# Note results are tidy. Change to 1 county per line:
# output = "wide"
county_fl_components <- get_estimates(
  geography = "county",
  product = "components",
  state = "FL",
  output = "wide"
)


# In Console, sort county_fl_components by largest domestic migration
county_fl_components %>% 
  arrange(desc(DOMESTICMIG))


# In Console, sort by lowest natural migration
county_fl_components %>% 
  arrange(NATURALINC)


## Migration Flows ----
# Use get_flows() to obtain migration flow for Miami-Dade County
miami_migration <- get_flows(
  geography = "county",
  state = "FL",
  county = "Miami-Dade",
  show_call = TRUE
)

# In Console, find largest source for moving in to Miami-Dade (hint: filter)
miami_migration %>% 
  filter(variable == "MOVEDIN") %>% 
  arrange(desc(estimate))


# In Console, find largest destination for moving away from Miami-Dade
miami_migration %>% 
  filter(variable == "MOVEDOUT") %>% 
  arrange(desc(estimate))

# Alachua County
alachua_migration <- get_flows(
  geography = "county",
  state = "FL",
  county = "Alachua",
  show_call = TRUE
)

alachua_migration %>% 
  filter(variable == "MOVEDIN") %>% 
  arrange(desc(estimate))

alachua_migration %>% 
  filter(variable == "MOVEDOUT") %>% 
  arrange(desc(estimate))

# ACS Data ----
# ACS = American Community Survey, the primary source for demographic data

## Metadata ----
# Get all variables in 2019 ACS 5-year data in dataset "metadata"
acs_metadata <- load_variables(2019, "acs5", cache = TRUE)

## Born in Cuba ----
# Get ACS data for people by state who were born in Cuba: born_in_cuba
# First, type "cuba" in metadata search bar in R Studio to find variable
# Most recent data drawn by default, 5-year, 2019
born_in_cuba <- get_acs(
  geography = "state",
  variables = "B05015_005"
) %>% 
  arrange(desc(estimate))

# Append previous query to sort by descending order
born_in_cuba %>% 
  arrange(desc(estimate))

# Visualize the top 10 states for born_in_cuba
born_in_cuba %>% 
  head(10) %>% 
  ggplot(aes(estimate, reorder(NAME, estimate)))+
  geom_col(fill = "darkorchid")


## Geographies----
# Search metadata file to find median age

# Obtain ACS data for United States, age_us
age_us <- get_acs(
  geography = "us",
  variables = "B01002_001"
)

# Obtain ACS data for each state, age_state
age_state <- get_acs(
  geography = "state",
  variables = "B01002_001"
)

# Obtain ACS data for each county, age_county
age_county <- get_acs(
  geography = "county",
  variables = "B01002_001"
)

# Obtain ACS data for each county within Florida, age_county_fl
age_county_fl <- get_acs(
  geography = "county",
  variables = "B01002_001",
  state = "FL"
)

# Obtain ACS data for each tract within Alachua County, Florida, age_tract_alachua
age_tract_alachua <- get_acs(
  geography = "tract",
  variables = "B01002_001",
  county = "Alachua",
  state = "FL"
)

# Obtain ACS data for metro/micro (cbsa), age_cbsa
age_cbsa <- get_acs(
  geography = "cbsa",
  variables = "B01002_001"
)

# Obtain ACS data for each place within Florida, age_place_fl
age_place_fl <- get_acs(
  geography = "place",
  variables = "B01002_001",
  state = "FL"
)

# Sort age_place_fl by highest age
age_place_fl %>% 
  arrange(desc(estimate))

# Is this CDP (census-designated place) populous? Need population data
# Search metadata file for total population variable 


## Two Variables----
# Create age_pop_place_fl 
# Input 2 variables: age (B01002_001) and population (B01003_001)
age_pop_place_fl <- get_acs(
  geography = "place",
  state = "Florida",
  variables = c("B01002_001", "B01003_001")
) 

# Each variable is on a different line, per tidy constraints
# To move to same line, use output = "wide"
age_pop_place_fl <- get_acs(
  geography = "place",
  state = "Florida",
  variables = c("B01002_001", "B01003_001"),
  output = "wide"
) 


# Now 1 line, but variables hard to read
# Rename upon import
age_pop_place_fl <- get_acs(
  geography = "place",
  state = "Florida",
  variables = c(age = "B01002_001", population = "B01003_001"),
  output = "wide"
)

# Note: ACS appends E for estimate and M for margin of error
# In Console, sort oldest Florida Places with at least 10,000 people
age_pop_place_fl_10k <- age_pop_place_fl %>% 
  filter(populationE >= 10000) %>% 
  arrange(desc(ageE))

# Graph previous script for 20 oldest places
age_pop_place_fl_10k %>% 
  head(20) %>%
  ggplot(aes(ageE, reorder(NAME, ageE)))+
  geom_col(fill="gold")


## String Remove ----
# Shorten place name by removing redundant ", Florida"
# Format: mutate(NAME = str_remove(NAME, ", Florida"))  
age_pop_place_fl %>% 
  filter(populationE >= 10000) %>% 
  arrange(desc(ageE)) %>% 
  mutate(NAME = str_remove(NAME, ", Florida")) %>% 
  head(20) %>% 
  ggplot(aes(ageE, reorder(NAME, ageE))) +
  geom_col()


## Parse State ----
# Search metadata table for median household income

# Get median household income for all US counties, county_hh_inc
county_hh_inc <- get_acs(
  geography = "county",
  variables = "B19013_001"
)

# In Console, sort county_hh_inc by highest
county_hh_inc %>% 
  arrange(desc(estimate))


# In Console, sort county_hh_inc by lowest
county_hh_inc %>% 
  arrange(estimate)


# What if want to evaluate without Puerto Rico?
# In new dataset, separate county from state
county_state_hh_inc <- county_hh_inc %>% 
  separate(
    NAME,
    into = c("county", "state"),
    sep = ", "
  )


### Slice Min ----
# In console, filter out Puerto Rico and sort county_state_hh_inc by lowest
county_state_hh_inc %>% 
  filter(state != "Puerto Rico") %>% 
  arrange(desc(estimate))

# Make permanent in new dataset, county_states


# Identify lowest county in each state in new dataset, county_lowest
# Replace summarize with slice(which.min(estimate))
county_lowest <- county_states %>% 
  group_by(state) %>%
  slice(which.min(estimate)) %>% 
  arrange(estimate)

# Inspect result
View(county_lowest)


## Largest Race ----
# Goal: Identify by county the largest racial group by percentage 
# Obtain race data for Florida and normalize for population

# Data are from table B03002
# First, re-define Census variable names into an R object, race_vars
race_vars <- c(
  White = "B03002_003",
  Black = "B03002_004",
  Asian = "B03002_006",
  Hispanic = "B03002_012"
)

# Obtain ACS race data by categories above, in race_vars, for fl_race
fl_race <- get_acs(
  geography = "county",
  state = "Florida",
  variables = race_vars,
) 

# To previous query, add row for summary_var, the population total for the county
# Field is B03002_001
fl_race <- get_acs(
  geography = "county",
  state = "FL",
  variables = race_vars,
  summary_var = "B03002_001"
) 

# Calculate percentage in new dataset, fl_race_pct
# Also, reduce variables: select(NAME, variable, percent)
fl_race_pct <- fl_race %>%
  mutate(percent = estimate / summary_est * 100) %>%
  select(NAME, variable, percent)

# In Console, filter for Hispanic and sort by percent 
fl_race_pct %>%
  filter (variable == "Hispanic") %>% 
  arrange(desc(percent))

# Find largest racial group in each county in new dataset, fl_race_plurality
# Sort by variable, then largest percent
fl_race_plurality <- fl_race_pct %>% 
  group_by(NAME) %>% 
  filter(percent == max(percent)) %>% 
  arrange(variable, desc(percent))


# Statistical Graphs ----

# Obtain percent of each US county with at least a 4-year degree 
# Table: Census Data Profiles (DP)
county_college <- get_acs(
  geography = "county",
  variables = "DP02_0068P"
)

college_state <- get_acs(
  geography = "state",
  variables = "DP02_0068P"
)

# In Console, find counties >= 50% college degree, sorted by highest
greater_county_college_50 <- county_college %>%
  filter(estimate >= 50)


# In Console, find counties < 10% college degree, sorted by lowest
lessthan_county_college_10 <- county_college %>%
  filter(estimate <= 10)


## Histogram ----
# Create histogram, showing distribution of percentages
# Defaults to 30 bins
county_college%>% 
  ggplot(aes(estimate)) +
  geom_histogram()

# Result:
## Percentage ranged from 0 to 78
## x-axis: all percents from 0 to 78 divided into 30 equal-width "bins"
## y-axis = number of counties in each bin

# Improve visibility: inside geom_histogram, add fill="gray", color = "black"
county_college%>% 
  ggplot(aes(estimate)) +
  geom_histogram(fill="gray", color = "black")


# Inside geom_histogram, change to 20 bins
county_college%>% 
  ggplot(aes(estimate)) +
  geom_histogram(fill="pink", color = "darkturquoise", bins = "15")


## Boxplot ----
# Use geom_boxplot() to search for outliers
county_college%>% 
  ggplot(aes(estimate)) +
  geom_boxplot()

# Flip orientation 90 degrees with coord_flip()
county_college%>% 
  ggplot(aes(estimate)) +
  geom_boxplot() +
  coord_flip()

# Analysis: Many outliers
# Reason: Interquartile range (IQR), or middle 50%, is compressed


## MOE Bars ----
### Get Data ----
# Data: Median Household Income

# Obtain median household income for FL counties, B19013
fl_income <- get_acs(
  state = "Florida",
  geography = "county",
  variables = "B19013_001"
)

### Plot Data ---- 
# Graph fl_income for 15 highest, as geom_point()
fl_income %>% 
  head(15) %>% 
  ggplot(aes(estimate, y = reorder(NAME, estimate))) + 
  geom_point()

# Improve dot visibility: geom_point(size = 3, color = "darkgreen")
fl_income %>% 
  head(15) %>% 
  ggplot(aes(estimate, y = reorder(NAME, estimate))) + 
  geom_point(size = 3, color = "magenta")

# Change to theme_minimal()
fl_income %>% 
  head(15) %>% 
  ggplot(aes(estimate, y = reorder(NAME, estimate))) + 
  geom_point(size = 3, color = "magenta") +
  theme_minimal()

# Change X axis so it start at zero via expand_limits(x = 0)
fl_income %>% 
  head(15) %>% 
  ggplot(aes(estimate, y = reorder(NAME, estimate))) + 
  geom_point(size = 3, color = "darkblue") +
  theme_minimal() +
  expand_limits(x = 0)

# Labels: add title, subtitle, and caption, remove labels
fl_income %>% 
  head(15) %>% 
  ggplot(aes(estimate, y = reorder(NAME, estimate))) + 
  geom_point(size = 3, color = "darkgreen",fill = "pink") +
  theme_minimal() +
  expand_limits(x = 0) + 
  labs(title = "Median Household Income",
       subtitle = "Twenty highest Florida counties",
       caption = "Census ACS estimates, 2015-2019",
       x = NULL, 
       y = NULL) 

# Change x-axis label: scale_x_continuous(labels = scales::dollar)
fl_income %>% 
  head(15) %>% 
  ggplot(aes(estimate, y = reorder(NAME, estimate))) + 
  geom_point(size = 3, color = "darkgreen",fill = "pink") +
  theme_minimal() +
  expand_limits(x = 0) + 
  labs(title = "Median Household Income",
       subtitle = "Twenty highest Florida counties",
       caption = "Census ACS estimates, 2015-2019",
       x = NULL, 
       y = NULL) +
  scale_x_continuous(labels = scales::dollar)

# Y-axis labels can be improved by removing " County, Florida"
# Mutate NAME in fl_income dataset with str_remove()
# Test in Console
fl_income %>% 
  mutate(NAME = str_remove(NAME, " County, Florida"))

# Make permanent
fl_income <- fl_income %>% 
  mutate(NAME = str_remove(NAME, " County, Florida"))

# Re-run last chart with shorter y-axis names
fl_income %>% 
  head(15) %>% 
  ggplot(aes(estimate, y = reorder(NAME, estimate))) + 
  geom_point(size = 3, color = "deeppink3",fill = "pink") +
  theme_minimal() +
  expand_limits(x = 0) + 
  labs(title = "Median Household Income",
       subtitle = "Twenty highest Florida counties",
       caption = "Census ACS estimates, 2015-2019",
       x = NULL, 
       y = NULL) +
  scale_x_continuous(labels = scales::dollar)


### Error Bars ----
# Look at fl_income and note variance in error figure
# Reflects sample size: less populous counties have more error

# In ggpolot, before geom_point, insert error bar
# Add: geom_errorbar(aes(xmin = estimate-moe, xmax = estimate +moe))
# This code places a bar around both sides of the estimate +/- moe
fl_income %>% 
  head(15) %>% 
  ggplot(aes(estimate, y = reorder(NAME, estimate))) + 
  geom_point(size = 3, color = "deeppink3",fill = "pink") +
  geom_errorbar(aes(xmin = estimate-moe, xmax = estimate +moe)) +
  theme_minimal() +
  expand_limits(x = 0) + 
  labs(title = "Median Household Income",
       subtitle = "Twenty highest Florida counties",
       caption = "Census ACS estimates, 2015-2019",
       x = NULL, 
       y = NULL) +
  scale_x_continuous(labels = scales::dollar)
  


# Interpretation:
# Census ACS calculates moe at 90% confidence level
# If survey repeated, 90 times of 100 true figure would be within vertical bars


## MOE: Shade ----
### Get Data ----
# Need data suitable for a line chart
# Solution: median home value for Alachua County over time
# 1-year ACS data supported by tidycensus started in 2005

# Establish years as 2005 to 2019 in R values
years <- 2005:2019
names(years) <- years

# Obtain 1-year ACS data for variable B25077_001
alachua_homes <- map_dfr(years, ~{
  get_acs(
    geography = "county",
    variables = "B25077_001",
    state = "Florida",
    county = "Alachua",
    year = .x,
    survey = "acs1"
  )
}, .id = "year")



### Plot Data ----
# Plot alachua_homes as both geom_line() and geom_point()
# x=year, y=estimate
alachua_homes %>% 
  ggplot(aes(year, estimate))+
  geom_line()+
  geom_point()

# Line not shown because just 1 data point per year
# Show a line by changing aes to: (aes(year, estimate, group=1))
alachua_homes %>% 
  ggplot(aes(year, estimate, group=1))+
  geom_line()+
  geom_point()

# Note y-axis does not start at zero. Leave it wrong, to emphasize error shading.
# Add theme_minimal()
alachua_homes %>% 
  ggplot(aes(year, estimate, group=1))+
  geom_line()+
  geom_point() +
  theme_minimal()

# Remove axis labels and change y-axis scale to dollars
# Remove axis labels and change y-axis scale to dollars
alachua_homes %>% 
  ggplot(aes(year, estimate, group=1))+
  geom_line()+
  geom_point() +
  theme_minimal() +
  labs(x=NULL, y=NULL) +
  scale_y_continuous(labels = scales::dollar)

# Add title and caption
alachua_homes %>% 
  ggplot(aes(year, estimate, group=1))+
  geom_line()+
  geom_point() +
  theme_minimal() +
  labs(x=NULL, y=NULL,
       title = "Median Home Value in Alachua County, Florida",
       caption = "Source: ACS 1-year data") +
  scale_y_continuous(labels = scales::dollar)

# Add error bar as a shaded ribbon after ggplot
# geom_ribbon(aes(ymax=estmate+moe, ymin=estimate-moe))
alachua_homes %>% 
  ggplot(aes(year, estimate, group=1)) +
  geom_ribbon(aes(ymax=estimate+moe, ymin=estimate-moe)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x=NULL, y=NULL,
       title = "Median Home Value in Alachua County, Florida",
       caption = "Source: ACS 1-year data") +
  scale_y_continuous(labels = scales::dollar)

# Improve ribbon visibility by changing transparency to 40%
# After aes in ribbon, add: alpha = 0.4  
alachua_homes %>% 
  ggplot(aes(year, estimate, group=1)) +
  geom_ribbon(aes(ymax=estimate+moe, ymin=estimate-moe),
              alpha = 0.4) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x=NULL, y=NULL,
       title = "Median Home Value in Alachua County, Florida",
       caption = "Source: ACS 1-year data") +
  scale_y_continuous(labels = scales::dollar)

# Love colors? Try navy for all 3: line, point, and ribbon
# Hint: line and point are color= while ribbon is fill=
alachua_homes %>% 
  ggplot(aes(year, estimate, group=1)) +
  geom_ribbon(aes(ymax=estimate+moe, ymin=estimate-moe),
              alpha = 0.4,
              fill = "deeppink2") +
  geom_line(color = "deeppink4") +
  geom_point(color = "deeppink3") +
  theme_minimal() +
  labs(x=NULL, y=NULL,
       title = "Median Home Value in Alachua County, Florida",
       caption = "Source: ACS 1-year data") +
  scale_y_continuous(labels = scales::dollar)

# Interpretation: ribbon = 90% CI
# 90 times out of 100, true home value is in the shaded area


## Beeswarm ----
# South Florida has 3 large counties: Broward, Miami-Dade, and Palm Beach
# Combine for this analysis
# What is the distribution of median household income by major race/ethnicity?
# Calculate by which racial group is the largest in each census tract
# Median household income is table B19013
# Race with Hispanic is table B03002
## For Hispanic, field _012 combines all (white + Black + Asian + etc.)

### Get Data----
# Obtain data through get_acs()
south_florida <- get_acs(
  geography = "tract",
  state = "Florida",
  county = c("Broward", "Miami-Dade", "Palm Beach"),
  variables = c(White = "B03002_003",
                Black = "B03002_004",
                Asian = "B03002_006",
                Hispanic = "B03002_012"),
  summary_var = "B19013_001"
)

# Reconfigure south_florida dataset to group by GEOID
# Also, find, largest group in each tract
# Then ungroup, and filter out estimates that are zero
south_florida <- south_florida %>% 
  group_by(GEOID) %>% 
  filter(estimate == max(estimate, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(estimate !=0)

### Plot Data----
# Install "ggbeeswarm," which extends ggplot, and load into library
install.packages("ggbeeswarm")
library(ggbeeswarm)

# Create chart
south_florida %>% 
  ggplot(aes(variable, summary_est, color = summary_est)) +
  geom_quasirandom(alpha = 0.5) +
  coord_flip() +
  theme_minimal() +
  scale_color_viridis_c(guide = FALSE) +
  scale_y_continuous(labels = scales::dollar) +
  labs(x=NULL, y=NULL,
       title = "South Florida Household Income Distribution",
       subtitle = "By largest racial/ethnic group in each census tract",
       caption = "Source: ACS for Broward, Miami-Dade, and Palm Beach counties")

