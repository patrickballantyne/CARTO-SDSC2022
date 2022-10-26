## Libraries
library(tidyverse) # Data wrangling
library(sf) # (Spatial) data wrangling and GIS
library(tidycensus) # Census API
library(tmap) # Interactive mapping 

## Options and Settings
tmap_mode("view")
census_api_key("e0f3cdfa0e1e13ab3a4d9dc32c67984d0be5523b")

# 1. Workshop Data --------------------------------------------------------

## SafeGraph places - Atlanta MSA 
places <- st_read("Data/places.gpkg", quiet = TRUE)

### What does the data look like?
glimpse(places)

### Map 
places %>%
  filter(top_category == "Grocery Stores") %>%
  tm_shape() +
  tm_dots(col = "brands", title = "Brand", size = 0.05) +
  tm_layout(legend.outside = TRUE)

################################################################################


## US Geodemographic Classification (Spielman and Singleton) - Atlanta MSA 
geodemo <- st_read("Data/geodemographic.gpkg", quiet = TRUE)

## What does the data look like?
glimpse(geodemo)

### Map 
tm_shape(geodemo) +
  tm_fill(col = "TractGroup", alpha = 0.5, palette = "Pastel1") +
  tm_borders(col = "black", lwd = 0.25) +
  tm_layout(legend.outside = TRUE)

################################################################################

## Population Data - Tidycensus
pop <- get_decennial(geography = "Tract", variables = "P001001",
                     state = "GA", year = 2010)

### What does the data look like?
glimpse(pop)

### Merge w/ geodemographic 
gd_pop <- merge(geodemo, pop, by = "GEOID")
glimpse(gd_pop)

### Map 
tm_shape(gd_pop) +
  tm_fill(col = "value", title = "TotalPopulation", alpha = 0.5, style = "jenks") +
  tm_borders(col = "black", lwd = 0.25) +
  tm_layout(legend.outside = TRUE)

################################################################################

# 2. Store Catchments -----------------------------------------------------

### Focusing on two different store brands in the U.S.
wholefoods <- places %>%
  filter(brands == "Whole Foods Market")
ollies <- places %>%
  filter(brands == "Ollie's Bargain Outlet")

### Map
q1 <- tm_shape(wholefoods) +
  tm_dots(col = "blue", size = 0.1, alpha = 0.5) +
  tm_text("brands", size = 1, just = "top") +
  tm_shape(ollies) +
  tm_dots(col = "orange", size = 0.1, alpha = 0.5) +
  tm_text("brands", size = 1, just = "top")
q1

### Map (w/ Geodemographic underneath)
tm_shape(gd_pop) +
  tm_fill(col = "TractGroup", alpha = 0.75, palette = "Pastel1") +
  tm_borders(col = "black", lwd = 0.1) +
  tm_layout(legend.outside = TRUE) +
  q1

### Build a 2.5k buffer for the stores
wholefoods_catch <- st_buffer(wholefoods, 2500)
ollies_catch <- st_buffer(ollies, 2500)

### Map 
q2 <- tm_shape(wholefoods_catch) +
  tm_polygons(col = "red", alpha = 0.3) +
  tm_shape(ollies_catch) +
  tm_polygons(col = "red", alpha = 0.3) + 
  q1
q2

########################################################################

# 3. Catchment Profiling  -------------------------------------------------

### Join the geodemographics with the store catchments
wholefoods_gd <- st_join(wholefoods_catch, gd_pop)
head(wholefoods_gd)

### Calculate total population in each store catchment
wholefood_profile <- wholefoods_gd %>%
  as.data.frame() %>%
  select(-c(geom)) %>%
  group_by(location_name, placekey) %>%
  summarise(catch_pop = sum(value))

### Calculate total geodemographic population in each store catchment
wholefood_gd_profile <- wholefoods_gd %>%
  as.data.frame() %>%
  select(-c(geom)) %>%
  group_by(location_name, placekey, TractGroup) %>%
  summarise(catch_gd_pop = sum(value))

### Bring together
wholefood_profile <- merge(wholefood_profile, wholefood_gd_profile, by = c("location_name", "placekey"), all.x = TRUE)

### Calculate proportion of catchment population occupied by each geodemographic group
wholefood_profile_final <- wholefood_profile %>%
  mutate(gd_prop = (catch_gd_pop / catch_pop) * 100) %>%
  select(location_name, placekey, TractGroup, gd_prop)
head(wholefood_profile_final)

### Visualise breakdown of catchment geodemographic population by store
wholefood_profile_final %>%
  ggplot(aes(fill = TractGroup, x = placekey, y = gd_prop)) +
  xlab("Store") +
  ylab("Catchment Proportion (%)") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_blank())


###############################################################################

# 4. Catchment Income Profile ---------------------------------------------

### Weights for each geodemographic group
zscores <- read.csv("Data/z-scores.csv")
zscores

### Merge weights onto profiles
wholefood_profile_final <- merge(wholefood_profile_final, zscores[, c("TractGroup", "z.score")], all.x = TRUE)

### Weight the catchment proportions
wholefood_profile_final <- wholefood_profile_final %>%
  arrange(location_name, placekey) %>%
  mutate(w_gd_prop = gd_prop * z.score)

### Calculate a weighted score, sums the weighted proportions to provide an Income Profile score:
wholefood_out <- wholefood_profile_final %>%
  select(location_name, placekey, w_gd_prop) %>%
  group_by(location_name, placekey) %>%
  summarise(exp = sum(w_gd_prop))
wholefood_out

### Mean Income Profile score for wholefoods
mean(wholefood_out$exp)


##############################################################################

# 5. Why my code is awful -------------------------------------------------

### Function that does all of these steps
getProfile <- function(brand) {
  
  ## Data
  x <- places %>%
    filter(brands == brand)
  x_catch <- st_buffer(x, 2500)
  
  ## Spatial Join
  x_gd <- st_join(x_catch, gd_pop)
  
  ## Calculate populations
  x_profile <- x_gd %>%
    as.data.frame() %>%
    select(-c(geom)) %>%
    group_by(location_name, placekey) %>%
    summarise(catch_pop = sum(value))
  x_gd_profile <- x_gd %>%
    as.data.frame() %>%
    select(-c(geom)) %>%
    group_by(location_name, placekey, TractGroup) %>%
    summarise(catch_gd_pop = sum(value))
  x_profile <- merge(x_profile, x_gd_profile, by = c("location_name", "placekey"), all.x = TRUE)
  x_profile_final <- x_profile %>%
    mutate(gd_prop = (catch_gd_pop / catch_pop) * 100) %>%
    select(location_name, placekey, TractGroup, gd_prop)
  
  ## Calculate Income Profile
  x_profile_final <- merge(x_profile_final, zscores[, c("TractGroup", "z.score")], all.x = TRUE)
  x_profile_final <- x_profile_final %>% 
    arrange(location_name, placekey) %>%
    mutate(w_gd_prop = gd_prop * z.score)
  x_out <- x_profile_final %>%
    select(location_name, placekey, w_gd_prop) %>%
    group_by(location_name, placekey) %>%
    summarise(exp = sum(w_gd_prop))
  x_income_prof <- x_out %>%
    group_by(location_name) %>%
    drop_na() %>%
    summarise(incomeProfile = mean(exp))
  return(x_income_prof)}

### Now we can supply a brand name and return it's mean Income Profile score
getProfile("T.J. Maxx")
getProfile("Burger King")


### What if we want to test it across multiple brands at the same time, instead 
### of line-by-line???

### First set up a list of brands you want to look at?
brand_ls <- c("Banana Republic", "Michael Kors", "T.J. Maxx", "JCPenney")

### Then lapply()
profiles <- lapply(brand_ls, getProfile)
glimpse(profiles)

### df
do.call(rbind, profiles)

### If working on linux, or within one of the lab machines
#mclapply(brand_ls, getProfile, mc.cores = 4)

###############################################################################

# 6. US Retail Centres example --------------------------------------------



##############################################################################


# 7. GitHub demo ----------------------------------------------------------



