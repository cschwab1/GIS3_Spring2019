library(sf)
library(raster)
library(dplyr)
library(stringr)
library(spData)

dim(world)
nrow(world)
ncol(world)

world[1:6, ] #subset rows by position
world[, 1:3] #subset columns by position
world[, c("name_long", "lifeExp")] #subsets columns by name

#select in dplyr
world1 = dplyr::select(world, name_long, pop)
names(world1)

#all columns between name_long and pop (inclusive)
world2 = dplyr::select(world, name_long:pop)

data("us_states")
data("us_states_df")

us_state_names = us_states$NAME
us_state_names1 = dplyr::select(us_states, NAME)
us_states_pop = dplyr::select(us_states, total_pop_10:total_pop_15)
us_states_pop1 = us_states %>% select(contains("pop"))
help(contains)

world7 = world %>%
  filter(continent == "Asia") %>%
  dplyr::select(name_long, continent) %>%
  slice(1:5)

us_states_totalpop = total_pop_15

new_states <- us_states %>% 
  mutate(pop_dens15 = total_pop_15 / AREA) %>%
  mutate(pop_dens10 = total_pop_10 / AREA) %>%
  mutate(pct_change = (pop_dens15 - pop_dens10)*100)

us_states %>% 
  mutate(pop_dens_change = pop_dens15 - pop_dens10)

new_states2 <- us_states_df %>%
  mutate(income_change = median_income_15 - median_income_10)

summary(new_states2$median_income_15)

old_states_df  <- us_states %>%
  select(NAME, REGION)

old_states_df <- st_drop_geometry(old_states_df)

solution=left_join(new_states2, old_states_df, by=c("state"="NAME"))

## raster dataset operations
##create raster 9x9
elev = raster(nrows = 9, ncols = 9, res = 5,
              vals = sample(1:36))
              
plot(p, a, l)


## Ch3 Exercises
library(spData)
data(us_states)
data(us_states_df)
