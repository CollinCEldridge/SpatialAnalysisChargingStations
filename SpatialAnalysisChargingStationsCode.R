# Load relevant libraries
suppressPackageStartupMessages(library(tidycensus))
suppressPackageStartupMessages(library(tigris))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(maditr))
suppressPackageStartupMessages(library(ggmap))
suppressPackageStartupMessages(library(spdep))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(sp))
suppressPackageStartupMessages(library(spatialreg))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(car))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(spatstat))
suppressPackageStartupMessages(library(methods))
suppressPackageStartupMessages(library(polyCub))
suppressPackageStartupMessages(library(ggcorrplot))
suppressPackageStartupMessages(library(here))


# Clear environment
rm(list = ls())


# Load datasets
load(here("Eldridge_Final_Project_Data_Stat310.RData"))


# Remove 71 duplicates and 5 incorrectly geocoded points from charger data
chargers <- chargers[-which(duplicated(chargers[, c("Longitude", "Latitude")])),]
chargers <- chargers %>% filter(chargers$Latitude >= 32.53444 & chargers$Longitude <= -114.13121)


# Load county shapefiles
ca_counties <- counties(state = "CA", year = 2021) %>% as("Spatial")


# Combine shapefiles and election results;
# narrow down data to California 2020, 
# calculate percentage votes, remove "Other" candidates
names(results_2020)[4] <- "County"
names(ca_counties)[5] <- "County"
ca_counties$County <- toupper(ca_counties$County)
results_2020 <- results_2020 %>% filter(year == 2020 & state_po == "CA" & candidate != "OTHER") %>% 
  mutate(perc_votes = candidatevotes/totalvotes) %>% 
  select(4, 7, 13) %>% # select county, candidate name, and percent of votes for that candidate
  pivot_wider(names_from = "candidate", values_from = "perc_votes")
colnames(results_2020)[2] <- "Biden"
ca_election_map <- merge(ca_counties, results_2020, by = "County")
election_tidy <- sf::st_as_sf(ca_election_map) # Data ready to be plotted


# Census tract data - household income, total population, 
# Blacks/African Americans, Hispanics/Latinos
ca_census <- get_acs(geography = "tract", 
                     variables = c("B19013_001E", # median household income, adjusted for inflation
                                   "B01003_001E", # estimated total population 
                                   "B02001_003", # estimated total black/African American population
                                   "B03001_003E"), # estimated total Hispanic/Latino population
                     year = 2021, 
                     state = "CA", 
                     geometry = T, 
                     output = "wide")


# Format census data for ease of use
isAboveZero <- as.vector(sf::st_area(ca_census)) > 0
ca_census <- ca_census[isAboveZero, ] %>% as("Spatial")
code_book <- rbind(c("B19013_001E", "median_household_income"), 
                   c("B01003_001E", "total_pop"), 
                   c("B02001_003E", "b_alone"), 
                   c("B03001_003E", "hisp_alone"))
code_book <- as.data.frame(code_book)
colnames(code_book) <- c("old_name", "new_name")
ca_census@data <- setnames(ca_census@data, 
                           old = code_book$old_name, 
                           new = code_book$new_name)


# Same census data, now at county level instead of tract
ca_countydata <- get_acs(geography = "county", 
                         variables = c("B19013_001E", 
                                       "B01003_001E", 
                                       "B02001_003E", 
                                       "B03001_003E"), 
                         year = 2021, 
                         state = "CA", 
                         geometry = T, 
                         output = "wide")


# Format county-level census data
isAboveZero2 <- as.vector(sf::st_area(ca_countydata)) > 0
ca_countydata <- ca_countydata[isAboveZero2,] %>% as("Spatial")
ca_countydata@data <- setnames(ca_countydata@data, 
                               old = code_book$old_name, 
                               new = code_book$new_name)


# Aggregate charger data to census tracts
sp::coordinates(chargers) <- ~Longitude+Latitude
methods::slot(chargers, "proj4string") <- CRS(SRS_string = "EPSG:4269")
chargers <- spTransform(chargers, sp::proj4string(ca_census))
overlap_ca <- over(chargers, ca_census)
ca_ag <- plyr::count(overlap_ca, c('GEOID'))
ca_ag$GEOID <- as.factor(ca_ag$GEOID)
colnames(ca_ag) <- c("GEOID", "num_chargers")
ca_census@data <- left_join(ca_census@data, ca_ag)


# Replace NAs in num_chargers with 0's
ca_census$num_chargers[is.na(ca_census$num_chargers)] <- 0


# Calculate percentage of race
ca_census@data <- ca_census@data %>% mutate(perc_b = b_alone/total_pop,
                                            perc_hisp = hisp_alone/total_pop)


# Aggregate charger data to counties
overlap_ca2 <- over(chargers, ca_countydata)
ca_ag2 <- plyr::count(overlap_ca2, c('GEOID'))
ca_ag2$GEOID <- as.factor(ca_ag2$GEOID)
colnames(ca_ag2) <- c("GEOID", "num_chargers")
ca_countydata@data <- left_join(ca_countydata@data, ca_ag2)


# Replace NAs in num_chargers with 0's
ca_countydata$num_chargers[is.na(ca_countydata$num_chargers)] <- 0


# Calculate percentage of race
ca_countydata@data <- ca_countydata@data %>% mutate(
  perc_b = b_alone/total_pop,
  perc_hisp = hisp_alone/total_pop,
  pop_mil = total_pop/1000000)


# Format county data so election results are correctly mapped
ca_countydata <- ca_countydata[order(ca_countydata$NAME), ]
ca_election_map <- ca_election_map[order(ca_election_map$County), ]
ca_countydata$election_results <- ca_election_map@data$Biden


# Map county-level election results to census tracts
countynames_censusdata <- regex("(?<=, ).+(?= County)")
countynames_countydata <- regex(".+(?= County)")
ca_census@data$countyname <- str_extract(ca_census@data$NAME, countynames_censusdata) # County names from census data
ca_countydata@data$countyname <- str_extract(ca_countydata@data$NAME, countynames_countydata) # County names from county data
for (i in 1:length(ca_census@data$GEOID)){ # Add election results to census data
  ca_census@data$election_results[i] <- 
    ca_countydata@data$election_results[which(ca_countydata@data$countyname == ca_census@data$countyname[i])] 
}


# Remove islands
ca_census <- ca_census[-1943, ]
ca_census <- ca_census[-2043, ]
ca_census <- ca_census[-5756, ]
ca_census <- ca_census[-9102, ]


# Remove tracts with NA values for income/race
ca_census <- ca_census[which(!is.na(ca_census$median_household_income)), ]


# Correct values of 0 in perc_b
b_0_index <- which(ca_census$perc_b == 0)
ca_census$perc_b[b_0_index] <- 0.001


# Edit num_chargers to accomodate log transformations
numcharg_new <- ca_census@data$num_chargers + 1
ca_census$num_chargers_usable <- numcharg_new


# Perform relevant log transformations
ca_census@data$logtotal_pop <- log(ca_census@data$total_pop)
ca_census@data$logmedian_household_income <- log(ca_census@data$median_household_income)
ca_census@data$logperc_b <- log(ca_census@data$perc_b)


# ggmap of California
cali_map <- ggmap(get_stadiamap(c(left = -124.42, bottom = 32.25,
                                  right = -114.2, top = 42.1)), zoom = 1) + 
  theme(text = element_text(size=18),
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank())


# Create sf object for plotting tract data
ca_sf <- sf::st_as_sf(ca_census)
ca_census$id <- row.names(ca_census)
ca_sf <- left_join(ca_sf, ca_census@data)


# Create sf object for plotting county data
ca_sf2 <- sf::st_as_sf(ca_countydata)
ca_countydata$id <- row.names(ca_countydata)
ca_sf2 <- left_join(ca_sf2, ca_countydata@data)


# Plot number of charging stations per county
p1 <- cali_map + geom_sf(data = ca_sf2, aes(fill = num_chargers), 
                         linewidth = NA, inherit.aes = FALSE) + 
  scale_fill_distiller(palette = "Spectral", labels = scales::comma) + 
  labs(title = "Figure 1", subtitle = "Number of Charging\nStations Per County",
       fill = "Number of\nChargers") +
  theme(text = element_text(size = 12))


# Plot total population per county
p2 <- cali_map + geom_sf(data = ca_sf2, aes(fill = total_pop), 
                         linewidth = NA, inherit.aes = FALSE) + 
  scale_fill_distiller(palette = "Spectral", labels = scales::comma) + 
  labs(title = "Figure 2", subtitle = "Total Population \nPer County", 
       fill = "Population") + 
  theme(text = element_text(size = 12))


# Plot median household income by county
p3 <- cali_map + geom_sf(data = ca_sf2, aes(fill = median_household_income), 
                         linewidth = NA, inherit.aes = FALSE) + 
  scale_fill_distiller(palette = "Spectral", labels = scales::comma) + 
  labs(subtitle = "Median Household \nIncome Per County", 
       title = "Figure 3", fill = "Median\nHousehold\nIncome") + 
  theme(text = element_text(size = 12))


# Plot proportions of blacks and African Americans per county
p4 <- cali_map + geom_sf(data = ca_sf2, aes(fill = perc_b), 
                         linewidth = NA, inherit.aes = FALSE) + 
  scale_fill_distiller(palette = "Spectral") + 
  labs(title = "Figure 4", subtitle = "Proportion of Blacks\nand African Americans\nPer County", 
       fill = "Proportion of\nBlacks and\nAfrican\nAmericans") + 
  theme(text = element_text(size = 12))


# Display first and second plots side-by-side
grid.arrange(p1, p2, nrow = 1, widths = c(1.85, 2))

# Display third and fourth plots side-by-side
grid.arrange(p3, p4, nrow = 1, widths = c(2, 2.1))


# Select variables used in modeling
corr_check <- ca_census@data %>%
  mutate("log(CS)" = log(num_chargers_usable)) %>% 
  select("logmedian_household_income", "logtotal_pop", "logperc_b", 
         "perc_hisp", "election_results", "log(CS)") %>% 
  rename("log(MHI)" = logmedian_household_income, "log(TP)" = logtotal_pop,
         "log(PBb)" = logperc_b, "PHL" = perc_hisp, "PD" = election_results)

# Create correlation plot
ggcorrplot(cor(corr_check), type = "upper", lab = TRUE) + 
  labs(title = "Figure 5", subtitle = "Correlation Matrix") + 
  theme(text = element_text(size = 12))


# Initial linear model
lin_mod <- lm(log(num_chargers_usable) ~ logmedian_household_income + 
                election_results + logperc_b + perc_hisp + logtotal_pop, 
              data=ca_census@data)
vif(lin_mod)


# Create nb object (neighborhood matrix)
resid <- lin_mod$residuals
ca_census@data$resid <- resid
ca_nb <- poly2nb(ca_census)


# Moran's I/Geary's C with binary neighborhood matrix
B_list <- nb2listw(ca_nb, style = "B")
#calculate using MC permutation testing
moran.mc(resid, listw = B_list, nsim = 5000)
geary.mc(resid, listw = B_list, nsim = 5000)


# Moran's I/Geary's C with row-standardized neighborhood matrix
W_list <- nb2listw(ca_nb, style = "W")
moran.mc(resid, listw = W_list, nsim = 5000)
geary.mc(resid, listw = W_list, nsim = 5000)


# Create 5-nearest neighbors matrix
ca_census_coords <- coordinates(ca_census)
knn_obj <- knearneigh(ca_census_coords, 5)
knn_nb <- knn2nb(knn_obj)
K_list <- nb2listw(knn_nb)


# Spatial autoregressive models (note: these take a while to run)
# Row-standardized error
ca_error <- errorsarlm(log(num_chargers_usable) ~ log(median_household_income) + 
                         election_results + log(perc_b) + perc_hisp + log(total_pop), 
                       data = ca_census@data, listw = W_list)
summary(ca_error)


# Row-standardized car
ca_car <- spautolm(log(num_chargers_usable) ~ log(median_household_income) + 
                     election_results + log(perc_b) + perc_hisp + log(total_pop), 
                   data = ca_census@data, family = "CAR", listw = W_list)
summary(ca_car)


# Row-standardized lag
ca_lag <- lagsarlm(log(num_chargers_usable) ~ log(median_household_income) + 
                     election_results + log(perc_b) + perc_hisp + log(total_pop), 
                   data = ca_census@data, listw = W_list)
summary(ca_lag)


# Row-standardized Durbin
ca_durbin <- lagsarlm(log(num_chargers_usable) ~ log(median_household_income) + 
                        election_results + log(perc_b) + perc_hisp + log(total_pop), 
                      data = ca_census@data, Durbin = T, listw = W_list)
summary(ca_durbin)


# Binary error
ca_error2 <- errorsarlm(log(num_chargers_usable) ~ log(median_household_income) + 
                          election_results + log(perc_b) + perc_hisp + log(total_pop), 
                        data = ca_census@data, listw = B_list)
summary(ca_error2)


# Binary car
ca_car2 <- spautolm(log(num_chargers_usable) ~ log(median_household_income) + 
                      election_results + log(perc_b) + perc_hisp + log(total_pop), 
                    data = ca_census@data, family = "CAR", listw = B_list)
summary(ca_car2)


# Binary lag
ca_lag2 <- lagsarlm(log(num_chargers_usable) ~ log(median_household_income) + 
                      election_results + log(perc_b) + perc_hisp + log(total_pop), 
                    data = ca_census@data, listw = B_list)
summary(ca_lag2)


# Binary Durbin
ca_durbin2 <- lagsarlm(log(num_chargers_usable) ~ log(median_household_income) + 
                         election_results + log(perc_b) + perc_hisp + log(total_pop), 
                       data = ca_census@data, Durbin = T, listw = B_list)
summary(ca_durbin2)


# 5-nearest error
ca_error3 <- errorsarlm(log(num_chargers_usable) ~ log(median_household_income) + 
                          election_results + log(perc_b) + perc_hisp + log(total_pop), 
                        data = ca_census@data, listw = K_list)
summary(ca_error3)


# 5-nearest car
ca_car3 <- spautolm(log(num_chargers_usable) ~ log(median_household_income) + 
                      election_results + log(perc_b) + perc_hisp + log(total_pop), 
                    data = ca_census@data, family = "CAR", listw = K_list)
summary(ca_car3)


# 5-nearest lag
ca_lag3 <- lagsarlm(log(num_chargers_usable) ~ log(median_household_income) + 
                      election_results + log(perc_b) + perc_hisp + log(total_pop), 
                    data = ca_census@data, listw = K_list)
summary(ca_lag3)


# 5-nearest Durbin
ca_durbin3 <- lagsarlm(log(num_chargers_usable) ~ log(median_household_income) + 
                         election_results + log(perc_b) + perc_hisp + log(total_pop), 
                       data = ca_census@data, Durbin = T, listw = K_list)
summary(ca_durbin3)


# Function to format data
# Adapted from the deprecated maptools package
as.im.SpatialGridDataFrame = function(from) {
  xi <- as.image.SpatialGridDataFrame(from)
  spatstat.geom::im(t(xi$z), xcol=xi$x, yrow=xi$y)
}


# Create PPP object
ca_census <- spTransform(ca_census, CRS(SRS_string = "EPSG:3310"))
poly_win <- polyCub::as.owin.SpatialPolygons(ca_census)
chargers <- spTransform(chargers, proj4string(ca_census))
ppp_charg <- as.ppp(coordinates(chargers), W = poly_win)


# Construct and plot G function
g_env_charg <- envelope(ppp_charg, fun = Gest, nrank = 2, nsim = 100)
plot(g_env_charg, main = "G function", xlim = c(0, max(g_env_charg$r)))


# Construct and plot F function
f_env_charg <- envelope(ppp_charg, fun = Fest, nrank = 2, nsim = 100)
plot(f_env_charg, main = "F function", xlim = c(0, max(f_env_charg$r)))

# Fit NHPP
# 200x200 grid
lattice <- expand.grid(Long = seq(ca_census@bbox[1,1],
                                  ca_census@bbox[1,2], length.out = 200), 
                       Lat = seq(ca_census@bbox[2,1],
                                 ca_census@bbox[2,2], length.out = 200))

# Remove lattice points not in California
sp_lattice <- lattice
sp::coordinates(sp_lattice) <- ~Long+Lat
methods::slot(sp_lattice, "proj4string") <- methods::slot(ca_census, "proj4string")
overlap_set <- over(sp_lattice, ca_census)
lattice <- lattice[!(is.na(overlap_set$GEOID)),]

# Combine with the rest of the Census data
lattice <- cbind(lattice, overlap_set[-which(is.na(overlap_set$GEOID)), ])

#remove grid points with NA for total_pop, med_income
lattice <- lattice %>% filter(!is.na(logtotal_pop),
                              !is.na(logmedian_household_income),
                              !is.na(logperc_b),
                              !is.na(perc_hisp),
                              !is.na(election_results))

# Assign projection to lattice
sp::coordinates(lattice) <- ~Long+Lat
methods::slot(lattice, "proj4string") <- methods::slot(ca_census, "proj4string")

# Change from SpatialPointsDataFrame to SpatialGridDataFrame
gridded(lattice) = TRUE
lattice_grid <- as(lattice, "SpatialGridDataFrame")

# Convert variables to IM objects
logtotalpop_im <- as.im.SpatialGridDataFrame(lattice_grid["logtotal_pop"])
logmedianhouseholdincome_im <- as.im.SpatialGridDataFrame(lattice_grid["logmedian_household_income"])
logpercb_im <- as.im.SpatialGridDataFrame(lattice_grid["logperc_b"])
perchisp_im <- as.im.SpatialGridDataFrame(lattice_grid["perc_hisp"])
electionresults_im <- as.im.SpatialGridDataFrame(lattice_grid["election_results"])


# Fitted model
fit_charg <- ppm(Q = ppp_charg, trend = ~logtotalpop_im + logmedianhouseholdincome_im + 
                   logpercb_im + perchisp_im + electionresults_im,
                 covariates = list(logtotal_pop = logtotalpop_im,
                                   logmedian_household_income = logmedianhouseholdincome_im,
                                   logperc_b = logpercb_im,
                                   perc_hisp = perchisp_im,
                                   election_results = electionresults_im))
summary(fit_charg)
