
# Clear environment
rm(list=ls())

################################################### 
#################### Libraries #################### 
###################################################

library(pak)
library(corrplot)
library(worldmet)
library(openair)
library(gridExtra)
library(dplyr)
library(gbm)
library(foreach)
library(purrr)
library(conflicted)
library(graphics)
library(kableExtra)
library(writexl)
library(lmtest)
library(xtable)
library(changepoint)
library(bcp)
library(stats)
library(ggmap)
library(maptools)
library(maps)
library(ggplot2)
library(sf)
library(lubridate)
library(tidyverse)
library(mapview) 
library(table1)
library(readxl)
# pak::pak("davidcarslaw/deweather")
library(deweather) 
library(openair)
register_google(key = "") # My API key
library(devtools)


################################################################################
################################### Load data ##################################
################################################################################

pm = read_csv("pm_meta.csv")
pm$SiteCode = as.factor(pm$SiteCode)


# --------------------------- 1. Pre-processing ----------------------------------#

################################################################################
################################ Remove outliers ###############################
################################################################################

# Function to remove outliers using IQR method
remove_outliers_iqr = function(data, col_name) {
  # Calculate Q1 and Q3
  Q1 = quantile(data[[col_name]], 0.25)
  Q3 = quantile(data[[col_name]], 0.75)

  # Calculate IQR
  IQR_value = Q3 - Q1

  # Define lower and upper bounds for outliers
  lower_bound = Q1 - 1.5 * IQR_value
  upper_bound = Q3 + 1.5 * IQR_value

  # Filter out rows with values within the lower and upper bounds
  filtered_data = data[data[[col_name]] >= lower_bound & data[[col_name]] <= upper_bound, ]

  return(filtered_data)
}

# Call the function to remove outliers from the "MeanScaled" column
pm = remove_outliers_iqr(pm, "MeanScaled")

# write_csv(pm, "pm_no_outliers.csv")

pm = subset(pm, SiteClassification == "Roadside")

################################################################################
###################### Make daily and filter > 550 days ########################
##############################################f#################################

pm_spatial_daily_old = pm
pm_spatial_daily_old$date = round(pm_spatial_daily_old$date, units = "days")
pm_spatial_daily_old$date = as.POSIXct(pm_spatial_daily_old$date)

pm_spatial_daily_old = pm_spatial_daily_old %>% group_by(SiteCode, date) %>%
  summarize(MeanScaled = mean(MeanScaled), wd = mean(wd), ws = mean(ws), air_temp = mean(air_temp),
            Longitude = mean(Longitude), Latitude = mean(Latitude))

# Filter to sitecodes with over 550 days worth of data
list_of_sitecodes_old = pm %>% group_by(SiteCode) %>% summarize()
list_of_sitecodes_old = list_of_sitecodes_old$SiteCode

list_of_sitecodes = list()
pm_spatial_daily = subset(pm_spatial_daily_old, MeanScaled < -100)

for (i in 1:length(list_of_sitecodes_old)){
  sitecode = list_of_sitecodes_old[i]
  node_df = subset(pm_spatial_daily_old, SiteCode == sitecode)
  if(nrow(node_df)  > 550){
    list_of_sitecodes= append(list_of_sitecodes, list(sitecode))
    pm_spatial_daily = rbind(pm_spatial_daily, node_df)
    print(i)
  }
}


# --------------------------- 2. Initial Plots ----------------------------------#


################################################################################
################################### Maps #######################################
################################################################################

# 2: Plot these sitecodes on map
# Get the base map
map = get_map(location = 'London', zoom = 10) # maps from Google Maps  are always provided in the WGS 84 (EPSG:4326) coordinate system

# Create a ggplot object with the map as the base layer
p = ggmap(map)

# Replace 'path/to/your/exported_kml_file.kml' with the actual path to your exported KML file
kml_2019 = "ULEZ Zone Map 2019.kml"
kml_2021 = "ULEZ Zone Map 2021.kml"

# Read the KML file using sf
st_2019 = st_read(kml_2019)
st_2021 = st_read(kml_2021)

st_crs(st_2019)
st_crs(st_2021)

sf_2019 = st_as_sf(st_2019, crs = 4326)
sf_2021 = st_as_sf(st_2021, crs = 4326)

# Plot maps
ggplot() +  geom_sf(data = sf_2021, fill = "yellow", color = "black") +
  geom_sf(data = st_2019, fill = "#FF918B") +
  geom_point(data = pm_spatial_daily, alpha = 0.8, aes(x = Longitude, y = Latitude))

ggmap(map) + geom_point(data = pm_spatial_daily, alpha = 0.8, aes(x = Longitude, y = Latitude)) +
  labs(title = "Breathe London sensor locations", x = "Longitude", y = "Latitude")


################################################################################
#################### Split into expansion and outer zones ######################
################################################################################

# Split nodes into 2 regions, pm_ulez_outer and pm_ulez_expansion
pm_sf = st_as_sf(pm_spatial_daily, coords = c("Longitude", "Latitude"), crs = 4326)
pm_sf$outer_or_expansion = !st_within(pm_sf, sf_2019)
pm_ulez = subset(pm_sf, lengths(pm_sf$outer_or_expansion) > 0)
pm_ulez$expansion = st_within(pm_ulez, sf_2021)

pm_ulez_expansion = subset(pm_ulez, lengths(pm_ulez$expansion) > 0)
pm_ulez_outer = subset(pm_ulez, lengths(pm_ulez$expansion) == 0)

pm_ulez_outer_list = pm_ulez_outer$SiteCode
pm_ulez_expansion_list = pm_ulez_expansion$SiteCode


################################################################################
###################### Plot inner vs expansion on map ##########################
################################################################################

ggplot() +
  geom_sf(data = sf_2021, fill = "lightyellow", color = "black") +
  geom_sf(data = sf_2019, fill = "#FF918B") +
  geom_sf(data = pm_ulez_expansion, color = "black") +
  geom_sf(data = pm_ulez_outer, color = "darkgrey") +
  labs(title = "Sensors partitioned by ULEZ zone", x = "Longitude", y = "Latitude")

# Change back to coordinates instead of geometry
pm_ulez_outer = subset(pm_spatial_daily, SiteCode %in% pm_ulez_outer_list)
pm_ulez_outer = subset(pm_ulez_outer, as.POSIXct(date) >= as.POSIXct("2021-03-01"))
pm_ulez_expansion = subset(pm_spatial_daily, SiteCode %in% pm_ulez_expansion_list)
pm_ulez_expansion = subset(pm_ulez_expansion, as.POSIXct(date) >= as.POSIXct("2021-03-01"))

pm_ulez = subset(pm_spatial_daily, SiteCode %in% c(pm_ulez_outer_list, pm_ulez_expansion_list) )
pm_ulez = subset(pm_ulez, as.POSIXct(date) >= as.POSIXct("2021-03-01"))


# p + geom_point(data = pm_ulez_expansion, alpha = 0.8, aes(x = Longitude, y = Latitude), color = "blue")
# +   geom_point(data = pm_ulez_outer, alpha = 0.8, aes(x = Longitude, y = Latitude), color = "blue")



################################################################################
###################### Box plots before vs after ULEZ ##########################
################################################################################

# Box plots before and after ULEZ

## Box plot 1
before = subset(pm_ulez, date < as.POSIXct("2021-10-25"))
before$Time = "Before ULEZ"
after = subset(pm_ulez, date >= as.POSIXct("2021-10-25"))
after$Time = "After ULEZ"

both = rbind(after, before)
both$Time = factor(both$Time, levels = c("Before ULEZ", "After ULEZ"))

# Plot box plots from different data frames on the same axes
boxplot(MeanScaled ~ Time, data = both, main = "Boxplots of PM2.5 concentration pre and post ULEZ expansion", ylab = "PM2.5 concentration", xlab = "Time period")


## Box plot 2
pm_ulez_expansion$Group = "Expansion"
pm_ulez_outer$Group = "Outer"

joined = rbind(pm_ulez_expansion, pm_ulez_outer)

boxplot(MeanScaled ~ Group, data = joined, xlab = "Group", ylab = "PM2.5 concentration", main = "Boxplots of PM2.5 concentration for expansion and outer (control) groups")


#### Table 1

summary_intervention = summary(pm_ulez_expansion$MeanScaled)
summary_control = summary(pm_ulez_outer$MeanScaled)


table_data = data.frame(
  Measure = c("Mean", "Median", "Min", "Max", "SD"),
  Intervention = c(
    mean(pm_ulez_expansion$MeanScaled),
    median(pm_ulez_expansion$MeanScaled),
    min(pm_ulez_expansion$MeanScaled),
    max(pm_ulez_expansion$MeanScaled),
    sd(pm_ulez_expansion$MeanScaled)
  ),
  Control = c(
    mean(pm_ulez_outer$MeanScaled),
    median(pm_ulez_outer$MeanScaled),
    min(pm_ulez_outer$MeanScaled),
    max(pm_ulez_outer$MeanScaled),
    sd(pm_ulez_outer$MeanScaled)
  )
)

# Create the formatted table using kableExtra
table_formatted = kable(table_data, format = "latex", booktabs = TRUE)


###################################################
####### Meteorological correlation graphs #########
###################################################

# --------------------------------------------------------------------------
# Correlation Heatmap:
# Calculate the correlation between PM2.5 concentrations and meteorological variables (e.g., temperature, humidity, wind speed) for each day.
# Create a heatmap to visualize the correlation matrix, showing the strength and direction of the relationships. This can help identify any significant meteorological factors influencing PM2.5 levels.
# --------------------------------------------------------------------------

## Non dw

# Compute the correlation matrix
cor_matrix_pm = cor(pm_ulez[3:6])

# Create the correlation heatmap
corrplot(cor_matrix_pm, method = "color", type = "full", tl.cex = 0.7,
         title = "Correlation between mean PM2.5 concentration
         and meteorological variables",
         mar = c(4,4,5,2))


# -------------------- 3. Meteorological normalisation  --------------------------#

################################################################################
##################### Deweathering expansion vs outer ##########################
################################################################################


# # Test model
#
# mod = testMod(
#   pm_ulez[2:6],
#   vars = c("trend", "air_temp", "ws", "wd", "weekday", "month"),
#   pollutant = "MeanScaled",
# )


# Build model


conflicted::conflict_prefer("%>%", "dplyr")

mod_pm = buildMod(
  pm_ulez[2:6],
  vars = c("trend", "air_temp", "ws", "wd", "weekday", "month"),
  pollutant = "MeanScaled",
  n.trees = 3986,
  n.core = 16,
)


### Model plots ###

# Statistics

plotPD(mod_pm, nrow = 4)

plot2Way(mod_pm, variable = c("ws", "air_temp"))

barplot(mod_pm$influence$mean  ~ mod_pm$influence$var, xlab = 'Variable', ylab = 'Relative influence', main = 'PM2.5')

mod$model

summary(mod_pm$model)
par(las = 1)


# Meteorological averaging

pm_ulez_outer_dw = metSim(mod_pm,
                          newdata = pm_ulez_outer[2:6],
                          metVars = c("ws", "air_temp", "wd", "weekday", "month"),
                          n.core = 16,
                          B = 200)

pm_ulez_expansion_dw = metSim(mod_pm,
                              pm_ulez_expansion[2:6],
                              metVars = c("ws", "air_temp", "wd", "weekday", "month"),
                              n.core = 16,
                              B = 200)

pm_ulez_dw = metSim(mod_pm,
                    pm_ulez[2:6],
                    metVars = c("ws", "air_temp", "wd", "weekday", "month"),
                    n.core = 16,
                    B = 200)


pm_ulez_dw = subset(pm_ulez_dw, as.POSIXct(date) >= as.POSIXct("2021-03-01"))


# write_csv(pm_ulez_outer_dw, "pm_ulez_outer_dw_road.csv")
# write_csv(pm_ulez_outer, "pm_ulez_outer_road.csv")
# write_csv(pm_ulez, "pm_ulez_road.csv")
# write_csv(pm_ulez_expansion_dw, "pm_ulez_expansion_dw_road.csv")
# write_csv(pm_ulez_expansion, "pm_ulez_expansion_road.csv")
# write_csv(pm_ulez_dw, "pm_ulez_dw_road.csv")
# 
# pm = read_csv("pm_no_outliers_road.csv")
# pm_ulez_outer_dw = read_csv("pm_ulez_outer_dw_road.csv")
# pm_ulez_expansion_dw = read_csv("pm_ulez_expansion_dw_road.csv")
# pm_ulez_dw = read_csv("pm_ulez_dw_road.csv")
# pm_ulez_outer = read_csv("pm_ulez_outer_road.csv")
# pm_ulez_expansion = read_csv("pm_ulez_expansion_road.csv")
# pm_ulez = read_csv("pm_ulez_road.csv")


# ------------------- 4. Post-normalisation plots -----------------------------#


################################################################################
################# Plot concentrations, split by ulez status ####################
################################################################################

# Plot expansion and outer

graph_expansion = ggplot(pm_ulez_expansion_dw, aes(date, MeanScaled)) +
  geom_rect(xmin = as.POSIXct("2021-01-06"), xmax = as.POSIXct("2021-03-08"),
            ymin = -Inf, ymax = Inf,
            fill = "#FF2926", alpha = 0.2) +

  geom_rect(xmin = as.POSIXct("2021-03-08"), xmax = as.POSIXct("2021-03-29"),
            ymin = -Inf, ymax = Inf,
            fill = "#FF6361", alpha = 0.2) +

  geom_rect(xmin = as.POSIXct("2021-03-29"), xmax = as.POSIXct("2021-04-12"),
            ymin = -Inf, ymax = Inf,
            fill = "#FF8381", alpha = 0.2) +

  geom_rect(xmin = as.POSIXct("2021-04-12"), xmax = as.POSIXct("2021-05-17"),
            ymin = -Inf, ymax = Inf,
            fill = "#FFA5A3", alpha = 0.2) +

  geom_rect(xmin = as.POSIXct("2021-05-17"), xmax = as.POSIXct("2021-07-19"),
            ymin = -Inf, ymax = Inf,
            fill = "#FFC3C2", alpha = 0.2) +

  geom_rect(xmin = as.POSIXct("2021-07-19"), xmax = as.POSIXct("2021-12-08"),
            ymin = -Inf, ymax = Inf,
            fill = "#85A5FE", alpha = 0.2) +

  geom_rect(xmin = as.POSIXct("2021-12-08"), xmax = as.POSIXct("2022-01-27"),
            ymin = -Inf, ymax = Inf,
            fill = "#FFD1D0", alpha = 0.2) +

  geom_rect(xmin = as.POSIXct("2022-01-27"), xmax = as.POSIXct("2022-02-04"),
            ymin = -Inf, ymax = Inf,
            fill = "#85A5FE", alpha = 0.2) +

  geom_rect(xmin = as.POSIXct("2022-02-04"), xmax = as.POSIXct("2023-01-01"),
            ymin = -Inf, ymax = Inf,
            fill = "#85A5FE", alpha = 0.2) +

  geom_vline(xintercept = as.POSIXct("2021-10-25"), color = "darkgreen", linetype = "dashed") +

  geom_line() +

  labs(title = "Ulez expansion",
       x = "Date",
       y = "PM2.5") +
  xlim(c(as.POSIXct("2021-01-01"), as.POSIXct("2023-01-01"))) + ylim(c(6,14))


  graph_expansion  + geom_smooth(method = "loess", n=1500)


graph_outer = ggplot(pm_ulez_outer_dw, aes(date, MeanScaled)) +
  geom_rect(xmin = as.POSIXct("2021-01-06"), xmax = as.POSIXct("2021-03-08"),
            ymin = -Inf, ymax = Inf,
            fill = "#FF2926", alpha = 0.2) +

  geom_rect(xmin = as.POSIXct("2021-03-08"), xmax = as.POSIXct("2021-03-29"),
            ymin = -Inf, ymax = Inf,
            fill = "#FF6361", alpha = 0.2) +

  geom_rect(xmin = as.POSIXct("2021-03-29"), xmax = as.POSIXct("2021-04-12"),
            ymin = -Inf, ymax = Inf,
            fill = "#FF8381", alpha = 0.2) +

  geom_rect(xmin = as.POSIXct("2021-04-12"), xmax = as.POSIXct("2021-05-17"),
            ymin = -Inf, ymax = Inf,
            fill = "#FFA5A3", alpha = 0.2) +

  geom_rect(xmin = as.POSIXct("2021-05-17"), xmax = as.POSIXct("2021-07-19"),
            ymin = -Inf, ymax = Inf,
            fill = "#FFC3C2", alpha = 0.2) +

  geom_rect(xmin = as.POSIXct("2021-07-19"), xmax = as.POSIXct("2021-12-08"),
            ymin = -Inf, ymax = Inf,
            fill = "#85A5FE", alpha = 0.2) +

  geom_rect(xmin = as.POSIXct("2021-12-08"), xmax = as.POSIXct("2022-01-27"),
            ymin = -Inf, ymax = Inf,
            fill = "#FFD1D0", alpha = 0.2) +

  geom_rect(xmin = as.POSIXct("2022-01-27"), xmax = as.POSIXct("2022-02-04"),
            ymin = -Inf, ymax = Inf,
            fill = "#85A5FE", alpha = 0.2) +

  geom_rect(xmin = as.POSIXct("2022-02-04"), xmax = as.POSIXct("2023-01-01"),
            ymin = -Inf, ymax = Inf,
            fill = "#85A5FE", alpha = 0.2) +


  geom_vline(xintercept = as.POSIXct("2021-10-25"), color = "darkgreen", linetype = "dashed") +

  geom_line() +

  labs(title = "Outer (control)",
       x = "Date",
       y = "PM2.5") +
  xlim(c(as.POSIXct("2021-01-01"), as.POSIXct("2023-01-01"))) + ylim(c(6,14))

graph_outer + geom_smooth(method = "loess", n=1500)



######################### Plot delta ###############################

pm_combined = inner_join(pm_ulez_outer_dw, pm_ulez_expansion_dw, by = 'date')
sum(is.na(pm_combined))
pm_combined$`Delta (Outer minus expansion)` = pm_combined$MeanScaled.x - pm_combined$MeanScaled.y
colnames(pm_combined)


pm_delta_graph = ggplot(pm_combined, aes(date, `Delta (Outer minus expansion)`)) + geom_smooth() +

  geom_vline(xintercept = as.POSIXct("2021-10-25"), color = "darkgreen", linetype = "dashed") + ylim(c(-1.5,1.5))



pm_delta_graph


# Plot expansion and outer

outer_minus_expansion = inner_join(pm_ulez_expansion_dw, pm_ulez_outer_dw, by='date')

outer_minus_expansion$delta = outer_minus_expansion$MeanScaled.y - outer_minus_expansion$MeanScaled.x

parr_exp = ggplot(outer_minus_expansion, aes(date, delta)) +

  geom_smooth() +

  labs(title = "Delta graph for PM2.5",
       x = "Date",
       y = "Difference in PM2.5") +   xlim(c(as.POSIXct("2021-03-01"), as.POSIXct("2021-10-25"))) + ylim(c(-1.5,1.5))

parr_exp




# --------------------------------------------------------------------------
# Boxplot by Meteorological Condition:
# Group the daily PM2.5 concentrations based on different meteorological conditions (e.g., temperature ranges, wind directions).
# Create boxplots to compare the distribution of PM2.5 concentrations across the different conditions.
# This can provide insights into how meteorological factors relate to PM2.5 levels.
# --------------------------------------------------------------------------

pm_combined = inner_join(pm_ulez, pm_ulez_dw, by = "date")

## Non dw

# Make temparature bands
pm_bands = pm_combined[,c("MeanScaled.x", "ws", "air_temp", "wd")]

low_pm_temp = quantile(pm_bands$air_temp, 1/3)
medium_pm_temp = quantile(pm_bands$air_temp, 2/3)
high_pm_temp = quantile(pm_bands$air_temp, 3/3)

pm_bands$temp_band = NA

# Add temparature category to dataframe
for (i in 1:nrow(pm_bands)){
  if(pm_bands$air_temp[i] < low_pm_temp){
    pm_bands$temp_band[i] = "low"
  }
  else if(pm_bands$air_temp[i] > medium_pm_temp){
    pm_bands$temp_band[i] = "high"
  }
  else{
    pm_bands$temp_band[i] = "medium"
  }
}

pm_bands$temp_band = factor(pm_bands$temp_band, levels = c("low", "medium", "high"))

# Create the boxplots
bp = boxplot(MeanScaled.x ~ temp_band, data = pm_bands,
             xlab = "Temparature", ylab = "Mean PM2.5 Concentration",
             main = "Boxplot of PM2.5 Concentrations by temparature")


## Dw

# Make temparature bands
pm_bands_dw = pm_combined[,c("MeanScaled.y", "ws", "air_temp", "wd")]

low_pm_temp_dw = quantile(pm_bands_dw$air_temp, 1/3)
medium_pm_temp_dw = quantile(pm_bands_dw$air_temp, 2/3)
high_pm_temp_dw = quantile(pm_bands_dw$air_temp, 3/3)

pm_bands_dw$temp_band = NA

# Add temparature category to dataframe
for (i in 1:nrow(pm_bands_dw)){
  if(pm_bands_dw$air_temp[i] < low_pm_temp_dw){
    pm_bands_dw$temp_band[i] = "low"
  }
  else if(pm_bands_dw$air_temp[i] > medium_pm_temp_dw){
    pm_bands_dw$temp_band[i] = "high"
  }
  else{
    pm_bands_dw$temp_band[i] = "medium"
  }
}


pm_bands_dw$temp_band = factor(pm_bands_dw$temp_band, levels = c("low", "medium", "high"))

# Create the boxplots
bp_dw = boxplot(MeanScaled.y ~ temp_band, data = pm_bands_dw,
                xlab = "Temparature", ylab = "Mean PM2.5 Concentration",
                main = "Boxplot of PM2.5 Concentrations by temparature")



# Plot box plots from different data frames on the same axes
ggplot() +
  geom_boxplot(data = pm_bands, aes(x = temp_band, y = MeanScaled.x), fill = "lightblue", color = "blue") +
  geom_boxplot(data = pm_bands_dw, aes(x = temp_band, y = MeanScaled.y), fill = "pink", color = "red") +
  labs(x = "Temparature", y = "Mean PM2.5 concentration")


#################################################


## Non dw

# Make temparature bands
pm_bands = pm_combined[,c("MeanScaled.x", "ws", "air_temp", "wd")]

low_pm_temp = quantile(pm_bands$ws, 1/3)
medium_pm_temp = quantile(pm_bands$ws, 2/3)
high_pm_temp = quantile(pm_bands$ws, 3/3)

pm_bands$temp_band = NA

# Add temparature category to dataframe
for (i in 1:nrow(pm_bands)){
  if(pm_bands$ws[i] < low_pm_temp){
    pm_bands$temp_band[i] = "low"
  }
  else if(pm_bands$ws[i] > medium_pm_temp){
    pm_bands$temp_band[i] = "high"
  }
  else{
    pm_bands$temp_band[i] = "medium"
  }
}

pm_bands$temp_band = factor(pm_bands$temp_band, levels = c("low", "medium", "high"))

# Create the boxplots
bp = boxplot(MeanScaled.x ~ temp_band, data = pm_bands,
             xlab = "Wind speed", ylab = "Mean PM2.5 Concentration",
             main = "Boxplot of PM2.5 Concentrations by wind speed")


## Dw

# Make temparature bands
pm_bands_dw = pm_combined[,c("MeanScaled.y", "ws", "air_temp", "wd")]

low_pm_temp_dw = quantile(pm_bands_dw$ws, 1/3)
medium_pm_temp_dw = quantile(pm_bands_dw$ws, 2/3)
high_pm_temp_dw = quantile(pm_bands_dw$ws, 3/3)

pm_bands_dw$temp_band = NA

# Add temparature category to dataframe
for (i in 1:nrow(pm_bands_dw)){
  if(pm_bands_dw$ws[i] < low_pm_temp_dw){
    pm_bands_dw$temp_band[i] = "low"
  }
  else if(pm_bands_dw$ws[i] > medium_pm_temp_dw){
    pm_bands_dw$temp_band[i] = "high"
  }
  else{
    pm_bands_dw$temp_band[i] = "medium"
  }
}


pm_bands_dw$temp_band = factor(pm_bands_dw$temp_band, levels = c("low", "medium", "high"))

# Create the boxplots
bp_dw = boxplot(MeanScaled.y ~ temp_band, data = pm_bands_dw,
                xlab = "Wind speed", ylab = "Mean PM2.5 Concentration",
                main = "Boxplot of PM2.5 Concentrations by wind speed")



# Plot box plots from different data frames on the same axes
ggplot() +
  geom_boxplot(data = pm_bands, aes(x = temp_band, y = MeanScaled.x), fill = "lightblue", color = "blue") +
  geom_boxplot(data = pm_bands_dw, aes(x = temp_band, y = MeanScaled.y), fill = "pink", color = "red") +
  labs(x = "Wind speed", y = "Mean PM2.5 concentration")




# -------------------------- 5. Compliance analysis  -----------------------------#


##########################################################################
################################ Compliance ##############################
##########################################################################


pm_ulez_outer_dw_comp = subset(pm_ulez_outer_dw, date <= as.POSIXct("2022-05-01"))
pm_ulez_outer_dw_comp$month = round(pm_ulez_outer_dw_comp$date, units = "month")
pm_ulez_outer_dw_comp$month = as.POSIXct(pm_ulez_outer_dw_comp$month)
dates_outer = pm_ulez_outer_dw_comp %>% group_by(month) %>% summarize()
dates_outer$compliance = c(75.5,76.7,77.3,77.7,78.3,79,79.4,80.4,82.2,82.8,83.3,83.2,83.2,83.4,83.8)
pm_ulez_outer_dw_comp = merge(pm_ulez_outer_dw_comp, dates_outer, by = "month")


pm_ulez_expansion_dw_comp = subset(pm_ulez_expansion_dw, date <= as.POSIXct("2022-05-01"))
pm_ulez_expansion_dw_comp$month = round(pm_ulez_expansion_dw_comp$date, units = "month")
pm_ulez_expansion_dw_comp$month = as.POSIXct(pm_ulez_expansion_dw_comp$month)
dates_expansion = pm_ulez_expansion_dw_comp %>% group_by(month) %>% summarize()
dates_expansion$compliance = c(80.3,81.1,81.6,82.2,82.7,83.4,84.2,87.5,91.6,92.2,92.8,92.7,93,93.2,93.3)
pm_ulez_expansion_dw_comp = merge(pm_ulez_expansion_dw_comp, dates_expansion, by = "month")



################################################### 
#################### DID model #################### 
###################################################


# Combine the dataframes
data = bind_rows(
  pm_ulez_outer_dw_comp %>% mutate(`Zone` = "Outer"),
  pm_ulez_expansion_dw_comp %>% mutate(`Zone` = "Expansion")
)


data$Zone = factor(data$Zone, levels = c("Outer", "Expansion"))


date_of_expansion = as.POSIXct("2021-10-25", tz = "UTC")


# Create a dummy variable for the post-intervention period
data = data %>% mutate(`Time period` = ifelse(date > date_of_expansion, "Post expansion", "Pre expansion"))
data$`Time period` = factor(data$`Time period`, levels = c("Pre expansion", "Post expansion"))

# Perform the DiD analysis
model2 = lm(compliance ~   Zone + `Time period` + Zone:`Time period`, data = data)
summary(model2)
# Hence being in the expansion zone raises compliance by 7% and being in the expansion zone
# post-expansion raises compliance by a further 7%


reg_table = xtable(model2)

print(reg_table, include.rownames = TRUE)


#################################################################################
#################################### Pollution ##################################
#################################################################################

pm_estimate = lm(MeanScaled ~ compliance, data = data)
summary(pm_estimate)
pm_estimate_table = xtable(pm_estimate)
pm_estimate_table


#################################################################################
###################### Check the parallel trends assumption #####################
#################################################################################

parr_outer = as.data.frame(subset(pm_ulez_outer_dw_comp, date < as.POSIXct("2021-10-01")))
parr_exp = as.data.frame(subset(pm_ulez_expansion_dw_comp, date < as.POSIXct("2021-10-01")))


parr_joined = merge(parr_outer, parr_exp, by="date")
parr_joined$delta = parr_joined$compliance.x - parr_joined$compliance.y



parr = ggplot(parr_joined, aes(date, delta)) +

  geom_line() +

  labs(title = "Checking parallel trends assumption",
       x = "Date",
       y = "Difference in compliance (outer - expansion)") +   xlim(c(as.POSIXct("2021-03-01"), as.POSIXct("2021-10-25"))) 


parr


mean(subset(pm_ulez_expansion, date < as.POSIXct("2021-10-25"))$MeanScaled)
mean(subset(pm_ulez_expansion, date >= as.POSIXct("2021-10-25"))$MeanScaled)



cpt_expansion = subset(data, Zone == "Expansion")
cpt_outer = subset(data, Zone == "Outer")


### Check if data is normal 
qqnorm(cpt_expansion$MeanScaled, main = "Q-Q Plot of compliance rates for expansion zone", pch = 20)
qqnorm(cpt_outer$MeanScaled, main = "Q-Q Plot of compliance rates for outer zone", pch = 20)


# Create a density plot
ggplot(cpt_expansion, aes(x = MeanScaled)) +
  geom_density(fill = "blue", alpha = 0.5, color = "black") +
  labs(title = "Density Plot of compliance rates for expansion zone", x = "ULEZ compliance (per 100 vehicles)", y = "Density")

# Perform Shapiro-Wilk test
shapiro_test = shapiro.test(cpt_expansion$MeanScaled)

# Print the results of the Shapiro-Wilk test
print(shapiro_test)



# Create a density plot
ggplot(cpt_outer, aes(x = MeanScaled)) +
  geom_density(fill = "blue", alpha = 0.5, color = "black") +
  labs(title = "Density Plot of compliance rates for expansion zone", x = "ULEZ compliance (per 100 vehicles)", y = "Density")

# Perform Shapiro-Wilk test
shapiro_test_2 = shapiro.test(cpt_outer$MeanScaled)

# Print the results of the Shapiro-Wilk test
print(shapiro_test2)


#############################################################################
####################### Perform change point analysis ####################### 
#############################################################################


# ## Main
# cp_1 = cpt.mean(pm_ulez_expansion_dw_comp$MeanScaled, method = "BinSeg", Q = 5)
# cp_1
# plot(cp_1, ylab = "PM2.5 concentration")
# abline(v = 214, col = "black", lty = 2)
# 
# pm_ulez_expansion_dw_comp$date[c(9, 96 ,354 )]

pm_ulez_expansion_dw_comp_cpt = subset(pm_ulez_expansion_dw_comp, date < as.POSIXct("2021-10-01") | date >= as.POSIXct("2021-11-01"))
pm_ulez_outer_dw_comp_cpt = subset(pm_ulez_outer_dw_comp, date < as.POSIXct("2021-10-01") | date >= as.POSIXct("2021-11-01"))
cpt_joined = merge(pm_ulez_expansion_dw_comp_cpt, pm_ulez_outer_dw_comp_cpt, by = "date")
cpt_joined$diff = cpt_joined$compliance.x - cpt_joined$compliance.y

# Expansion compliance change point 
cp_comp = cpt.mean(pm_ulez_expansion_dw_comp_cpt$compliance, method = "BinSeg", Q = 1)
cp_comp
plot(cp_comp, ylab = "PM2.5 compliance (expansion zone)",  main="Change point detection for mean PM2.5 compliance (expansion)")
abline(v = 214, col = "forestgreen", lty = 1, style = "dashed")


# Outer (control) change point 
cp_comp_2 = cpt.mean(pm_ulez_outer_dw_comp_cpt$compliance, method = "BinSeg", Q = 1)
cp_comp_2
plot(cp_comp_2, ylab = "PM2.5 compliance (outer zone)",  main="Change point detection for mean PM2.5 compliance (outer)")
abline(v = 214, col = "forestgreen", lty = 1)


# Expansion minus outer change point
cp_comp_3 = cpt.mean(cpt_joined$diff, method = "BinSeg", Q = 2)
cp_comp_3
plot(cp_comp_3, ylab = "PM2.5 compliance (expansion minus outer)", main="Change point detection for mean PM2.5 compliance (outer minus expansion)")
abline(v = 214, col = "forestgreen", lty = 1)
cpt_joined$date[197]



