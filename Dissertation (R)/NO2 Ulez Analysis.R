
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

no2 = read_csv("no2_meta.csv")
no2$SiteCode = as.factor(no2$SiteCode)

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
no2 = remove_outliers_iqr(no2, "MeanScaled")

# write_csv(no2, "no2_no_outliers.csv")

no2 = subset(no2, SiteClassification == "Roadside")

################################################################################
###################### Make daily and filter > 550 days ########################
##############################################f#################################

no2_spatial_daily_old = no2
no2_spatial_daily_old$date = round(no2_spatial_daily_old$date, units = "days")
no2_spatial_daily_old$date = as.POSIXct(no2_spatial_daily_old$date)

no2_spatial_daily_old = no2_spatial_daily_old %>% group_by(SiteCode, date) %>%
  summarize(MeanScaled = mean(MeanScaled), wd = mean(wd), ws = mean(ws), air_temp = mean(air_temp),
            Longitude = mean(Longitude), Latitude = mean(Latitude))

# Filter to sitecodes with over 550 days worth of data
list_of_sitecodes_old = no2 %>% group_by(SiteCode) %>% summarize()
list_of_sitecodes_old = list_of_sitecodes_old$SiteCode

list_of_sitecodes = list()
no2_spatial_daily = subset(no2_spatial_daily_old, MeanScaled < -100)

for (i in 1:length(list_of_sitecodes_old)){
  sitecode = list_of_sitecodes_old[i]
  node_df = subset(no2_spatial_daily_old, SiteCode == sitecode)
  if(nrow(node_df)  > 550){
    list_of_sitecodes= append(list_of_sitecodes, list(sitecode))
    no2_spatial_daily = rbind(no2_spatial_daily, node_df)
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
  geom_point(data = no2_spatial_daily, alpha = 0.8, aes(x = Longitude, y = Latitude))
ggmap(map) + geom_point(data = no2_spatial_daily, alpha = 0.8, aes(x = Longitude, y = Latitude)) +
  labs(title = "Breathe London sensor locations", x = "Longitude", y = "Latitude")

d = no2 %>% group_by(SiteCode) %>% summarize(MeanScaled = mean(MeanScaled), Longitude = mean(Longitude), Latitude = mean(Latitude))
plot_pm_yearly = p +
  geom_point(data = d, aes(x = Longitude, y = Latitude, color = MeanScaled)) +
  scale_color_gradientn(colors = c("lightblue", "darkblue")) + labs(title="Average NO2 concentrations at each sensor")
print(plot_pm_yearly)



################################################################################
#################### Split into expansion and outer zones ######################
################################################################################

# Split nodes into 2 regions, no2_ulez_outer and no2_ulez_expansion
no2_sf = st_as_sf(no2_spatial_daily, coords = c("Longitude", "Latitude"), crs = 4326)
no2_sf$outer_or_expansion = !st_within(no2_sf, sf_2019)
no2_ulez = subset(no2_sf, lengths(no2_sf$outer_or_expansion) > 0)
no2_ulez$expansion = st_within(no2_ulez, sf_2021)

no2_ulez_expansion = subset(no2_ulez, lengths(no2_ulez$expansion) > 0)
no2_ulez_outer = subset(no2_ulez, lengths(no2_ulez$expansion) == 0)

no2_ulez_outer_list = no2_ulez_outer$SiteCode
no2_ulez_expansion_list = no2_ulez_expansion$SiteCode


################################################################################
###################### Plot inner vs expansion on map ##########################
################################################################################

ggplot() +
  geom_sf(data = sf_2021, fill = "lightyellow", color = "black") +
  geom_sf(data = sf_2019, fill = "#FF918B") +
  geom_sf(data = no2_ulez_expansion, color = "black") +
  geom_sf(data = no2_ulez_outer, color = "darkgrey") +
  labs(title = "Sensors partitioned by ULEZ zone", x = "Longitude", y = "Latitude")

# Change back to coordinates instead of geometry
no2_ulez_outer = subset(no2_spatial_daily, SiteCode %in% no2_ulez_outer_list)
no2_ulez_outer = subset(no2_ulez_outer, as.POSIXct(date) >= as.POSIXct("2021-03-01"))
no2_ulez_expansion = subset(no2_spatial_daily, SiteCode %in% no2_ulez_expansion_list)
no2_ulez_expansion = subset(no2_ulez_expansion, as.POSIXct(date) >= as.POSIXct("2021-03-01"))

no2_ulez = subset(no2_spatial_daily, SiteCode %in% c(no2_ulez_outer_list, no2_ulez_expansion_list) )
no2_ulez = subset(no2_ulez, as.POSIXct(date) >= as.POSIXct("2021-03-01"))


p + geom_point(data = no2_ulez_expansion, alpha = 0.8, aes(x = Longitude, y = Latitude), color = "blue")
+   geom_point(data = no2_ulez_outer, alpha = 0.8, aes(x = Longitude, y = Latitude), color = "blue")



################################################################################
###################### Box plots before vs after ULEZ ##########################
################################################################################


## Box plot 1
before = subset(no2_ulez, date < as.POSIXct("2021-10-25"))
before$Time = "Before ULEZ"
after = subset(no2_ulez, date >= as.POSIXct("2021-10-25"))
after$Time = "After ULEZ"

both = rbind(after, before)
both$Time = factor(both$Time, levels = c("Before ULEZ", "After ULEZ"))

# Plot box plots from different data frames on the same axes
boxplot(MeanScaled ~ Time, data = both, main = "Boxplots of NO2 concentration pre and post ULEZ expansion", ylab = "NO2 concentration", xlab = "Time period")


## Box plot 2
no2_ulez_expansion$Group = "Expansion"
no2_ulez_outer$Group = "Outer"

joined = rbind(no2_ulez_expansion, no2_ulez_outer)

boxplot(MeanScaled ~ Group, data = joined, xlab = "Group", ylab = "NO2 concentration", main = "Boxplots of NO2 concentration for expansion and outer (control) groups")


#### Table 1

summary_intervention = summary(no2_ulez_expansion$MeanScaled)
summary_control = summary(no2_ulez_outer$MeanScaled)


table_data = data.frame(
  Measure = c("Mean", "Median", "Min", "Max", "SD"),
  Intervention = c(
    mean(no2_ulez_expansion$MeanScaled),
    median(no2_ulez_expansion$MeanScaled),
    min(no2_ulez_expansion$MeanScaled),
    max(no2_ulez_expansion$MeanScaled),
    sd(no2_ulez_expansion$MeanScaled)
  ),
  Control = c(
    mean(no2_ulez_outer$MeanScaled),
    median(no2_ulez_outer$MeanScaled),
    min(no2_ulez_outer$MeanScaled),
    max(no2_ulez_outer$MeanScaled),
    sd(no2_ulez_outer$MeanScaled)
  )
)

# Create the formatted table using kableExtra
table_formatted = kable(table_data, format = "latex", booktabs = TRUE)



###################################################
####### Meteorological correlation graphs #########
###################################################

# --------------------------------------------------------------------------
# Correlation Heatmap:
# Calculate the correlation between no22.5 concentrations and meteorological variables (e.g., temperature, humidity, wind speed) for each day.
# Create a heatmap to visualize the correlation matrix, showing the strength and direction of the relationships. This can help identify any significant meteorological factors influencing no22.5 levels.
# --------------------------------------------------------------------------

## Non dw

# Compute the correlation matrix
cor_matrix_no2 = cor(no2_ulez[3:6])

# Create the correlation heatmap
corrplot(cor_matrix_no2, method = "color", type = "full", tl.cex = 0.7,
         title = "Correlation between mean NO2 concentration
         and meteorological variables",
         mar = c(4,4,5,2))


# -------------------- 3. Meteorological normalisation  --------------------------#


################################################################################
##################### Deweathering expansion vs outer ##########################
################################################################################


# Test model

mod = testMod(
  no2_ulez[2:6],
  vars = c("trend", "air_temp", "ws", "wd", "weekday", "month"),
  pollutant = "MeanScaled",
)


# Build model

conflicted::conflict_prefer("%>%", "dplyr")


mod_no2 = buildMod(
  no2_ulez[2:6],
  vars = c("trend", "air_temp", "ws", "wd", "weekday", "month"),
  pollutant = "MeanScaled",
  n.trees = 4773,
  n.core = 16,
)


### Model plots ###

# Statistics

plotPD(mod_no2)
plot2Way(mod_no2, variable = c("ws", "air_temp"))

mod_no2$model
barplot(mod_no2$influence$mean  ~ mod_no2$influence$var, xlab = 'Variable', ylab = 'Relative influence', main = 'NO2')

summary(mod_no2$model)
par(las = 1)


# Meteorological averaging

no2_ulez_outer_dw = metSim(mod_no2,
                          newdata = no2_ulez_outer[2:6],
                          metVars = c("ws", "air_temp", "wd", "weekday", "month"),
                          n.core = 16,
                          B = 200)

no2_ulez_expansion_dw = metSim(mod_no2,
                              no2_ulez_expansion[2:6],
                              metVars = c("ws", "air_temp", "wd", "weekday", "month"),
                              n.core = 16,
                              B = 200)

no2_ulez_dw = metSim(mod_no2,
                    no2_ulez[2:6],
                    metVars = c("ws", "air_temp", "wd", "weekday", "month"),
                    n.core = 16,
                    B = 200)


no2_ulez_dw = subset(no2_ulez_dw, as.POSIXct(date) >= as.POSIXct("2021-03-01"))

# write_csv(no2_ulez_outer_dw, "no2_ulez_outer_dw_road.csv")
# write_csv(no2_ulez_outer, "no2_ulez_outer_road.csv")
# write_csv(no2_ulez, "no2_ulez_road.csv")
# write_csv(no2_ulez_expansion_dw, "no2_ulez_expansion_dw_road.csv")
# write_csv(no2_ulez_expansion, "no2_ulez_expansion_road.csv")
# write_csv(no2_ulez_dw, "no2_ulez_dw_road.csv")
# 
# no2 = read_csv("no2_no_outliers_road.csv")
# no2_ulez_outer_dw = read_csv("no2_ulez_outer_dw_road.csv")
# no2_ulez_expansion_dw = read_csv("no2_ulez_expansion_dw_road.csv")
# no2_ulez_dw = read_csv("no2_ulez_dw_road.csv")
# no2_ulez_outer = read_csv("no2_ulez_outer_road.csv")
# no2_ulez_expansion = read_csv("no2_ulez_expansion_road.csv")
# no2_ulez = read_csv("no2_ulez_road.csv")


# ------------------- 4. Post-normalisation plots -----------------------------#


################################################################################
################# Plot concentrations, split by ulez status ####################
################################################################################

# Plot expansion and outer


graph_expansion = ggplot(no2_ulez_expansion_dw, aes(date, MeanScaled)) +
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
       y = "no22.5") +
  xlim(c(as.POSIXct("2021-01-01"), as.POSIXct("2023-01-01"))) + ylim(c(11,23))


graph_expansion  + geom_smooth(method = "loess", n=1500)



graph_outer = ggplot(no2_ulez_outer_dw, aes(date, MeanScaled)) +
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
       y = "no22.5") +
  xlim(c(as.POSIXct("2021-01-01"), as.POSIXct("2023-01-01"))) + ylim(c(11,23))

graph_outer + geom_smooth(method = "loess", n=1500)



######################### Plot delta ###############################

no2_combined = inner_join(no2_ulez_outer_dw, no2_ulez_expansion_dw, by = 'date')
sum(is.na(no2_combined))
no2_combined$Delta = no2_combined$MeanScaled.x - no2_combined$MeanScaled.y
colnames(no2_combined)



no2_delta_graph = ggplot(no2_combined, aes(date, Delta)) + geom_smooth() +

  geom_vline(xintercept = as.POSIXct("2021-10-25"), color = "darkgreen", linetype = "dashed") +
  ylab("Delta (Outer minus expansion)")


  no2_delta_graph

  
  



# --------------------------------------------------------------------------
# Boxplot by Meteorological Condition:
# Group the daily no22.5 concentrations based on different meteorological conditions (e.g., temperature ranges, wind directions).
# Create boxplots to compare the distribution of no22.5 concentrations across the different conditions.
# This can provide insights into how meteorological factors relate to no22.5 levels.
# --------------------------------------------------------------------------

no2_combined = inner_join(no2_ulez, no2_ulez_dw, by = "date")

## Non dw

# Make temparature bands
no2_bands = no2_combined[,c("MeanScaled.x", "ws", "air_temp", "wd")]

low_no2_temp = quantile(no2_bands$air_temp, 1/3)
medium_no2_temp = quantile(no2_bands$air_temp, 2/3)
high_no2_temp = quantile(no2_bands$air_temp, 3/3)

no2_bands$temp_band = NA

# Add temparature category to dataframe
for (i in 1:nrow(no2_bands)){
  if(no2_bands$air_temp[i] < low_no2_temp){
    no2_bands$temp_band[i] = "low"
  }
  else if(no2_bands$air_temp[i] > medium_no2_temp){
    no2_bands$temp_band[i] = "high"
  }
  else{
    no2_bands$temp_band[i] = "medium"
  }
}

no2_bands$temp_band = factor(no2_bands$temp_band, levels = c("low", "medium", "high"))

# Create the boxplots
bp = boxplot(MeanScaled.x ~ temp_band, data = no2_bands,
             xlab = "Temparature", ylab = "Mean no22.5 Concentration",
             main = "Boxplot of no22.5 Concentrations by temparature")


## Dw

# Make temparature bands
no2_bands_dw = no2_combined[,c("MeanScaled.y", "ws", "air_temp", "wd")]

low_no2_temp_dw = quantile(no2_bands_dw$air_temp, 1/3)
medium_no2_temp_dw = quantile(no2_bands_dw$air_temp, 2/3)
high_no2_temp_dw = quantile(no2_bands_dw$air_temp, 3/3)

no2_bands_dw$temp_band = NA

# Add temparature category to dataframe
for (i in 1:nrow(no2_bands_dw)){
  if(no2_bands_dw$air_temp[i] < low_no2_temp_dw){
    no2_bands_dw$temp_band[i] = "low"
  }
  else if(no2_bands_dw$air_temp[i] > medium_no2_temp_dw){
    no2_bands_dw$temp_band[i] = "high"
  }
  else{
    no2_bands_dw$temp_band[i] = "medium"
  }
}


no2_bands_dw$temp_band = factor(no2_bands_dw$temp_band, levels = c("low", "medium", "high"))

# Create the boxplots
bp_dw = boxplot(MeanScaled.y ~ temp_band, data = no2_bands_dw,
                xlab = "Temparature", ylab = "Mean no22.5 Concentration",
                main = "Boxplot of no22.5 Concentrations by temparature")



# Plot box plots from different data frames on the same axes
ggplot() +
  geom_boxplot(data = no2_bands, aes(x = temp_band, y = MeanScaled.x), fill = "lightblue", color = "blue") +
  geom_boxplot(data = no2_bands_dw, aes(x = temp_band, y = MeanScaled.y), fill = "pink", color = "red") +
  labs(x = "Temparature", y = "Mean no22.5 concentration")


#################################################


## Non dw

## Non dw

# Make temparature bands
no2_bands = no2_combined[,c("MeanScaled.x", "ws", "air_temp", "wd")]

low_no2_temp = quantile(no2_bands$ws, 1/3)
medium_no2_temp = quantile(no2_bands$ws, 2/3)
high_no2_temp = quantile(no2_bands$ws, 3/3)

no2_bands$temp_band = NA

# Add temparature category to dataframe
for (i in 1:nrow(no2_bands)){
  if(no2_bands$ws[i] < low_no2_temp){
    no2_bands$temp_band[i] = "low"
  }
  else if(no2_bands$ws[i] > medium_no2_temp){
    no2_bands$temp_band[i] = "high"
  }
  else{
    no2_bands$temp_band[i] = "medium"
  }
}

no2_bands$temp_band = factor(no2_bands$temp_band, levels = c("low", "medium", "high"))

# Create the boxplots
bp = boxplot(MeanScaled.x ~ temp_band, data = no2_bands,
             xlab = "Wind speed", ylab = "Mean no22.5 Concentration",
             main = "Boxplot of no22.5 Concentrations by wind speed")


## Dw

# Make temparature bands
no2_bands_dw = no2_combined[,c("MeanScaled.y", "ws", "air_temp", "wd")]

low_no2_temp_dw = quantile(no2_bands_dw$ws, 1/3)
medium_no2_temp_dw = quantile(no2_bands_dw$ws, 2/3)
high_no2_temp_dw = quantile(no2_bands_dw$ws, 3/3)

no2_bands_dw$temp_band = NA

# Add temparature category to dataframe
for (i in 1:nrow(no2_bands_dw)){
  if(no2_bands_dw$ws[i] < low_no2_temp_dw){
    no2_bands_dw$temp_band[i] = "low"
  }
  else if(no2_bands_dw$ws[i] > medium_no2_temp_dw){
    no2_bands_dw$temp_band[i] = "high"
  }
  else{
    no2_bands_dw$temp_band[i] = "medium"
  }
}


no2_bands_dw$temp_band = factor(no2_bands_dw$temp_band, levels = c("low", "medium", "high"))

# Create the boxplots
bp_dw = boxplot(MeanScaled.y ~ temp_band, data = no2_bands_dw,
                xlab = "Wind speed", ylab = "Mean no22.5 Concentration",
                main = "Boxplot of no22.5 Concentrations by wind speed")



# Plot box plots from different data frames on the same axes
ggplot() +
  geom_boxplot(data = no2_bands, aes(x = temp_band, y = MeanScaled.x), fill = "lightblue", color = "blue") +
  geom_boxplot(data = no2_bands_dw, aes(x = temp_band, y = MeanScaled.y), fill = "pink", color = "red") +
  labs(x = "Wind speed", y = "Mean no22.5 concentration")






# -------------------------- 5. Compliance analysis  -----------------------------#

  

##########################################################################
################################ Compliance ##############################
##########################################################################


no2_ulez_outer_dw_comp = subset(no2_ulez_outer_dw, date <= as.POSIXct("2022-05-01"))
no2_ulez_outer_dw_comp$month = round(no2_ulez_outer_dw_comp$date, units = "month")
no2_ulez_outer_dw_comp$month = as.POSIXct(no2_ulez_outer_dw_comp$month)
dates_outer = no2_ulez_outer_dw_comp %>% group_by(month) %>% summarize()
dates_outer$compliance = c(75.5,76.7,77.3,77.7,78.3,79,79.4,80.4,82.2,82.8,83.3,83.2,83.2,83.4,83.8)
no2_ulez_outer_dw_comp = merge(no2_ulez_outer_dw_comp, dates_outer, by = "month")



no2_ulez_expansion_dw_comp = subset(no2_ulez_expansion_dw, date <= as.POSIXct("2022-05-01"))
no2_ulez_expansion_dw_comp$month = round(no2_ulez_expansion_dw_comp$date, units = "month")
no2_ulez_expansion_dw_comp$month = as.POSIXct(no2_ulez_expansion_dw_comp$month)
dates_expansion = no2_ulez_expansion_dw_comp %>% group_by(month) %>% summarize()
dates_expansion$compliance = c(80.3,81.1,81.6,82.2,82.7,83.4,84.2,87.5,91.6,92.2,92.8,92.7,93,93.2,93.3)
no2_ulez_expansion_dw_comp = merge(no2_ulez_expansion_dw_comp, dates_expansion, by = "month")



################################################### 
#################### DID model #################### 
###################################################


# Combine the dataframes
data = bind_rows(
  no2_ulez_outer_dw_comp %>% mutate(`Zone` = "Outer"),
  no2_ulez_expansion_dw_comp %>% mutate(`Zone` = "Expansion")
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

no2_estimate = lm(MeanScaled ~ compliance, data = data)
summary(no2_estimate)
no2_estimate_table = xtable(no2_estimate)
no2_estimate_table
  

#################################################################################
###################### Check the parallel trends assumption #####################
#################################################################################

parr_outer = as.data.frame(subset(no_ulez_outer_dw_comp, date < as.POSIXct("2021-10-01")))
parr_exp = as.data.frame(subset(no_ulez_expansion_dw_comp, date < as.POSIXct("2021-10-01")))


parr_joined = merge(parr_outer, parr_exp, by="date")
parr_joined$delta = parr_joined$compliance.x - parr_joined$compliance.y



parr = ggplot(parr_joined, aes(date, delta)) +
  
  geom_line() +
  
  labs(title = "Checking parallel trends assumption",
       x = "Date",
       y = "Difference in compliance (outer - expansion)") +   xlim(c(as.POSIXct("2021-03-01"), as.POSIXct("2021-10-25"))) 


parr


mean(subset(no_ulez_expansion, date < as.POSIXct("2021-10-25"))$MeanScaled)
mean(subset(no_ulez_expansion, date >= as.POSIXct("2021-10-25"))$MeanScaled)



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
# cp_1 = cpt.mean(no_ulez_expansion_dw_comp$MeanScaled, method = "BinSeg", Q = 5)
# cp_1
# plot(cp_1, ylab = "NO2 concentration")
# abline(v = 214, col = "black", lty = 2)
# 
# no_ulez_expansion_dw_comp$date[c(9, 96 ,354 )]

no_ulez_expansion_dw_comp_cpt = subset(no_ulez_expansion_dw_comp, date < as.POSIXct("2021-10-01") | date >= as.POSIXct("2021-11-01"))
no_ulez_outer_dw_comp_cpt = subset(no_ulez_outer_dw_comp, date < as.POSIXct("2021-10-01") | date >= as.POSIXct("2021-11-01"))
cpt_joined = merge(no_ulez_expansion_dw_comp_cpt, no_ulez_outer_dw_comp_cpt, by = "date")
cpt_joined$diff = cpt_joined$compliance.x - cpt_joined$compliance.y

# Expansion compliance change point 
cp_comp = cpt.mean(no_ulez_expansion_dw_comp_cpt$compliance, method = "BinSeg", Q = 1)
cp_comp
plot(cp_comp, ylab = "NO2 compliance (expansion zone)",  main="Change point detection for mean NO2 compliance (expansion)")
abline(v = 214, col = "forestgreen", lty = 1, style = "dashed")


# Outer (control) change point 
cp_comp_2 = cpt.mean(no_ulez_outer_dw_comp_cpt$compliance, method = "BinSeg", Q = 1)
cp_comp_2
plot(cp_comp_2, ylab = "NO2 compliance (outer zone)",  main="Change point detection for mean NO2 compliance (outer)")
abline(v = 214, col = "forestgreen", lty = 1)


# Expansion minus outer change point
cp_comp_3 = cpt.mean(cpt_joined$diff, method = "BinSeg", Q = 2)
cp_comp_3
plot(cp_comp_3, ylab = "NO2 compliance (expansion minus outer)", main="Change point detection for mean NO2 compliance (outer minus expansion)")
abline(v = 214, col = "forestgreen", lty = 1)
cpt_joined$date[197]


