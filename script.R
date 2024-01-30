# Author: Simone Maiorani


### Import libraries ###
library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)


### Import dataset(s) ###
traffic_death_data_both <-
  read.csv(
    "/Users/simonemaiorani/Desktop/Data Science/Data Science for Health Systems/Progetto Maiorani/dataset/data_both.csv",
    header = TRUE,
    sep = ","
  )

traffic_death_data_male <-
  read.csv(
    "/Users/simonemaiorani/Desktop/Data Science/Data Science for Health Systems/Progetto Maiorani/dataset/data_male.csv",
    header = TRUE,
    sep = ","
  )

traffic_death_data_female <-
  read.csv(
    "/Users/simonemaiorani/Desktop/Data Science/Data Science for Health Systems/Progetto Maiorani/dataset/data_female.csv",
    header = TRUE,
    sep = ","
  )

cat("\n")
print("Information about the datasets before cleaning them")
cat("\n")


##########################
## First View of Datasets ##
##########################


cat("\n")
print("First rows of datasets")
cat("\n")

head(traffic_death_data_both)
cat("\n")

head(traffic_death_data_male)
cat("\n")

head(traffic_death_data_female)
cat("\n")

cat("\n")
print("Summary of datasets")
cat("\n")

summary(traffic_death_data_both)
cat("\n")

summary(traffic_death_data_male)
cat("\n")

summary(traffic_death_data_female)
cat("\n")

cat("\n")
print("Structure of datasets")
cat("\n")

str(traffic_death_data_both)
cat("\n")

str(traffic_death_data_male)
cat("\n")

str(traffic_death_data_female)
cat("\n")

cat("\n")
print("Dimensions of datasets")
cat("\n")

dim(traffic_death_data_both)
cat("\n")

dim(traffic_death_data_male)
cat("\n")

dim(traffic_death_data_female)
cat("\n")

cat("\n")
print("Names of columns of datasets")
cat("\n")

names(traffic_death_data_both)
cat("\n")

names(traffic_death_data_male)
cat("\n")

names(traffic_death_data_female)
cat("\n")

cat("\n")
print("Number of missing values for each column of datasets")
cat("\n")

colSums(is.na(traffic_death_data_both))
cat("\n")

colSums(is.na(traffic_death_data_male))
cat("\n")

colSums(is.na(traffic_death_data_female))
cat("\n")

traffic_death_data_both <-
  traffic_death_data_both[, colSums(is.na(traffic_death_data_both)) != nrow(traffic_death_data_both)]
cat("\n")
print("Names of columns of dataset after removing features with all missing values")
cat("\n")
names(traffic_death_data_both)
cat("\n")

traffic_death_data_male <-
  traffic_death_data_male[, colSums(is.na(traffic_death_data_male)) != nrow(traffic_death_data_male)]
cat("\n")
print("Names of columns of dataset after removing features with all missing values")
cat("\n")
names(traffic_death_data_male)
cat("\n")

traffic_death_data_female <-
  traffic_death_data_female[, colSums(is.na(traffic_death_data_female)) != nrow(traffic_death_data_female)]
cat("\n")
print("Names of columns of dataset after removing features with all missing values")
cat("\n")
names(traffic_death_data_female)
cat("\n")


# View number of missing values for each column of dataset and our values
# for (col_name in colnames(traffic_death_data)) {
#     unique_values <- unique(traffic_death_data[[col_name]])

#     cat("Numero di valori univoci in", col_name, ":", length(unique_values), "\n")
#     cat("Valori univoci in", col_name, ":", paste(unique_values, collapse = ", "), "\n\n")
# }


## Remove features unnecessary to the analysis ##
traffic_death_data_both <-
  traffic_death_data_both[,!(
    names(traffic_death_data_both) %in% c(
      "IndicatorCode",
      "Indicator",
      "ValueType",
      "ParentLocationCode",
      "Location.type",
      "SpatialDimValueCode",
      "Period.type",
      "IsLatestYear",
      "Dim1.type",
      "Dim1ValueCode",
      "Value",
      "Language",
      "DateModified"
    )
  )]

traffic_death_data_male <-
  traffic_death_data_male[,!(
    names(traffic_death_data_male) %in% c(
      "IndicatorCode",
      "Indicator",
      "ValueType",
      "ParentLocationCode",
      "Location.type",
      "SpatialDimValueCode",
      "Period.type",
      "IsLatestYear",
      "Dim1.type",
      "Dim1ValueCode",
      "Value",
      "Language",
      "DateModified"
    )
  )]

traffic_death_data_female <-
  traffic_death_data_female[,!(
    names(traffic_death_data_female) %in% c(
      "IndicatorCode",
      "Indicator",
      "ValueType",
      "ParentLocationCode",
      "Location.type",
      "SpatialDimValueCode",
      "Period.type",
      "IsLatestYear",
      "Dim1.type",
      "Dim1ValueCode",
      "Value",
      "Language",
      "DateModified"
    )
  )]

cat("\n")
print("Names of columns of dataset remaining")
cat("\n")
names(traffic_death_data_both)
cat("\n")
names(traffic_death_data_male)
cat("\n")
names(traffic_death_data_female)
cat("\n")


## Rename columns of dataset ##
colnames(traffic_death_data_both) <-
  c(
    "GeoArea",
    "State",
    "Year",
    "Sex",
    "Value",
    "LowerConfidenceInterval",
    "UpperConfidenceInterval"
  )

colnames(traffic_death_data_male) <-
  c(
    "GeoArea",
    "State",
    "Year",
    "Sex",
    "Value",
    "LowerConfidenceInterval",
    "UpperConfidenceInterval"
  )

colnames(traffic_death_data_female) <-
  c(
    "GeoArea",
    "State",
    "Year",
    "Sex",
    "Value",
    "LowerConfidenceInterval",
    "UpperConfidenceInterval"
  )

cat("\n")
print("New names of columns of dataset")
cat("\n")

names(traffic_death_data_both)
cat("\n")

names(traffic_death_data_male)
cat("\n")

names(traffic_death_data_female)
cat("\n")


## Check NaN values ##
cat("\n")
print("Number of missing values for each column of dataset")
cat("\n")

colSums(is.na(traffic_death_data_both))
cat("\n")

colSums(is.na(traffic_death_data_male))
cat("\n")

colSums(is.na(traffic_death_data_female))
cat("\n")


## Check Non-Numeric values for Value column on each dataset ##
cat("\n")
print("Non-numeric values for Value column of dataset traffic_death_data_both")
cat("\n")

non_numeric_values <-
  traffic_death_data_both$Value[!is.numeric(traffic_death_data_both$Value)]
unique(non_numeric_values)

cat("\n")
print("Non-numeric values for Value column of dataset traffic_death_data_male")
cat("\n")

non_numeric_values <-
  traffic_death_data_male$Value[!is.numeric(traffic_death_data_male$Value)]
unique(non_numeric_values)

cat("\n")
print("Non-numeric values for Value column of dataset traffic_death_data_female")
cat("\n")

non_numeric_values <-
  traffic_death_data_female$Value[!is.numeric(traffic_death_data_female$Value)]
unique(non_numeric_values)


## Filter datasets for Sex columns and calculate mean of Value columns ##
cat("\n")
print("Mean Value column of dataset traffic_death_data_both")
cat("\n")

traffic_death_data_both <-
  filter(traffic_death_data_both, Sex == "Both sexes")
mean_value_both <- mean(traffic_death_data_both$Value, na.rm = TRUE)

cat("\n")
print("Mean Value column of dataset traffic_death_data_male")
cat("\n")

traffic_death_data_male <-
  filter(traffic_death_data_male, Sex == "Male")
mean_value_male <-
  mean(traffic_death_data_male$Value, na.rm = TRUE)

cat("\n")
print("Mean Value column of dataset traffic_death_data_female")
cat("\n")

traffic_death_data_female <-
  filter(traffic_death_data_female, Sex == "Female")
mean_value_female <-
  mean(traffic_death_data_female$Value, na.rm = TRUE)

cat("\n")
print("Mean Value for Both:")
cat("\n")
mean_value_both
cat("\n")

cat("\n")
print("Mean Value for Male:")
cat("\n")
mean_value_male
cat("\n")

cat("\n")
print("Mean Value for Female:")
cat("\n")
mean_value_female
cat("\n")


# Creation of a single, final dataset by merging traffic_death_data_male and traffic_death_data_female.
# Removal of the traffic_death_data_both dataset because it represents the average of the values in the other two datasets.
# Therefore not considered useful for the purpose of this analysis.
final_dataset <-
  rbind(traffic_death_data_male, traffic_death_data_female)
rm(traffic_death_data_both)
rm(traffic_death_data_male)
rm(traffic_death_data_female)


################################
## Exploratory Data Analysis ##
################################


## View distribution of the values in the Value column ##
# Histogram Plot
cat("\n")
hist(
  final_dataset$Value,
  main = "Distribution of values",
  xlab = "Value",
  col = alpha("lightblue", 0.5),
  border = "black",
  cex.main = 1.15,
  cex.lab = 1.15,
  font.main = 1,
  breaks = 20
)


## View distribution of filtered values by Sex ##
# Box Plot
box_plot_both <-
  ggplot(final_dataset,
         aes(x = Sex, y = Value, fill = Sex)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(values = c("Male" = "steelblue", "Female" = "pink")) +
  labs(title = "Distribution of values by Sex feature",
       x = "Sex",
       y = "Value") +
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(size = 14),
    axis.text.x = element_text(color = "black", size = 14),
    axis.text.y = element_text(color = "black", size = 14),
    plot.title = element_text(hjust = 0.5, size = 14)
  )
print(box_plot_both)

# Density Plots
density_plot_male <-
  ggplot(final_dataset[final_dataset$Sex == "Male",],
         aes(x = Value, fill = Sex)) +
  geom_density(alpha = 0.5, color = "black") +
  labs(title = "Distribution of values by Males",
       x = "Value",
       y = "Density") +
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(size = 14),
    axis.text.x = element_text(color = "black", size = 14),
    axis.text.y = element_text(color = "black", size = 14),
    plot.title = element_text(hjust = 0.5, size = 14)
  )
print(density_plot_male)

density_plot_female <-
  ggplot(final_dataset[final_dataset$Sex == "Female",],
         aes(x = Value, fill = Sex)) +
  geom_density(alpha = 0.5, color = "black") +
  labs(title = "Distribution of values by Females",
       x = "Value",
       y = "Density") +
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(size = 14),
    axis.text.x = element_text(color = "black", size = 14),
    axis.text.y = element_text(color = "black", size = 14),
    plot.title = element_text(hjust = 0.5, size = 14)
  )
print(density_plot_female)

# Histogram Plot
ggplot(final_dataset, aes(x = Value, fill = Sex)) +
  geom_histogram(binwidth = 1,
                 position = "identity",
                 alpha = 0.5) +
  facet_wrap(~ Sex, scales = "free") +
  labs(title = "Distribution of values by Sex feature",
       x = "Value",
       y = "Frequency") +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(color = "black", size = 14),
    axis.text.y = element_text(color = "black", size = 14),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 14, hjust = 0.5),
    strip.text.x = element_blank()
  )

# Violin Plot
ggplot(final_dataset, aes(x = Sex, y = Value, fill = Sex)) +
  geom_violin(alpha = 0.7) +
  labs(title = "Distribution of values by Sex",
       x = "Sex",
       y = "Value") +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(color = "black", size = 14),
    axis.text.y = element_text(color = "black", size = 14),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 14, hjust = 0.5)
  ) +
  guides(fill = guide_legend(title = "Sex"))


## View distribution of Value column values over the years ##
# Line Plot
ggplot(final_dataset, aes(x = Year, y = Value)) +
  geom_ribbon(
    aes(ymin = LowerConfidenceInterval, ymax = UpperConfidenceInterval),
    fill = "blue",
    alpha = 0.3
  ) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Trend of values over the years",
       x = "Year",
       y = "Value") +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(color = "black", size = 14),
    axis.text.y = element_text(color = "black", size = 14),
    plot.title = element_text(size = 14, hjust = 0.5)
  ) +
  scale_x_continuous(breaks = unique(final_dataset$Year))

# Bar Plot
ggplot(final_dataset, aes(x = Year, y = Value)) +
  geom_bar(stat = "identity",
           position = "dodge",
           fill = "#7D7098") +
  labs(x = "Year",
       y = "Value") +
  ggtitle("Distribution of values over the years") +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(
      color = "black",
      size = 14,
      angle = 45
    ),
    axis.text.y = element_text(color = "black", size = 14),
    plot.title = element_text(size = 14, hjust = 0.5)
  ) +
  scale_x_continuous(breaks = unique(final_dataset$Year)) +
  scale_y_continuous(labels = scales::comma_format())

# Box Plot
ggplot(final_dataset, aes(
  x = factor(Year, levels = rev(unique(Year))),
  y = Value,
  fill = Sex
)) +
  geom_boxplot() +
  labs(x = "Year",
       y = "Value") +
  ggtitle("Trend in values over the years between males and females") +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(size = 14, angle = 45),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14)
  ) +
  scale_y_continuous(labels = scales::comma_format())


## View distribution of values in the GeoAreas ##
# Box Plot
ggplot(final_dataset,
       aes(x = GeoArea, y = Value, fill = GeoArea)) +
  geom_boxplot() +
  labs(title = "Distribution of values in GeoAreas",
       x = "Geographical Area",
       y = "Value") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      size = 14,
      hjust = 1
    ),
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.y = element_text(size = 14),
    legend.position = "none"
  )

# Violin Plot
ggplot(data = final_dataset, aes(x = GeoArea, y = Value)) +
  geom_violin() +
  labs(title = "Distribution of values in GeoAreas",
       x = "GeoArea",
       y = "Value") +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 14
    ),
    text = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 14)
  ) +
  scale_y_continuous(labels = comma_format())


## View the mean distribution of values in the GeoAreas ##
mean_for_region <-
  aggregate(Value ~ GeoArea, data = final_dataset, FUN = mean)

mean_for_region <-
  mean_for_region[order(mean_for_region$Value, decreasing = FALSE), ]

bar_plot <- ggplot(mean_for_region, aes(
  x = reorder(GeoArea, Value),
  y = Value,
  fill = GeoArea
)) +
  geom_bar(stat = "identity") +
  labs(title = "Average value distribution in GeoAreas", x = "GeoArea", y = "Average value") +
  theme(
    axis.text.x = element_text(
      hjust = 1,
      size = 14,
      angle = 45
    ),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    text = element_text(size = 14),
    legend.position = "none",
    plot.title = element_text(size = 14, hjust = 0.5)
  )
print(bar_plot)


## View the mean distribution of values over the years among GeoAreas ##
filtered_data <- final_dataset %>%
  mutate(Year = as.factor(Year))

average_data <- filtered_data %>%
  group_by(GeoArea, Year) %>%
  summarise(Average_Value = mean(Value))

combined_plot <- ggplot(average_data,
                        aes(
                          x = Year,
                          y = Average_Value,
                          group = GeoArea,
                          color = GeoArea
                        )) +
  geom_line(linewidth = 1.5) +
  theme_minimal() +
  labs(
    title = "Comparison of average values between GeoAreas over the years",
    x = "Year",
    y = "Average value",
    color = "GeoArea"
  ) +
  scale_color_manual(values = c("red", "green", "blue", "orange", "purple", "pink")) +
  theme(
    legend.position = "bottom",
    text = element_text(size = 14),
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.text.x = element_text(size = 14, angle = 45),
  )
print(combined_plot)


## View the average trend in African states over the years ##
african_states_data <- filtered_data %>%
  filter(GeoArea == "Africa")

summary_data_africa <- african_states_data %>%
  group_by(State) %>%
  summarise(Average_Value = mean(Value))

summary_data_africa <- summary_data_africa %>%
  arrange(Average_Value)

africa_distribution_plot <- ggplot(summary_data_africa,
                                   aes(x = reorder(State, Average_Value),
                                       y = Average_Value)) +
  geom_bar(stat = "identity",
           width = 0.8,
           fill = "brown") +
  theme_minimal() +
  labs(title = "Road traffic deaths in African states",
       x = "State",
       y = "Average value") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 10
    ),
    plot.title = element_text(hjust = 0.5),
    text = element_text(size = 14)
  )
print(africa_distribution_plot)

# View difference in mean values between male and female in Africa
filtered_data <- final_dataset %>%
  filter(GeoArea == "Africa") %>%
  mutate(Year = as.factor(Year))

average_data <- filtered_data %>%
  group_by(Year, Sex) %>%
  summarise(Average_Value = mean(Value))

combined_plot <- ggplot(average_data,
                        aes(
                          x = Year,
                          y = Average_Value,
                          group = Sex,
                          color = Sex
                        )) +
  geom_line(linewidth = 1.5) +
  theme_minimal() +
  labs(
    title = "Difference between sexes in Africa over the years",
    x = "Year",
    y = "Average value",
    color = "Sex"
  ) +
  scale_color_manual(values = c("#AA336A", "blue")) +
  theme(
    legend.position = "bottom",
    text = element_text(size = 14),
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.text.x = element_text(size = 14, angle = 45),
  )
print(combined_plot)

## View the average trend in European states over the years ##
# Rename State names for better readability
final_dataset <- final_dataset %>%
  mutate(
    State = case_when(
      State == "United Kingdom of Great Britain and Northern Ireland" ~ "Great Britain and North Ireland",
      State == "The former Yugoslav Republic of Macedonia" ~ "Former Yugoslav Republic of Macedonia",
      TRUE ~ State
    )
  )

european_states_data <- final_dataset %>%
  filter(GeoArea == "Europe")

summary_data_europe <- european_states_data %>%
  group_by(State) %>%
  summarise(Average_Value = mean(Value))

summary_data_europe <- summary_data_europe %>%
  arrange(Average_Value)

europe_distribution_plot <- ggplot(summary_data_europe,
                                   aes(x = reorder(State, Average_Value),
                                       y = Average_Value)) +
  geom_bar(stat = "identity",
           width = 0.8,
           fill = "blue") +
  theme_minimal() +
  labs(title = "Road traffic deaths in European states",
       x = "State",
       y = "Average value") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 10
    ),
    plot.title = element_text(hjust = 0.5),
    text = element_text(size = 14)
  )
print(europe_distribution_plot)


# View difference in mean values between male and female in Europe
filtered_data <- final_dataset %>%
  filter(GeoArea == "Europe") %>%
  mutate(Year = as.factor(Year))

average_data <- filtered_data %>%
  group_by(Year, Sex) %>%
  summarise(Average_Value = mean(Value))

combined_plot <- ggplot(average_data,
                        aes(
                          x = Year,
                          y = Average_Value,
                          group = Sex,
                          color = Sex
                        )) +
  geom_line(linewidth = 1.5) +
  theme_minimal() +
  labs(
    title = "Difference between sexes in Europe over the years",
    x = "Year",
    y = "Average value",
    color = "Sex"
  ) +
  scale_color_manual(values = c("#AA336A", "blue")) +
  theme(
    legend.position = "bottom",
    text = element_text(size = 14),
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.text.x = element_text(size = 14, angle = 45),
  )
print(combined_plot)


# View difference in mean values between male and female for remaining GeoArea
final_dataset %>%
  mutate(Year = as.factor(Year)) %>%
  filter(!(GeoArea %in% c("Africa", "Europe"))) %>%
  group_by(GeoArea, Year, Sex) %>%
  summarise(Average_Value = mean(Value)) %>%
  ggplot(aes(
    x = Year,
    y = Average_Value,
    group = Sex,
    color = Sex
  )) +
  geom_line(linewidth = 1.5) +
  theme_minimal() +
  labs(
    title = "Difference between sexes by GeoArea over the years",
    x = "Year",
    y = "Average value",
    color = "Sex"
  ) +
  scale_color_manual(values = c("#AA336A", "blue")) +
  theme(
    legend.position = "bottom",
    text = element_text(size = 14),
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.text.x = element_text(size = 12, angle = 45),
  ) +
  facet_wrap( ~ GeoArea, scales = "free_y", ncol = 2)


## Comparison between the State with the highest mean value in Africa and Italy over the years ##
filtered_data_africa <-
  filter(final_dataset, GeoArea == "Africa")

max_avg_value_africa <- filtered_data_africa %>%
  group_by(State) %>%
  summarise(mean_value = mean(Value)) %>%
  arrange(desc(mean_value)) %>%
  slice(1)

custom_colors <- c("Italy" = "#ffb366")
custom_colors[max_avg_value_africa$State] <- "#8cb3d9"

filtered_data_italy <-
  filter(final_dataset, GeoArea == "Europe", State == "Italy")

filtered_data_max_avg_africa <-
  filter(final_dataset,
         GeoArea == "Africa",
         State == max_avg_value_africa$State)

combined_data <-
  rbind(filtered_data_italy, filtered_data_max_avg_africa)

combined_plot <-
  ggplot(combined_data, aes(x = as.factor(Year), y = Value, fill = State)) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    width = 0.7,
    color = "black",
    size = 0
  ) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  labs(title = "Difference between Italy and Zimbabwe over the years",
       x = "Year",
       y = "Average value") +
  geom_text(
    data = filtered_data_max_avg_africa %>% filter(Year == max(Year)),
    aes(label = State),
    position = position_dodge(width = 0.5),
    vjust = -0.5,
    color = "black",
    size = 0
  ) +
  guides(fill = guide_legend(title = "State")) +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 10
    ),
    plot.title = element_text(hjust = 0.5)
  )
print(combined_plot)


######################
## Statistical Tests ##
######################


## General statistics ##
# Mean confidence interval for male gender
confidenceinterval_male <- final_dataset %>%
  filter(Sex == "Male") %>%
  summarize(
    lower_ci = mean(LowerConfidenceInterval),
    upper_ci = mean(UpperConfidenceInterval)
  )

# Mean confidence interval for female gender
confidenceinterval_female <- final_dataset %>%
  filter(Sex == "Female") %>%
  summarize(
    lower_ci = mean(LowerConfidenceInterval),
    upper_ci = mean(UpperConfidenceInterval)
  )

# Check whether the confidence intervals for males and females overlap
if (confidenceinterval_male$upper_ci < confidenceinterval_female$lower_ci ||
    confidenceinterval_female$upper_ci < confidenceinterval_male$lower_ci) {
  print("Confidence intervals do not overlap")
} else {
  print("Confidence intervals overlap")
}

# Calculate the average traffic fatalities between males and females
mean_male <-
  mean(final_dataset$Value[final_dataset$Sex == "Male"])
mean_female <-
  mean(final_dataset$Value[final_dataset$Sex == "Female"])

cat("Average of traffic accidents for male:", mean_male, "\n")
cat("Average of traffic accidents for female:", mean_female, "\n")

# Calculate the standard deviation of traffic accidents for males and females
standardderivation_male <-
  sd(final_dataset$Value[final_dataset$Sex == "Male"])
standardderivation_female <-
  sd(final_dataset$Value[final_dataset$Sex == "Female"])

cat("Standard deviation of traffic accidents for males:",
    standardderivation_male,
    "\n")
cat(
  "Standard deviation of traffic accidents for females:",
  standardderivation_female,
  "\n"
)

# Calculate the median of traffic accidents for males and females
median_male <-
  median(final_dataset$Value[final_dataset$Sex == "Male"])
median_female <-
  median(final_dataset$Value[final_dataset$Sex == "Female"])

cat("Median of traffic accidents for males:", median_male, "\n")
cat("Median of traffic accidents for females:", median_female, "\n")


## Q-Q Plots ##
# Based on feature Sex
par(mfrow = c(2, 1))

qqnorm(final_dataset$Value[final_dataset$Sex == "Male"])
qqline(final_dataset$Value[final_dataset$Sex == "Male"], col = "red")
mtext(
  expression(bold("(Male)")),
  side = 3,
  line = 0.5,
  col = "black",
  cex.main = 1.2,
  cex.lab = 1.3
)

qqnorm(final_dataset$Value[final_dataset$Sex == "Female"])
qqline(final_dataset$Value[final_dataset$Sex == "Female"], col = "red")
mtext(
  expression(bold("(Female)")),
  side = 3,
  line = 0.5,
  col = "black",
  cex.main = 1.2,
  cex.lab = 1.3
)

# Based on feature GeoArea
create_qq_plot <- function(data, geoarea_name) {
  qqplot_data <- data %>%
    filter(GeoArea == geoarea_name) %>%
    select(Value)
  
  ggplot(qqplot_data, aes(sample = Value)) +
    geom_qq() +
    geom_qq_line(color = "red") +
    labs(
      title = paste("QQ Plot for: ", geoarea_name),
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    theme_minimal() +
    theme(text = element_text(size = 14),
          plot.title = element_text(hjust = 0.5))
}

area <- unique(final_dataset$GeoArea)
qq_plot <-
  lapply(area, function(area)
    create_qq_plot(final_dataset, area))

for (i in seq_along(area)) {
  print(qq_plot[[i]])
}


## Shapiro-Wilk Test ##
# Based on feature Sex
shapiro_test_male <-
  shapiro.test(final_dataset$Value[final_dataset$Sex == "Male"])

shapiro_test_female <-
  shapiro.test(final_dataset$Value[final_dataset$Sex == "Female"])

print("Verifying normality - Male:")
print(shapiro_test_male)

print("Verifying normality - Female:")
print(shapiro_test_female)


## Mann-Whitney U Test ##
# Based on feature Sex
mannwhitney_sex_test <-
  wilcox.test(Value ~ Sex, data = final_dataset, paired = FALSE)
cat("Mann-Whitney U test results (Sex):\n")
print(mannwhitney_sex_test)


## ANOVA ##
# Based on feature GeoArea
# Check normality using Shapiro-Wilk test for each group
shapiro_test_results <- final_dataset %>%
  group_by(GeoArea) %>%
  summarize(p_value = shapiro.test(Value)$p.value)

shapiro_test_results

# Check homoscedasticity using Bartlett's test
bartlett_test_result <-
  bartlett.test(Value ~ GeoArea, data = final_dataset)

print("Bartlett's test results:\n")
print(bartlett_test_result)

# If Bartlett's test indicates significant differences in variances
if (bartlett_test_result$p.value < 0.05) {
  cat(
    "Bartlett's test indicates significant differences in variances. Applying Kruskal-Wallis test.\n"
  )
  
  # Apply Kruskal-Wallis test
  kruskal_wallis_result <-
    kruskal.test(Value ~ GeoArea, data = final_dataset)
  
  print(kruskal_wallis_result)
} else {
  cat(
    "Bartlett's test does not indicate significant differences in variances. Proceeding with ANOVA.\n"
  )
  
  # Apply ANOVA if variances are not significantly different
  anova_result <-
    aov(Value ~ GeoArea, data = final_dataset)
  
  print(summary(anova_result))
}


# Post hoc analysis
# Based on feature GeoArea
# Bonferroni correction
pairwise_test_bonferroni <-
  pairwise.wilcox.test(final_dataset$Value,
                       final_dataset$GeoArea,
                       p.adjust.method = "bonferroni")

print(pairwise_test_bonferroni)

# Benjamini-Hochberg correction
pairwise_test_bh <-
  pairwise.wilcox.test(final_dataset$Value,
                       final_dataset$GeoArea,
                       p.adjust.method = "BH")

print(pairwise_test_bh)