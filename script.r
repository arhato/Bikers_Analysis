library(ggplot2)
library(corrplot)

# task 1 - data cleaning and preparation
bike_buyers.dataset <- read.csv("./bike_buyers.csv", stringsAsFactors = TRUE)
bike_buyers.dataset[bike_buyers.dataset == ""] <- NA
dataset <- bike_buyers.dataset
str(dataset)
summary(dataset)
colSums(is.na(dataset))
dataset <- na.omit(dataset)
dataset <- droplevels(dataset)
write.csv(dataset, "new_data.csv", row.names = FALSE)
str(dataset)
summary(dataset)

colors <- c("skyblue", "coral1","darkseagreen","mediumpurple","darkorange1")

# pie chart marital status distribution
marital_count <- table(dataset$Marital.Status)
marital_percent <- round(100 * marital_count / sum(marital_count), 1)
martial_label <- paste(names(marital_count), "\n", marital_percent, "%")
pie(marital_count,
    labels = martial_label,
    col = colors,
    main = "Marital Status Distribution")

# pie chart gender distribution
gender_count <- table(dataset$Gender)
gender_percent <- round(100 * gender_count / sum(gender_count), 1)
gender_label <- paste(names(gender_count), "\n", gender_percent, "%")
pie(gender_count,
    labels = gender_label,
    col = colors,
    main = "Gender Distribution")

# box plot children per household
ggplot(dataset, aes(x = "", y = Children)) +
  geom_boxplot(fill = "skyblue",
               color = "black",
               width = 0.2) +
  geom_hline(
    aes(yintercept = mean(Children)),
    color = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  labs(title = "Children", y = "Number of Children") +
  theme(axis.title.x = element_blank())

# bar chart education distribution
ggplot(dataset, aes(x = Education)) +
  geom_bar(fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Education Distribution", x = "Education", y = "Count")

# bar chart occupation distribution
ggplot(dataset, aes(x = Occupation)) +
  geom_bar(fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Occupation Distribution", x = "Occupation", y = "Count")

# pie chart for home ownership
home_owner_count <- table(dataset$Home.Owner)
home_owner_percent <- round(100 * home_owner_count / sum(home_owner_count), 1)
home_owner_label <- paste(names(home_owner_count), "\n", home_owner_percent, "%")
pie(home_owner_count,
    labels = home_owner_label,
    col = colors,
    main = "Home Ownership Distribution")

# bar chart for number of cars owned
ggplot(dataset, aes(x = factor(Cars))) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribution of Cars Owned", 
       x = "Number of Cars", 
       y = "Count") +
  theme_minimal()

# pie chart for commute distance
commute_count <- table(dataset$Commute.Distance)
commute_percent <- round(100 * commute_count / sum(commute_count), 1)
commute_label <- paste(names(commute_count), "\n", commute_percent, "%")
pie(commute_count,
    labels = commute_label,
    col = colors,
    main = "Commute Distance Distribution")

# pie chart for region
region_count <- table(dataset$Region)
region_percent <- round(100 * region_count / sum(region_count), 1)
region_label <- paste(names(region_count), "\n", region_percent, "%")
pie(region_count,
    labels = region_label,
    col = colors,
    main = "Regional Distribution")

# histogram age distribution
ggplot(dataset, aes(x = Age)) +
  geom_histogram(
    binwidth = 5,
    fill = "skyblue",
    color = "black",
    alpha = 1
  ) +
  labs(title = "Age Distribution", x = "Age", y = "Frequency")

# pie chart for bike purchase
bike_purchase_count <- table(dataset$Purchased.Bike)
bike_purchase_percent <- round(100 * bike_purchase_count / sum(bike_purchase_count), 1)
bike_purchase_label <- paste(names(bike_purchase_count), "\n", bike_purchase_percent, "%")
pie(bike_purchase_count,
    labels = bike_purchase_label,
    col = colors,
    main = "Bike Purchase Distribution")

# histogram income distribution
ggplot(dataset, aes(x = Income)) +
  geom_histogram(
    binwidth = 10000,
    fill = "skyblue",
    color = "black",
    alpha = 1
  ) +
  labs(title = "Income Distribution", x = "Income", y = "Frequency")

# summary statistics income
summary_stats <- data.frame(
  Mean = mean(dataset$Income),
  Median = median(dataset$Income),
  Variance = var(dataset$Income),
  SD = sd(dataset$Income)
)
summary_stats

# income ranges
income_groups <- cut(
  dataset$Income,
  breaks = c(0, 40000, 80000, 120000, 170000),
  labels = c(
    "Low 0-40k",
    "Medium 40k-80k",
    "High 80-120k",
    "Very High 120-170k"
  ),
  include.lowest = TRUE
)

# bike by income summary
bikebyincome_summary <- do.call(rbind, by(dataset, income_groups, function(x) {
  data.frame(
    Total = nrow(x),
    Purchased = sum(x$Purchased.Bike == "Yes"),
    Not_Purchased = sum(x$Purchased.Bike == "No")
  )
}))
bikebyincome_summary

# box plot  income outliers
ggplot(dataset, aes(x = "", y = Income)) +
  geom_boxplot(fill = "skyblue",
               color = "black",
               width = 0.2) +
  geom_hline(
    aes(yintercept = mean(Income)),
    color = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  labs(title = "Income Distribution Outliers", y = "Income ($)") +
  theme(axis.title.x = element_blank())


# data frame bike purchase correlation
cor_data <- data.frame(
  Marital.Status = as.numeric(factor(dataset$Marital.Status)),
  Gender = as.numeric(factor(dataset$Gender)),
  Income = as.numeric(as.character(dataset$Income)),
  Children = as.numeric(as.character(dataset$Children)),
  Education = as.numeric(factor(dataset$Education)),
  Home.Owner = as.numeric(factor(dataset$Home.Owner)),
  Cars = as.numeric(as.character(dataset$Cars)),
  Commute.Distance = as.numeric(factor(dataset$Commute.Distance)),
  Region = as.numeric(factor(dataset$Region)),
  Age = as.numeric(as.character(dataset$Age)),
  BikePurchase = ifelse(dataset$Purchased.Bike == "Yes", 1, 0)
)

# correlation matrix
correlation_matrix <- cor(cor_data)

# correlation graph
corrplot(
  correlation_matrix,
  method = "color",
  type = "full",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45
)

#scatter plot age by bike
ggplot(subset(dataset, Purchased.Bike == "Yes"), aes(x = Purchased.Bike, y = Age, color = Purchased.Bike)) +
  geom_violin(fill = colors[1], color = "black", alpha = 0.6) +  
  geom_jitter(width = 0.3, alpha = 0.6) +  
  labs(title = "Age Correlation to Bike Purchased", x = "Bike Purchased", y = "Age") +
  scale_color_manual(values = c(colors[3])) +  
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())  


# age group dataframe
age_group <- cut(
  dataset$Age,
  breaks = c(0, 35, 50, 100),
  labels = c("Young (18-35)", "Middle-aged (36-50)", "Senior (51+)")
)

# density plot income by age groups
ggplot(dataset, aes(x = Income, fill = age_group)) +
  geom_density(alpha = 0.5) +
  labs(title = "Income Density Distribution by Age Groups", x = "Income", y = "Density") 

# density plot income by gender
ggplot(dataset, aes(x = Income, fill = Gender)) +
  geom_density(alpha = 0.5) +
  labs(title = "Income Density Distribution by Gender", x = "Income", y = "Density") 
