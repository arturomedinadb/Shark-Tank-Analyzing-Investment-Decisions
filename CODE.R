###Libraries required----
require(ggplot2)
require(plm)
library(splines)
library(car)
library(boot)
library(corrplot)
require(lmtest)
library(glmnet)
library(dplyr)
library(tidyr)
library(psych)
library(skimr)
library(ggfortify)
require(MASS)
require(klaR)
require(randomForest)
library(caret)
require(rms)
library(gbm)
library(klaR)
library(stargazer)

##########################################################
###Step 0 Set the data and initial feature engineering ###
##########################################################

data <- read.csv("D:/01.Educación/03.MMA/03.Fall 2024/01.Multivariate Statistics (MGSC 661)/07.Final project/Dataset 3 — Shark tank pitches.csv")
attach(data)

#First we identify all the categorical columns
categorical_columns <- sapply(data, is.character)

# Replace empty strings with NA for all categorical columns otherwise skim would not mark as missing values
data[categorical_columns] <- lapply(data[categorical_columns], function(x) {
  x[x == ""] <- NA
  return(x)
})


skim(data) #We have missing values for entrepeneurs and website for the website we will create a new colum to drop 
#the current one while preserving the information.
#If the pitch has a website the column will take value as 1, 0 otherwise

data$does_it_have_website <- ifelse(!is.na(data$website) & data$website != "", 1, 0)
attach(data)

table(does_it_have_website)

#Since most of the values are 1 we drop this created column as well

#We will not perform NLP with the description column however we can retain some of the information of this variable
#by creating a new colmun that counts the length of the description 

data$description_length <- nchar(data$description)


# Transform 'deal' and 'Multiple.Entreprenuers' into binary numeric variables (1 for TRUE, 0 for FALSE) 
data$deal <- as.numeric(data$deal)  # Convert TRUE to 1, FALSE to 0
data$Multiple.Entreprenuers <- as.numeric(data$Multiple.Entreprenuers)

#Intitial remove of the following variables due to the lack of predictive or descriptive power (title and entrepreneurs)
#We remove as well episode-season, because we will use the more aggregated function season if we found the program 
#characteristic has some relevance
columns_drop <- c("title","entrepreneurs","episode.season","website", "description","does_it_have_website")
data <- data[, !colnames(data) %in% columns_drop]
attach(data)

#Test for multicollinearity in numerical data  

# Select numerical columns
numerical_data <- data %>% select_if(is.numeric)

#Create a correlation matrix
corr_matrix = cor(numerical_data)
round(corr_matrix,2)

#There is no value greater than .8 however we have .76 between askedFor and valuation
#but before we plot the PCA to confirm the hunch that these two variables provide same information

#Explore PCA 
# Subset numeric variables for PCA
pca_vars <- data[, c("deal","askedFor", "exchangeForStake", "valuation","season" ,"description_length")]

pca_shark <- prcomp(pca_vars, scale = TRUE)

# Plot the first two principal components
autoplot(pca_shark, data = pca_vars, loadings = TRUE, loadings.label = TRUE, loadings.colour ="#6A5ACD",loadings.label.colour = "black" )

# Calculate percentage of variance explained (PVE)
pve <- (pca_shark$sdev^2) / sum(pca_shark$sdev^2)

# Create a data frame for PVE
pve_table <- data.frame(
  Principal_Component = paste0("PC", 1:length(pve)),
  Standard_Deviation = pca_shark$sdev,
  `Proportion of Variance` = pve,
  `Cumulative Proportion` = cumsum(pve)
)


#The PCA analysis confirm same direction for valuation and askedFor
#The program works in a way that the entrepreneur ask for certain money and then the 
#investor valuates the project since these two are related instead of taking both 
#to reduce dimensionality we take the difference and drop the values

#Create a new variable for the difference between 'askedFor' and 'valuation'
data$ratio_difference_asked <- data$askedFor/data$valuation 

#Upon further inspection it was discorvered that exchange For represents exactly this ratio multiplied by 
#100 so using insights from PCA we decided to do the following, drop asked For (given is movement in PC1) since
#valuation moves slightly more we keep the original exchangeFor for models like trees that do not requiered standarization 
#and use the ratio for models that do 


##########################################################
################Exploratory analysis######################
##########################################################

#To explore whether to make additional feature engineering we will plot the unique values for each type of variable in 
#our data set as well we will create a table to understand the percentage distribution for each variable
# Identify numeric variables
numeric_vars <- c("ratio_difference_asked","description_length")
#numeric_vars <- c("ratio_difference_asked","description_length","exchangeForStake","valuation") #To plot variables after feature engineering 
# Identify categorical variables
categorical_vars <- c("episode","category","location","season","shark1","shark2","shark3","shark4","shark5","episode.season")
#categorical_vars <- c("episode","category","region","season_part","episode.season","investor_combination","season_grouped") #To plot variables after feature engineering 
# Identify logical variables
logical_vars <- c("deal","Multiple.Entreprenuers","does_it_have_website")
#logical_vars <- c("deal","Multiple.Entreprenuers","does_it_have_website","CA") #To plot variables after feature engineering 
# Bar charts for numeric variables (binned for better readability)
for (var in numeric_vars) {
  print(
    ggplot(data, aes_string(x = var)) +
      geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
      labs(
        title = paste(var),
        y = "Frequency"
      ) +
      theme_minimal()+
    theme(
      axis.title.x = element_blank(),  # Remove X-axis title
      panel.grid = element_blank() 
    )
  )
}

# Bar charts for categorical variables
for (var in categorical_vars) {
  print(
    ggplot(data, aes_string(x = var)) +
      geom_bar(fill = "skyblue", color = "black", alpha = 0.7) +
      labs(
        title = paste(var),
        x = var,
        y = "Count"
      ) +
      theme_minimal() +  
    theme(
      axis.title.x = element_blank(),  # Remove X-axis title
      #axis.text.x = element_blank(),   # Remove X-axis values
      axis.ticks.x = element_blank(),  # Remove X-axis ticks
      #axis.title.y = element_blank(),  # Remove Y-axis title
      panel.grid = element_blank(),
      axis.line = element_line(color = "black") # Add axis lines# Remove grid lines
    ) +
      scale_y_continuous(expand = c(0, 0)) # Remove padding on Y-axis
  )
}

# Bar charts for logical variables
for (var in logical_vars) {
  print(
    ggplot(data, aes(x = as.factor(!!sym(var))))+
      geom_bar(fill = "purple", color = "black", alpha = 0.7) +
      labs(
        title = paste(var),
        x = var,
        y = "Count"
      ) +
      theme_minimal() +  
      theme(
        #axis.title.x = element_blank(),  # Remove X-axis title
        #axis.text.x = element_blank(),   # Remove X-axis values
        axis.ticks.x = element_blank(),  # Remove X-axis ticks
        #axis.title.y = element_blank(),  # Remove Y-axis title
        panel.grid = element_blank(),
        axis.line = element_line(color = "black") # Add axis lines# Remove grid lines
      ) +
      scale_y_continuous(expand = c(0, 0)) # Remove padding on Y-axis
  )
}
#Drop askedFor due to our new ratio variable
columns_drop <- c("askedFor")
data <- data[, !colnames(data) %in% columns_drop]

#Insights for numeric variables
#diffence ask has some outliers

#Insights for categorical variables
#episode is a categorical with 30 values We can try to reduce the dimensionality by binning it in 
#first part, second part and third part of the season
#location is highly unbalanced with some location predominating against others
#binning is considered
#shark1&2&3&4&5 have many repeated values but in different order we decided to create categorical for each 
#shark to have a clearer way to work with the investors also there is a great imbalance
#bining into other cateogry 

#Insights for logical
#No remarkable insight for this type of variable 

#Before performing some feature engineering we look for the percentage of unique values

# Function to calculate percentage distributions for each variable
calculate_percentage_distributions <- function(data, vars) {
  for (var in vars) {
    cat("\nPercentage Distribution for variable:", var, "\n")
    percentages <- prop.table(table(data[[var]], useNA = "ifany")) * 100
    print(round(percentages, 2))  # Display percentages rounded to 2 decimals
  }
}

# Percentage distributions for numeric variables
cat("\n--- Numeric Variables ---\n")
calculate_percentage_distributions(data, numeric_vars)

# Percentage distributions for categorical variables
cat("\n--- Categorical Variables ---\n")
calculate_percentage_distributions(data, categorical_vars)

# Percentage distributions for logical variables
cat("\n--- Logical Variables ---\n")
calculate_percentage_distributions(data, logical_vars)

#We decide to modify three and replace three columns in the next step
#1 create bins for the episodes to dive in early season, mid season and late season
#2 create a columns with the state instead of the specific location
#3 create binary variables for each shark instead of having them as a shark1,2, etc.
#  and move sharks appearing less than 20% to others


##########################################################
################Feature engineering#######################
##########################################################

#Bins for episodes
data$season_part <- ifelse(data$episode >= 1 & data$episode <= 10, "early season",
                           ifelse(data$episode >= 11 & data$episode <= 20, "mid season",
                                  ifelse(data$episode >= 21 & data$episode <= 30, "late season", NA)))

attach(data)

#State for location
data$state <- sub(".*,\\s*", "", data$location)

# Create a binary column 'CA' indicating whether the state is 'CA'this due to two approaches will be test,
#next we created balanced region groups. 
data$CA <- ifelse(data$state == "CA", 1, 0)

# We further breakdown in 4 to balance the categories

state_to_region <- list(
  "Region1" = c("CA"),
  "Region2" = c("TX", "FL", "GA", "NC", "SC", "AL", "MS", "LA", "AR", "TN", "OK"),
  "Region3" = c("NY", "PA", "NJ", "MA", "CT", "RI", "MD", "DC", "VA"),
  "Region4" = c("OH", "IL", "IN", "MI", "WI", "MN", "IA", "MO", "KS", "NE", "NH", "VT", "ME", "KY", "WV"),
  "Region5" = c("WA", "OR", "AZ", "UT", "CO", "NV", "ID", "MT") # Remaining states
)


# Function to map states to regions
map_state_to_region <- function(state) {
  for (region in names(state_to_region)) {
    if (state %in% state_to_region[[region]]) {
      return(region)
    }
  }
  return("Region5")  # Assign to "Region5" if state not found
}

# Apply the mapping to create the 'region' variable
data$region <- sapply(as.character(data$state), map_state_to_region)
data$region <- as.factor(data$region)

# Drop the 'state' variable if no longer needed
data$state <- NULL

attach(data)

table(region)

#Remove sharks appearing less than 20% of the time 
all_sharks <- unlist(data[, c("shark1", "shark2", "shark3", "shark4", "shark5")])


# Calculate the frequency of each shark
shark_frequency <- prop.table(table(all_sharks)) * 100

# Identify sharks appearing less than 20% of the time
sharks_below_20 <- names(shark_frequency[shark_frequency < 5])

# Replace these sharks with "Others" in the data
data <- data %>%
  mutate(
    shark1 = ifelse(shark1 %in% sharks_below_20, "Others", shark1),
    shark2 = ifelse(shark2 %in% sharks_below_20, "Others", shark2),
    shark3 = ifelse(shark3 %in% sharks_below_20, "Others", shark3),
    shark4 = ifelse(shark4 %in% sharks_below_20, "Others", shark4),
    shark5 = ifelse(shark5 %in% sharks_below_20, "Others", shark5)
  )
attach(data)

# Combine all shark columns and  create a column for each unique shark
all_sharks <- unique(c(data$shark1, data$shark2, data$shark3, data$shark4, data$shark5))

for (shark in all_sharks) {
  data[[shark]] <- apply(data[, c("shark1", "shark2", "shark3", "shark4", "shark5")], 1, function(x) {
    ifelse(shark %in% x, 1, 0)
  })
}
attach(data)

#We drop previous columns
columns_drop <- c("episode","location","shark1","shark2","shark3","shark4","shark5")
data <- data[, !colnames(data) %in% columns_drop]
attach(data)

#We check once more to be sure no new null values were generated 
categorical_columns <- sapply(data, is.character)
data[categorical_columns] <- lapply(data[categorical_columns], function(x) {
  x[x == ""] <- NA
  return(x)
})
attach(data)

skim(data)

##########################################################
################Exploratory analysis II###################
##########################################################

# Correlation between numeric variables (multicollinearity) ----

# Specify the numeric variable names
quant_var <- c( "exchangeForStake", "ratio_difference_asked", "season", "description_length")

# Extract the actual numeric data from the dataset
quant_data <- data[, quant_var]

# Compute the correlation matrix for numeric variables
corr_matrix <- cor(quant_data, use = "complete.obs")

# Threshold for significant correlations
threshold_corr <- 0.4

# Create a loop to identify pairs of variables with correlations above the threshold
for (i in 1:(ncol(corr_matrix) - 1)) {
  for (j in (i + 1):ncol(corr_matrix)) {
    if (!is.na(corr_matrix[i, j]) && abs(corr_matrix[i, j]) > threshold_corr) { # Handle NA values
      cat("Correlation between", colnames(corr_matrix)[i], "and", colnames(corr_matrix)[j],
          "is", round(corr_matrix[i, j], 2), "\n")
    }
  }
}

#We transform all the categorical columns to a factor to use in the models
columns_facts <- c("category","season","season_part","region")

#Convert the specified columns to factors
data[columns_facts] <- lapply(data[columns_facts], as.factor)
attach(data)

#Convert binary columns to 0 and 1
# Identify binary columns (logical type)
binary_columns <- c("deal", "Multiple.Entreprenuers")
data[, binary_columns] <- lapply(data[, binary_columns, drop = FALSE], as.numeric)
attach(data)



##########################################################
################Feature engineering 2#####################
##########################################################

#After running some models we realized that there is a consistent problem due to the
#high number of categories in our variables, so we need to further reduced dimensionality
#before running our final model 

#Categories 

#We grouped in related categories the main category since there are some topics
#that are highly related
# Define the category mappings

category_mapping <- c(
  "Alcoholic Beverages" = "Food and Beverages",
  "Automotive" = "Professional Services",
  "Baby and Child Care" = "Baby and Child",
  "Baby and Children's Apparel and Accessories" = "Baby and Child",
  "Baby and Children's Bedding" = "Baby and Child",
  "Baby and Children's Entertainment" = "Baby and Child",
  "Baby and Children's Food" = "Baby and Child",
  "Consumer Services" = "Professional Services",
  "Costumes" = "Entertainment and Recreation",
  "Cycling" = "Entertainment and Recreation",
  "Education" = "Professional Services",
  "Electronics" = "Technology and Gadgets",
  "Entertainment" = "Entertainment and Recreation",
  "Fashion Accessories" = "Fashion and Apparel",
  "Fitness Apparel and Accessories" = "Fitness and Health",
  "Fitness Equipment" = "Fitness and Health",
  "Fitness Programs" = "Fitness and Health",
  "Furniture" = "Home and Living",
  "Gardening" = "Home and Living",
  "Golf Products" = "Home and Living",
  "Health and Well-Being" = "Fitness and Health",
  "Holiday Cheer" = "Seasonal and Event-Specific",
  "Home Accessories" = "Home and Living",
  "Home Improvement" = "Home and Living",
  "Home Security Solutions" = "Home and Living",
  "Homeopathic Remedies" = "Fitness and Health",
  "Kitchen Tools" = "Home and Living",
  "Maternity" = "Fashion and Apparel",
  "Men's Accessories" = "Fashion and Apparel",
  "Men and Women's Accessories" = "Fashion and Apparel",
  "Men and Women's Apparel" = "Fashion and Apparel",
  "Men and Women's Shoes" = "Fashion and Apparel",
  "Mobile Apps" = "Technology and Gadgets",
  "Music" = "Entertainment and Recreation",
  "Non-Alcoholic Beverages" = "Food and Beverages",
  "Novelties" = "Entertainment and Recreation",
  "Online Services" = "Professional Services",
  "Outdoor Recreation" = "Entertainment and Recreation",
  "Party Supplies" = "Entertainment and Recreation",
  "Personal Care and Cosmetics" = "Personal Care",
  "Pest Control" = "Personal Care",
  "Pet Products" = "Personal Care",
  "Productivity Tools" = "Technology and Gadgets",
  "Professional Services" = "Professional Services",
  "Specialty Food" = "Food and Beverages",
  "Storage and Cleaning Products" = "Home and Living",
  "Toys and Games" = "Entertainment and Recreation",
  "Undergarments and Basics" = "Fashion and Apparel",
  "Water Bottles" = "Food and Beverages",
  "Weddings" = "Seasonal and Event-Specific",
  "Wine Accessories" = "Food and Beverages",
  "Women's Accessories" = "Fashion and Apparel",
  "Women's Apparel" = "Fashion and Apparel",
  "Women's Shoes" = "Fashion and Apparel"
)

# Replace the categories in the data
data$category <- as.factor(category_mapping[data$category])

attach(data)


# Lastly due to over representation of some sharks we combine investor columns into a single string
investor_columns <- c("Barbara Corcoran", "Lori Greiner", "Robert Herjavec", 
                      "Kevin O'Leary", "Daymond John", 
                      "Mark Cuban", "Others")

data$investor_combination <- apply(data[, investor_columns], 1, function(row) {
  paste(names(row)[row == 1], collapse = ",")
})
attach(data)

# Calculate the frequency of each combination
combination_counts <- table(data$investor_combination)

# Set the threshold for less common combinations
threshold <- 100

# Identify less common combinations
less_common_combinations <- names(combination_counts[combination_counts < threshold])

# Replace less common combinations with "Other"
data$investor_combination <- as.character(data$investor_combination)  # Ensure it's character type
data$investor_combination[data$investor_combination %in% less_common_combinations] <- "Other"

# Verify the replacement
table(data$investor_combination)

# Convert investor_combination to a factor
data$investor_combination <- as.factor(data$investor_combination)


#We drop column not being used
columns_drop <- c("Barbara Corcoran", "Lori Greiner", "Robert Herjavec", 
                      "Kevin O'Leary", "Daymond John", 
                      "Mark Cuban", "Others")
data <- data[, !colnames(data) %in% columns_drop]
attach(data)

#There's some imbalance still in the categories three categories with 3% of representation
#we combined them as other to improve class balancing 

# Identify underrepresented categories
underrepresented_categories <- c("Seasonal and Event-Specific", "Technology and Gadgets", "Fitness and Health")

# Replace them with "Other"
data$category <- as.character(data$category)
data$category[data$category %in% underrepresented_categories] <- "Other"
data$category <- as.factor(data$category)
attach(data)

#After checking for imbalance categories we found seasons are still highly unbalanced
#we create to groups to adress this
# Group Seasons 1 to 3 as 'first seasons' and Seasons 4 to 6 as 'later seasons'
data$season_grouped <- ifelse(data$season %in% c(1, 2, 3), "first seasons", "later seasons")
data$season_grouped <- as.factor(data$season_grouped)

#Drop the original 'season' variable if it's no longer needed
data$season <- NULL  # Uncomment this line if you want to remove 'season'
attach(data)

names(data)

#Visualize outliers of numerical variables
#Exchange for stake
ggplot(data, aes(x = "", y = exchangeForStake)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red") +
  labs(title = "Boxplot of exchangeForStake", y = "exchangeForStake") +
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),  # Remove X-axis title
    axis.title.y = element_blank()   # Remove Y-axis title
  )
#Description length
ggplot(data, aes(x = "", y = description_length)) +
  geom_boxplot(fill = "lightgreen", outlier.color = "red") +
  labs(title = "Boxplot of Description Length", y = "Description Length") +
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),  # Remove X-axis title
    axis.title.y = element_blank()   # Remove Y-axis title
  )
#ratio_difference_asked
ggplot(data, aes(x = "", y = ratio_difference_asked)) +
  geom_boxplot(fill = "lightpink", outlier.color = "red") +
  labs(title = "Ratio Asked Valuation", y = "Ratio Asked Valuation") +
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),  # Remove X-axis title
    axis.title.y = element_blank()   # Remove Y-axis title
  )

#ratio_difference_asked
ggplot(data, aes(x = "", y = valuation)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red") +
  labs(title = "Valuation", y = "Ratio Asked Valuation") +
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),  # Remove X-axis title
    axis.title.y = element_blank()   # Remove Y-axis title
  )

#Function to remove outliers based on 3 standard deviations
remove_outliers_sd <- function(data, column_name) {
  mean_value <- mean(data[[column_name]], na.rm = TRUE)
  sd_value <- sd(data[[column_name]], na.rm = TRUE)
  lower_bound <- mean_value - 3 * sd_value
  upper_bound <- mean_value + 3 * sd_value

#Filter the data to remove outliers
  data <- data[data[[column_name]] >= lower_bound & data[[column_name]] <= upper_bound, ]
  return(data)}

# Remove outliers for 'ratio_difference_asked'
data <- remove_outliers_sd(data, "ratio_difference_asked")
attach(data)

# Remove outliers for 'description_length'
data <- remove_outliers_sd(data, "description_length")
attach(data)

# Remove outliers for 'exchangeForStake'
data <- remove_outliers_sd(data, "exchangeForStake")
attach(data)

# Remove outliers for 'Valuation'
data <- remove_outliers_sd(data, "valuation")
attach(data)

#Lastly before the model we scale the continuous variables
#data$exchangeForStake <- scale(data$exchangeForStake)
#attach(data)
#data$description_length <- scale(data$description_length)
#attach(data)
#data$ratio_difference_asked <- scale(data$ratio_difference_asked)
#attach(data)

processed_data =data
data = processed_data

table(investor_combination)
##########################################################
################ Logistic Regression #####################
##########################################################

# Ensure 'deal' is a factor with levels "no" and "yes"
data$deal <- factor(data$deal, levels = c(0, 1), labels = c("no", "yes"))


# Define cross-validation settings
control <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation

# Fit GLM with cross-validation
cv_model <- train( deal ~ category + description_length +  
                     season_part  + ratio_difference_asked+Multiple.Entreprenuers+investor_combination+season_grouped+CA+valuation,  data = data, method = "glm", family = "binomial", trControl = control)

# Print results
print(cv_model)

# Accuracy
accuracy_lr <- cv_model$results$Accuracy
print(paste("Cross-validated Accuracy:", round(accuracy_lr,3)))
#Accuracy 0.575

summary(cv_model)
#Removing not significant predictors: season_part, region, multiple.entrepreneurs, season_grouped, investor_combinations Pr<.10
# Fit GLM with cross-validation
cv_model <- train( deal ~ category + description_length + ratio_difference_asked+valuation,  data = data, method = "glm", family = "binomial", trControl = control)

# Print results
print(cv_model)

# Accuracy
accuracy_lr <- cv_model$results$Accuracy
print(paste("Cross-validated Accuracy:", round(accuracy_lr,3)))
#Accuracy 0.604
summary(cv_model)

# Fit the final GLM model (logistic regression)
final_model <- glm(deal ~ category + description_length + ratio_difference_asked + valuation, 
                   data = data, family = binomial)

# Use stargazer to create a summary table
stargazer(final_model, 
          type = "html", # Change to "html" or "latex" for other formats
          title = "Logistic Regression Results",
          dep.var.labels = "Deal (yes/no)",
          covariate.labels = c("Category", "Description Length", "Ratio Difference Asked", "Valuation"),
          out = NULL)
##########################################################
################Boosted with cross validation ############
##########################################################

set.seed(1)

# Convert categorical variables to factors
categorical_vars <- c("category", "season_part", "investor_combination", "season_grouped","CA")
data[categorical_vars] <- lapply(data[categorical_vars], as.factor)

data$deal_numeric <- ifelse(data$deal == "yes", 1, 0)
# Train the GBM model with 10-fold cross-validation
boosted_cv <- gbm(
  deal_numeric ~ category + exchangeForStake +
    Multiple.Entreprenuers + description_length + 
    season_part + investor_combination + season_grouped + CA+valuation,
  data = data,
  distribution = "bernoulli",
  n.trees = 10000,            # Number of trees
  shrinkage = 0.01,           # Learning rate
  interaction.depth = 4,      # Depth of each tree
  cv.folds = 10,              # 10-fold cross-validation
  verbose = FALSE             # Suppress detailed output
)
# Determine the optimal number of trees based on cross-validation
best_iter <- gbm.perf(boosted_cv, method = "cv",plot.it = FALSE)

cat("Optimal number of trees:", best_iter, "\n")

# Make predictions on the entire dataset using the optimal number of trees
test_probs <- predict(boosted_cv, newdata = data, n.trees = best_iter+180, type = "response")

# Convert probabilities to class labels using a threshold of 0.5
test_classes <- ifelse(test_probs > 0.5, 1, 0)

# Confusion Matrix
conf_matrix_b <- table(Predicted = test_classes, Actual = data$deal)
print("Confusion Matrix:")
print(conf_matrix_b)

# Calculate accuracy
accuracy_b <- sum(diag(conf_matrix_b)) / sum(conf_matrix_b)
cat("Cross-validated GBM Accuracy:", accuracy_b, "\n")
#0.70

# Display feature importance
feature_importance <- summary(boosted_cv, n.trees = best_iter)  # Use the optimal number of trees
print(feature_importance)

# Plot the feature importance
summary(boosted_cv, n.trees = best_iter, plotit = TRUE)

# Convert the feature importance to a data frame for easier handling
feature_importance_df <- as.data.frame(feature_importance)

# Generate a stargazer table for feature importance
stargazer(
  feature_importance_df, 
  type = "html",                # HTML output
  title = "Feature Importance for Boosted Model",
  summary = FALSE,              # Suppress summary statistics
  rownames = TRUE               # Include feature names as row labels
)

##########################################################
################ QDA Model  ##############################
##########################################################

# Ensure 'deal' is a factor
data$deal <- as.factor(data$deal)
data$category <- as.factor(data$category)
data$season_part <- as.factor(data$season_part)
data$investor_combination <- as.factor(data$investor_combination)
data$season_grouped <- as.factor(data$season_grouped)
data$region <- as.factor(data$region)
data$CA <- as.factor(data$CA)

# Prepare data for QDA
predictors <- c("exchangeForStake", "description_length", "ratio_difference_asked",
                "category", "season_part", "investor_combination", "season_grouped","valuation","CA")

# Ensure all predictors are numeric
data_qda <- data[, c("deal", predictors)]

# Remove rows with missing values
data_qda <- na.omit(data_qda)

# Identify numeric predictors
numeric_vars <- c("exchangeForStake", "description_length", "ratio_difference_asked","valuation")

# Identify categorical predictors
categorical_vars <- c("category", "season_part", "investor_combination", "season_grouped", "region","CA")

# Histograms for numeric variables
for (var in numeric_vars) {
  p <- ggplot(data_qda, aes_string(x = var)) +
    geom_histogram(bins = 30, fill = 'blue', color = 'black', alpha = 0.7) +
    labs(title = paste('Histogram of', var), x = var, y = 'Frequency') +
    theme_minimal()
  print(p)
}

# Bar charts for categorical variables
for (var in categorical_vars) {
  p <- ggplot(data_qda, aes_string(x = var)) +
    geom_bar(fill = 'green', color = 'black', alpha = 0.7) +
    labs(title = paste('Bar Chart of', var), x = var, y = 'Count') +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if necessary
  print(p)
}
#We can see that the distribution of the numeric variable are not distributed normally specially the 
#the description length therefore we try to use qde to capture more complex relationships

# Fit QDA model
qda_model <- qda(deal ~ ., data = data_qda)
qda_model
# Predict on the training data
qda_pred <- predict(qda_model, data_qda)

# Confusion matrix
conf_matrix_qda <- table(Actual = data_qda$deal, Predicted = qda_pred$class)
print("QDA Confusion Matrix:")
print(conf_matrix_qda)

# Calculate accuracy
qda_accuracy <- mean(qda_pred$class == data_qda$deal)
cat("QDA Model Accuracy:", qda_accuracy, "\n")

#######################Cross validation############
# Prepare data for modeling
predictors <- c("description_length", "ratio_difference_asked",
                "category", "season_part", "investor_combination", "season_grouped", "CA","valuation")

data_model <- data[, c("deal", predictors)]

# Remove rows with missing values
data_model <- na.omit(data_model)

# Identify numeric and categorical predictors
numeric_vars <- c("description_length", "ratio_difference_asked","valuation")
categorical_vars <- setdiff(predictors, numeric_vars)

# Scale numeric predictors
data_model[, numeric_vars] <- scale(data_model[, numeric_vars])

# Ensure predictors are factors where necessary
data_model[, categorical_vars] <- lapply(data_model[, categorical_vars], as.factor)


train_control <- trainControl(method = "cv", number = 10)
set.seed(123)  # For reproducibility
qda_cv_model <- train(deal ~ ., data = data_model,
                      method = "qda",
                      trControl = train_control)

# Make predictions on the training data
qda_cv_pred <- predict(qda_cv_model, data_model)

# Confusion matrix
conf_matrix_qda_cv <- confusionMatrix(qda_cv_pred, data_model$deal)
print(conf_matrix_qda_cv)

# Extract key metrics from the confusion matrix
conf_matrix_metrics <- data.frame(
  Metric = c("Accuracy", "Kappa", "Sensitivity", "Specificity", "Precision", "F1 Score"),
  Value = c(
    conf_matrix_qda_cv$overall["Accuracy"],
    conf_matrix_qda_cv$overall["Kappa"],
    conf_matrix_qda_cv$byClass["Sensitivity"],
    conf_matrix_qda_cv$byClass["Specificity"],
    conf_matrix_qda_cv$byClass["Pos Pred Value"], # Precision
    2 * ((conf_matrix_qda_cv$byClass["Pos Pred Value"] * conf_matrix_qda_cv$byClass["Sensitivity"]) /
           (conf_matrix_qda_cv$byClass["Pos Pred Value"] + conf_matrix_qda_cv$byClass["Sensitivity"])) # F1
  )
)

# Use stargazer to display the table
stargazer(
  conf_matrix_metrics,
  type = "html",                # Output HTML code
  title = "QDA Model Results",
  summary = FALSE,              # Do not display summary stats
  rownames = FALSE )             # Remove row names

##########################################################
######### PCA to understand deal characteristics#########
##########################################################
# Read the dataset
data <- read.csv("D:/01.Educación/03.MMA/03.Fall 2024/01.Multivariate Statistics (MGSC 661)/07.Final project/Dataset 3 — Shark tank pitches.csv")

data$description_length <- nchar(data$description)
data$deal <- as.numeric(data$deal)  

data$Multiple.Entreprenuers <- as.numeric(data$Multiple.Entreprenuers)
# Subset numeric variables for PCA
pca_vars <- data[, c("askedFor", "exchangeForStake", "valuation","season" ,"episode","description_length")]

pca_shark <- prcomp(pca_vars, scale = TRUE)
data$deal <- factor(data$deal, levels = c(0, 1), labels = c("No Deal", "Deal"))

# Calculate the percentage of variance explained by each component
explained_variance <- round((pca_shark$sdev^2 / sum(pca_shark$sdev^2)) * 100, 2)

# Create axis labels with variance percentages
x_label <- paste0("PC1 (", explained_variance[1], "%)")
y_label <- paste0("PC2 (", explained_variance[2], "%)")


#Plot PCA
autoplot(
  pca_shark, 
  data = data, 
  colour = 'deal',  # Color points by 'deal'
  loadings = TRUE,  # Include variable loadings (arrows)
  loadings.label = TRUE,  # Show labels for loadings  
 loadings.colour ="#6A5ACD",
 loadings.label.colour = "black"# Color loading labels,
) +
  labs(
    title = "PCA of Shark Tank Pitches",
    x = x_label,
    y = y_label,
    color = "Deal"
  ) +
  scale_color_manual(values = c("#08306b", "#4292c6")) +  # Dark blue for "No Deal" and light blue for "Deal"
  theme_minimal()


##########################################################
######### K-Means difference in deals made################
##########################################################

# Read the dataset
data_clusters <- read.csv("D:/01.Educación/03.MMA/03.Fall 2024/01.Multivariate Statistics (MGSC 661)/07.Final project/Dataset 3 — Shark tank pitches.csv")

# Select columns for TV information (episode, season, and deal)
Tv_info <- data_clusters[, c("episode", "season", "deal")]

# Perform K-Means clustering
set.seed(1)  # Set seed for reproducibility
km.2 <- kmeans(Tv_info[, c("episode", "season")], centers = 2)
km.3 <- kmeans(Tv_info[, c("episode", "season")], centers = 3)
km.4 <- kmeans(Tv_info[, c("episode", "season")], centers = 4)

# Print K-Means metrics
cat("Total within-cluster sum of squares:", km.2$tot.withinss, "\n")
cat("Total within-cluster sum of squares:", km.3$tot.withinss, "\n")
cat("Total within-cluster sum of squares:", km.4$tot.withinss, "\n")


#in three parts the variation has a decrease from 2 to 3 but for 4 the
#change is not that noticeable, stay with 3 clusters
#This make sense similar to our categorical variable created dividing the season

# Add cluster information to Tv_info
Tv_info$clusters <- as.factor(km.3$cluster)

# Convert deal to a factor for visualization
Tv_info$deal <- as.factor(Tv_info$deal)

# Scatterplot with clusters and deal information
plot <- ggplot(Tv_info, aes(x = season, y = episode)) +
  geom_point(aes(color = clusters, shape = deal), size = 3) +
  labs(
    title = "K-Means Clustering of Shark Tank Projects",
    x = "Season",
    y = "Episode",
    color = "Cluster",
    shape = "Deal"
  ) +
  theme_minimal()

# Display the plot
print(plot)






