## LSE Data Analytics Online Career Accelerator 
# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment 5 scenario
## Turtle Games’s sales department has historically preferred to use R when performing 
## sales analyses due to existing workflow systems. As you’re able to perform data analysis 
## in R, you will perform exploratory data analysis and present your findings by utilising 
## basic statistics and plots. You'll explore and prepare the data set to analyse sales per 
## product. The sales department is hoping to use the findings of this exploratory analysis 
## to inform changes and improvements in the team. (Note that you will use basic summary 
## statistics in Module 5 and will continue to go into more detail with descriptive 
## statistics in Module 6.)

################################################################################


################################################################################

# Your code here.



# Load necessary libraries
library(dplyr)    # Data manipulation
library(ggplot2)  # Visualization
library(readr)    # Reading data

# Load the data
sales_df <- read.csv("turtle_reviews_clean.csv", header = TRUE)

# Initial Data Exploration
# Preview the data
head(sales_df)

# Get an overview of the dataset
glimpse(sales_df)

# Summary statistics
summary(sales_df)


###############################################################################

# Check for missing values
missing_values <- colSums(is.na(sales_df))
missing_values

# Check for possible duplicate customers by gender and age (without customer_id)
possible_duplicates <- sales_df %>%
  group_by(gender, age, remuneration, education) %>%
  summarise(appearance_count = n()) %>%
  filter(appearance_count > 1)

# View if there are likely repeated customers
if (nrow(possible_duplicates) > 0) {
  print("There are likely repeated customers (multiple interactions/purchases).")
  print(possible_duplicates)
} else {
  print("Each customer appears unique based on available demographic data.")
}


###############################################################################

#### Histograms for Key Variables ####

# Histogram for Age
ggplot(sales_df, aes(x = age)) + 
  geom_histogram(binwidth = 5, fill = "brown", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Customer Age", x = "Age (Years)", y = "Number of Customers") +
  scale_x_continuous(breaks = seq(0, 80, by = 5)) +  
  scale_y_continuous(breaks = seq(0, 500, by = 50)) +  
  theme_minimal()

# Histograms for Remuneration
ggplot(sales_df, aes(x = remuneration)) + 
  geom_histogram(binwidth = 5, fill = "lightgreen", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Remuneration (k£)", x = "Remuneration (k£)", y = "Number of Customers") +
  scale_x_continuous(breaks = seq(0, 120, by = 10)) +  
  scale_y_continuous(breaks = seq(0, 500, by = 50)) + 
  theme_minimal()

# Histograms for Spending Score
ggplot(sales_df, aes(x = spending_score)) + 
  geom_histogram(binwidth = 5, fill ="peachpuff", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Spending Score", x = "Spending Score", y = "Number of Customers") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +  
  scale_y_continuous(breaks = seq(0, 500, by = 50)) +  
  theme_minimal()

# Histogram for Loyalty Points
ggplot(sales_df, aes(x = loyalty_points)) + 
  geom_histogram(binwidth = 200, fill = "lavender", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Loyalty Points", x = "Loyalty Points", y = "Number of Customers") +
  scale_x_continuous(breaks = seq(0, 7000, by = 500)) +  
  scale_y_continuous(breaks = seq(0, 400, by = 50)) +  
  theme_minimal()


#### Boxplots for Outliers in Key Variables####

# Boxplots for Age 
ggplot(sales_df, aes(y = age)) + 
  geom_boxplot(fill = "brown", color = "black") +
  labs(title = "Boxplot of Customer Age", y = "Age (Years)") +
  theme_minimal()

# Boxplots for Remuneration
ggplot(sales_df, aes(y = remuneration)) + 
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Boxplot of Remuneration (k£)", y = "Remuneration (k£)") +
  theme_minimal()

# Boxplots for Spending Score
ggplot(sales_df, aes(y = spending_score)) + 
  geom_boxplot(fill = "peachpuff", color = "black") +
  labs(title = "Boxplot of Spending Score", y = "Spending Score") +
  theme_minimal()

# Boxplots for Loyalty Points
ggplot(sales_df, aes(y = loyalty_points)) + 
  geom_boxplot(fill = "lavender", color = "black") +
  labs(title = "Boxplot of Loyalty Points", y = "Loyalty Points") +
  theme_minimal()

#####Calculate IQR-Based Outlier Bounds for loyalty_points####

# Calculate the first quartile (Q1), third quartile (Q3), and interquartile range (IQR)
Q1 <- quantile(sales_df$loyalty_points, 0.25)
Q3 <- quantile(sales_df$loyalty_points, 0.75)
IQR <- Q3 - Q1

# Define the lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Output the IQR and the bounds
print(paste("First Quartile (Q1):", round(Q1, 2)))
print(paste("Third Quartile (Q3):", round(Q3, 2)))
print(paste("Interquartile Range (IQR):", round(IQR, 2)))
print(paste("Lower Bound (Outlier threshold):", round(lower_bound, 2)))
print(paste("Upper Bound (Outlier threshold):", round(upper_bound, 2)))


#### Bar plots for categorical variables ####

# Bar Plot for Gender as a %

# Compute percentages for Gender
gender_percent <- sales_df %>%
  count(gender) %>%
  mutate(percentage = n / sum(n) * 100)

# Print Gender Percentages
print(gender_percent)

# Bar Plot for Gender (%)
ggplot(gender_percent, aes(x = gender, y = percentage, fill = gender)) + 
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Gender (%)", x = "Gender", y = "Percentage (%)") +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +  
  theme_minimal() +
  scale_fill_manual(values = c("lightpink","lightblue"))

# Compute percentages for Gender
gender_percent <- sales_df %>%
  count(gender) %>%
  mutate(percentage = n / sum(n) * 100)

# Add a label column that includes both gender and percentage
gender_percent <- gender_percent %>%
  mutate(label = paste0(gender, " (", round(percentage, 1), "%)"))

# Pie chart with labels and no legend
ggplot(gender_percent, aes(x = "", y = percentage, fill = gender)) + 
  geom_bar(stat = "identity", width = 1, color = "black", alpha = 0.7) +
  coord_polar(theta = "y") +  # Convert to pie chart
  labs(title = "Gender Distribution (%)") +
  theme_void() +  # Clean chart without axes
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +  
  scale_fill_manual(values = c("Female" = "lightpink", "Male" = "lightblue")) +  
  theme(legend.position = "none")  


# Bar Plot for Education Levels as %

# Compute percentages for Education
education_percent <- sales_df %>%
  count(education) %>%
  mutate(percentage = n / sum(n) * 100)

# Print Education %
print(education_percent)

# Bar Plot for Education (%)
ggplot(education_percent, aes(x = education, y = percentage, fill = education)) + 
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Education Levels (%)", x = "Education Level", y = "Percentage (%)") +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) + 
  theme_minimal() +
  scale_fill_manual(values = c("lightgreen", "lightyellow", "plum1", "peachpuff", "lightblue"))

###############################################################################

####Do higher spending scores correlate with higher loyalty points?####


# Scatterplot for Spending Score vs Loyalty Points, with gender comparison
ggplot(sales_df, aes(x = spending_score, y = loyalty_points, color = gender)) + 
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Spending Score vs Loyalty Points by Gender", x = "Spending Score", y = "Loyalty Points") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 100, by = 5)) +  # Adjust x-axis (Spending Score) intervals from 0 to 100, with steps of 5
  scale_y_continuous(breaks = seq(0, 7000, by = 1000))  # Adjust y-axis (Loyalty Points) intervals from 0 to 7000, with steps of 1000

# Correlation between Spending Score and Loyalty Points (Overall)
correlation <- cor(sales_df$spending_score, sales_df$loyalty_points, use = "complete.obs")
print(paste("Overall Correlation between Spending Score and Loyalty Points:", round(correlation, 2)))

# Correlation by Gender
correlation_male <- cor(sales_df$spending_score[sales_df$gender == "Male"], 
                        sales_df$loyalty_points[sales_df$gender == "Male"], use = "complete.obs")
correlation_female <- cor(sales_df$spending_score[sales_df$gender == "Female"], 
                          sales_df$loyalty_points[sales_df$gender == "Female"], use = "complete.obs")

print(paste("Correlation for Males:", round(correlation_male, 2)))
print(paste("Correlation for Females:", round(correlation_female, 2)))


#### How does customer behavior (spending score and loyalty points) vary by gender?####

# Boxplot for Spending Score by Gender
ggplot(sales_df, aes(x = gender, y = spending_score, fill = gender)) + 
  geom_boxplot() +
  labs(title = "Spending Score by Gender", x = "Gender", y = "Spending Score") +
  theme_minimal()

# Boxplot for Loyalty Points by Gender
ggplot(sales_df, aes(x = gender, y = loyalty_points, fill = gender)) + 
  geom_boxplot() +
  labs(title = "Loyalty Points by Gender", x = "Gender", y = "Loyalty Points") +
  theme_minimal()

#### How does education level affect spending behavior and loyalty ?####

# Boxplot for Spending Score by Education Level
ggplot(sales_df, aes(x = education, y = spending_score, fill = education)) + 
  geom_boxplot() +
  labs(title = "Spending Score by Education Level", x = "Education Level", y = "Spending Score") +
  theme_minimal()

# Boxplot for Loyalty Points by Education Level
ggplot(sales_df, aes(x = education, y = loyalty_points, fill = education)) + 
  geom_boxplot() +
  labs(title = "Loyalty Points by Education Level", x = "Education Level", y = "Loyalty Points") +
  theme_minimal()

#### Is there a significant relationship between age and spending or loyalty points?####

# Scatterplot for Age vs Spending Score by Gender
ggplot(sales_df, aes(x = age, y = spending_score, color = gender)) + 
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Age vs Spending Score by Gender", x = "Age", y = "Spending Score") +
  theme_minimal()

# Scatterplot for Age vs Loyalty Points by Gender
ggplot(sales_df, aes(x = age, y = loyalty_points, color = gender)) + 
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Age vs Loyalty Points by Gender", x = "Age", y = "Loyalty Points") +
  theme_minimal()

### Which age group has the highest purchase activity, and how does gender impact this?###

# Adjust age groups to include all possible age ranges
sales_df <- sales_df %>%
  mutate(age_group = cut(age, 
                         breaks = c(0, 18, 25, 35, 45, 55, 65, 75, Inf), 
                         labels = c("<18", "18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")))

# Group by product_code, age_group, and gender, and summarize total purchases (counts of purchases)
demographics_summary <- sales_df %>%
  group_by(age_group, gender) %>%
  summarise(total_purchases = n(), .groups = 'drop')  

# View the summary data 
head(demographics_summary)

# Bar chart for Total Purchases by Age Group and Gender across all products
ggplot(demographics_summary, aes(x = age_group, y = total_purchases, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  labs(title = "Total Purchases by Age Group and Gender (Across All Products)", 
       x = "Age Group", y = "Total Purchases") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  scale_fill_manual(values = c("Female" = "lightpink", "Male" = "lightblue"))  


#### How does income vary across different age groups and genders, and does this income influence spending behavior?####

# Scatterplot: Remuneration vs Loyalty Points by Gender 
ggplot(sales_df, aes(x = remuneration, y = loyalty_points, color = gender)) + 
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Remuneration vs Loyalty Points by Gender", x = "Remuneration (k£)", y = "Loyalty Points") +
  scale_x_continuous(breaks = seq(0, 120, by = 10)) +  
  scale_y_continuous(breaks = seq(0, 7000, by = 1000)) +  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),          
    axis.line = element_line(color = "black"), 
    axis.ticks = element_line(color = "black")  
  )


# Boxplot: Remuneration by Age Group and Gender 
ggplot(sales_df, aes(x = age_group, y = remuneration, fill = gender)) + 
  geom_boxplot(alpha = 0.7) +
  labs(title = "Remuneration by Age Group and Gender", x = "Age Group", y = "Remuneration (k£)") +
  theme_minimal() +
  scale_fill_manual(values = c("Male" = "lightblue", "Female" = "lightpink")) +
  theme(
    panel.grid = element_blank(),  # Remove background grid
    axis.line = element_line(color = "black"),  # Keep x and y axis lines
    axis.ticks = element_line(color = "black")  # Keep axis ticks
  )


### How does a customer's income (remuneration) affect their spending behavior, and are higher-income customers likely to spend more?###

# Scatter Plot: Spending Score vs. Remuneration
ggplot(sales_df, aes(x = remuneration, y = spending_score)) + 
  geom_point(color = "darkgreen", alpha = 0.6) +
  labs(title = "Spending Score vs. Remuneration", x = "Remuneration (k£)", y = "Spending Score") +
  scale_x_continuous(breaks = seq(0, 120, by = 10)) +  
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +  
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black")  # Keep only axis lines
  )



### Which products are most/least popular overall, and how does gender influence this?####

##Top 10 and Bottom 10 Products by Purchase Count (with Gender)##

# Count the number of males and females for each product code
filtered_gender_count <- sales_df %>%
  group_by(product_code, gender) %>%
  summarise(count = n()) %>%
  ungroup()

# Extract top 10 and bottom 10 products based on total purchase count
total_count_by_product <- filtered_gender_count %>%
  group_by(product_code) %>%
  summarise(total_count = sum(count)) %>%
  arrange(desc(total_count))

top_10_products <- head(total_count_by_product, 10)
bottom_10_products <- tail(total_count_by_product, 10)

# Filter for top 10 and bottom 10 products
filtered_gender_count <- filtered_gender_count %>%
  filter(product_code %in% c(top_10_products$product_code, bottom_10_products$product_code))

# Add a new column to distinguish top and bottom products and reorder the category
filtered_gender_count <- filtered_gender_count %>%
  mutate(category = factor(ifelse(product_code %in% top_10_products$product_code, "Top 10", "Bottom 10"), 
                           levels = c("Top 10", "Bottom 10")))

# Reorder the product codes by total purchase count
filtered_gender_count <- filtered_gender_count %>%
  mutate(product_code = factor(product_code, levels = total_count_by_product$product_code))

# Create the stacked bar plot with facet graphs for Top 10 and Bottom 10
ggplot(filtered_gender_count, aes(x = product_code, y = count, fill = gender)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Top 10 and Bottom 10 Products by Purchase Count (with Gender)", 
       x = "Product Code", y = "Purchase Count") +
  theme_minimal() +
  scale_fill_manual(values = c("Male" = "lightblue", "Female" = "lightpink")) +
  facet_wrap(~ category, scales = "free_x", strip.position = "top") +  
  theme(strip.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1))  


#### Which products generate higher/lower loyalty points or spending?####

# Check how many unique product codes are in the dataset
num_unique_products <- sales_df %>%
  summarise(unique_products = n_distinct(product_code))

# Display the result
num_unique_products


## Top 10 and Bottom 10 Products by Spending Score

# Calculate the average spending score for each product
avg_spending_by_product <- sales_df %>%
  group_by(product_code) %>%
  summarise(avg_spending = mean(spending_score)) %>%
  arrange(desc(avg_spending))

# Extract top 10 and bottom 10 products by average spending score
top_10_spending <- head(avg_spending_by_product, 10)
bottom_10_spending <- tail(avg_spending_by_product, 10)

# Combine top 10 and bottom 10 into one dataset for plotting
top_bottom_spending <- rbind(top_10_spending, bottom_10_spending)

# Add a new column to distinguish top and bottom products
top_bottom_spending <- top_bottom_spending %>%
  mutate(category = ifelse(row_number() <= 10, "Top 10", "Bottom 10"))

# Bar plot for top and bottom 10 products by average spending score with two different shades
ggplot(top_bottom_spending, aes(x = reorder(factor(product_code), avg_spending), y = avg_spending, fill = category)) + 
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Top 10 and Bottom 10 Products by Average Spending Score", 
       x = "Product Code", y = "Average Spending Score") +
  theme_minimal() +
  scale_fill_manual(values = c("Top 10" = "orange", "Bottom 10" = "orangered")) +
  coord_flip()  # Flip for better readability

#### 

## Top 10 and Bottom 10 Products by Loyalty Points

# Calculate the average loyalty points for each product
avg_loyalty_by_product <- sales_df %>%
  group_by(product_code) %>%
  summarise(avg_loyalty = mean(loyalty_points)) %>%
  arrange(desc(avg_loyalty))

# Extract top 10 and bottom 10 products by average loyalty points
top_10_loyalty <- head(avg_loyalty_by_product, 10)
bottom_10_loyalty <- tail(avg_loyalty_by_product, 10)

# Combine top 10 and bottom 10 into one dataset for plotting
top_bottom_loyalty <- rbind(top_10_loyalty, bottom_10_loyalty)

# Add a new column to distinguish top and bottom products
top_bottom_loyalty <- top_bottom_loyalty %>%
  mutate(category = ifelse(row_number() <= 10, "Top 10", "Bottom 10"))

# Bar plot for top and bottom 10 products by average loyalty points
ggplot(top_bottom_loyalty, aes(x = reorder(factor(product_code), avg_loyalty), y = avg_loyalty, fill = category)) + 
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Top 10 and Bottom 10 Products by Average Loyalty Points", x = "Product Code", y = "Average Loyalty Points") +
  theme_minimal() +
  scale_fill_manual(values = c("Top 10" = "lightblue", "Bottom 10" = "darkblue")) +
  coord_flip()  


# Scatter plot of Spending Score vs Loyalty Points by Education Level with line of best fit and small dots using Set2 palette
ggplot(sales_df, aes(x = spending_score, y = loyalty_points, color = education)) +
  geom_jitter(alpha = 0.6, width = 0.5, height = 50, shape = 16, size = 1) +  # Small dots (shape = 16), set small size
  geom_smooth(method = "lm", se = FALSE) +  # Add line of best fit (without confidence interval shading)
  labs(title = "Spending Score vs Loyalty Points by Education Level", 
       x = "Spending Score", 
       y = "Loyalty Points", 
       color = "Education Level") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(), 
    panel.background = element_blank(),  
    axis.line = element_line(color = "black"), 
    axis.ticks = element_line(color = "black")  
  ) +
  scale_color_brewer(palette = "Set2")

###############################################################################

# Distribution of Customer Age:
# The age distribution is skewed towards younger age groups, with the majority of customers falling between 25 and 45 years.
# The highest number of customers are in the 35-40 age range.
# There is a notable drop in the number of customers aged over 55, indicating lower engagement from older age groups.

# Boxplot of Customer Age:
# The median age is around 35-40 years, with a fairly symmetrical spread.
# There are no significant outliers, suggesting a well-distributed dataset in terms of age.

# Distribution of Spending Score:
# The spending score is fairly uniformly distributed across the range, with peaks around scores of 40-50 and 70-80.
# This suggests a diverse range of customer spending behaviors, with no single group dominating.

# Boxplot of Spending Score:
# The median spending score is around 50, with a fairly even spread.
# There are no extreme outliers, indicating consistent spending behavior across the customer base.

# Distribution of Loyalty Points:
# The loyalty points distribution is right-skewed, with a majority of customers accumulating fewer than 2000 points.
# A small group of high-loyalty customers with points exceeding 5000 is evident, indicating a minority of very engaged customers.

# Boxplot of Loyalty Points:
# The boxplot shows several high outliers above 5000 loyalty points, indicating that some customers are much more engaged than the average.
# The median loyalty points are around 2000.

# Distribution of Remuneration:
# The remuneration distribution shows a concentration around £30,000-50,000, with a slight skew towards lower income groups.
# There is a drop in the number of customers earning above £70,000.

# Boxplot of Remuneration:
# The median remuneration is around £40,000, with no extreme outliers.
# The spread is relatively even, suggesting a well-distributed income range.

# Distribution of Education Levels:
# The majority of customers (over 40%) have a graduate education level.
# There is a smaller percentage of customers with basic or postgraduate education.

# Distribution of Gender:
# Female customers make up a higher proportion of the customer base (around 55%) compared to male customers.
# This suggests that females are more engaged with the business than males.

# Spending Score vs Loyalty Points by Gender:
# There is a positive correlation between spending score and loyalty points across both genders.
# Female and male customers show similar spending and loyalty trends, with some variation in loyalty points for higher spending scores.
# Higher spending scores tend to result in higher loyalty points for both genders.

# Spending Score by Education Level:
# Customers with a basic education have the highest median spending score, followed by graduate-level customers.
# Customers with diploma and postgraduate education levels show the lowest spending scores.
# The variation in spending score decreases with higher education levels, indicating more consistent spending behavior in those groups.

# Loyalty Points by Education Level:
# Customers with a basic education accumulate the most loyalty points on average.
# Graduate, PhD, and postgraduate customers exhibit similar distributions of loyalty points, with smaller ranges and fewer outliers.
# The boxplot shows many outliers in the diploma group, indicating some customers with unusually high loyalty points despite the lower median.

#  Age vs Spending Score by Gender:
# There is a slight negative correlation between age and spending score for both genders.
# Younger customers (aged below 40) generally have higher spending scores compared to older customers.
# Female and male customers exhibit similar patterns, with no significant gender-based differences in spending as age increases.

# Top 10 and Bottom 10 Products by Purchase Count (with Gender):
# Products 1012 and 1031 are the most purchased products, particularly popular among female customers.
# The bottom 10 products show balanced engagement from both genders but have low overall purchase counts.
# Product preferences differ significantly by gender, with females dominating purchases in the top-performing products.

# Top 10 and Bottom 10 Products by Average Loyalty Points:
# Product 9080 leads in average loyalty points, followed closely by products 8962 and 4399.
# The bottom 10 products have very low average loyalty points, with several having no loyalty points at all.
# The top-performing products provide high engagement in terms of loyalty points, suggesting a strong correlation between specific products and customer loyalty.

# Plot: Top 10 and Bottom 10 Products by Average Spending Score:
# Product 5510 has the highest average spending score, followed by products 6466 and 3153.
# The bottom 10 products exhibit much lower spending scores, indicating limited customer interest or engagement with these items.
# This plot highlights a clear distinction between high and low-performing products in terms of customer spending.


###############################################################################

## To investigate customer behaviour and the effectiveness of the current loyalty program based 
## on the work completed 

##  - Can we predict loyalty points given the existing features using a relatively simple MLR model?
##  - Do I have confidence in the model results (Goodness of fit evaluation)
##  - Where should the business focus their marketing efforts?
##  - How could the loyalty program be improved?
##  - How could the analysis be improved?

################################################################################


# Load necessary libraries
library(dplyr)    # Data manipulation
library(ggplot2)  # Visualization
library(readr)    # Reading data

# Load the data
sales_df <- read.csv("turtle_reviews_clean.csv", header = TRUE)

# Initial Data Exploration
# Preview the data
head(sales_df)

# Get an overview of the dataset
glimpse(sales_df)

# Summary statistics
summary(sales_df)

# Check for missing values
missing_values <- colSums(is.na(sales_df))
missing_values

###############################################################################

# Compute descriptive statistics on age
summary(sales_df$age)
mean(sales_df$age)
median(sales_df$age)
min(sales_df$age)
max(sales_df$age)
quantile(sales_df$age, 0.25)
quantile(sales_df$age, 0.75)
var(sales_df$age)
sd(sales_df$age)

# Visualization for age
boxplot(sales_df$age, main = "Boxplot of Age Distribution", ylab = "Age")
hist(sales_df$age, main = "Histogram of Age Distribution", xlab = "Age", col = "brown")

# QQ plot for age
qqnorm(sales_df$age, main = "QQ Plot of Age Distribution")
qqline(sales_df$age)

# Normality test and distribution
shapiro.test(sales_df$age)
skewness(sales_df$age)
kurtosis(sales_df$age)


# Age - observations

# The skewness value of 0.61 suggests that the age distribution is moderately right-skewed, 
# That means there are more younger customers than older customers, but with a tail extending toward the older ages.
# The kurtosis of 2.81 is slightly lower than 3, indicating a distribution that is close to normal but with lighter tails.
# The Shapiro-Wilk normality test returned a p-value < 2.2e-16, which suggests the age data does not follow a normal distribution.

###############################################################################

# Compute descriptive statistics on spending_score
summary(sales_df$spending_score)
mean(sales_df$spending_score)
median(sales_df$spending_score)
min(sales_df$spending_score)
max(sales_df$spending_score)
quantile(sales_df$spending_score, 0.25)
quantile(sales_df$spending_score, 0.75)
var(sales_df$spending_score)
sd(sales_df$spending_score)

# Visualization for spending_score
boxplot(sales_df$spending_score, main = "Boxplot of Spending Score", ylab = "Spending Score")
hist(sales_df$spending_score, main = "Histogram of Spending Score", xlab = "Spending Score", col = "peachpuff")

# QQ plot for spending_score
qqnorm(sales_df$spending_score, main = "QQ Plot of Spending Score")
qqline(sales_df$spending_score)

# Normality test and distribution
shapiro.test(sales_df$spending_score)
skewness(sales_df$spending_score)
kurtosis(sales_df$spending_score)

# Spending Score - observations

# Skewness: -0.04 indicates almost no skew, suggesting a near-symmetric distribution
# Kurtosis: 2.11 indicates a distribution slightly flatter than normal
# Shapiro-Wilk test p-value < 2.2e-16 indicates that the spending score distribution is not perfectly normal.
# The boxplot shows a relatively compact distribution, with few or no extreme outliers.
# The histogram displays a nearly symmetric distribution centered around the middle value (50).
# The QQ plot suggests a mild deviation from normality, but overall, the spending scores follow a fairly balanced distribution.

###############################################################################

# Compute descriptive statistics on Loyality points
summary(sales_df$loyalty_points)
mean(sales_df$loyalty_points)
median(sales_df$loyalty_points)
min(sales_df$loyalty_points)
max(sales_df$loyalty_points)
quantile(sales_df$loyalty_points, 0.25)
quantile(sales_df$loyalty_points, 0.75)
var(sales_df$loyalty_points)
sd(sales_df$loyalty_points)

# Visualization for loyalty_points
boxplot(sales_df$loyalty_points, main = "Boxplot of Loyalty Points", ylab = "Loyalty Points")
hist(sales_df$loyalty_points, main = "Histogram of Loyalty Points", xlab = "Loyalty Points", col = "lavender")

# QQ plot for loyalty_points
qqnorm(sales_df$loyalty_points, main = "QQ Plot of Loyalty Points")
qqline(sales_df$loyalty_points)

# Normality test and distribution
shapiro.test(sales_df$loyalty_points)
skewness(sales_df$loyalty_points)
kurtosis(sales_df$loyalty_points)

# Loyalty Points- observations

# Skewness: 1.46 indicates positive skew  which means a tail on the right side of the distribution
# Kurtosis: 4.71 suggests a more peaked distribution compared to a normal distribution
# Shapiro-Wilk test p-value < 2.2e-16 indicates the distribution is not normal
# Boxplot shows the presence of high outliers (loyalty points towards the upper range)
# Histogram and QQ plot suggest the positive skewness in the distribution, 
# with more customers having lower loyalty points and fewer with very high points.

################################################################################

# Compute descriptive statistics on remuneration
summary(sales_df$remuneration)
mean(sales_df$remuneration)
median(sales_df$remuneration)
min(sales_df$remuneration)
max(sales_df$remuneration)
quantile(sales_df$remuneration, 0.25)
quantile(sales_df$remuneration, 0.75)
var(sales_df$remuneration)
sd(sales_df$remuneration)

# Visualization for remuneration
boxplot(sales_df$remuneration, main = "Boxplot of Remuneration", ylab = "Remuneration")
hist(sales_df$remuneration, main = "Histogram of Remuneration", xlab = "Remuneration", col = "lightgreen")

# QQ plot for remuneration
qqnorm(sales_df$remuneration, main = "QQ Plot of Remuneration")
qqline(sales_df$remuneration)

# Normality test and distribution
shapiro.test(sales_df$remuneration)
skewness(sales_df$remuneration)
kurtosis(sales_df$remuneration)

# Remuneration (x1000£)- observations

# Skewness: 0.41 - slightly right-skewed
# Kurtosis: 2.59 - indicating a distribution slightly flatter than normal.
# Shapiro-Wilk test p-value < 2.2e-16 indicates the distribution is not normal.
# Boxplot suggests the presence of some outliers in higher remuneration values.
# Histogram shows a slight right skew with more people clustered around lower remuneration values.
# QQ plot shows deviation from normality at the tails.

################################################################################

# Create a Multiple Linear Regression Model
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Fit the multiple linear regression model
mlr_model <- lm(loyalty_points ~ age + remuneration + spending_score, data = sales_df)

# View the model summary
summary(mlr_model)

################################################################################

# Create predictions using the model
sales_df$predicted_loyalty <- predict(mlr_model, newdata = sales_df)

# Plot actual vs predicted loyalty points with customizations
ggplot(sales_df, aes(x = loyalty_points, y = predicted_loyalty)) +
  geom_point(color = 'thistle') +
  geom_smooth(method = 'lm', color = 'blue', se = FALSE) +  # `se = FALSE` removes the grey confidence interval
  labs(title = "Actual vs Predicted Loyalty Points",
       x = "Actual Loyalty Points",
       y = "Predicted Loyalty Points") +
  theme_minimal() +  # Use a minimal theme without grid
  scale_x_continuous(breaks = seq(0, max(sales_df$loyalty_points), by = 500)) +  # X-axis intervals in 500
  scale_y_continuous(breaks = seq(0, max(sales_df$predicted_loyalty), by = 500)) +  # Y-axis intervals in 500
  theme(
    panel.grid = element_blank(),  # Remove background grid
    axis.line.x = element_line(color = "black"),  # Black axis line on x
    axis.line.y = element_line(color = "black")   # Black axis line on y
  )


################################################################################

# Evaluating the model performance 

# Calculate Mean Squared Error (MSE)
predicted_loyalty <- predict(mlr_model, sales_df)
mse <- mean((sales_df$loyalty_points - predicted_loyalty)^2)
print(paste("Mean Squared Error:", mse))

# Calculate Mean Absolute Error (MAE)
mae <- mean(abs(sales_df$loyalty_points - predicted_loyalty))
print(paste("Mean Absolute Error:", mae))

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)
print(paste("Root Mean Squared Error:", rmse))

################################################################################ 
# Checking for Multicollinearity Using Variance Inflation Factor (VIF)

# Install the 'car' package if you haven't already
install.packages("car")

# Load the car package for VIF calculation
library(car)

# Calculate VIF for the model
vif(mlr_model)

################################################################################
# Residual Analysis - Checking Residual Normality:

# Get residuals from the model
residuals <- residuals(mlr_model)

# Histogram of residuals
hist(residuals, main="Histogram of Residuals", col="lightblue", xlab="Residuals")

# Q-Q plot of residuals 
qqnorm(residuals, pch = 16, col = "thistle")  
qqline(residuals, col = "blue")  


# Shapiro-Wilk test for normality of residuals
shapiro.test(residuals)

################################################################################
# Residual Analysis - Checking for Homoscedasticity (Constant Variance of Residuals):
# Plot residuals vs fitted values 
plot(fitted(mlr_model), residuals, 
     xlab="Fitted Values", 
     ylab="Residuals", 
     main="Residuals vs Fitted Values",
     col="thistle",
     pch=10)  # Set pch=4 for solid crosses
abline(h=0, col="blue", lwd=2)  # Horizontal line at zero

# Residual Analysis- Breusch-Pagan Test for Homoscedasticity:
# Install the lmtest package if not installed
install.packages("lmtest")

# Load the lmtest package for the Breusch-Pagan test
library(lmtest)

# Perform the Breusch-Pagan test
bptest(mlr_model)


# Residual Analysis - Checking for Homoscedasticity (Constant Variance of Residuals):
# Plot residuals vs fitted values 
plot(fitted(mlr_model), residuals, 
     xlab="Fitted Values", 
     ylab="Residuals", 
     main="Residuals vs Fitted Values",
     col="thistle",
     pch=16)  # Set pch=4 for solid crosses
abline(h=0, col="blue", lwd=2)  # Horizontal line at zero

################################################################################
# Predict Loyalty Points for New Scenarios

# Define some example scenarios based on the data
# Average case scenario (mean values)
avg_scenario <- data.frame(age = mean(sales_df$age),
                           remuneration = mean(sales_df$remuneration),
                           spending_score = mean(sales_df$spending_score))

# Young and low spender scenario
young_low_spender <- data.frame(age = 20, 
                                remuneration = 20,  # Assume lower remuneration
                                spending_score = 10)  # Assume low spending

# Older and high spender scenario
older_high_spender <- data.frame(age = 60, 
                                 remuneration = 100,  # Higher remuneration
                                 spending_score = 90)  # High spending

# Extreme case scenario (min/max values)
extreme_case <- data.frame(age = 72,  # Max age
                           remuneration = 112.34,  # Max remuneration
                           spending_score = 99)  # Max spending score

# Use the model to predict loyalty points for each scenario
avg_pred <- predict(mlr_model, newdata = avg_scenario)
young_low_pred <- predict(mlr_model, newdata = young_low_spender)
older_high_pred <- predict(mlr_model, newdata = older_high_spender)
extreme_pred <- predict(mlr_model, newdata = extreme_case)

# Print the results
cat("Predicted loyalty points for average scenario:", avg_pred, "\n")
cat("Predicted loyalty points for young and low spender scenario:", young_low_pred, "\n")
cat("Predicted loyalty points for older and high spender scenario:", older_high_pred, "\n")
cat("Predicted loyalty points for extreme case scenario:", extreme_pred, "\n")

################################################################################

# Further exploratory data analysis (EDA)

#1) Correlation Matrix: look at the correlation between variables to 
# identify relationships.

# Load necessary library
library(corrplot)

# Calculate correlation matrix for numerical variables
cor_matrix <- cor(sales_df[, c("age", "remuneration", "spending_score", "loyalty_points")])

# Update the row and column names with the desired labels
colnames(cor_matrix) <- c("Age", "Remuneration", "Spending Score", "Loyalty Points")
rownames(cor_matrix) <- c("Age", "Remuneration", "Spending Score", "Loyalty Points")

# Define a color palette with a red-white-olive green gradient
color_palette <- colorRampPalette(c("#D73027", "white", "#556B2F"))(200)  # Red for negative, white for neutral, olive green for positive

# Visualize the correlation matrix with red-white-olive green color shades
corrplot(cor_matrix, method = "color", type = "upper", 
         col = color_palette, tl.col = "black", tl.srt = 45)

# 2) Scatter Plots:
# Visualize the relationship between individual variables and loyalty points 
# See Week 5 script please.



################################################################################

# Create a Multiple Linear Regression Model without age variable
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load the data
sales_df <- read.csv("turtle_reviews_clean.csv", header = TRUE)

# Summary statistics
summary(sales_df)


# Fit the multiple linear regression model (without age)
mlr_model_no_age <- lm(loyalty_points ~ remuneration + spending_score, data = sales_df)

# View the model summary
summary(mlr_model_no_age)

################################################################################

# Create predictions using the updated model
sales_df$predicted_loyalty <- predict(mlr_model_no_age, newdata = sales_df)

# Plot actual vs predicted loyalty points with customizations
ggplot(sales_df, aes(x = loyalty_points, y = predicted_loyalty)) +
  geom_point(color = 'peachpuff') +
  geom_smooth(method = 'lm', color = 'blue', se = FALSE) +  # `se = FALSE` removes the grey confidence interval
  labs(title = "Actual vs Predicted Loyalty Points (No Age)",
       x = "Actual Loyalty Points",
       y = "Predicted Loyalty Points") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, max(sales_df$loyalty_points), by = 500)) +
  scale_y_continuous(breaks = seq(0, max(sales_df$predicted_loyalty), by = 500)) +
  theme(
    panel.grid = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black")
  )

################################################################################

# Evaluating the model performance without age

# Calculate Mean Squared Error (MSE)
predicted_loyalty <- predict(mlr_model_no_age, sales_df)
mse <- mean((sales_df$loyalty_points - predicted_loyalty)^2)
print(paste("Mean Squared Error (No Age):", mse))

# Calculate Mean Absolute Error (MAE)
mae <- mean(abs(sales_df$loyalty_points - predicted_loyalty))
print(paste("Mean Absolute Error (No Age):", mae))

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)
print(paste("Root Mean Squared Error (No Age):", rmse))

################################################################################ 
# Checking for Multicollinearity Using Variance Inflation Factor (VIF)

# Load the car package for VIF calculation
library(car)

# Calculate VIF for the updated model
vif(mlr_model_no_age)

################################################################################
# Residual Analysis - Checking Residual Normality and Homoscedasticity

# Get residuals from the model without age
residuals <- residuals(mlr_model_no_age)

# Histogram of residuals
hist(residuals, main="Histogram of Residuals (No Age)", col="lightblue", xlab="Residuals")

# Q-Q plot of residuals 
qqnorm(residuals, pch = 16, col = "peachpuff")  
qqline(residuals, col = "blue")  

# Shapiro-Wilk test for normality of residuals
shapiro.test(residuals)

# Plot residuals vs fitted values 
plot(fitted(mlr_model_no_age), residuals, 
     xlab="Fitted Values", 
     ylab="Residuals", 
     main="Residuals vs Fitted Values (No Age)",
     col="peachpuff",
     pch=16)
abline(h=0, col="blue", lwd=2)

# Perform the Breusch-Pagan test for homoscedasticity
library(lmtest)
bptest(mlr_model_no_age)


###############################################################################
###############################################################################

# Model Results Without Age:
# Intercept: -1700.30
# Remuneration Coefficient: 33.98 (significant)
# Spending Score Coefficient: 32.89 (significant)
# R-squared: 0.8269 (indicating 82.69% of the variance is explained by the model)
# Residual Standard Error: 534.1
# F-statistic: 4770 on 2 DF, p-value < 2.2e-16

# Model Results With Age:
# Intercept: -2203.06
# Age Coefficient: 11.06 (significant)
# Remuneration Coefficient: 34.01 (significant)
# Spending Score Coefficient: 34.18 (significant)
# R-squared: 0.8399 (higher than without age, explaining 83.99% of the variance)
# Residual Standard Error: 513.8
# F-statistic: 3491 on 3 DF, p-value < 2.2e-16

# Insights:
# Adding the age variable increases the R-squared value slightly, indicating better variance explanation.
# The coefficients for remuneration and spending score remain significant in both models.
# Residual Standard Error is slightly lower with age, indicating better accuracy.

# Error Metrics Without Age:
# Mean Squared Error (MSE): 284,879.79
# Mean Absolute Error (MAE): 414.83
# Root Mean Squared Error (RMSE): 533.74

# Error Metrics With Age:
# Mean Squared Error (MSE): 263,486.60 (lower, which is better)
# Mean Absolute Error (MAE): 394.98 (lower)
# Root Mean Squared Error (RMSE): 513.31 (lower, indicating better fit)

# Insights:
# The model with age performs better on all error metrics, showing improved accuracy.

# Variance Inflation Factor (VIF):
# Without Age:
# Remuneration: 1.000032
# Spending Score: 1.000032
# With Age:
# Age: 1.05
# Remuneration: 1.000052
# Spending Score: 1.05

# Insights:
# No issues with multicollinearity, as VIF values are very low (all close to 1).

# Diagnostic Plots:
# Actual vs Predicted Plot: Both models align well, but struggle with predicting very high loyalty points.
# Q-Q Plot: Both models show deviations from normality, confirmed by Shapiro-Wilk test (p-value < 0.001).
# Residuals vs Fitted Values Plot: U-shaped pattern indicating heteroscedasticity, confirmed by Breusch-Pagan test.
# Without Age: BP = 53.727, p-value = 2.155e-12
# With Age: BP = 43.322, p-value = 2.102e-09

# Heteroscedasticity (Breusch-Pagan Test):
# Without Age: BP = 53.727, p-value = 2.155e-12 (significant heteroscedasticity)
# With Age: BP = 43.322, p-value = 2.102e-09 (significant heteroscedasticity)

# Insights:
# Both models exhibit heteroscedasticity, suggesting the need for transformations .I have chosen decision tree 

# Conclusion:
# The model with Age slightly improves fit but suffers from heteroscedasticity and non-normal residuals.




###############################################################################
###############################################################################




