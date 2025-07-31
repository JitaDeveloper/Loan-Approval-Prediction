# read and view data
data <- read.csv("C:\\Users\\JIT MONDAL\\Desktop\\Projects\\loan_approval_dataset.csv")
head(data)
View(data)
summary(data)

install.packages('IRkernel')
IRkernel::installspec()

# creating a new column for total assets value
data$total_assets_value <- data$residential_assets_value + data$luxury_assets_value + data$bank_asset_value + data$commercial_assets_value

categorize <- function(breaks, labels, x) {
  # Categorize income into groups using cut()
  y <- cut(x, breaks = breaks, labels = labels, include.lowest = TRUE)
  return(y)
}

# Print the resulting dataframe
data$income_groups <- categorize(breaks <- c(200000, 1000000, 4000000, 7500000, 99900000), labels <- c("Low", "Slightly Lower", "Slightly Upper", "Upper"), data$income_annum)
data$cibil_score_rating <- categorize(breaks = c(300, 450, 600, 750, 900), labels = c("Poor", "Average", "Good", "Excellent"), data$cibil_score)



# checking for missing values
sum(is.na(data$loan_id))
sum(is.na(data$no_of_dependents))
sum(is.na(data$education))
sum(is.na(data$self_employed))
sum(is.na(data$income_annum))
sum(is.na(data))



# Load required package
library(ggplot2)
library(ggfx)
library(dplyr)
data$income_annum

# Count frequencies of Cibil Score Rating
frequency_table <- table(data$cibil_score_rating)
print(frequency_table)
# Convert frequency table to data frame
frequency_df <- as.data.frame(frequency_table)
names(frequency_df) <- c("category", "frequency")



# Create pie chart
pie_chart <- ggplot(frequency_df, aes(x = "", y = frequency, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_manual(values = rainbow(length(unique(frequency_df$category)))) + # Custom colors
  labs(title = "Pie Chart Showing Cibil Score Rating", fill = "Category") # Chart title and legend title

# Display the pie chart
print(pie_chart)

# Count frequencies of income groups
frequency_table_income <- table(data$income_groups)
print(frequency_table_income)

# Convert frequency table to data frame
frequency_df_income <- as.data.frame(frequency_table_income)
names(frequency_df_income) <- c("category", "frequency")
frequency_df_income$percentage <- frequency_df_income$frequency / sum(frequency_df_income$frequency) * 100

# Create pie chart
pie_chart_income <- ggplot(frequency_df_income, aes(x = "", y = frequency, fill = category)) +
  with_shadow(
    geom_bar(stat = "identity", width = 1, show.legend = TRUE, color = "black"),
    sigma = 15, x_offset = 5, y_offset = 5, colour = "grey50"
  ) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
    position = position_stack(vjust = 0.5)
  ) +
  coord_polar("y", start = 0) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "#333333"),
    legend.title = element_text(size = 14, face = "bold", color = "#333333"),
    legend.text = element_text(size = 12, color = "#333333"),
    plot.background = element_rect(fill = "#F9F9F9", color = NA),
    panel.background = element_rect(fill = "#F9F9F9", color = NA),
    legend.background = element_rect(fill = "#FFFFFF", color = "#DDDDDD", size = 0.5),
    legend.key = element_rect(fill = "#FFFFFF", color = "#DDDDDD")
  ) +
  scale_fill_manual(values = rainbow(length(unique(frequency_df_income$category)))) + # Custom colors
  labs(title = "Income Groups", fill = "Category") # Chart title and legend title

# Display the pie chart
print(pie_chart_income)


# Count frequencies of self_employed
frequency_table_self_employed <- table(data$loan_status)
print(frequency_table_self_employed)

# Convert frequency table to data frame
frequency_df_self_employed <- as.data.frame(frequency_table_self_employed)
names(frequency_df_self_employed) <- c("category", "frequency")
frequency_df_self_employed$percentage <- frequency_df_self_employed$frequency / sum(frequency_df_self_employed$frequency) * 100

# Create pie chart
pie_chart_self_employed <- ggplot(frequency_df_self_employed, aes(x = "", y = frequency, fill = category)) +
  with_shadow(
    geom_bar(stat = "identity", width = 1, show.legend = TRUE, color = "black"),
    sigma = 15, x_offset = 5, y_offset = 5, colour = "grey50"
  ) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
    position = position_stack(vjust = 0.5)
  ) +
  coord_polar("y", start = 0) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "#333333"),
    legend.title = element_text(size = 14, face = "bold", color = "#333333"),
    legend.text = element_text(size = 12, color = "#333333"),
    plot.background = element_rect(fill = "#F9F9F9", color = NA),
    panel.background = element_rect(fill = "#F9F9F9", color = NA),
    legend.background = element_rect(fill = "#FFFFFF", color = "#DDDDDD", size = 0.5),
    legend.key = element_rect(fill = "#FFFFFF", color = "#DDDDDD")
  ) +
  scale_fill_manual(values = rainbow(length(unique(frequency_df_self_employed$category)))) + # Custom colors
  labs(title = "Loan Status", fill = "Category") # Chart title and legend title

# Display the pie chart
print(pie_chart_self_employed)



# Count frequencies of Loan Status
frequency_table_loanStatus <- table(data$loan_status)
print(frequency_table_loanStatus)
# Convert frequency table to data frame
frequency_df_loanStatus <- as.data.frame(frequency_table_loanStatus)
names(frequency_df_loanStatus) <- c("category", "frequency")

# Create pie chart
pie_chart <- ggplot(frequency_df_loanStatus, aes(x = "", y = frequency, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_manual(values = rainbow(length(unique(frequency_df_loanStatus$category)))) + # Custom colors
  labs(title = "Pie Chart Showing Loan Status", fill = "Category") # Chart title and legend title

# Display the pie chart
print(pie_chart)

# Count frequencies of education and loan status
frequency_table_loanStatus <- table(data$education, data$loan_status)
print(frequency_table_loanStatus)
# Convert frequency table to data frame
frequency_df_loanStatus <- as.data.frame(frequency_table_loanStatus)
names(frequency_df_loanStatus) <- c("education", "loan_status", "frequency")

library(dplyr)
# Calculate total frequency for each attribute
total_frequency <- frequency_df_loanStatus %>%
  group_by(education, loan_status) %>%
  summarise(total_frequency = sum(frequency)) %>%
  ungroup()

# Calculate percentage for each attribute
total_frequency <- total_frequency %>%
  group_by(education) %>%
  mutate(Percentage = (total_frequency / sum(total_frequency)) * 100)

# Create a heatmap using ggplot2
heatmap <- ggplot(total_frequency, aes(x = education, y = loan_status, fill = Percentage)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), color = "black") +
  scale_fill_gradient(low = "brown", high = "blue") + # Change colors as needed
  labs(x = "Education", y = "Loan Status", title = "Loan Status by Education") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "#333333"),
    legend.title = element_text(size = 14, face = "bold", color = "#333333"),
    legend.text = element_text(size = 12, color = "#333333"),
    plot.background = element_rect(fill = "#F9F9F9", color = NA),
    panel.background = element_rect(fill = "#F9F9F9", color = NA),
    legend.background = element_rect(fill = "#FFFFFF", color = "#DDDDDD", size = 0.5),
    legend.key = element_rect(fill = "#FFFFFF", color = "#DDDDDD")
  )

# Print the heatmap
print(heatmap)

# Count frequencies of loanstatus and self employed
frequency_table_loanStatus <- table(data$self_employed, data$loan_status)
print(frequency_table_loanStatus)
# Convert frequency table to data frame
frequency_df_loanStatus <- as.data.frame(frequency_table_loanStatus)
names(frequency_df_loanStatus) <- c("self_employed", "loan_status", "frequency")

library(dplyr)
# Calculate total frequency for each attribute
total_frequency <- frequency_df_loanStatus %>%
  group_by(self_employed, loan_status) %>%
  summarise(total_frequency = sum(frequency)) %>%
  ungroup()

# Calculate percentage for each attribute
total_frequency <- total_frequency %>%
  group_by(self_employed) %>%
  mutate(percentage = (total_frequency / sum(total_frequency)) * 100)

# Create a heatmap using ggplot2
heatmap <- ggplot(total_frequency, aes(x = self_employed, y = loan_status, fill = percentage)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), color = "black") +
  scale_fill_gradient(low = "brown", high = "blue") + # Change colors as needed
  labs(x = "Self Employed", y = "Loan Status", title = "Loan Status by Self-employment") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "#333333"),
    legend.title = element_text(size = 14, face = "bold", color = "#333333"),
    legend.text = element_text(size = 12, color = "#333333"),
    plot.background = element_rect(fill = "#F9F9F9", color = NA),
    panel.background = element_rect(fill = "#F9F9F9", color = NA),
    legend.background = element_rect(fill = "#FFFFFF", color = "#DDDDDD", size = 0.5),
    legend.key = element_rect(fill = "#FFFFFF", color = "#DDDDDD")
  )


# Print the heatmap
print(heatmap)

# Count frequencies of loanstatus and cibil_score_rating
frequency_table_loanStatus <- table(data$cibil_score_rating, data$loan_status)
print(frequency_table_loanStatus)
# Convert frequency table to data frame
frequency_df_loanStatus <- as.data.frame(frequency_table_loanStatus)
names(frequency_df_loanStatus) <- c("cibil_score_rating", "loan_status", "frequency")

library(dplyr)
# Calculate total frequency for each attribute
total_frequency <- frequency_df_loanStatus %>%
  group_by(cibil_score_rating, loan_status) %>%
  summarise(total_frequency = sum(frequency)) %>%
  ungroup()

# Calculate percentage for each attribute
total_frequency <- total_frequency %>%
  group_by(cibil_score_rating) %>%
  mutate(percentage = (total_frequency / sum(total_frequency)) * 100)

# Create a heatmap using ggplot2
heatmap <- ggplot(total_frequency, aes(x = cibil_score_rating, y = loan_status, fill = percentage)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), color = "black") +
  scale_fill_gradient(low = "brown", high = "blue") + # Change colors as needed
  labs(x = "Cibil Score Rating", y = "Loan Status", title = "Loan Status by Cibil Score Rating") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "#333333"),
    legend.title = element_text(size = 14, face = "bold", color = "#333333"),
    legend.text = element_text(size = 12, color = "#333333"),
    plot.background = element_rect(fill = "#F9F9F9", color = NA),
    panel.background = element_rect(fill = "#F9F9F9", color = NA),
    legend.background = element_rect(fill = "#FFFFFF", color = "#DDDDDD", size = 0.5),
    legend.key = element_rect(fill = "#FFFFFF", color = "#DDDDDD")
  )

# Print the heatmap
print(heatmap)

# Count frequencies of loanstatus and income_annum
frequency_table_loanStatus <- table(data$income_groups, data$loan_status)
print(frequency_table_loanStatus)
# Convert frequency table to data frame
frequency_df_loanStatus <- as.data.frame(frequency_table_loanStatus)
names(frequency_df_loanStatus) <- c("income_groups", "loan_status", "frequency")

library(dplyr)
# Calculate total frequency for each attribute
total_frequency <- frequency_df_loanStatus %>%
  group_by(income_groups, loan_status) %>%
  summarise(total_frequency = sum(frequency)) %>%
  ungroup()

# Calculate percentage for each attribute
total_frequency <- total_frequency %>%
  group_by(income_groups) %>%
  mutate(percentage = (total_frequency / sum(total_frequency)) * 100)

# Create a heatmap using ggplot2
heatmap <- ggplot(total_frequency, aes(x = income_groups, y = loan_status, fill = percentage)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), color = "black") +
  scale_fill_gradient(low = "brown", high = "blue") + # Change colors as needed
  labs(x = "Income Groups", y = "Loan Status", title = "Loan Status by Income Groups") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "#333333"),
    legend.title = element_text(size = 14, face = "bold", color = "#333333"),
    legend.text = element_text(size = 12, color = "#333333"),
    plot.background = element_rect(fill = "#F9F9F9", color = NA),
    panel.background = element_rect(fill = "#F9F9F9", color = NA),
    legend.background = element_rect(fill = "#FFFFFF", color = "#DDDDDD", size = 0.5),
    legend.key = element_rect(fill = "#FFFFFF", color = "#DDDDDD")
  )

# Print the heatmap
print(heatmap)

# Count frequencies of loanstatus and no of dependents
frequency_table_dependents <- table(data$no_of_dependents, data$loan_status)

frequency_table_dependents
# Convert frequency table to data frame

chisq.test(frequency_table_dependents)


# Count frequencies of loanstatus and no of dependents
frequency_table_dependents <- table(data$loan_term, data$loan_status)

frequency_table_dependents
# Convert frequency table to data frame

chisq.test(frequency_table_dependents)

# Perform one-way ANOVA
anova_result <- aov(total_assets_value ~ loan_status, data = data)

hist(data[,c("total_assets_value")])
fligner.test(total_assets_value ~ loan_status, data = data)

# Print the ANOVA table
summary(anova_result)
# Perform one-way ANOVA
anova_result <- aov(cibil_score ~ loan_status, data = data)

# Print the ANOVA table

summary(anova_result)

# Calculate Pearson's correlation coefficient
correlation_coeff <- cor(data$income_annum, data$total_assets_value)
correlation_coeff <- cor(data$cibil_score, data$loan_term)
cormatrix <- cor(data[, -c(1, 15, 16)])
library(ggcorrplot)
ggcorrplot(cormatrix)
# Print the correlation coefficient
cat("Pearson's correlation coefficient:", correlation_coeff, "\n")

# Alternatively, you can also get the p-value
cor_test <- cor.test(data$income_annum, data$total_assets_value)
cor_test$p.value

# replacing characters with numeric values
data[data == " Yes"] <- 1
data[data == " No"] <- 0
data[data == " Approved"] <- 1
data[data == " Rejected"] <- 0
data[data == " Graduate"] <- 1
data[data == " Not Graduate"] <- 0
str(data)
data$education <- as.numeric(data$education)
data$self_employed <- as.numeric(data$self_employed)
data$loan_status <- as.numeric(data$loan_status)

# importing essential libraries
library(caTools)
library(ROCR)

# splitting the data into two parts; one for training and other for testing purpose
split <- sample.split(data, SplitRatio = 0.7)
split

train_reg <- subset(data, split == "TRUE")
test_reg <- subset(data, split == "FALSE")

# Training model
logistic_model <- glm(loan_status ~ loan_amount + income_annum + no_of_dependents + education + self_employed + loan_term + cibil_score + residential_assets_value + commercial_assets_value + luxury_assets_value + bank_asset_value,
  data = train_reg,
  family = "binomial"
)
logistic_model


# Summary
summary(logistic_model)

# prediction
predict <- predict(logistic_model, type = "response", newdata = test_reg)
predict
fitted <- ifelse(predict > 0.5, 1, 0)
fitted

# checking the accuracy
table(test_reg$loan_status, predict > 0.5)
library(MLmetrics)
MLmetrics::Accuracy(fitted, test_reg$loan_status)



# Training model using selected factors
logistic_model <- glm(loan_status ~ cibil_score + loan_term,
  data = train_reg,
  family = "binomial"
)

logistic_model
# coefficients of the logistic model
coefficient <- coef(logistic_model)
coefficient
# Summary
summary(logistic_model)
test_reg[, c(5, 6, 7, 8, 14)]

# prediction
predict <- predict(logistic_model, type = "response", newdata = test_reg[, c(7, 8, 13)])
predict
fitted <- ifelse(predict > 0.5, 1, 0)
fitted

# checking the accuracy
table(test_reg$loan_status, predict > 0.5)
library(MLmetrics)
MLmetrics::Accuracy(fitted, test_reg$loan_status)

#### principal component analysis
my_pca <- prcomp(train_reg[, -13],
  scale = TRUE,
  center = TRUE, retx = T
)
names(my_pca)

# Summary
summary(my_pca)
my_pca
my_pca$rotation
my_pca$x
dim(my_pca$x)


### visualizing the principal components
library(factoextra)
fviz_eig(my_pca, addlabels = TRUE)

new_train <- as.data.frame(cbind(train_reg$loan_status, my_pca$x))
new_train[, -c(11:14)]

colnames(new_train)[1] <- "loan_status"
str(new_train)
library(psych)
pc.fit <- prcomp(~., data = train_reg[, -13])
summary(pc.fit)
screeplot(pc.fit, type = "l")
new_train
new_test <- as.data.frame(predict(pc.fit, test_reg))[, -c(4:14)]
logistic_model <- glm(loan_status ~ .,
  data = new_train[-c(4:14)],
  family = "binomial"
)
summary(logistic_model)

library(ggcorrplot)
ggcorrplot(cor(train_reg[c(5, 6, 7, 8, 13, 14)]))
# prediction
predict <- predict(logistic_model, type = "response", newdata = new_test)
predict
fitted <- ifelse(predict > 0.5, 1, 0)
fitted

# checking the accuracy
table(test_reg$loan_status, predict > 0.5)
library(MLmetrics)
MLmetrics::Accuracy(fitted, test_reg$loan_status)

logistic_model <- glm(loan_status ~ loan_amount + income_annum + no_of_dependents + education + self_employed + loan_term + cibil_score + total_assets_value,
  data = train_reg,
  family = "binomial"
)
logistic_model


library(randomForest)
### using random forest model
# Training model using selected factors
logistic_model <- randomForest(loan_status ~ cibil_score + loan_term,
  data = train_reg,
  importance=TRUE
)

logistic_model
# coefficients of the logistic model
coefficient <- coef(logistic_model)
coefficient
# Summary
summary(logistic_model)
test_reg[, c(5, 6, 7, 8, 14)]

# prediction
predict <- predict(logistic_model, type = "response", newdata = test_reg[, c(7, 8, 13)])
predict
fitted <- ifelse(predict > 0.5, 1, 0)
fitted

# checking the accuracy
table(test_reg$loan_status, predict > 0.5)
library(MLmetrics)
MLmetrics::Accuracy(fitted, test_reg$loan_status)


# Custom rupee format function
rupees_format <- function() {
  function(x) {
    paste0("₹", formatC(x, format = "f", big.mark = ",", digits = 0))
  }
}
# Create a sophisticated boxplot
ggplot(data, aes(x = loan_status, y = total_assets_value, fill = loan_status)) +
  geom_boxplot() +
  labs(
    title = "Comparison of Total Assets Value and Loan Status",                         
    x = "Loan Status", y = "Total Assets Value"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),            # Title appearance
    axis.text.x = element_text(size = 14, color = "black"),         # X-axis text
    axis.text.y = element_text(size = 14, color = "black"),         # Y-axis text
    
  )+ labs(fill="Loan Status")+
  
  scale_y_continuous(labels = rupees_format())
