### Multilateral Development Institution Data
foo <- read.csv("https://tinyurl.com/yb4phxx8") # read in the data

date.columns <- c(11, 12, 14, 15, 16, 17, 18, 25)

for(i in date.columns)  # this "for loop" only loops through the "date.columns" -- no other columns.
  
{
  which_values_are_missing <- which(as.character(foo[, i]) == "")
  which_values_are_missing
  foo[which_values_are_missing, i] <- NA
  foo[, i] <- as.Date(as.character(foo[, i]))
}

###################### MY CODE ###################### 

# Filter for rows with non-missing CirculationDate that is >= "2008-01-01"
condition <- which(foo$CirculationDate < "2008-01-01" | is.na(foo$CirculationDate == TRUE))

# The filtered dataset
clean.data <- foo[-condition,]

#(1)

#(a)

# Clean the data for NAs in OriginalCompletionDate
clean.opcd <- which(is.na(clean.data$OriginalCompletionDate))
clean.opcd.data <- clean.data[-clean.opcd,]

# Order the data according to CirculationDate
order_circ_date <- order(clean.opcd.data$CirculationDate)
clean.opcd.data <- clean.opcd.data[order_circ_date, ]

# Calculate the duration 
difference <- clean.opcd.data$OriginalCompletionDate - clean.opcd.data$ApprovalDate
mean(difference)

# Find out the earliest and latest projects
bottom20 <- 1:trunc(nrow(clean.opcd.data)*0.2)
top20 <- (1853 - trunc(nrow(clean.opcd.data)*0.2)):1853

# Calculate the mean, median, and quartile ranges of the bottom 20%
mean_bottom_20 <- mean(difference[bottom20])
median_bottom_20 <- median(difference[bottom20])
quantile_bottom_20 <- quantile(difference[bottom20])
mean_bottom_20; median_bottom_20; quantile_bottom_20

# Calculate the mean, median, and quartile ranges of the top 20%
mean_top_20 <- mean(difference[top20])
median_top_20 <- median(difference[top20])
quantile_top_20 <- quantile(difference[top20])
mean_top_20; median_top_20; quantile_top_20

#(b)

# Calculate actual duration
difference_actual <- clean.opcd.data$RevisedCompletionDate - clean.opcd.data$ApprovalDate

# Calculate the mean and quartiles of planned durations
mean_planned <- mean(difference)
quanile_planned <- quantile(difference)
mean_planned; quanile_planned

# Calculate the mean and quartiles of actual durations
mean_actual <- mean(difference_actual)
quantile_actual <- quantile(difference_actual)
mean_actual; quantile_actual

# Graph showing the distributions of actual and planned durations
hist(as.numeric(difference_actual),breaks = c(0, 500, 1000, 1500, 2000,2500, 3000, 3500, 4000, 4500, 5000),
     main = "Frequency of planned and actual duration", xlab = "Duration (days)", border = 'red', ylim = c(0, 1000))
hist(as.numeric(difference), breaks = c(0, 500, 1000, 1500, 2000,2500, 3000, 3500, 4000, 4500, 5000), 
     border="blue",ylim = c(0, 1000), add = T)
legend("topright", legend=c("Actual", "Planned")
       ,col = c('red', 'blue'), lty = 1:1, cex=0.8)

# (2)

# Data without NAs in Rating
clean.rating.data <- clean.data[-which(is.na(clean.data$Rating)),]
ratings <- clean.rating.data$Rating

# Function that returns the percentage of each rating as a vector
percentage <- function(seq) {
  count_0 <- 0
  count_1 <- 0
  count_2 <- 0
  count_3 <- 0
  for (i in seq) {
    if (i == 0){
      count_0 <- count_0 + 1
    } else if (i == 1) {
      count_1 <- count_1 + 1
    } else if (i == 2) {
      count_2 <- count_2 + 1
    } else {
      count_3 <- count_3 + 1
    }
  }
  percent_0 <- round(count_0/length(seq), 2)
  percent_1 <- round(count_1/length(seq), 2)
  percent_2 <- round(count_2/length(seq), 2)
  percent_3 <- 1 - percent_0 - percent_1 - percent_2
  perc_vec <- c(percent_0, percent_1, percent_2, percent_3)
  return(perc_vec)
}

# Generate a bar plot showing the percentages
barplot(percentage(ratings)*100, names.arg = c(0, 1, 2, 3), ylab = "Percentage of total projects (%)", xlab = "Type of rating",
        main = "Recent ratings of development projects", ylim = c(0, 70))

# (3)

# Data with no PPTA
no.ppta.data <- clean.rating.data[-which(clean.rating.data$Type == "PPTA"),]
no.ppta.ratings <- no.ppta.data$Rating

# Generate a bar plot showing the percentages
barplot(percentage(no.ppta.ratings)*100, names.arg = c(0,1,2,3), ylab = "Percentage of total projects (%)", xlab = "Type of rating",
        main = "Recent ratings of non-PPTA development projects", ylim = c(0, 70))

# (4)

# Check for NAs in the RevisedAmount column
which(is.na(clean.rating.data$RevisedAmount))

# Check the 25% and 75% quartiles
quantile(clean.rating.data$RevisedAmount)

# Subset the bottom 25% and the top 25%
top25 <- clean.rating.data[clean.rating.data$RevisedAmount >= 1,]
bottom25 <- clean.rating.data[clean.rating.data$RevisedAmount <= 0.4,]

# Ratings of the top and bottom 25% as vectors
ratings.top.25 <- top25$Rating
ratings.bot.25 <- bottom25$Rating

# Bar plots showing rating distributions
barplot(percentage(ratings.top.25)*100, space = 2, names.arg = c(0,1,2,3), ylab = "Percentage of total projects (%)",
        xlab = "Type of rating", main = "Ratings of the top and bottom 25% of projects by final project budget", ylim = c(0, 70),
        col = "red")

barplot(percentage(ratings.bot.25)*100, space = c(3,2,2,2), ylab = "Percentage of total projects (%)",
        xlab = "Type of rating", ylim = c(0, 70), add = T,
        col= 'blue')

legend("topleft", legend=c("Top 25%", "Bottom 25%")
       , fill = c('red', 'blue'), cex=0.8)

# The average values of the ratings of two groups
mean(ratings.bot.25)
mean(ratings.top.25)

# Comparing differences in $Type
table_bot_type <- table(bottom25$Type)
table_top_type <- table(top25$Type)
barplot((table_bot_type ), main = "Number of projects in terms of Types", 
        ylab = "Number of projects", xlab = 'Type',
        cex.names = 0.7)
barplot((table_top_type), add = T, col = "blue", cex.names = 0.7)
legend("topright", legend=c("Top 25%", "Bottom 25%")
       , fill = c('blue', 'grey'), cex=0.8)

# Comparing differences in $Country
table_bot_coun <- table(bottom25$Country)
table_top_coun <- table(top25$Country)
barplot((table_bot_coun ), main = "Number of projects in terms of Countries", 
        ylab = "Number of projects", xlab = 'Country', ylim = c(0, 300), cex.names = 0.8)
barplot((table_top_coun), add = T, col = "blue", cex.names = 0.8)
legend("topright", legend=c("Top 25%", "Bottom 25%")
       , fill = c('blue', 'grey'), cex=0.8)

# Comparing differences in $Dept
table_bot_dept <- table(bottom25$Dept)
table_top_dept <- table(top25$Dept)
barplot((table_bot_dept ), main = "Number of projects in terms of Departments", 
        ylab = "Number of projects", xlab = 'Department', ylim = c(0, 180), cex.names = 0.8)
barplot((table_top_dept), add = T, col = "blue", cex.names = 0.8)
legend("topleft", legend=c("Top 25%", "Bottom 25%")
       , fill = c('blue', 'grey'), cex=0.8)