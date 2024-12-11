# Load necessary packages
if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
}
p_load("tidyverse", "ggplot2")

# Set working directory
setwd("~/Desktop/psy15/data")  # Adjust the path if necessary

# Load Eco-emotions datasets
pre_eco <- read_csv("Eco-emotions 1  - pre_eco-emotions.csv")
post_eco <- read_csv("Eco-emotions 1  - post_eco-emotions.csv")

# Drop non-numeric columns (e.g., Timestamp)
pre_eco <- pre_eco %>% select(where(is.numeric))
post_eco <- post_eco %>% select(where(is.numeric))

# Calculate row-wise averages for pre- and post-intervention scores
pre_eco <- pre_eco %>% mutate(Average_Pre = rowMeans(across(everything()), na.rm = TRUE))
post_eco <- post_eco %>% mutate(Average_Post = rowMeans(across(everything()), na.rm = TRUE))

# Combine pre and post averages for analysis
eco_data <- tibble(
  Pre_Score = pre_eco$Average_Pre,
  Post_Score = post_eco$Average_Post
)

# Perform paired t-test for Eco-emotions
t_test_eco <- t.test(eco_data$Pre_Score, eco_data$Post_Score, paired = TRUE)

# Print t-test results
cat("Eco-emotions Paired T-Test Results:\n")
print(t_test_eco)

# Save and visualize results
output_dir <- "~/Desktop/psy15"

# Box Plot
png(file = file.path(output_dir, "Eco_Emotions_Boxplot.png"), width = 600, height = 500)
ggplot(eco_data %>% pivot_longer(cols = c(Pre_Score, Post_Score), 
                                 names_to = "Condition", 
                                 values_to = "Score"),
       aes(x = Condition, y = Score, fill = Condition)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8) +
  theme_minimal() +
  labs(title = "Eco-emotions: Comparison of Pre and Post Scores",
       x = "Condition",
       y = "Score") +
  scale_fill_manual(values = c("lightblue", "lightgreen"))
dev.off()

# Histogram
png(file = file.path(output_dir, "Eco_Emotions_Histogram.png"), width = 600, height = 500)
ggplot(eco_data %>% pivot_longer(cols = c(Pre_Score, Post_Score), 
                                 names_to = "Condition", 
                                 values_to = "Score"),
       aes(x = Score, fill = Condition)) +
  geom_histogram(bins = 15, alpha = 0.7, position = "identity") +
  theme_minimal() +
  labs(title = "Eco-emotions: Score Distributions",
       x = "Score",
       y = "Count") +
  scale_fill_manual(values = c("lightblue", "lightgreen"))
dev.off()
