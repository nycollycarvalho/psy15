# Load necessary packages
if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
}
p_load("tidyverse", "ggplot2")

# Set working directory
setwd("~/Desktop/psy15/data")  # Adjust the path if necessary

# Load STAI datasets
pre_stai <- read_csv("Eco-emotions 1  - pre_stai.csv")
post_stai <- read_csv("Eco-emotions 1  - post_stai.csv")

# Drop non-numeric columns (e.g., Timestamp)
pre_stai <- pre_stai %>% select(where(is.numeric))
post_stai <- post_stai %>% select(where(is.numeric))

# Calculate row-wise averages for pre- and post-intervention scores
pre_stai <- pre_stai %>% mutate(Average_Pre = rowMeans(across(everything()), na.rm = TRUE))
post_stai <- post_stai %>% mutate(Average_Post = rowMeans(across(everything()), na.rm = TRUE))

# Combine pre and post averages for analysis
stai_data <- tibble(
  Pre_Score = pre_stai$Average_Pre,
  Post_Score = post_stai$Average_Post
)

# Perform paired t-test for STAI
t_test_stai <- t.test(stai_data$Pre_Score, stai_data$Post_Score, paired = TRUE)

# Print t-test results
cat("STAI Paired T-Test Results:\n")
print(t_test_stai)

# Save and visualize results
output_dir <- "~/Desktop/psy15"

# Box Plot
png(file = file.path(output_dir, "STAI_Boxplot.png"), width = 600, height = 500)
ggplot(stai_data %>% pivot_longer(cols = c(Pre_Score, Post_Score), 
                                  names_to = "Condition", 
                                  values_to = "Score"),
       aes(x = Condition, y = Score, fill = Condition)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8) +
  theme_minimal() +
  labs(title = "STAI: Comparison of Pre and Post Scores",
       x = "Condition",
       y = "Score") +
  scale_fill_manual(values = c("lightblue", "lightgreen"))
dev.off()

# Histogram
png(file = file.path(output_dir, "STAI_Histogram.png"), width = 600, height = 500)
ggplot(stai_data %>% pivot_longer(cols = c(Pre_Score, Post_Score), 
                                  names_to = "Condition", 
                                  values_to = "Score"),
       aes(x = Score, fill = Condition)) +
  geom_histogram(bins = 15, alpha = 0.7, position = "identity") +
  theme_minimal() +
  labs(title = "STAI: Score Distributions",
       x = "Score",
       y = "Count") +
  scale_fill_manual(values = c("lightblue", "lightgreen"))
dev.off()
