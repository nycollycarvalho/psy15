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
output_dir <- "~/Desktop/psy15/figures"

# Box Plot
png(file = file.path(output_dir, "STAI_Boxplot.png"), width = 700, height = 600)
ggplot(stai_data %>% pivot_longer(cols = c(Pre_Score, Post_Score), 
                                  names_to = "Condition", 
                                  values_to = "Score"),
       aes(x = Condition, y = Score, fill = Condition)) +
  geom_boxplot(outlier.colour = "black", outlier.shape = 16, alpha = 0.8) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "#d9d9d9"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    legend.position = "none"
  ) +
  labs(title = "STAI: Comparison of Pre and Post Scores",
       x = "Condition",
       y = "Score") +
  scale_fill_manual(values = c("darkseagreen", "darkolivegreen"))
dev.off()

# Histogram
png(file = file.path(output_dir, "STAI_Histogram.png"), width = 700, height = 600)
ggplot(stai_data %>% pivot_longer(cols = c(Pre_Score, Post_Score), 
                                  names_to = "Condition", 
                                  values_to = "Score"),
       aes(x = Score, fill = Condition)) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.8) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "#d9d9d9"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    legend.position = "none"
  ) +
  labs(title = "STAI: Score Distributions",
       x = "Score",
       y = "Frequency") +
  scale_fill_manual(values = c("darkseagreen", "darkolivegreen"))
dev.off()

