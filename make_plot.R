library(tidyverse)

# Read the cleaned data
top_songs <- read_rds("clean_data.rds")

# Define breaks and labels for months
month_breaks <- seq(0, 48, by = 4)
month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan")

# Create the plot
billboard_plot <- ggplot(top_songs, aes(x = week_num, y = rank, color = song_artist)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  geom_point(size = 2) +
  scale_y_reverse(limits = c(100, 1)) +
  scale_x_continuous(breaks = month_breaks, labels = month_labels) +
  scale_color_manual(values = c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", "#FFEEAD")) +
  labs(
    title = "Billboard Top 100: Top 5 Songs in Jul 2000",
    subtitle = "Tracking rank changes from Jan to Dec 2000",
    x = "Month",
    y = "Chart Position",
    color = "Song (Artist)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    plot.background = element_rect(fill = "#F5F5F5", color = NA)
  ) +
  guides(color = guide_legend(nrow = 2))

# Save the plot
ggsave("billboard_plot.png", plot = billboard_plot, width = 8, height = 5)
