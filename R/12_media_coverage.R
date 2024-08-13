# "media_coverage" =============================================================
# https://www.bigkinds.or.kr/
# ==============================================================================
source(here::here("R", "utilities.R"))

# Load necessary libraries
require(jsonlite)
require(reshape2)

# Load JSON files
epeople_data <- fromJSON("data/epeople_media.json")
epetition_data <- fromJSON("data/epetition_media.json")

# Join the data into a single DataFrame
media <- left_join(epeople_data, epetition_data, by = "date")
head(media)

# Rename columns
colnames(media) <- c("date", "epeople_media_count", "epetition_media_count")

# Convert date to a Date object using the correct format
media$date <- as.Date(media$date, format = "%Y%m%d")

# save the data
write_csv(media, file = here("data", "media_coverage.csv"))

# Plotting =====================================================================
# Aggregate data by month
media_monthly <- media %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(
    epeople_media_count = sum(epeople_media_count, na.rm = TRUE),
    epetition_media_count = sum(epetition_media_count, na.rm = TRUE)
  )

# Draw a line plot by ePeople and ePetition media coverage by months
p <- ggplot(media_monthly, aes(x = month)) +
  geom_line(aes(y = epeople_media_count, color = "ePeople"), size = 1.2) +
  geom_line(aes(y = epetition_media_count, color = "ePetition"), size = 1.2) +
  geom_point(aes(y = epeople_media_count, color = "ePeople"), size = 2) +
  geom_point(aes(y = epetition_media_count, color = "ePetition"), size = 2) +
  labs(
    title = "Monthly Media Coverage of ePeople and ePetition (from 2012-01-01)",
    x = "Month",
    y = "Number of Articles",
    color = "Category"
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(
    values = c("ePeople" = "black", "ePetition" = "gray"),
    labels = c("ePeople", "ePetition")
  ) +
  theme_bw()

# Display the plot
p
ggsave(here("fig", "media_coverage.pdf"), width = 8, height = 5)
