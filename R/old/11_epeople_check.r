# "epeople_petitions_public" ===================================================
# check the rationale for focusing on petitions (proposals) that are public ----

source(here::here("R", "utilities.R"))

# Load necessary libraries
require(jsonlite)
require(reshape2)

# Load the JSON file
data <- fromJSON("data/epeople.json")

# Convert to a Data Frame
data_df <- as.data.frame(data)

# Rename columns for better readability
colnames(data_df) <- c(
  "Sinmungo_Website", "Presidential_Office_Letters", "Year",
  "Local_Governments_Transferred", "Ministry_Website"
)

# Convert all columns to numeric
data_df[] <- lapply(data_df, function(x) as.numeric(as.character(x)))

# Melt the data for easier plotting (long format)
data_melted <- melt(data_df, id.vars = "Year")

# Plotting the data
p <- ggplot(data_melted, aes(x = Year, y = value, color = variable)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Trends in ePeople Petitions (2011-2023)",
    x = "Year",
    y = "Number of Submissions",
    color = "Category"
  ) +
  scale_x_continuous(
    breaks = seq(min(data_melted$Year), max(data_melted$Year), by = 1)
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()
p

## pdf_default(p)
ggsave(here("fig", "epeople_petitions.pdf"), width = 8, height = 5)
