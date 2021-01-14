library(ggplot2)
library(gganimate)

# # Daily mortality data from Ministry of interior for various spatial units and age groups.
xy <- read.table("./data/mnz_daily_deaths_2021_01_13.csv", header = TRUE, sep = ",")
xy <- xy[, c("Datum.smrti", "Starostna.skupina.COUNT")]
colnames(xy) <- c("datum", "count")

xy <- aggregate(count ~ datum, FUN = sum, data = xy)

xy$datum <- as.Date(xy$datum, format = "%d/%m/%Y")
xy$fake_datum <- as.Date(strftime(xy$datum, format = "0-%m-%d"))
xy$year <- strftime(xy$datum, format = "%Y")

xy <- xy[order(xy$datum), ]

# Variables needed for the animation.
xy$frame <- 1:nrow(xy)
# Color and alpha for "regular" years.
xy$color <- "black"
xy$alpha <- 0.5
# Color and transparency of 2020.
xy[xy$year == 2020, "color"] <- "#F4A460"  # sandybrown
xy[xy$year == 2020, "alpha"] <- 1

# Create a smoothed count which will be used to draw label near the point.
xy$smoothed <- predict(loess(count ~ as.numeric(datum), data = xy), newdata = xy[, c("datum")])

# > str(xy)
# 'data.frame':	3984 obs. of  8 variables:
# $ datum     : Date, format: "2010-01-01" "2010-01-02" "2010-01-03" ...
# $ count     : num  58 43 58 65 49 57 56 53 51 49 ...
# $ fake_datum: Date, format: "1-01-01" "1-01-02" "1-01-03" ...
# $ year      : chr  "2010" "2010" "2010" "2010" ...
# $ frame     : int  1 2 3 4 5 6 7 8 9 10 ...
# $ color     : chr  "grey" "grey" "grey" "grey" ...
# $ alpha     : num  0.15 0.15 0.15 0.15 0.15 0.15 0.15 0.15 0.15 0.15 ...
# $ smoothed  : num  54 54 54 54 54 ...
# 
xy <- xy[xy$datum >= as.Date("2016-01-01"), ]
xy <- xy[xy$datum <= as.Date("2020-12-31"), ]

minmax <- strftime(xy$datum, format = "%Y")
minmax <- as.numeric(minmax)

fig <- ggplot(xy, aes(x = fake_datum, y = count)) +
  theme_bw() +
  theme(
    rect = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  ) +
  labs(title = sprintf("Daily deaths in Slovenia (%s-%s)", min(minmax), max(minmax)),
  caption = "Source: Ministry of interior") +
  geom_line(aes(group = year, color = I(color), alpha = alpha), size = 0.25) +
  geom_point(color = "gray20") +
  scale_y_continuous(limits = c(0, 135)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = expansion(add = c(0, -15))) +
  geom_text(aes(y = smoothed, label = year), nudge_x = 15, size = 3, color = "gray20") +
  coord_polar() +
  transition_reveal(along = frame, keep_last = FALSE)

animate(fig, nframes = nrow(xy), fps = 60, end_pause = 200, res = 130,
        render = av_renderer("slovenia_deaths.mp4", codec = "libx264"))
