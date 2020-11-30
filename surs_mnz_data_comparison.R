library(readxl)
library(ggplot2)

# SURS and MNZ daily death data differ. To what degree is to be determined in this document.

mnz <- read_excel("./data/umrljivost_update_2020_11_30.xls", sheet = "adapted")  # 2010-2020
mnz <- as.data.frame(mnz)
mnz$value <- as.numeric(mnz$value)
colnames(mnz) <- c("datum", "mnz")
mnz$datum <- as.Date(mnz$datum)

surs <- read.table(file = "./data/surs_death_by_day_2000-2020-09.txt", 
                   header = TRUE, sep = "\t", nrows = 7719)  # dbd = death by day

colnames(surs) <- c("year_month", "day", "count")
find.year.month <- "^(\\d{4})M(\\d{2}).*$"
surs$year <- gsub(find.year.month, replacement = "\\1", x = surs$year_month)
surs$month <- gsub(find.year.month, replacement = "\\2", x = surs$year_month)
surs$datum <- as.Date(paste(surs$year, surs$month, surs$day, sep = "-"))

surs$count <- as.numeric(as.character(surs$count))

# Use a fake year to align the data.
surs$fake_datum <- as.Date(paste(1, surs$month, surs$day, sep = "-"))

surs$year_month <- NULL

surs <- surs[!is.na(surs$datum), c("datum", "count")]
colnames(surs) <- c("datum", "surs")

xy <- merge(x = surs, y = mnz)
xy <- merge(x = mnz, y = surs, all.x = TRUE)

xyl <- reshape(data = xy,
               direction = "long",
               varying = c("surs", "mnz"),
               v.names = "deceased",
               times = c("surs", "mnz"),
               timevar = "source",
               idvar = "datum")
rownames(xyl) <- NULL

xy$diff <- xy$mnz - xy$surs

ggplot(xyl, aes(x = datum, y = deceased, color = source)) +
  theme_bw() +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "4 months") +
  scale_color_brewer(palette = "Dark2") +
  geom_line(size = 0.5) +
  # geom_smooth(method = "loess", span = 0.01) +
  geom_line(data = xy, aes(x = datum, y = diff), color = "black", alpha = 0.4) +
  geom_hline(data = xy, aes(yintercept = mean(diff)), color = "black") +
  annotate(geom = "text", 
           x = c(as.Date("2009-10-01"), as.Date("2021-01-01")), 
           y = c(2.5, 2.5), 
           label = c("difference", "difference"))
ggsave("./figures/comparing_surs_mnz_series.png", width = 25, height = 6)  

ggplot(xy, aes(x = diff)) +
  theme_bw() +
  geom_histogram(binwidth = 1)

xy.ccf <- ccf(x = xy$surs, y = xy$mnz, na.action = na.pass)
xy.acf <- acf(xy[, c("surs", "mnz")], na.action = na.pass)
