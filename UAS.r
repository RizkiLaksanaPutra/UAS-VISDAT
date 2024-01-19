library(readr)
library(ggplot2)
library(dplyr)
library(sf)
library(mapview)
library(ggcorrplot)

data = read.csv("NY-House-Dataset.csv")
pdf("output_plot.pdf", width = 10, height = 5)

# Univariate
ggplot(data, aes(x = reorder(TYPE, -table(TYPE)[TYPE]), fill = TYPE)
) + geom_bar(stat = 'count', show.legend = FALSE
) + coord_flip() + labs(x = 'Type', y = 'Count', title = 'Tipe Penjualan Rumah'
) + theme(legend.position = 'none'
) + geom_text(stat = 'count', aes(label = after_stat(count)), hjust = 0.5
) + scale_y_continuous(breaks = seq(0, 1500, 100))

# Bivariate
ggplot(data, aes(x = BEDS, y = BATH)
) + geom_point(color = "blue", size = 3, alpha = 0.2
) + labs( x = 'Beds', y = 'Bath', title = 'Jumlah Kamar Tidur beserta Jumlah Kamar Mandi'
) + scale_y_continuous(breaks = seq(0, 50, 5))

# Multivariate
ggplot(data, aes(x = PROPERTYSQFT, y = PRICE, color = TYPE)
) + geom_point() + scale_x_log10() + scale_y_log10(labels = scales::dollar_format()) + labs(
    x = 'Property Square Foot', y = 'Price',
    title = 'Sebaran Harga Rumah dengan Luasnya',
    subtitle = 'Tiap tipenya dibedakan berdasarkan warna')

tipeTerbanyak = data[data$TYPE %in% c("Co-op for sale", "House for sale", "Condo for sale"),]
ggplot(tipeTerbanyak, aes(x = PRICE)
) + geom_histogram() + facet_wrap(~TYPE, ncol = 1
) + labs(title = 'Harga Rumah Berdasarkan 3 Tipe Penjualan Terbanyak'
) + stat_bin(bins = 30
) + scale_x_continuous(breaks = seq(0, 60000000, 10000000), labels = scales::dollar_format())

#Map Graphs
koordinat <- data.frame(LONGITUDE = data$LONGITUDE, LATITUDE = data$LATITUDE)
peta <- st_as_sf(koordinat, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
mapview(peta, map.types = "OpenStreetMap")

#Statistic
numeric_columns <- sapply(data, is.numeric)
numeric_data <- data[, numeric_columns]
correlation_matrix <- cor(numeric_data)
ggcorrplot(correlation_matrix, type = "lower", lab = TRUE) + labs(title = 'Korelasi Antar Variabel')

ggplot(data, aes(x = BEDS, y = BATH)
) + geom_point(color = "blue", size = 3, alpha = 0.2
) + geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "solid"
) + labs(x = 'Beds', y = 'Bath', title = 'Hubungan Antara Jumlah Kamar Tidur dan Jumlah Kamar Mandi'
) + scale_y_continuous(breaks = seq(0, 50, 5))

#Word Cloud
script <- "http://www.sthda.com/upload/rquery_wordcloud.r"
source(script)
res<-rquery.wordcloud(
    "https://gitlab.com/nangdul56/pda/-/raw/main/alice_novel.txt", type ="file", lang = "english", textStemming=FALSE, min.freq=10, max.words=200)