setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(cranlogs)
library(lubridate)
library(plyr)
library(ggplot2)
library(JASPgraphs)
library(scales)
library(bbplot)

jfaData <- cranlogs::cran_downloads(packages = "jfa", from = "2020-01-01")[, -3]
jfaData$date <- floor_date(jfaData$date, "month")
plotData <- ddply(jfaData, "date", summarise, count = sum(count))

plotData$date[2] <- as.Date("2020-02-01")

xBreaks <- JASPgraphs::getPrettyAxisBreaks(plotData$date)

p <- ggplot(plotData, aes(x = date, y = count)) +
      geom_bar(stat = "identity", fill = "darkblue") +
      xlab("") +
      ylab("Monthly downloads") +
      scale_x_date(breaks = xBreaks, labels = date_format("%m-%Y")) +
      bbc_style() +
      labs(title="jfa (R package)",
            subtitle = "Monthly downloads since january 2020") +
      geom_text(mapping = aes(x = date, y = count + 50), label = plotData$count) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

finalise_plot(plot_name = p,
              source = "Source: https://cran-logs.rstudio.com/",
              save_filepath = "downloads.png",
              width_pixels = 640,
              height_pixels = 350,
              logo_image_path = paste0(dirname(getwd()), "/logo/jfaLogo.png"))
