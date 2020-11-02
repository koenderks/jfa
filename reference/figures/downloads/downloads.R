setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(cranlogs)
library(lubridate)
library(plyr)
library(ggplot2)
library(scales)
library(bbplot)

jfaData           <- cranlogs::cran_downloads(packages = "jfa", from = "2020-01-01")[, -3]
jfaData[["date"]] <- lubridate::floor_date(jfaData[["date"]], "month")
plotData          <- plyr::ddply(jfaData, "date", summarise, count = sum(count))
plotData$date[2]  <- as.Date("2020-02-01")

xBreaks           <- plotData[["date"]]
yBreaks           <- pretty(c(0, plotData[["count"]], max(plotData[["count"]]) + 100))

# Releases 
releases          <- c("2020-01-01", "2020-08-01", "2020-09-01", "2020-11-01")
releaseLabs       <- c("v0.1.0", "v0.2.0", "v0.3.0", "v0.4.0")

p <- ggplot(plotData, aes(x = date, y = count)) +
      geom_bar(stat = "identity", fill = rgb(65, 104, 195, 200, maxColorValue = 255), col = "black", size = 1) +
      scale_x_date(name = "", breaks = xBreaks, labels = scales::date_format("%m-%Y")) +
      scale_y_continuous(name = "Monthly downloads", breaks = yBreaks, labels = yBreaks, limits = range(yBreaks)) +
      bbc_style() +
      labs(title="CRAN downloads since January 2020") +
      geom_text(mapping = aes(x = date, y = count + 50), label = plotData$count) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
            axis.text.y = element_text(size = 12), 
            plot.title = element_text(size = 15)) +
      geom_curve(data = data.frame(x = as.Date(releases), y = rep(max(yBreaks) - 25, length(releases)), xend = as.Date(releases), yend = rep(max(yBreaks) - 100, length(releases))), 
                 aes(x = x, y = y, xend = xend, yend = yend), colour = "#555555", size=1, curvature = 0, arrow = arrow(length = unit(0.025, "npc"))) +
      annotate(geom = "text", x = as.Date(releases), y = rep(max(yBreaks), length(releases)), label = releaseLabs, vjust = -0.25)

finalise_plot(plot_name = p, source = "Source: https://cran-logs.rstudio.com", save_filepath = "downloads.png",
              width_pixels = 640, height_pixels = 350, logo_image_path = paste0(dirname(getwd()), "/logo/jfaLogo.png"))
