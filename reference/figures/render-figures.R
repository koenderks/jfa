########################################################################################
# This file renders the figures for the `jfa` readme (https://github.com/koenderks/jfa).
# It uses the cranlogs package to extract data for the downloads per country.
########################################################################################

width <- 9
height <- 6

#########################################################
#### Create the bar chart with downloads per month ######
#########################################################

# Acquire data using the cranlogs package
jfaData           <- cranlogs::cran_downloads(packages = 'jfa', from = '2020-01-01')[, -3]
jfaData[['date']] <- lubridate::floor_date(jfaData[['date']], 'month')
plotData          <- plyr::ddply(jfaData, 'date', plyr::summarise, count = sum(count))

# Determine axis breaks
xBreaks           <- plotData[['date']]
yBreaks           <- pretty(c(0, plotData[['count']], max(plotData[['count']]) + 200), n = 6)

# Specify the specific release dates (rounded down)
releases          <- c('2020-01-01', '2020-08-01', '2020-09-01', '2020-11-01', '2021-01-01', '2021-03-01', '2021-04-01', '2021-05-01')
releaseLabs       <- c('0.1.0', '0.2.0', '0.3.0', '0.4.0', '0.5.0', '0.5.1', '0.5.2', '0.5.3')

# Create the figure
p <- ggplot2::ggplot(plotData, ggplot2::aes(x = date, y = count)) +
  ggplot2::geom_bar(stat = 'identity', fill = rgb(65, 104, 195, maxColorValue = 255),
                    col = 'black', size = 1) +
  ggplot2::scale_x_date(name = '', breaks = xBreaks, labels = scales::date_format('%m-%Y')) +
  ggplot2::scale_y_continuous(name = '', breaks = yBreaks,
                              labels = yBreaks, limits = c(0, (max(yBreaks) + 0.1 * max(yBreaks)))) +
  ggplot2::labs(title = 'Monthly CRAN downloads since January 2020',
                subtitle = 'Source: http://cran-logs.rstudio.com') +
  ggplot2::geom_text(mapping = ggplot2::aes(x = date, y = count + 50), label = plotData[['count']]) +
  ggplot2::geom_curve(data = data.frame(x = as.Date(releases),
                                        y = rep(max(yBreaks) - 25, length(releases)),
                                        xend = as.Date(releases),
                                        yend = rep(max(yBreaks) - 100, length(releases))),
                      mapping = ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                      colour = '#555555', size = 1, curvature = 0,
                      arrow = ggplot2::arrow(length = ggplot2::unit(0.025, 'npc'))) +
  ggplot2::annotate(geom = 'text', x = as.Date(releases), y = rep(max(yBreaks), length(releases)),
                    label = releaseLabs, vjust = -0.25, angle = 45, size = 4) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10,
                                                     margin = ggplot2::margin(5, b = 10)),
                 axis.text.y = ggplot2::element_text(size = 12),
                 plot.title = ggplot2::element_text(size = 15),
                 plot.subtitle = ggplot2::element_text(size = 8),
                 panel.background = ggplot2::element_rect(fill = NA),
                 axis.ticks.y = ggplot2::element_blank(),
                 axis.ticks.x = ggplot2::element_blank())

# Save the figure
ggplot2::ggsave(plot = p, filename = 'man/figures/readme/downloads/downloads.svg', width = width, height = height, dpi = 300)

#########################################################
#### Create the world map with downloads per country ####
#########################################################

# Load previously stored data
prevData <- read.csv(paste0(dirname(getwd()), '/jfa/man/figures/readme/worldmap/downloads.csv'))

# Download new data from the day before yesterday (today - 2)
dnldata_folder <- installr::download_RStudio_CRAN_data(START = Sys.Date() - 2, END = Sys.Date() - 2)
dnldata <- installr::read_RStudio_CRAN_data(dnldata_folder, packages = 'jfa', use_data_table = TRUE)

# Merge new data with previously stored data and save the merged data set
data <- rbind(prevData, dnldata)

write.csv(x = data,
          file = paste0(dirname(getwd()), '/jfa/man/figures/readme/worldmap/downloads.csv'),
          row.names = FALSE)

# Transform data into something plottable
counts <- cbind.data.frame(table(data$country))
names(counts) <- c('country', 'count')

# Read world map coordinates into R
world<-maptools::readShapePoly(fn = paste0(dirname(getwd()),
                                           '/jfa/man/figures/readme/worldmap/ne_110m_admin_0_countries'))

# Convert incorrect names
ISO_full <- as.character(world@data$ISO_A2)
ISO_full[146] <- 'SOM'  # The iso identifier for the Republic of Somaliland is missing
ISO_full[89]  <- 'KV' # as for the Republic of Kosovo
ISO_full[39]  <- 'CYP' # as for Cyprus

# Create color codes for countires
colcode <- numeric(length(ISO_full))
names(colcode) <- ISO_full
dnl_places <- names(colcode[which(names(colcode) %in% as.character(counts$country))])
rownames(counts) <- counts$country
colcode[dnl_places] <- counts[dnl_places, 'count']

# Perform some magic
world@data$id <- rownames(world@data)
world.points <- ggplot2::fortify(world, by='id')
names(colcode) <- rownames(world@data)
world.points$Downloads <- colcode[world.points$id]

# Create the figure
world.map <-  ggplot2::ggplot(data=world.points) +
  ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, group=group, fill=Downloads),
                        color='black') +
  ggplot2::coord_equal() +
  ggplot2::theme_classic() +
  ggplot2::labs(title = 'Total CRAN downloads since January 2020',
                subtitle = 'Source: http://cran-logs.rstudio.com') +
  ggplot2::scale_fill_gradient2(low = 'white',
                                mid = rgb(107, 146, 237, maxColorValue = 255),
                                high = rgb(48, 68, 115, maxColorValue = 255),
                                midpoint = 5000,
                                na.value = 'white',
                                name = '',
                                limits = c(0, 10000)) +
  ggplot2::theme(axis.line = ggplot2::element_blank(),
                 axis.text = ggplot2::element_blank(),
                 axis.ticks = ggplot2::element_blank(),
                 axis.title = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(size = 15),
                 legend.position = 'bottom',
                 legend.key.width = ggplot2::unit(3, 'cm'),
                 legend.key.height = ggplot2::unit(0.25, 'cm'),
                 plot.subtitle = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(color = '#222222'))

# Save the figure
ggplot2::ggsave(plot = world.map, filename = 'man/figures/readme/worldmap/worldmap.svg', width = width, height = height, dpi = 300)