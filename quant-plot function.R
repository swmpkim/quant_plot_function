# required packages: dplyr, lubridate, ggplot2
# assuming data originally imported and processed with SWMPr

# definitions:
# data = data frame to use
# paramtoplot = parameter of interest (salinity, temperature, etc.)
# yr = year to feature as a red line
# yrstart = year to begin background comparison
# yrend = year to end background comparison

# this function aggregates data into daily average values for the entire time series
# then generates quantiles (min, 10%, 25%, 75%, 90%, max) across all years included in the background comparison for each individual calendar day (01/01, 01/02, ... 12/31)
# and then plots the 'feature' year's daily averages on top of that background

# example:
# cpwq <- apacpwq
# cpwq <- qaqc(cpwq)
# quant.plot(cpwq, 'sal', 2013, 2012, 2013)



quant.plot <- function(data, paramtoplot, yr, yrstart, yrend) {
  
  # pull out daily averages; name it 'dat2'
  dat2 <- aggreswmp(data, by='days', FUN='mean')
  
  
  # make a column for just mm-dd, and another column for year
  # these commands are from the lubridate package
  dat2$month <- month(dat2$datetimestamp)
  dat2$day <- day(dat2$datetimestamp)
  dat2$year <- as.character(year(dat2$datetimestamp))
  dat2$monthday <- as.character(paste0(dat2$month, "-", dat2$day))
  
  
  # graphing ----
  
  # split into feature year (for red) and backdrop years(to all be gray)
  # this uses the filter() function of dplyr to subset on year
  dat_feature <- filter(dat2, year == yr)
  dat_backdrop <- filter(dat2, year >= yrstart & year <= yrend)
  
  # work with quantiles ----
  
  # do some subsetting and sorting on backdrop data (using dplyr)
  dat_quantiles <- select_(dat_backdrop, paramtoplot = paramtoplot, 'monthday', 'year')
  by_doy <- group_by(dat_quantiles, monthday)
  
  # subset featured data (using dplyr)
  dat_feature <- select_(dat_feature, paramtoplot = paramtoplot, 'monthday', 'year')
  
  # generate a summary table (using dplyr)
  doy_sum <- summarise(by_doy,
                       min = quantile(paramtoplot, 0, na.rm=TRUE),
                       pct10 = quantile(paramtoplot, 0.10, na.rm=TRUE),
                       pct25 = quantile(paramtoplot, 0.25, na.rm=TRUE),
                       pct75 = quantile(paramtoplot, 0.75, na.rm=TRUE),
                       pct90 = quantile(paramtoplot, 0.90, na.rm=TRUE),
                       max = quantile(paramtoplot, 1, na.rm=TRUE)
  )
  
  # join the two data frames into one (using dplyr)
  all_doy <- full_join(dat_feature, doy_sum, by = "monthday")
  
  # get monthday back into date format by adding a year and then using lubridate's mdy function
  # using 2008 as the arbitrary year so leap days go where they should
  all_doy$monthday <- paste0(all_doy$monthday, "-2008")
  all_doy$monthday <- mdy(all_doy$monthday)
  
  
  # make a ribbon plot ----
  
  ggplot(all_doy) +
    geom_ribbon(aes(x=monthday, ymin=min, ymax=max, fill ='0-100 %iles')) +
    geom_ribbon(aes(x=monthday, ymin=pct25, ymax=pct75, fill ='25-75 %iles')) +
    geom_line(aes(x=monthday, y=paramtoplot, color='year of interest'), lwd=1.3) +
    theme_minimal() +
    scale_x_date(date_labels = "%m/%d", date_breaks = "1 month", date_minor_breaks="1 month") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x='Day of Year', y=paramtoplot, title=paste0(attributes(data)$station," ", yr," Daily Average ",paramtoplot," overlaid on data from ", yrstart," - ", yrend)) +
    scale_color_manual(name='',  values=c('year of interest' = 'red3')) +
    scale_fill_manual(name='', values=c('0-100 %iles' = 'lightgray', '25-75 %iles' = 'gray65'))
  
}
