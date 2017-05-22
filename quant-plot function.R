# updates 5/22/17:
  # plotting works now
  # a nice plot title is automatically generated, but user can specify their own
  # y-axis label automatically generated, but user can specify their own

# for testing: ----
# library(SWMPr)
# library(dplyr)
# library(lubridate)
# library(ggplot2)
# library(tibble)
# setwd("data visualization")
# path <- "C:/Users/kimberly.cressman/Desktop/Main Docs/Data-latest"
# bhwq <- import_local(path, 'gndbhwq', trace=TRUE)
# bhwq <- qaqc(bhwq, qaqc_keep = c(0,4))


# function ----
# required packages: dplyr, lubridate, ggplot2, tibble
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



quant.plot <- function(data, paramtoplot, 
                       yr, yrstart, yrend, 
                       yaxislab = paramtoplot,
                       maintitle = paste0(toupper(attributes(data)$station)," ", yr," Daily Average ",paste0(toupper(substr(paramtoplot, 1, 1)), substr(paramtoplot, 2, nchar(paramtoplot))),"\noverlaid on ", yrstart," - ", yrend, " daily averages") ) 
  {
  
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
  # need to pull this out so a column can be named 'paramtoplot'
  # which makes later coding easier
  dat_quantiles <- select_(dat_backdrop, paramtoplot = paramtoplot, 'monthday', 'year')
  
  # by_doy <- group_by(dat_quantiles, monthday)
  
  # subset featured data (using dplyr)
  dat_feature <- select_(dat_feature, paramtoplot = paramtoplot, 'monthday', 'year')
  
  # generate a summary table (using dplyr and tibble) ----
  
  # first gather quantiles for every monthday
  # could probably make this quantile(x, n, na.rm=TRUE)
  # where n is a vector that the user inputs at the beginning of the function to pull out whatever percentiles they want
  doy_sum2 <- tapply(dat_quantiles$paramtoplot, dat_quantiles$monthday, function(x) quantile(x, c(0, 0.25, 0.75, 1), na.rm=TRUE))
  # that spits out a list.
  # pull the list together with do.call and rbind:
  doy_sum3 <- do.call(rbind, doy_sum2)
  # but that's a matrix, so make it a data frame:
  doy_sum3 <- data.frame(doy_sum3)
  # and turn the row names into the column "monthday", using tibble::rownames_to_column(), and name it doy_sum
  doy_sum <- rownames_to_column(doy_sum3, var="monthday")
  
  # the above code probably lends itself to more flexibility and less error than the below stuff, but I'm not crazy about how the names come out
  # could hard code better ones but that defeats the purpose of flexibility
  # maybe user needs to input both a vector of desired quantiles AND a vector of desired names for those quantiles?
  
  # function(data, paramtoplot, yr, yrstart, yrend)
  # becomes
  # function(data, paramtoplot, yr, yrstart, yrend, toquant = c(0, 0.25, 0.75, 1), quantnames = c('min', '25%', '75%', 'max'), ...)

  # but then how does that translate down into the ribbon plot?
  # need to specify other things too; this could become a long function call when defaults are specified for everything:
  # outer ribbon (currenty hard coded as min and max)
  # inner ribbon (currently hard coded as 25th - 75th percentiles)
  # outer ribbon color (currently hard coded as light gray)
  # innter ribbon color (currently hard coded as gray65)
  # feature year line color (currently hard coded as red3)
  # axis titles
  # main title
  
    
  
  # doy_sum <- summarise(by_doy,
  #                      min = quantile(paramtoplot, 0, na.rm=TRUE),
  #                      pct10 = quantile(paramtoplot, 0.10, na.rm=TRUE),
  #                      pct25 = quantile(paramtoplot, 0.25, na.rm=TRUE),
  #                      pct75 = quantile(paramtoplot, 0.75, na.rm=TRUE),
  #                      pct90 = quantile(paramtoplot, 0.90, na.rm=TRUE),
  #                      max = quantile(paramtoplot, 1, na.rm=TRUE)
  # )
  
  # join the two data frames into one (using dplyr)
  all_doy <- full_join(dat_feature, doy_sum, by = "monthday")
  
  # get monthday back into date format by adding a year and then using lubridate's mdy function
  # using 2008 as the arbitrary year so leap days go where they should
  all_doy$monthday <- paste0(all_doy$monthday, "-2008")
  all_doy$monthday <- mdy(all_doy$monthday)
  
  
  # make a ribbon plot ----
  
  ggplot(all_doy) +
    geom_ribbon(aes(x=monthday, ymin=X0., ymax=X100., fill ='0-100 %iles')) +
    geom_ribbon(aes(x=monthday, ymin=X25., ymax=X75., fill ='25-75 %iles')) +
    geom_line(aes(x=monthday, y=paramtoplot, color='year of interest'), lwd=1.3) +
    theme_minimal() +
    scale_x_date(date_labels = "%m/%d", date_breaks = "1 month", date_minor_breaks="1 month") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x='Day of Year', 
         y=yaxislab, 
         title=maintitle) +
    scale_color_manual(name='',  values=c('year of interest' = 'red3')) +
    scale_fill_manual(name='', values=c('0-100 %iles' = 'lightgray', '25-75 %iles' = 'gray65'))
  
}
