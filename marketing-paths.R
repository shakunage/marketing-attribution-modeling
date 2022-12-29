# Installing and loading the Channel Attribution library
install.packages("ChannelAttribution")
library(ChannelAttribution)

# First, we read the path data into a variable, and analyze the path data using the heuristic_models() function in the ChannelAttribution package.
# This data set is based on impressions, which tracks exposure to advertising.
# Both the conversion values and the number of conversions are collated into the same data frame. 

path_data <- read.csv("path_data.csv", sep=';')
conversion_values <- heuristic_models(path_data, "path", "total_conversion_value")

names(conversion_values)[names(conversion_values) == "first_touch"] <- "first_touch_value"
names(conversion_values)[names(conversion_values) == "last_touch"] <- "last_touch_value"
names(conversion_values)[names(conversion_values) == "linear_touch"] <- "linear_touch_value"

conversion_counts <- heuristic_models(path_data, "path", "total_conversions")

names(conversion_counts)[names(conversion_counts) == "first_touch"] <- "first_touch_conversions"
names(conversion_counts)[names(conversion_counts) == "last_touch"] <- "last_touch_conversions"
names(conversion_counts)[names(conversion_counts) == "linear_touch"] <- "linear_touch_conversions"


conversions <- conversion_values
conversions <- merge(conversions, conversion_counts, by="channel_name")

conversions

# Rounding the values to the nearest integer for better readability.

round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
(df)
}

round_df(conversions)

# Output: 

#       channel_name first_touch_value last_touch_value linear_touch_value first_touch_conversions last_touch_conversions linear_touch_conversions
# 1        Affiliate             21692            23797              22592                     308                    339                      321
# 2          Display              6728                0               4767                      96                      0                       68
# 3       Newsletter              2628             8825               4665                      37                    125                       66
# 4            Other              2094             1882               2020                      30                     26                       29
# 5 Price_comparison              6175             6681               6318                      89                     98                       92
# 6      Retargeting               630              649                676                       9                      9                        9
# 7              SEA             65931            60448              62929                     928                    851                      886
# 8              SEO             14367            17963              16278                     211                    260                      237


# Out of the featured marketing channels, search engine advertising generates the largest share of revenue by far. 
# It appears to be widely present both at the beginnings and ends of customer paths. 
# Generally, all marketing channels seem to be present at first and last touches proportionally, apart from two exceptions, which are email marketing (‘Newsletter’) and banner advertising (‘Display’). 
# These channels seem to have the most impact at last touches and first touches respectively. 


# In the second stage, we are working with session-level data, which tracks advertising engagement by counting clicks.
# The data is read to a variable and then converted into a more presentable data frame format.

session_log_data <- read.csv("session_log_data.csv", sep=';')

clicks <- table(session_log_data["channel"])
clicks
clicks <- t(data.frame(lapply(clicks, type.convert), stringsAsFactors=FALSE))
clicks
colnames(clicks) <- c('clicks')
colnames(clicks)[1]<- "clicks"
clicks <- data.frame("channel_name"= c(row.names(clicks)), "clicks" = c(data.frame(clicks)))
clicks 

# Output: 

#   channel_name      clicks
# 1 Affiliate          6130
# 2 Display          135432
# 3 Newsletter         7234
# 4 Other              6323
# 5 Price_comparison   9262
# 6 Retargeting         961
# 7 SEA               57007
# 8 SEO               16942

# Display advertisements (i.e. website banners) seem to have the most engagement, despite not generating all that much revenue, as seen in the previous stage of this analysis.
# Search engines seem to generate a fair amount of traffic as well, which is to be expected, as they generate most of the revenue out of all marketing channels.

# In the third stage, we are considering marketing costs into the scope of analysis.
# Once again, a new table, this time the cost data is loaded into a variable.
# Then, data based on impressions and clicks is combined with the cost data into a single table. 


cost_data <- read.csv("cost_data.csv", sep=';')
names(cost_data)[names(cost_data) == 'channel'] <- 'channel_name'
cost_data

conv_clicks <- merge(conversions, clicks, by="channel_name")
alldata <- merge(conv_clicks, cost_data, by="channel_name")
alldata

# In this part we analyze the cost-efficiency of these channels.
# Thus, marketing channels with no cost data are disregarded

alldata <- alldata[-c(3,4,5,8), ]
alldata$clicks <- as.numeric(as.character(alldata$clicks))
alldata

# mROI is short for marketing return on investment. 
# This number is interesting, as in theory, it should tell us whether our marketing efforts generate value to our business.
# Affiliate marketing has cost data for both click-based and commission-based cost model. Let's calculate the click-based mROI first.

alldata$mROI_first_touch <- with(alldata, ((first_touch_value-(cost_per_click*clicks))/(clicks*cost_per_click)))
alldata$mROI_last_touch <- with(alldata, ((last_touch_value-(cost_per_click*clicks))/(clicks*cost_per_click)))
alldata$mROI_linear_touch <- with(alldata, ((linear_touch_value-(cost_per_click*clicks))/(clicks*cost_per_click)))

mROI_data <- alldata[,c(1,11,12,13) ]
mROI_data

# Then, let's calculate the commission-based mROI for Affiliate marketing. 
# In this model, the affiliates are awarded a 50% commission of the sale every time affiliate marketing ends up being the last touch before the conversion.  

affiliate_data <- alldata[c(1), ]
affiliate_data <- affiliate_data[, -c(11,12,13)]

affiliate_data$mROI_first_touch <- with(affiliate_data, ((first_touch_value-(0.5*last_touch_value))/(0.5*last_touch_value)))
affiliate_data$mROI_last_touch <- with(affiliate_data, ((last_touch_value-(0.5*last_touch_value))/(0.5*last_touch_value)))
affiliate_data$mROI_linear_touch <- with(affiliate_data, ((linear_touch_value-(0.5*last_touch_value))/(0.5*last_touch_value)))

affiliate_data[1,1] <- "Affiliate_commission"
affiliate_data <- affiliate_data[,c(1,11,12,13) ]

# Finally, let's compare the mROIs to conclude this analysis. 

mROI_data[1,1] <- "Affiliate_perclick"
mROI_data <- c(affiliate_data)
mROI_data <- rbind(mROI_data, affiliate_data)
mROI_data

# Output: 

#             channel_name mROI_first_touch mROI_last_touch mROI_linear_touch
# 1    Affiliate_perclick        0.7693312       0.9410277         0.8427354
# 2               Display       -0.8580627      -1.0000000        -0.8994233
# 6           Retargeting        0.8730489       0.9295377         1.0110500
# 7                   SEA       -0.2289719      -0.2930927        -0.2640763
# 11 Affiliate_commission        0.8230869       1.0000000         0.8987214

# The marketing channels seem to generate positive return, apart from banner ads (Display) and search engine advertising (SEA).
# These channels could be improved by perhaps targeting better keywords on searches, or making the webstore site more usable, leading to more conversions. 
# The commission-based model for affiliate marketing generates better returns for the advertiser across the board, so it should be favored over the per-click model.