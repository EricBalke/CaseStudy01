# CaseStudy01
MSDS CaseStudy01 repo

The purpose of this Case Study is to answer the following 7 questions to the CEO of Budweiser for decision support.

1.	How many breweries are present in each state?

2.	Merge beer data with the breweries data. Print the first 6 observations and the last six observations to check the merged file.

3.	Report the number of NA's in each column.

4.	Compute the median alcohol content and international bitterness unit for each state. Plot a bar chart to compare.

5.	Which state has the maximum alcoholic (ABV) beer? Which state has the most bitter (IBU) beer?

6.	Summary statistics for the ABV variable.

7.	Is there an apparent relationship between the bitterness of the beer and its alcoholic content? Draw a scatter plot.

Brewer - Initial Round of Slides, Final Plots beyond initial quesionts, and polish.

Balke - Initial Round of code answering questions, and polish.

beers - data provided by Dr. Sadler for project containing beer selections, ABV, and IBU values for breweries by state.

brewery - data provided by Dr. Sadler for project containing breweries available in every state

state.pop - state populations from 2010 census and estimates for 2018 population for individual states

beer.consume - data from https://vinepair.com/articles/map-states-drink-beer-america-2018/ showing beer consumption per capita and total consumption by state.

state.brewery - summarized by state calculating total breweries in the individual states.

state.index - a cross reference built using state.name (r data with all the state names) and state.abb (r data with all the state abbreviations

state.brew.m - merged data set combining state.brewery and state.index

state.pop.num - derived from state.pop but cleaning up population statistics and converting to numeric values

brewery.sum.pop - merged data set combining state.brew.m and state.pop.num

brewery.sum.pop.cons - merged data set combining brewery.sum.pop and beer.consume

merged_data - merged data set combining beers and brewery

top.twenty.brewery - top twenty breweries by beer selection derived from merged_data

abv.df - data set derived from counts of missing and non-missing values of ABV variable in merged_data

ibu.df - data set derived from counts of missing and non-missing values of IBU variable in merged_data

IBU.med - data set summarizing median IBU values by state

ABV.med - data set summarizing median ABV values by state

ABVMax - data set identifying max ABV value in merged_data

IBUMax - data set identifying max IBU value in merged_data
