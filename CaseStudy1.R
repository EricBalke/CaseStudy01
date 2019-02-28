library(XML)
library(dplyr)
library(tidyr)
library(stringi)
library(rvest)
library(ggplot2)
library(RCurl)
library(data.table)
library(plyr)
library(knitr)
library(stringr)
library(RColorBrewer)

setwd("./CaseStudy1")

# 1.	How many breweries are present in each state?
# Put URL address into the beerURL and breweryURL objects
beerURL <- paste0("https://raw.githubusercontent.com/BivinSadler/MSDS-6306-Doing-Data-Science/master/Unit%207/Beers.csv")
beers <- read.csv(beerURL,stringsAsFactors = F)
str(beers)

breweryURL <- paste0("https://raw.githubusercontent.com/BivinSadler/MSDS-6306-Doing-Data-Science/master/Unit%207/Breweries.csv")
brewery <- read.csv(breweryURL, stringsAsFactors = F)
brewery$State <- trimws(brewery$State)

# import us state populations
url <- 'https://simple.wikipedia.org/wiki/List_of_U.S._states_by_population'
page <- read_html(url)
html.table <- page %>% html_table(header=T)
state.pop <- html.table[[1]]
state.pop <- state.pop[,3:5]
names(state.pop) <- c('State','Estimate2018','Population2010')
str(state.pop)

# import beer consumption by states statistics
beer.consume <- read.csv('BeerConsumptionPerState.csv', stringsAsFactors = F)

# Answer to question 1
# calculate breweries per state
state.brewery <- ddply(brewery,"State",summarize,Breweries=length(Brew_ID))
kable(state.brewery)

# plot of answer to question 1
state.brewery %>% ggplot(aes(x=reorder(State, -Breweries), y=Breweries)) + 
  theme (axis.text.x = element_text(angle = 90, vjust = .5, size = 6) ) +
  theme (axis.text.y = element_text(size = 6)) +
  xlab("State") +
  ylab("Number of Breweries") + 
  ggtitle("Breweries per State") +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_bar(stat = "Identity")

# create an index for state name and state code
state.index <- data.frame(state.name=as.character(state.name), state.code=as.character(state.abb))
state.index$state.name <- as.character(state.index$state.name)
state.index$state.code <- as.character(state.index$state.code)

# merge data frame showing breweries per state and state index
state.brew.m <- merge(state.brewery,state.index,by.x="State",by.y="state.code",all.x = T)
head(state.brew.m)

# clean up state populations tables
state.pop.num <- state.pop
find.bracket <- grep("\\[", state.pop.num$Estimate2018)
bracket.loc <- stringr::str_locate(state.pop.num$Estimate2018[find.bracket],"\\[")
state.pop.num$Estimate2018[find.bracket] <- substr(state.pop.num$Estimate2018[find.bracket],1,bracket.loc[,1] - 1)
state.pop.num$Estimate2018[find.bracket]

find.bracket <- grep("\\[", state.pop.num$Population2010)
bracket.loc <- stringr::str_locate(state.pop.num$Population2010[find.bracket],"\\[")
state.pop.num$Population2010[find.bracket] <- substr(state.pop.num$Population2010[find.bracket],1,bracket.loc[,1] - 1)
state.pop.num$Population2010[find.bracket]

# convert population statistics from text to numeric
state.pop.num$Estimate2018 <- as.numeric(gsub(",","",state.pop.num$Estimate2018))
state.pop.num$Population2010 <- as.numeric(gsub(",","",state.pop.num$Population2010))

# merge state brewery counts and state populations
brewery.sum.pop <- merge(state.brew.m,state.pop.num,by.x="state.name",by.y="State",all.x = T)
check.na <- is.na(brewery.sum.pop$Estimate2018) | is.na(brewery.sum.pop$Population2010)

# remove na's for state populations - removes dc
brewery.sum.pop <- brewery.sum.pop[!check.na,]
check.na <- is.na(brewery.sum.pop$Estimate2018) | is.na(brewery.sum.pop$Population2010)
check.na

# merge beer consuption statistics with state brewery and population counts
brewery.sum.pop.cons <- merge(brewery.sum.pop,beer.consume,by.x="state.name",by.y="State",all.x = T)
head(brewery.sum.pop.cons)

# plot data beer consumption per capita per state
brewery.sum.pop.cons %>% ggplot(aes(x=reorder(State, -PerCapita), y=PerCapita)) + 
  theme (axis.text.x = element_text(angle = 90, vjust = .5, size = 6) ) +
  theme (axis.text.y = element_text(size = 6)) +
  xlab("State") +
  ylab("Gallons per Year") + 
  ggtitle("Consumpion per Capita") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_bar(stat = "Identity")

# plot beer consuption per capita per number of breweries
brewery.sum.pop.cons %>% ggplot(aes(x=Breweries, y=PerCapita, label = State)) +
  xlab("Number of Breweries") +
  ylab("Gallons per Year") +
  ggtitle("Statewide Personal Consumption per Number of Breweries") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = State), hjust = 0, vjust = 0, size = 3, position = position_jitter(width = 0.5, height = 0.5))

# plot beer total consumption per number of breweries
brewery.sum.pop.cons %>% ggplot(aes(x=Breweries, y=Total, label = State)) +
  xlab("Number of Breweries") +
  ylab("Consumption in Gallons") +
  ggtitle("Statewide Total Consumption per Number of Breweries") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = State), hjust = 0, vjust = 0, size = 3, position = position_jitter(width = 0.5, height = 0.5))

# plot total population (projected) per number of breweries
brewery.sum.pop.cons %>% ggplot(aes(x=Breweries, y=Estimate2018, label = State)) +
  xlab("Number of Breweries") +
  ylab("2018 Population") +
  ggtitle("Total State Population per Number of Breweries") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = State), hjust = 0, vjust = 0, size = 3, position = position_jitter(width = 0.5, height = 0.5))

# 2.	Merge beer data with the breweries data. Print the first 6 observations and the last six observations to check the merged file.
# have to fix this merge before proceeding... EB 2/18
merged_data <- merge(beers, brewery, by.x = "Brewery_id", by.y = "Brew_ID")
head(merged_data)
tail(merged_data)

# fix name collisions
colnames(merged_data)[2] <- c("Beer_Name")
colnames(merged_data)[8] <- c("Brewery_Name")

head(merged_data)

beers.per.brewery <- ddply(merged_data, "Brewery_Name", summarize, BeerCount = length(Beer_ID))
head(beers.per.brewery)
brewery.state <- data.frame(Brewery_Name = merged_data$Brewery_Name, State = merged_data$State)
brewery.state <- unique(brewery.state)
beers.per.brewery <- merge(beers.per.brewery, brewery.state, by = "Brewery_Name", all.y = T)
top.twenty.brewery <- beers.per.brewery[order(beers.per.brewery$BeerCount, decreasing = T),]
top.twenty.brewery <- top.twenty.brewery[1:20,]
bottom.twenty.brewery <- beers.per.brewery[order(beers.per.brewery$BeerCount, decreasing = F),]
bottom.twenty.brewery <- bottom.twenty.brewery[1:20,]

# plot breweries with most beers by brewery color by state
top.twenty.brewery %>% ggplot(aes(x=reorder(Brewery_Name, -BeerCount), y=BeerCount, color = State)) + 
  theme (axis.text.x = element_text(angle = 90, vjust = .5, size = 6) ) +
  theme (axis.text.y = element_text(size = 6)) +
  xlab("Brewery Name") +
  ylab("Number of Beers") + 
  labs("Beers per Brewery") +
  scale_fill_brewer() +
  geom_bar(stat = "Identity")

# 3.	Report the number of NA's in each column.
NAs <- colSums(is.na(merged_data))
notNAs <- colSums(!is.na(merged_data))
NAs
notNAs
NAs[4]
NAs[5]
notNAs[4]
notNAs[5]

abv.na.df <- data.frame(NA_NOTNA = "NAs", Number = NAs[4])
abv.not.df <- data.frame(NA_NOTNA = "NotNA", Number = notNAs[4])
ibu.na.df <- data.frame(NA_NOTNA = "NAs", Number = NAs[5])
ibu.not.df <- data.frame(NA_NOTNA = "NotNA", Number = notNAs[5])

abv.df <- rbind(abv.na.df, abv.not.df)
ibu.df <- rbind(ibu.na.df, ibu.not.df)

par(mfrow = c(1,1))

# pie chart showing NAs of IBU and ABV
abv.df %>% ggplot(aes(x="", y = Number, fill = NA_NOTNA)) + 
  xlab("")+
  theme(legend.title = element_blank()) +
  ggtitle("Number of Null ABV Values") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_bar(width = 1, stat = "Identity") +
  coord_polar("y", start = 0)


ibu.df %>% ggplot(aes(x="", y = Number, fill = NA_NOTNA)) +
  xlab("") +
  theme(legend.title = element_blank()) +
  ggtitle("Number of Null IBU Values") +
  theme(plot.title = element_text(hjust = =0.5)) +
  geom_bar(width = 1, stat = "Identity") +
  coord_polar("y", start = 0)

# 4.	Compute the median alcohol content and international bitterness unit for each state. Plot a bar chart to compare.
median(merged_data$ABV, na.rm=TRUE)
median(merged_data$IBU, na.rm=TRUE)

hist(merged_data$ABV, xlab = "ABV", main = "Histogram of ABV")
hist(merged_data$IBU, xlab = "IBU", main = "Histogram of IBU")

#ABV_med <- setDT(as.data.frame(sapply(c("ABV"), function(i) tapply(merged_data[[i]], merged_data$State, median, na.rm=TRUE))), keep.rownames = "State")[]
#IBU_med <- setDT(as.data.frame(sapply(c("IBU"), function(i) tapply(merged_data[[i]], merged_data$State, median, na.rm=TRUE))), keep.rownames = "State")[]

IBU.med <- ddply(merged_data,"State",summarize, IBU.med=median(IBU,na.rm = TRUE))
ABV.med <- ddply(merged_data,"State",summarize, ABV.med=median(ABV,na.rm = TRUE))

ggplot(ABV.med, aes(x=State, y =ABV.med)) + 
  geom_bar(stat = "identity") + 
  labs(x="State",y="ABV") + 
  ggtitle("Median ABV by State") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text=element_text(size=5), axis.text.x = element_text(angle = 90, vjust = 0.5))

ggplot(IBU.med, aes(x=State, y =IBU.med)) + 
  geom_bar(stat = "identity") + 
  labs(x="State",y="IBU") + 
  ggtitle("Median IBU by State") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text=element_text(size=5), axis.text.x = element_text(angle = 90, vjust = 0.5))

# 5.	Which state has the maximum alcoholic (ABV) beer? Which state has the most bitter (IBU) beer?
ABVMax <- merged_data[which.max(merged_data$ABV),]
ABVMax
IBUMax <- merged_data[which.max(merged_data$IBU),]
IBUMax

# 6.	Summary statistics for the ABV variable.
summary(merged_data$ABV)

# 7.	Is there an apparent relationship between the bitterness of the beer and its alcoholic content? Draw a scatter plot.
ggplot(merged_data, aes(x=ABV, y = IBU)) + geom_point() + geom_smooth(method="lm", se=TRUE, fullrange=FALSE, level=0.95, linetype="dashed", color="darkred", fill="blue")

# There appears to be a positive correlation between ABV and IBU.