Does money buy happiness? Insights from the World Happiness Report Index 2019
================
Bilgeyis Tahmasib, Marcela Cevallos
January 9, 2020

Introduction and objective
==========================

The main objective of the present work is to analyze the World Happiness Report Index 2019,to get insights to answer:

### Does money buy happiness?

For this, we will be working on various datasets.

Our main dataset will be the data downloaded from the World Happiness Report. The second data set that we are going to work with is the population dataset downloaded from the open repository of the World Bank for the period: 1960-2018. In addition, we will be considering a dataset used in class, that uses the happiness 2018 index and has a column detailing the regions of the countries in our previous datasets. And finally, we will be downloading the world map, to construct the base layer of our world maps to present the happiness information.

``` r
library(gifski)
library(ggridges)
library(PerformanceAnalytics)
library(corrplot)
library(plotly)
library(ggpubr)
library(MASS)
library(reshape)
library(readxl)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(dplyr)
library(maps)
library(stringr)
library(tidyverse)
library(gganimate)
library(gifski)
library(png)
library(streamgraph)
```

``` r
#setwd ("C:/Users/user/Desktop/classes/sem3/avd_R/Advanced_Visualization_in_R")



#Loading the first dataframe: population from World Bankl 1960-2018

population <- read_excel("C:/Users/user/Desktop/classes/sem3/avd_R/project/melt.xls", skip = 2)
as.data.frame(population)

#Melting the dataframe population to get panel data of the years

population2<-reshape2::melt(population, id= c ("Country Name",  "Country Code", "Indicator Name" ,"Indicator Code"))

#Loading the second dataframe, which is our main dataframe: information was downloaded from the World Happiness Report, 2018

data<-read_excel("C:/Users/user/Desktop/classes/sem3/avd_R/project/Chapter2OnlineData.xls")


#Loading the third dataframe: happy, that links countries with regions
happy <- read.csv("C:/Users/user/Desktop/classes/sem3/visual_R/Class_04/AdvancedVisualizationR-2019/Dane/happy2015.csv")


#Renaming the heterogeneous names in both dataframes and ordering by country name 

population2 <-population2[order(population2$`Country Name`),]
names(population2)[names(population2) == "Country Name"] <- "Country_name"
names(data)[names(data) == "Country name"] <- "Country_name"
names(population2)[names(population2) == "variable"] <- "Year"
names(population2)[names(population2) == "value"] <- "Population"
names(happy)[names(happy) == "Country"] <- "Country_name"
happy <-happy[order(happy$Country_name),]


#Finding the differences between data and population dataframes: the following command shows all the names existing in the first dataframe, but not in the second. 

setdiff(data$Country_name, population2$Country_name)

#Renamed the countries for population2 

population2$Country_name<-recode(population2$Country_name, 
                                 "Russian Federation"= "Russia",
                                 "Iran, Islamic Rep."= "Iran",
                                 "Egypt, Arab Rep."= "Egypt",
                                 "Gambia, The" = "Gambia",
                                 "Kyrgyz Republic" = "Kyrgyzstan",
                                 "Lao PDR" = "Laos",
                                 "Slovak Republic" = "Slovakia",
                                 "Syrian Arab Republic" = "Syria",
                                 "Yemen, Rep."= "Yemen",
                                 "Venezuela, RB" = "Venezuela",
                                 "Cote d'Ivoire"= "Ivory Coast",
                                 "Hong Kong SAR, China" = "Hong Kong S.A.R. of China",
                                 "Congo, Rep."  =  "Congo (Brazzaville)",
                                 "Congo, Dem. Rep."=  "Congo (Kinshasa)" ,
                                 "Korea, Rep." = "South Korea",
                                 "North Macedonia" = "Macedonia",
                                 "Cyprus"= "North Cyprus", 
                                 "Somalia" = "Somaliland region")




#Merge the main dataframe and population2 by country name

total <- merge(data, population2, by=c("Country_name", 'Year'))

#Delete the columns that we do not need using select function

total=subset(total,select=-c(17,20:26,28,29))

setdiff(total$Country_name, happy$Country_name)

happy$Country_name<-recode(happy$Country_name, 
                           "Hong Kong"= "Hong Kong S.A.R. of China",
                           "Sudan" ="South Sudan")

#Selecting only regions from "happy" dataframe
regions<-happy[0:2]


#Merge total and regions dataframes by country name

total <- merge(total, regions, by=c("Country_name"))



#Uploading the world map
world <- map_data("world")

#region is the country name, so lets rename it
names(world)[names(world) == "region"] <- "Country_name"


#Finding the differences between total and world:

setdiff(total$Country_name, world$Country_name)

#And changing in world


world$Country_name<-recode(world$Country_name, 
                           "USA" = "United States",
                           "UK"= "United Kingdom",
                           "Cyprus" ="North Cyprus", 
                           "Republic of Congo" = "Congo (Brazzaville)",
                           "Democratic Republic of the Congo"  = "Congo (Kinshasa)",
                           "Hong Kong" = "Hong Kong S.A.R. of China",
                           "Somalia"= "Somaliland region" ,
                           "Trinidad" = "Trinidad and Tobago")
```

Happiest regions in the World, 2005-2018
========================================

``` r
#Visualizing the happiest regions in the complete time scope



table_1<-total %>% group_by(Region) %>% 
  summarise(Mean.response = mean(`Life Ladder`))

as.data.frame.matrix(table_1) 
```

``` r
ordered_table_1 <- table_1[order(-(table_1$Mean.response)),]
ordered_table_1
```

    ## # A tibble: 10 x 2
    ##    Region                          Mean.response
    ##    <fct>                                   <dbl>
    ##  1 Australia and New Zealand                7.31
    ##  2 North America                            7.27
    ##  3 Western Europe                           6.84
    ##  4 Latin America and Caribbean              6.01
    ##  5 Eastern Asia                             5.47
    ##  6 Middle East and Northern Africa          5.45
    ##  7 Southeastern Asia                        5.34
    ##  8 Central and Eastern Europe               5.31
    ##  9 Southern Asia                            4.59
    ## 10 Sub-Saharan Africa                       4.25

The present table describes the happiest regions considering the complete available time scope.

Some insights reflect that in the period 2005-2018, the happiest region was Australia and New Zealand with a happiness score of 7.31, followed by North America with a happiness score of 7.27 and Western Europe with 6.84 as a score. From this table we can derive our first educated guess: Wealthier regions are on overage happier in contrast with the poorer ones. We can see this effect clearly if we compare the unhappiest region: Sub-Saharan Africa with a score lower than half of the index (4.25), followed by Southern Asia (4.59)

-   Note: the means are in descending order

<a href="#top">Back to table of content</a>

World Map happiness index heatmap, 2017
=======================================

``` r
#From the dataframe "total" we take only life ladder, country name of the year 2018 and creating a new data frame happy_2017 and then join it with the "world" dataframe


happy_2017<-subset(total, total$Year==2017)


happy_2017 <- happy_2017 %>%
  dplyr::select("Country_name", "Life Ladder", "Country Code", "Region", "Year", "Population")



happySubset_17 <- inner_join(world, happy_2017, by = "Country_name")

# Creating the mean coordinates to get one coordinate of latitude and longitude per country

test <- 
  happySubset_17 %>%
  group_by(Country_name) %>%
  summarise(mean_long = mean(long), mean_lat = mean(lat))  



happySubset_17test <- merge(happySubset_17, test, by = "Country_name")

# Plotting

plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)


 missing_countries<-world %>% filter(world$Country_name== c('Sudan', 'Angola', 'Namibia' ,'Cuba', 'Taiwan', 'Eritrea', 'Gambia', 'Guam', 'Malaysia', 'Oman', 'Qatar', 'Swaziland', 'Syria'))

all_countries <- bind_rows(happySubset_17,missing_countries)
 
ggplot(data = all_countries, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = `Life Ladder`), colour = "black") +
  scale_fill_distiller(palette ="YlOrRd", direction = -1) + 
  ggtitle("Life Ladder Indext (LLI) for 2017") +
  plain +
  geom_text(data= happySubset_17test,aes(x= mean_long, y= mean_lat, label= Country_name),
            color = "black", check_overlap = TRUE, size = 3, hjust = 0) 
```

![](Advanced_R_project_files/figure-markdown_github/unnamed-chunk-5-1.png)

The 2017-happiness subset reflects similar behaviour to the table of the happiests regions in the world. In this sense, we can see that the happiest countries are comprised in the regions displaying the full time frame, but for 2017. The absolute happiest country is Finland with a happiness score of 7.78, followed by Denmark with a score of 7.59. On the third place comes very close Norway with a score of 7.57. Similarly, the bottom 3 places were held by Afghanistan (2.66), South Sudan (2.81) and Rwanda (3.10)

-   Note: the countries displayed in grey correspond to those where the life ladder is not available for the year of 2017

<a href="#top">Back to table of content</a>

Happiness per selected countries: Poland, Ecuador, Azerbaijan for all years
===========================================================================

``` r
ecuador_all=total[total$Country_name == "Ecuador",]
poland_all=total[total$Country_name == "Poland",]
azerbaijan_all=total[total$Country_name == "Azerbaijan",]


countries_allyears=rbind(ecuador_all,poland_all,azerbaijan_all)


# plot separately
ggplot(countries_allyears, aes(fill=countries_allyears$Country_name, y=countries_allyears$`Life Ladder`, x=countries_allyears$Year)) + 
  geom_bar(width = 0.6, position= position_dodge(width=0.5),stat="identity", colour="black") +
  facet_wrap(~Country_name, scales = "free_y",ncol=3) + 
  theme_bw() +
  theme(strip.text = element_text(size=15, face="bold"))+
  theme(legend.position="none")+
  #theme(panel.grid.major  = element_line(colour = "black", size = 0.2))+
  #theme(panel.grid.minor  = element_line(colour = "black", size = 0.2))+
  theme(axis.text.x = element_text(angle = 30, hjust =1, vjust =0.5, size=12))+
  labs(x = expression(paste("Years")), y = expression(paste("Happiness evolution per selected countries")))
```

![](Advanced_R_project_files/figure-markdown_github/unnamed-chunk-6-1.png)

<a href="#top">Back to table of content</a>

Happiness per selected countries: Poland, Ecuador, Azerbaijan for 2017
======================================================================

``` r
ecuador=happySubset_17[happySubset_17$Country_name == "Ecuador",]
poland=happySubset_17[happySubset_17$Country_name == "Poland",]
azerbaijan=happySubset_17[happySubset_17$Country_name == "Azerbaijan",]


countries=rbind(ecuador,poland,azerbaijan)

countries$Year = as.character(countries$Year)

ggplot(countries, aes(fill=countries$Country_name, y=countries$`Life Ladder`, x=countries$Year)) + 
  geom_bar(width = 0.6, position= position_dodge(width=0.8),stat="identity", colour="black") +
  labs(x = expression(paste("Years")), y = expression(paste("Happiness index per selected countries in the year of 2017")), fill = "Countries") +
  coord_flip()
```

![](Advanced_R_project_files/figure-markdown_github/unnamed-chunk-7-1.png)

On the micro-level, we have selected 3 countries to present to the class, being these: Azerbaijan, Ecuador and Poland. The first 2 as they represent the origin countries of the authors, and the third one to be able to analyze the current country of residence. From this horizontal histogram, we can observe that the happiest country among the 3 in 2007 is Poland, followed by Ecuador and Azerbaijan. Although the structural differences among the countries are very different, the happiness score are not that disperse, being these: Poland 6.20, Ecuador 5.83, and Azerbaijan 5.15. if we consider the inequality and log income per capita we see that the same relationship of the order holds, being the wealthiest: Poland with a log GDP per capita of 10.211, a GINI(N/A), Azerbaijan with a log GDP of 9.670 and a Gini of 0.21 and Ecuador with a log GDP per capita of 9.266, and a GINI of 0.496. Even though there are major differences among happiness index and log GDP there are differences in Gini coefficient where Ecuador has more than twice as much inequality as Azerbaijan.

<a href="#top">Back to table of content</a>

Distribution of the log of the income GDP per capita and inequality, 2017
=========================================================================

``` r
gdp_2017<-subset(total, total$Year==2017)



gdp_2017 <- gdp_2017 %>%
  dplyr::select("Country_name", "Life Ladder", "Country Code", "Region", "Year", "Population", "Log GDP per capita",  "Generosity",  "GINI index (World Bank estimate), average 2000-16")


one <- ggplot(gdp_2017, aes(x = `Log GDP per capita`, y = Region, fill = Region )) +
  geom_density_ridges() +
  theme_ridges() + 
  labs (title = "Distribution of the log of the income GDP per capita and Inequality, 2017", x= "GINI Index") +
  theme(legend.position = "none")

  


two <- ggplot(gdp_2017, aes(x = `GINI index (World Bank estimate), average 2000-16`, y = Region, fill = Region )) +
  geom_density_ridges() +
  theme_ridges() + 
  labs (x= "Log GDP per capita") +
  theme(legend.position = "none")
  



#Loading the function multiplot
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(two,one)
```

![](Advanced_R_project_files/figure-markdown_github/unnamed-chunk-8-1.png)

The 2017 country distribution displayed by region and their correspondent log GDP per capita displays the inner wealth distribution of the countries. In this graph we can see that Western Europe is by far the wealthiest region with a distribution that resembles a bell curve. In contrast, Sub-Saharan Africa presents a distribution closer to the axis which translates into lower log GDP per capita. Plus, we can see that specially the distribution of the countries in this region is skewed to the right which denotes inequality in the distribution. As we account also for Gini coefficient information we will be also presenting this relation.

Similarly to the log income distribution per capita, the gini coefficient places the most equal regions in 2017 Central and Eastern Europe followed by Western Europe. Converserly, the most inequal regions are Sub-Saharan Africa, and Latin America and the Caribbean. It is important to mention that these distributions show the inequality levels in each country, reason why we can observe in Sub-Saharan Africa a smaller distribution on the right side of the axis, denoting a much higher level of inequality that represents the countries of: Botswana with a Gini index of (0.62) followed by South Africa (0.62).

From the condensed graph, we can say that Sub-Saharan Africa is a region with both: low log income per capita and high Gini inequality index.

<a href="#top">Back to table of content</a>

Streamgraph of Generosity Perception
====================================

``` r
generosity = subset(total,select= c( 1, 2, 8, 21))




generosity_mean <- generosity %>%
  group_by(Region, Year) %>%
  summarise(Mean.response = mean(Generosity, na.rm = TRUE))




generosity_mean %>%
  group_by(Year, Region) %>%
  streamgraph("Region", "Mean.response", "Year")%>%
  sg_axis_x(1, "year", "%Y") %>% 
  sg_legend(TRUE, "Region ")
```

<!--html_preserve-->

<center>
<label style='padding-right:5px' for='htmlwidget-caab83327d459a84ecb5-select'></label><select id='htmlwidget-caab83327d459a84ecb5-select' style='visibility:hidden;'></select>
</center>

<script type="application/json" data-for="htmlwidget-caab83327d459a84ecb5">{"x":{"data":{"key":["Australia and New Zealand","Central and Eastern Europe","Eastern Asia","Latin America and Caribbean","Middle East and Northern Africa","North America","Southeastern Asia","Southern Asia","Sub-Saharan Africa","Western Europe","Australia and New Zealand","Central and Eastern Europe","Eastern Asia","Latin America and Caribbean","Middle East and Northern Africa","North America","Southeastern Asia","Southern Asia","Sub-Saharan Africa","Western Europe","Australia and New Zealand","Central and Eastern Europe","Eastern Asia","Latin America and Caribbean","Middle East and Northern Africa","North America","Southeastern Asia","Southern Asia","Sub-Saharan Africa","Western Europe","Australia and New Zealand","Central and Eastern Europe","Eastern Asia","Latin America and Caribbean","Middle East and Northern Africa","North America","Southeastern Asia","Southern Asia","Sub-Saharan Africa","Western Europe","Australia and New Zealand","Central and Eastern Europe","Eastern Asia","Latin America and Caribbean","Middle East and Northern Africa","North America","Southeastern Asia","Southern Asia","Sub-Saharan Africa","Western Europe","Australia and New Zealand","Central and Eastern Europe","Eastern Asia","Latin America and Caribbean","Middle East and Northern Africa","North America","Southeastern Asia","Southern Asia","Sub-Saharan Africa","Western Europe","Australia and New Zealand","Central and Eastern Europe","Eastern Asia","Latin America and Caribbean","Middle East and Northern Africa","North America","Southeastern Asia","Southern Asia","Sub-Saharan Africa","Western Europe","Australia and New Zealand","Central and Eastern Europe","Eastern Asia","Latin America and Caribbean","Middle East and Northern Africa","North America","Southeastern Asia","Southern Asia","Sub-Saharan Africa","Western Europe","Australia and New Zealand","Central and Eastern Europe","Eastern Asia","Latin America and Caribbean","Middle East and Northern Africa","North America","Southeastern Asia","Southern Asia","Sub-Saharan Africa","Western Europe","Australia and New Zealand","Central and Eastern Europe","Eastern Asia","Latin America and Caribbean","Middle East and Northern Africa","North America","Southeastern Asia","Southern Asia","Sub-Saharan Africa","Western Europe","Australia and New Zealand","Central and Eastern Europe","Eastern Asia","Latin America and Caribbean","Middle East and Northern Africa","North America","Southeastern Asia","Southern Asia","Sub-Saharan Africa","Western Europe","Australia and New Zealand","Central and Eastern Europe","Eastern Asia","Latin America and Caribbean","Middle East and Northern Africa","North America","Southeastern Asia","Southern Asia","Sub-Saharan Africa","Western Europe","Australia and New Zealand","Central and Eastern Europe","Eastern Asia","Latin America and Caribbean","Middle East and Northern Africa","North America","Southeastern Asia","Southern Asia","Sub-Saharan Africa","Western Europe","Australia and New Zealand","Central and Eastern Europe","Eastern Asia","Latin America and Caribbean","Middle East and Northern Africa","North America","Southeastern Asia","Southern Asia","Sub-Saharan Africa","Western Europe"],"value":[0,0,0,0,0,0.244575247168541,0,0,0,0,0.308866262435913,-0.191481423098594,0.0440743193030357,0.0254147706651374,-0.0841382381816705,0,0.218778043985367,0.0663538295775652,0.00923257130419924,0.121016926797373,0.307072803378105,-0.137003254045558,-0.0759608140215278,0.0259707938770161,-0.0281505798920989,0.215101853013039,0.213036734610796,0.0981315590441227,0.0323431689880396,0.121656216681004,0.295914277434349,-0.144841344437251,-0.00370692908763885,-0.000916610658168793,-0.0264105128331317,0.250036850571632,0.172773563768715,0.100103533516328,0.000617621162046607,0.115330780960254,0,-0.129719447246596,-0.0499575044959784,-0.0174421546980739,-0.0320098363704996,0.215545453131199,0.0974155775870063,0.0850425983468692,0.007624856905923,0.139320403337479,0.279433384537697,-0.118285100692167,0.0141934812068939,-0.0108319167721722,-0.0580275268002879,0.229262009263039,0.189799157736291,0.13480085056896,-0.00889091504504904,0.0896378452889621,0.320552363991737,-0.127328872837223,0.00888368859887123,-0.0267885207375955,-0.0720664010766675,0.198943465948105,0.21034568734467,0.0351875394893189,-0.0395413926224027,0.0857917101820931,0.274077773094177,-0.132884764867225,0.0350821912288666,-0.00454885180500385,-0.068259732930788,0.244407579302788,0.22027723613428,0.109142581000924,-0.0324935737945553,0.0906384457392912,0.246658556163311,-0.117824683608166,-0.0657577030360699,-0.0283787788911944,-0.0630236477591097,0.286846533417702,0.248773423489183,0.143094242816525,-0.0283677438731372,0.0872723323786083,0.327011048793793,-0.0372172521787559,-0.0170435354113579,-0.0264598305511754,-0.0518567820079625,0.237611576914787,0.27590890933061,0.115600841918162,-0.0215844568592729,0.0840733759299231,0.324047148227692,-0.0706785385304227,-0.0769518138840795,-0.0426255374362594,-0.0181073735157649,0.228044100105762,0.252820110879838,0.130939118298037,-0.0041008566866719,0.102317298713483,0.245461478829384,-0.0719783465510459,-0.0261206187307835,-0.0513235262448066,-0.0496064859131972,0.16940750181675,0.21972893178463,0.0520940817892551,0.0028350422525722,0.0613476945773551,0.299239352345467,-0.0667756664855727,-0.0331383975222707,-0.0835413753853313,-0.0597944135467211,0.171924494206905,0.207031061606748,0.0188794825226068,-0.0107231589711525,0.0858270248006049,0.127272717654705,-0.0850840144231916,-0.125278283841908,-0.0756310992486154,-0.122861567207358,0.102839756757021,0.169500913936645,0.0369958020746708,0.0085598859305103,-0.0159810945829924],"date":["2005-01-01","2005-01-01","2005-01-01","2005-01-01","2005-01-01","2005-01-01","2005-01-01","2005-01-01","2005-01-01","2005-01-01","2006-01-01","2006-01-01","2006-01-01","2006-01-01","2006-01-01","2006-01-01","2006-01-01","2006-01-01","2006-01-01","2006-01-01","2007-01-01","2007-01-01","2007-01-01","2007-01-01","2007-01-01","2007-01-01","2007-01-01","2007-01-01","2007-01-01","2007-01-01","2008-01-01","2008-01-01","2008-01-01","2008-01-01","2008-01-01","2008-01-01","2008-01-01","2008-01-01","2008-01-01","2008-01-01","2009-01-01","2009-01-01","2009-01-01","2009-01-01","2009-01-01","2009-01-01","2009-01-01","2009-01-01","2009-01-01","2009-01-01","2010-01-01","2010-01-01","2010-01-01","2010-01-01","2010-01-01","2010-01-01","2010-01-01","2010-01-01","2010-01-01","2010-01-01","2011-01-01","2011-01-01","2011-01-01","2011-01-01","2011-01-01","2011-01-01","2011-01-01","2011-01-01","2011-01-01","2011-01-01","2012-01-01","2012-01-01","2012-01-01","2012-01-01","2012-01-01","2012-01-01","2012-01-01","2012-01-01","2012-01-01","2012-01-01","2013-01-01","2013-01-01","2013-01-01","2013-01-01","2013-01-01","2013-01-01","2013-01-01","2013-01-01","2013-01-01","2013-01-01","2014-01-01","2014-01-01","2014-01-01","2014-01-01","2014-01-01","2014-01-01","2014-01-01","2014-01-01","2014-01-01","2014-01-01","2015-01-01","2015-01-01","2015-01-01","2015-01-01","2015-01-01","2015-01-01","2015-01-01","2015-01-01","2015-01-01","2015-01-01","2016-01-01","2016-01-01","2016-01-01","2016-01-01","2016-01-01","2016-01-01","2016-01-01","2016-01-01","2016-01-01","2016-01-01","2017-01-01","2017-01-01","2017-01-01","2017-01-01","2017-01-01","2017-01-01","2017-01-01","2017-01-01","2017-01-01","2017-01-01","2018-01-01","2018-01-01","2018-01-01","2018-01-01","2018-01-01","2018-01-01","2018-01-01","2018-01-01","2018-01-01","2018-01-01"]},"markers":null,"annotations":null,"offset":"silhouette","interactive":true,"interpolate":"cardinal","palette":"Spectral","text":"black","tooltip":"black","x_tick_interval":1,"x_tick_units":"year","x_tick_format":"%Y","y_tick_count":5,"y_tick_format":",g","top":20,"right":40,"bottom":30,"left":50,"legend":true,"legend_label":"Region ","fill":"brewer","label_col":"black","x_scale":"date","sort":true,"order":"none"},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->
From the graph we can get insights regarding the generosity scores in the region for the time scope, being the most generous regions on average: Australia and New Zealand, followed by South Eastern Asia, and North America.

<a href="#top">Back to table of content</a>

Linear regression approximation between Happiness Index and Log GDP Per Capita and GINI
=======================================================================================

``` r
three <-ggplot(data=gdp_2017,aes(x=gdp_2017$`GINI index (World Bank estimate)`,y=gdp_2017$`Life Ladder`, colour = Region, size = Population))+
  geom_point() +
  labs (title = "Linear regression approximation between Happiness Index and Log GDP Per Capita and GINI
", y= "Happiness Index", x= "GINI") +
  geom_abline(intercept =  7.045, slope =  -4.169) + guides (size = FALSE)




four <- ggplot(data=gdp_2017,aes(x=gdp_2017$`Log GDP per capita`,y=gdp_2017$`Life Ladder`, colour = Region, size = Population))+
  geom_point() +
  labs (y= "Happiness Index", x= "Log GDP per capita") +
  geom_abline(intercept =  -1.1414 , slope =  0.7145) + guides (size = FALSE)



ggarrange(three, four,  
          labels = c("A", "B"),
          ncol = 1, nrow = 2, common.legend = TRUE, legend = "right")
```

![](Advanced_R_project_files/figure-markdown_github/unnamed-chunk-10-1.png)

From the regression results, we can interpret that the Gini inequality index has a strong significant negative relationship with the happiness index as one point increase in inequality, will decrease 4.16 points in the happiness index score.

From the regression results, among the log of the GDP and its relationship with the happiness index score, we can see that the inner wealth of a country has a strong significant positive relationship. As 1% increase in the log of the GDP per capita will increase 0.0071 points in the happiness index score.

-   Note: The size of the points corresponds to the population density

``` r
total2=total
happiness_ranked <- total2 %>%
  group_by(Year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-`Life Ladder`),
         Value_rel = `Life Ladder`/`Life Ladder`[rank==1],
         Value_lbl = paste0(" ",round(`Life Ladder`,2))) %>%
  group_by(Country_name) %>%
  filter(rank <=10) %>%
  ungroup()


happiness_ranked_2018<- happiness_ranked %>% filter(happiness_ranked$Year==2018)
```

<a href="#top">Back to table of content</a>

Animated bar chart race for the Happiness Index by Top 10 countries, 2005- 2018
===============================================================================

``` r
staticplot = ggplot(happiness_ranked, aes(rank, group = Country_name,
                                          fill = as.factor(Country_name), color = as.factor(Country_name))) +
  geom_tile(aes(y = `Life Ladder`/2,
                height = `Life Ladder`,
                width = 0.9), alpha = 0.8, color = "black") +
  geom_text(aes(y = `Life Ladder`/2, label = paste(Country_name, " ")), size=12, vjust = 0.2, hjust = 1, colour = "black") +
  geom_text(aes(y=`Life Ladder`,label = Value_lbl, hjust=0), size=8,  colour = "black")  +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="black", vjust=-1),
        plot.subtitle=element_text(size=16, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))


###Animating it
###############################
anim = staticplot + transition_states(Year, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Happiness Index per Year : {closest_state}',  
       subtitle  =  "Top 10 Countries",
       caption  = "Happiness Index per Year | Data Source: Happiness World Report 2019")




animate(anim, 300, fps = 15,  width = 1200, height = 1000,
        renderer = gifski_renderer("gganim.gif"))
```

![](gganim.gif)

The animated graph for the happiness index is a very useful tool and way to display in each year the top ten happiest countries. With this information we can animate the ranking to create a race for the user to identify how dynamic is the behaviour of this index among the ranking created for the countries.

<a href="#top">Back to table of content</a>

Happiness Bar Chart ranked Top 10 Countries, 2018
=================================================

``` r
ggplot(happiness_ranked_2018, aes(rank, group = Country_name,
                                  fill = as.factor(Country_name), color = as.factor(Country_name))) +
  geom_tile(aes(y = `Life Ladder`/2,
                height = `Life Ladder`,
                width = 0.9), alpha = 0.8, color = "black") +
  geom_text(aes(y = `Life Ladder`/2, label = paste(Country_name, " ")), size=7, vjust = 0.2, hjust = 1, colour = "black") +
  geom_text(aes(y=`Life Ladder`,label = Value_lbl, hjust=0), size=8,  colour = "black")  +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="black", vjust=-1),
        plot.subtitle=element_text(size=16, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))+
  labs(title = 'Happiness Index per Year :2018',  
       subtitle  =  "Top 10 Countries",
       caption  = "Data Source: Happiness World Report 2019")
```

![](Advanced_R_project_files/figure-markdown_github/unnamed-chunk-13-1.png)

As most of the information in this report is based on the year 2017, to follow that pattern, the static graph of the horizontal barchart race is presented for the user to have an updated information about the ranking of the countries according to the happiness index in 2018.

``` r
gdp_2017_corr<-subset(total, total$Year==2017)

gdp_2017_corr = subset(gdp_2017_corr,select= -c( 1, 2 , 13, 14, 19, 21))

names(gdp_2017_corr)[names(gdp_2017_corr) == "gini of household income reported in Gallup, by wp5-year"] <- "GINI"
names(gdp_2017_corr)[names(gdp_2017_corr) == "Standard deviation of ladder by country-year"] <- "STD_of_HI"
names(gdp_2017_corr)[names(gdp_2017_corr) == "Standard deviation/Mean of ladder by country-year" ] <- "STD/Mean_of_HI"


names(gdp_2017_corr)[names(gdp_2017_corr) == "Healthy life expectancy at birth"  ] <- "Expectancy at birth"
names(gdp_2017_corr)[names(gdp_2017_corr) == "Confidence in national government" ] <- "Confidence_in_gov"
names(gdp_2017_corr)[names(gdp_2017_corr) == "Freedom to make life choices" ] <- "Freedom to make choices"

names(gdp_2017_corr)[names(gdp_2017_corr) == "GINI index (World Bank estimate), average 2000-16"] <- "GINI_Avg"

gdp_2017_corr <- na.omit(gdp_2017_corr)
```

<a href="#top">Back to table of content</a>

Correlation Graphs, 2017
========================

For the correlation analysis we will be taking our base year, 2017. For this, we will be using the standard correlation statistic (Pearson) for our numerical features in the dataset, and we will be comparing the results in the correlation plot to the results displayed with the package Performance Analytics that we used in class.

``` r
gdp_cor<-cor(gdp_2017_corr)


corrplot(gdp_cor, method="color")
```

![](Advanced_R_project_files/figure-markdown_github/unnamed-chunk-15-1.png)

``` r
corrplot(gdp_cor, method="color",
         type="upper", order="hclust",
         addCoef.col = "black", sig.level = 0.01,
         tl.col="black")
```

![](Advanced_R_project_files/figure-markdown_github/unnamed-chunk-16-1.png)

From the correlation plot, we can identify the features that display higher correlations. In this sense, we will be focusing on our target variable "Happiness index score" or Life Ladder. From the chart, we can see that the highest positive correlations are displayed between the happiness index score and: Log GDP per capita (0.78), Social support (0.76), and Expectancy at birth (0.76) whereas on the negative side, we have Standard mean of the happiness index (-0.88)

``` r
chart.Correlation(gdp_2017_corr, histogram=TRUE)
```

![](Advanced_R_project_files/figure-markdown_github/unnamed-chunk-17-1.png)

This is a controversial graph, nevertheless we wanted to display it because it shows a lot of information, and hopefully with the background provided will be easier to follow. First of all, we can find the same correlation indices of the previous graph as well as the regression performed with both: Gini index and log GDP per capita.

<a href="#top">Back to table of content</a>

Box Plot of Social Support among Regions, 2017
==============================================

``` r
gdp_2017_box <- subset(total, total$Year==2017)

theme_set(theme_classic())

# Plot
g <- ggplot(gdp_2017_box, aes(Region, `Social support`))
g + geom_boxplot(varwidth=T, fill="plum") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Social support amoung Regions, 2017", 
       x="Regions of the World",
       y="Social Support")
```

![](Advanced_R_project_files/figure-markdown_github/unnamed-chunk-18-1.png)

Following the insights from the correlation plot, we decided to include the feature of social support among regions for the year 2017, as its correlation with the happiness score was among the highest (0.76). From the graph, we can see that the highest median of social support among regions was held by Australia and New Zealand, followed by Western Europe in 2017. However, the top countries ranking in social support are Iceland and Finland, both with 0.96 as a social support index.

In addition, as it is expected from the correlation, we see again the Sub\_Saharan Africa region ranking low and with outliers representing the countries of: Central African Region and Benin

<a href="#top">Back to table of content</a>

Violin Plot: Expectancy at birth, 2017
======================================

``` r
v <- ggplot(gdp_2017_box, aes(Region, `Healthy life expectancy at birth`))

# Combine with box plot to add median and quartiles and remove legend

v + geom_violin(aes(fill = `Healthy life expectancy at birth`), trim = FALSE, fill = "lightblue", width = 2) + 
  geom_boxplot(width = 0.1)+
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Healthy life expectancy at birth, 2017", 
       x="Regions of the World",
       y="Life expectancy at birth")
```

![](Advanced_R_project_files/figure-markdown_github/unnamed-chunk-19-1.png)

To conclude we will cover the feature: life expectancy at birth, as from the correlation plot this was also a feature ranking high (0.76). Following the tendency of the results, we again see Western Europe displaying the highest life expectancy in the world, followed by Australia and New Zealand and North America. Similarly, the bottom two regions are Sub-Saharan Africa and Southern Asia.

Conclusion: Does money buy happiness?
-------------------------------------

From the results obtained, yes it does. Wealth has a strong impact on the quality of life of the people and certaintly can buy some of the indicators displayed in the present work. According to the 2019 Happiness Report, Finland is the happiest country in the world, with Denmark, Norway, Iceland, and The Netherlands holding the next top positions. Four countries have held the top spot in the last four reports: Denmark, Switzerland, Norway and now Finland. All the top countries tend to have high values for six of the key variables found to support well-being: income, healthy life expectancy, social support, freedom, trust and generosity. The analysis of happiness changes from 2008-2015 shows Togo as the biggest gainer, moving up 17 places in the overall rankings from 2015. The biggest loser is Venezuela, down 2.2 points. Five of the report's seven chapters deal primarily with migration.

<a href="#top">Back to table of content</a>
