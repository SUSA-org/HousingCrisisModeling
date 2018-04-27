library(ggplot2)
library(ggthemr)
all_counties <- readRDS("Monthly_RDS/AllCounties.RDS")
county_lm %>% ggplot(aes(x=`Median Home Price`, y=`ValuePerSquareFoot`)) + geom_point() +
  ggtitle("Median Home Price vs Value Per Square Foot") + geom_smooth(method='lm')
all_counties %>% ggplot(aes(x=`Population Density`, y=`Median Income`)) + geom_point() +
  ggtitle("Population Density vs Median Income") + geom_smooth(method='lm')
all_counties %>% ggplot(aes(x=`Building permits`, y=log(`Modified HAI`))) + geom_point() +
  ggtitle("Building Permits vs Modified HAI") + geom_smooth(method='lm')
