library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)
library(gridExtra)

#setwd(ROOT)

##### Load data:

### Educational attainment data
### https://data.census.gov/cedsci/table?q=educational%20attainment&g=0100000US%240500000&y=2020&tid=ACSST5Y2020.S1501
unzip('./data/ACSST5Y2020.S1501_2022-04-15T192149.zip', exdir='./data/')
edu_data <- read.csv('./data/ACSST5Y2020.S1501_data_with_overlays_2022-03-17T165902.csv', skip=1)

### Income data
### https://data.census.gov/cedsci/table?q=income&g=0100000US%240500000&y=2020&tid=ACSST5Y2020.S2503
unzip('./data/ACSST5Y2020.S2503_2022-04-15T192216.zip', exdir='./data/')
inc_data <- read.csv('./data/ACSST5Y2020.S2503_data_with_overlays_2022-03-19T143716.csv', skip=1)

###### Prepare data:

### Create education variables
edu_data$edu_hs_or_less <- (
  edu_data$"Estimate..Total..AGE.BY.EDUCATIONAL.ATTAINMENT..Population.25.years.and.over..Less.than.9th.grade"
  + edu_data$"Estimate..Total..AGE.BY.EDUCATIONAL.ATTAINMENT..Population.25.years.and.over..9th.to.12th.grade..no.diploma"
  + edu_data$"Estimate..Total..AGE.BY.EDUCATIONAL.ATTAINMENT..Population.25.years.and.over..High.school.graduate..includes.equivalency."
  + edu_data$"Estimate..Total..AGE.BY.EDUCATIONAL.ATTAINMENT..Population.18.to.24.years..Less.than.high.school.graduate"
  + edu_data$"Estimate..Total..AGE.BY.EDUCATIONAL.ATTAINMENT..Population.18.to.24.years..High.school.graduate..includes.equivalency."
)

edu_data$edu_less_than_bach <- (
  edu_data$"Estimate..Total..AGE.BY.EDUCATIONAL.ATTAINMENT..Population.25.years.and.over..Some.college..no.degree"
  + edu_data$"Estimate..Total..AGE.BY.EDUCATIONAL.ATTAINMENT..Population.25.years.and.over..Associate.s.degree"
  + edu_data$"Estimate..Total..AGE.BY.EDUCATIONAL.ATTAINMENT..Population.18.to.24.years..Some.college.or.associate.s.degree"
)

edu_data$edu_bach_or_more <- (
  edu_data$"Estimate..Total..AGE.BY.EDUCATIONAL.ATTAINMENT..Population.25.years.and.over..Bachelor.s.degree.or.higher"
  + edu_data$"Estimate..Total..AGE.BY.EDUCATIONAL.ATTAINMENT..Population.18.to.24.years..Bachelor.s.degree.or.higher"
)

edu_data <- edu_data %>%
  select(c(id, Geographic.Area.Name, edu_hs_or_less, edu_less_than_bach, edu_bach_or_more))

### Create income variables
inc_data$inc_over_100 <- (
  inc_data$"Estimate..Occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS....100.000.to..149.999"
  + inc_data$"Estimate..Occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS....150.000.or.more"
)

inc_data$inc_50_to_100 <- (
  inc_data$"Estimate..Occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS....75.000.to..99.999"
  + inc_data$"Estimate..Occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS....50.000.to..74.999"
)

inc_data$inc_20_to_50 <- (
  inc_data$"Estimate..Occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS....35.000.to..49.999"
  + inc_data$"Estimate..Occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS....25.000.to..34.999"
  + inc_data$"Estimate..Occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS....20.000.to..24.999"
)

inc_data$inc_under_20 <- (
  inc_data$"Estimate..Occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS....15.000.to..19.999"
  + inc_data$"Estimate..Occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS....10.000.to..14.999"
  + inc_data$"Estimate..Occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS....5.000.to..9.999"
  + inc_data$"Estimate..Occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS...Less.than..5.000"
)

inc_data <- inc_data %>%
  select(c(id, Geographic.Area.Name, inc_under_20, inc_20_to_50, inc_50_to_100, inc_over_100))

### Get most common education level per county
dominant_edu <- select(edu_data, -c(id, Geographic.Area.Name))
edu_data$dominant_edu <- colnames(dominant_edu)[apply(dominant_edu, 1, which.max)]

### Get most common income level per county
dominant_inc <- select(inc_data, -c(id, Geographic.Area.Name))
inc_data$dominant_inc <- colnames(dominant_inc)[apply(dominant_inc, 1, which.max)]

### Make dataframe of most common income and education levels per county
data_full <- merge(
  select(edu_data, c(id, Geographic.Area.Name, dominant_edu)),
  select(inc_data, c(id, Geographic.Area.Name, dominant_inc)),
  by=c('id', 'Geographic.Area.Name')
)

### Merge full datasets
data_full_wide <- merge(inc_data, edu_data, by=c('id', 'Geographic.Area.Name'))

##### Perform analysis:

### Make twoway table
twoway_table <- as.data.frame(table(data_full$dominant_edu, data_full$dominant_inc))
twoway_table <- as.data.frame(pivot_wider(twoway_table, names_from=Var1, values_from=Freq))
rownames(twoway_table) <- twoway_table$Var2
twoway_table <- twoway_table %>% 
  slice(match(c('inc_under_20', 'inc_20_to_50', 'inc_50_to_100', 'inc_over_100'), Var2)) %>%
  select(-c(Var2)) %>%
  select('edu_hs_or_less', 'edu_less_than_bach', 'edu_bach_or_more')

### Mosaic plot of twoway table
labels <- c('edu_bach_or_more'='Bachelor\'s or More', 
            'edu_hs_or_less'='High School or Less', 
            'edu_less_than_bach'='Some College',
            'inc_under_20'='Under $20k', 
            'inc_20_to_50'='$20k to $50k', 
            'inc_50_to_100'='$50k to $100k', 
            'inc_over_100'='Over $100k')
twoway_table_renamed <- twoway_table
names(twoway_table_renamed) <- as_labeller(labels)(names(twoway_table_renamed))
rownames(twoway_table_renamed) <- as_labeller(labels)(rownames(twoway_table_renamed))
mosaicplot(twoway_table_renamed, las=1, color='dark red',
           xlab='Counts of Income Level',
           ylab='Counts of Educational Attainment Level',
           main='Dominant Educational Attainment Level vs Dominant Income Level in the U.S.')
### Get expected values
expected_counts <- as.data.frame(chisq.test(twoway_table)$expected)

### Run chi-sq test
test <- chisq.test(twoway_table)
test

##### Visualize with heat maps:

### Make long form observed counts dataframe
observed_long <- as.data.frame(table(data_full$dominant_edu, data_full$dominant_inc))
observed_long <- rename(observed_long, observed=Freq, edu=Var1, inc=Var2)
observed_long$ln_observed <- log(observed_long$observed)

### Make long form expected counts dataframe
expected_long <- as.data.frame(test$expected)
expected_long$Var2 <- rownames(expected_long)
expected_long <- pivot_longer(expected_long, 
                              c(edu_hs_or_less, edu_less_than_bach, edu_bach_or_more),
                              names_to='Var1')
expected_long <- rename(expected_long, expected=value, inc=Var2, edu=Var1)
expected_long$ln_expected <- log(expected_long$expected)

### Make long form residuals dataframe
residual_long <- as.data.frame((test$observed-test$expected)/sqrt(test$expecte))
residual_long$Var2 <- rownames(residual_long)
residual_long <- pivot_longer(residual_long, 
                              c(edu_hs_or_less, edu_less_than_bach, edu_bach_or_more),
                              names_to='Var1')
residual_long <- rename(residual_long, residual=value, inc=Var2, edu=Var1)

### Merge observed, expected, and residuals then compute statistic
chisq_data <- merge(observed_long, expected_long, by=c('edu', 'inc'))
chisq_data <- merge(chisq_data, residual_long, by=c('edu', 'inc'))

### Plot of observed counts
observed_plot <- ggplot(chisq_data, aes(x=edu, y=inc, fill=observed)) + 
  geom_tile() + 
  scale_fill_gradient(trans='log', name='', labels=function(x) round(x, 2), low='pink', high='dark red') +
  theme(panel.background=element_blank(), legend.position='bottom', axis.text.y=element_text(hjust=0.5, angle=90)) + 
  geom_text(aes(label=round(observed, 2)), color='white', size=5) +
  guides(fill=guide_colourbar(barheight=1, barwidth=11, title.position='top')) + 
  labs(subtitle='Observed Counts') + 
  scale_x_discrete(
    name='Education Level',
    labels=c('edu_bach_or_more'='Bachelor\'s or more', 'edu_hs_or_less'='High School or Less', 'edu_less_than_bach'='Some College'),
    limits=c('edu_hs_or_less', 'edu_less_than_bach', 'edu_bach_or_more')) +
  scale_y_discrete(
    name='Income Level',
    labels=c('inc_under_20'='Under $20k', 'inc_20_to_50'='$20k to $50k', 'inc_50_to_100'='$50k to $100k', 'inc_over_100'='Over $100k'),
    limits=c('inc_under_20', 'inc_20_to_50', 'inc_50_to_100', 'inc_over_100'))

### Plot of expected counts
expected_plot <- ggplot(chisq_data, aes(x=edu, y=inc, fill=expected)) + 
  geom_tile() + 
  scale_fill_gradient(trans='log', name='', labels=function(x) round(x, 2), low='light green', high='dark green') +
  theme(panel.background=element_blank(), legend.position='bottom', axis.text.y=element_text(hjust=0.5, angle=90)) + 
  geom_text(aes(label=round(expected, 2)), color='white', size=5) +
  guides(fill=guide_colourbar(barheight=1, barwidth=11, title.position='top')) + 
  labs(subtitle='Expected Counts') + 
  scale_x_discrete(
    name='Education Level',
    labels=c('edu_bach_or_more'='Bachelor\'s or more', 'edu_hs_or_less'='High School or Less', 'edu_less_than_bach'='Some College'),
    limits=c('edu_hs_or_less', 'edu_less_than_bach', 'edu_bach_or_more')) +
  scale_y_discrete(
    name='Income Level',
    labels=c('inc_under_20'='Under $20k', 'inc_20_to_50'='$20k to $50k', 'inc_50_to_100'='$50k to $100k', 'inc_over_100'='Over $100k'),
    limits=c('inc_under_20', 'inc_20_to_50', 'inc_50_to_100', 'inc_over_100'))

### Plot of residuals
residual_plot <- ggplot(chisq_data, aes(x=edu, y=inc, fill=residual)) + 
  geom_tile() + 
  scale_fill_gradient(name='', labels=function(x) round(x, 2), low='light blue', high='dark blue') +
  theme(panel.background=element_blank(), legend.position='bottom', axis.text.y=element_text(hjust=0.5, angle=90)) + 
  geom_text(aes(label=round(residual, 2)), color='white', size=5) +
  guides(fill=guide_colourbar(barheight=1, barwidth=11, title.position='top')) + 
  labs(subtitle='Residuals') + 
  scale_x_discrete(
    name='Education Level',
    labels=c('edu_bach_or_more'='Bachelor\'s or more', 'edu_hs_or_less'='High School or Less', 'edu_less_than_bach'='Some College'),
    limits=c('edu_hs_or_less', 'edu_less_than_bach', 'edu_bach_or_more')) +
  scale_y_discrete(
    name='Income Level',
    labels=c('inc_under_20'='Under $20k', 'inc_20_to_50'='$20k to $50k', 'inc_50_to_100'='$50k to $100k', 'inc_over_100'='Over $100k'),
    limits=c('inc_under_20', 'inc_20_to_50', 'inc_50_to_100', 'inc_over_100'))

sum(chisq_data$residual**2)
grid.arrange(observed_plot, expected_plot, residual_plot, 
             nrow=1,
             top='Chi-Squared Test of Education Level vs Income Level in the U.S.')

### Plot distribution of test statistic
dof <- (nrow(twoway_table)-1)*(ncol(twoway_table)-1)
ggplot(data.frame(x=c(0,50)), aes(x=x)) +
  geom_area(stat='function', fun=dchisq, fill='light blue', color='dark blue', args=list(df=dof)) +
  labs(title='Chi-Squared Distribution with 6 Degrees of Freedom',
       x='Domain', y='Density')

##### Post-hoc analysis and scatterplot visualization:

### Make population counts into percentages
perc_data <- data_full_wide %>%
  rowwise() %>%
  mutate(pop_sum=sum(c(edu_hs_or_less, edu_less_than_bach, edu_bach_or_more))) %>%
  mutate(edu_hs_or_less=edu_hs_or_less/pop_sum, 
         edu_less_than_bach=edu_less_than_bach/pop_sum,
         edu_bach_or_more=edu_bach_or_more/pop_sum) %>%
  mutate(house_sum=sum(c(inc_under_20, inc_20_to_50, inc_50_to_100, inc_over_100))) %>%
  mutate(inc_under_20=inc_under_20/house_sum, 
         inc_20_to_50=inc_20_to_50/house_sum,
         inc_50_to_100=inc_50_to_100/house_sum,
         inc_over_100=inc_over_100/house_sum) %>%
  select(-c(dominant_edu, dominant_inc, house_sum, pop_sum))
  
### Make long form data
perc_data_long <- perc_data %>% 
  pivot_longer(cols=c(edu_hs_or_less, edu_less_than_bach, edu_bach_or_more), 
               names_to='edu_level', values_to='edu_level_count') %>%
  pivot_longer(cols=c(inc_under_20, inc_20_to_50, inc_50_to_100, inc_over_100),
               names_to='inc_level', values_to='inc_level_count') 

### Correlations
perc_data_long %>%
  group_by(edu_level, inc_level) %>%
  summarise(r=cor(edu_level_count, inc_level_count)) %>%
  ggplot(aes(x=edu_level, y=inc_level, fill=r)) + 
    geom_tile() + 
    scale_fill_gradient2(name='', limits=c(-1,1), labels=function(x) round(x, 2), low='blue', mid='white', high='red') +
    theme(panel.background=element_blank(), legend.position='right', axis.text.y=element_text(hjust=0.5, angle=90)) + 
    geom_text(aes(label=round(r, 2)), color='black', size=5) +
    guides(fill=guide_colourbar(barheight=10, barwidth=2, title.position='top')) + 
    labs(subtitle='Correlation Matrix of Educational Attainment and Income Level') + 
    scale_x_discrete(
      name='Educational Attainment Percentage',
      labels=c('edu_bach_or_more'='Bachelor\'s or more', 'edu_hs_or_less'='High School or Less', 'edu_less_than_bach'='Some College'),
      limits=c('edu_hs_or_less', 'edu_less_than_bach', 'edu_bach_or_more')) +
    scale_y_discrete(
      name='Income Level Percentage',
      labels=c('inc_under_20'='Under $20k', 'inc_20_to_50'='$20k to $50k', 'inc_50_to_100'='$50k to $100k', 'inc_over_100'='Over $100k'),
      limits=c('inc_under_20', 'inc_20_to_50', 'inc_50_to_100', 'inc_over_100'))


### Pairplots
pplot <- perc_data %>%
  select(c(edu_hs_or_less, edu_less_than_bach, edu_bach_or_more, inc_under_20, inc_20_to_50, inc_50_to_100, inc_over_100)) %>%
  ggpairs()

### Choose specific pairplots to display
pplots_list = list(
  getPlot(pplot, i=4, j=1), getPlot(pplot, i=4, j=2), getPlot(pplot, i=4, j=3),
  getPlot(pplot, i=5, j=1), getPlot(pplot, i=5, j=2), getPlot(pplot, i=5, j=3),
  getPlot(pplot, i=6, j=1), getPlot(pplot, i=6, j=2), getPlot(pplot, i=6, j=3),
  getPlot(pplot, i=7, j=1), getPlot(pplot, i=7, j=2), getPlot(pplot, i=7, j=3)
)

### Labelling
labels <- c('edu_bach_or_more'='Bachelor\'s or More', 
            'edu_hs_or_less'='High School or Less', 
            'edu_less_than_bach'='Some College',
            'inc_under_20'='Under $20k', 
            'inc_20_to_50'='$20k to $50k', 
            'inc_50_to_100'='$50k to $100k', 
            'inc_over_100'='Over $100k')

### Display selected pairplots
ggmatrix(pplots_list, nrow=4, ncol=3,
         xAxisLabels=pplot$xAxisLabels[1:3],
         yAxisLabels=pplot$yAxisLabels[4:7],
         labeller=as_labeller(labels)) + 
  labs(title='Educational Attainment vs. Income Level in U.S. Counties', 
       x='Percent of County Population with given Level of Educational Attainment', 
       y='Percent of County Population within given Income Bracket')


