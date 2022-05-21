library(sandwich)
library(lmtest)
library(dplyr)
library(tidyr)
library(GGally)

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

### Aggregate columns
edu_data$less_than_hs <- (
  edu_data$"Estimate..Total..AGE.BY.EDUCATIONAL.ATTAINMENT..Population.25.years.and.over..Less.than.9th.grade"
  + edu_data$"Estimate..Total..AGE.BY.EDUCATIONAL.ATTAINMENT..Population.25.years.and.over..9th.to.12th.grade..no.diploma"
  + edu_data$"Estimate..Total..AGE.BY.EDUCATIONAL.ATTAINMENT..Population.18.to.24.years..Less.than.high.school.graduate"
)
edu_data$highschool <- (
  edu_data$"Estimate..Total..AGE.BY.EDUCATIONAL.ATTAINMENT..Population.25.years.and.over..High.school.graduate..includes.equivalency."
  + edu_data$"Estimate..Total..AGE.BY.EDUCATIONAL.ATTAINMENT..Population.18.to.24.years..High.school.graduate..includes.equivalency."
)
edu_data$less_than_bach <- (
  edu_data$"Estimate..Total..AGE.BY.EDUCATIONAL.ATTAINMENT..Population.25.years.and.over..Some.college..no.degree"
  + edu_data$"Estimate..Total..AGE.BY.EDUCATIONAL.ATTAINMENT..Population.25.years.and.over..Associate.s.degree"
  + edu_data$"Estimate..Total..AGE.BY.EDUCATIONAL.ATTAINMENT..Population.18.to.24.years..Some.college.or.associate.s.degree"
)
edu_data$bach_or_more <- (
  edu_data$"Estimate..Total..AGE.BY.EDUCATIONAL.ATTAINMENT..Population.25.years.and.over..Bachelor.s.degree.or.higher"
  + edu_data$"Estimate..Total..AGE.BY.EDUCATIONAL.ATTAINMENT..Population.18.to.24.years..Bachelor.s.degree.or.higher"
)
inc_data$median_income <- (
  inc_data$"Estimate..Occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS...Median.household.income..dollars."
) 

##### Prepare data:

### Merge income and education
data <- merge(
  select(edu_data, c(id, Geographic.Area.Name, less_than_hs, highschool, less_than_bach, bach_or_more)),
  select(inc_data, c(id, Geographic.Area.Name, median_income)), 
  by=c('id', 'Geographic.Area.Name')
)

### Fix median_income dtype
data <- data %>%
  mutate(median_income=as.numeric(median_income)) %>%
  drop_na() # Should be row 2646

### Normalize education counts
data <- data %>% 
  rowwise() %>%
  mutate(population=sum(c(less_than_hs, highschool, less_than_bach, bach_or_more))) %>%
  mutate(less_than_hs=less_than_hs/population, 
         highschool=highschool/population,
         less_than_bach=less_than_bach/population,
         bach_or_more=bach_or_more/population)

### Apply log transform
data_ln <- data %>%
  mutate(log_less_than_hs=log(less_than_hs), 
         log_highschool=log(highschool),
         log_less_than_bach=log(less_than_bach),
         log_bach_or_more=log(bach_or_more)) %>%
  select(-c(less_than_hs, highschool, less_than_bach, bach_or_more)) %>%
  filter_all(all_vars(!is.infinite(.) | is.na(.)))

##### Visualize:

### Define labels
labels <- c('less_than_hs'='Less than High School',
            'highschool'='High School',
            'less_than_bach'='Less than Bachelor\'s',
            'bach_or_more'='Bachelor\'s or More',
            'median_income'='Median Household Income',
            'log_less_than_hs'='Less than High School',
            'log_highschool'='High School',
            'log_less_than_bach'='Some College',
            'log_bach_or_more'='Bachelor\'s or More')

### Plot regular data
data %>%
  select(c(less_than_hs, highschool, less_than_bach, bach_or_more, median_income)) %>%
  ggpairs(labeller=as_labeller(labels)) +
    labs(title='Educational Attainment Levels vs. Median Income in U.S. Counties',
         caption='Educational attainment level measured as a proportion of county population')

### Plot logged data
data_ln %>%
  select(c(log_less_than_hs, log_highschool, log_less_than_bach, log_bach_or_more, median_income)) %>%
  ggpairs(labeller=as_labeller(labels)) + 
    labs(title='Educational Attainment Levels vs. Median Income in U.S. Counties',
         subtitle='Log Transformation',
         caption='Educational attainment level measured as a logged proportion of county population')

##### Perform modeling:

### Model without log transform
reg1 <- lm(median_income~less_than_hs+highschool+less_than_bach+bach_or_more, data=data)
plot(reg1, main='Median Income vs. Education Level Percentages', sub.caption='')
summary(reg1)

### Model with log transform
reg2 <- lm(median_income~log_less_than_hs+log_highschool+log_less_than_bach+log_bach_or_more, data=data_ln)
plot(reg2, main='Median Income vs. Logged Education Level Percentages', sub.caption='')
summary(reg2)

### Heteroskedasticity correction
reg2_robust <- coeftest(reg2, vcov=vcovHC(reg2, type='HC0'))
reg2_robust


