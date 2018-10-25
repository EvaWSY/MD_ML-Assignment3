# Purpose: Making predictions with the NYC SQF data from 2008-2016
# Date: October 2018
# Team: Andrea Hassler, Eva Wang, Alan Chen

source('code/library_team.R')


# Import data
sqf.data <- read_csv('data/output/sqf_08_16.csv')

##############################################
# A)
# Restrict to suspected.crime is 'cpw'
sqf <- sqf.data %>% filter(suspected.crime=='cpw')

# Standardize real-valued attributes 
train <-  sqf %>% filter(year==2008) %>% mutate(suspect.height = standardize(suspect.height),
                                                suspect.weight = standardize(suspect.weight),
                                                suspect.age = standardize(suspect.age),
                                                observation.period = standardize(observation.period))

model <- glm(found.weapon ~ precinct + location.housing +  
                 additional.report + additional.investigation + additional.sights +
                 additional.proximity + additional.evasive + additional.associating +
                 additional.direction + additional.highcrime + additional.time + 
                 stopped.bc.object + stopped.bc.bulge + stopped.bc.desc + stopped.bc.violent +
                 stopped.bc.casing + stopped.bc.lookout + stopped.bc.drugs + stopped.bc.clothing +
                 stopped.bc.furtive + suspect.age + suspect.build + suspect.sex +
                 suspect.height + suspect.weight + inside + observation.period +
                 radio.run + day + month + time.period, 
             data = train, family = 'binomial')

# I) Ten largest and ten smallest (in magnitude) coefficients, not including the intercept
largest10 <- sort(abs(model$coefficients[-1]), decreasing = TRUE)[0:10]
smallest10 <- sort(abs(model$coefficients[-1]), decreasing = FALSE)[0:10]
kable(data.frame(largest10), align='c', format='markdown')
kable(data.frame(smallest10), align='c', format='markdown')



# II)  
friendData = data.frame(
    found.weapon=FALSE,
    precinct = 6,
    location.housing = 'transit',
    additional.report = FALSE,
    additional.investigation = FALSE,
    additional.sights = FALSE,
    additional.proximity = FALSE,
    additional.evasive =  FALSE,
    additional.associating = FALSE,
    additional.direction = FALSE,
    additional.highcrime = TRUE,
    additional.time = FALSE,
    stopped.bc.object = FALSE,
    stopped.bc.bulge = TRUE,
    stopped.bc.desc = FALSE,
    stopped.bc.violent = FALSE,
    stopped.bc.casing = FALSE,
    stopped.bc.lookout =FALSE,
    stopped.bc.drugs = FALSE, 
    stopped.bc.clothing = FALSE,
    stopped.bc.furtive = FALSE,
    suspect.age = 30,
    suspect.build = 'medium',
    suspect.sex = 'male',
    suspect.height = 6,
    suspect.weight = 165.0,
    inside = TRUE,
    observation.period = 10,
    radio.run = FALSE,
    day = 'Thursday',
    month = 'October',
    time.period = 6,
    stringsAsFactors=FALSE)


# Standardize the single record based on train data.
friendData$suspect.age <- (friendData$suspect.age - 
                               mean(sqf$suspect.age[sqf$year==2008], na.rm = T)) / 
    sd(sqf$suspect.age[sqf$year==2008], na.rm = T)

friendData$suspect.height <- (friendData$suspect.height - 
                                  mean(sqf$suspect.height[sqf$year==2008], na.rm = T)) / 
    sd(sqf$suspect.height[sqf$year==2008], na.rm = T)

friendData$suspect.weight <- (friendData$suspect.weight - 
                                  mean(sqf$suspect.weight[sqf$year==2008], na.rm = T)) / 
    sd(sqf$suspect.weight[sqf$year==2008], na.rm = T)

friendData$observation.period <- (friendData$observation.period - 
                                      mean(sqf$observation.period[sqf$year==2008], na.rm = T)) /
    sd(sqf$observation.period[sqf$year==2008], na.rm = T)


# Probability for friend as male
(p.male = predict(model, newdata = friendData, type = "response"))

# Probability for friend as female
friendData$suspect.sex <- 'female'
(p.female = predict(model, newdata = friendData, type = "response"))




# III) Compute AUC
test <- sqf %>% filter(year==2009)
test <- test %>% mutate(suspect.height = standardize(suspect.height),
                        suspect.weight = standardize(suspect.weight),
                        suspect.age = standardize(suspect.age),
                        observation.period = standardize(observation.period))

# compute the AUC using ROCR package
test$predicted.probability <- predict(model, newdata = test, type='response') 
test.pred <- prediction(test$predicted.probability, test$found.weapon)
test.perf <- performance(test.pred, "auc")
cat('the auc score is ', 100*test.perf@y.values[[1]], "\n") 

# IV sampling (with replacement) 10,000 random pairs of true (weapon is found) and false (weapon is not found) examples from 2009, 
# and computing the proportion of pairs where your model predicts that the true example is more likely to find a weapon 
# than the false example. Confirm that your answer is approximately equal to the answer computed in part III)

set.seed(1234)
true_sample <- test %>% filter(complete.cases(test)) %>% filter(found.weapon == TRUE) %>% sample_n(10000, replace=T)
false_sample <- test %>% filter(complete.cases(test)) %>% filter(found.weapon == FALSE) %>% sample_n(10000, replace=T)
#add predict and use predict to compare.
true_sample$predicted.probability <- predict(model, newdata = true_sample, type='response') 
false_sample$predicted.probability <- predict(model, newdata = false_sample, type='response')

samplingAUC <-mean(true_sample$predicted.probability > false_sample$predicted.probability)
cat('the auc score is ', 100*samplingAUC, "\n") 




################################################
# B) AUC plot accross 2009-2016
# Compute AUC for each year in 2009-2016, then make the plot.
AUC.scores <- vector(length=8)
test.years <- c(2009:2016)
for (i in 1:length(test.years)) {
    # Create test set for year
    test <- sqf %>% filter(year == test.years[i])
    # Standardize real-valued variables
    test <- test %>% mutate(suspect.height = standardize(suspect.height),
                        suspect.weight = standardize(suspect.weight),
                        suspect.age = standardize(suspect.age),
                        observation.period = standardize(observation.period))
    # Compute the AUC using ROCR package
    test$predicted.probability <- predict(model, newdata = test, type='response')
    test.pred <- prediction(test$predicted.probability, test$found.weapon)
    test.perf <- performance(test.pred, "auc")
    AUC.scores[i] <- 100*test.perf@y.values[[1]]
}

# Put variables in data frame
AUC.data <- as.data.frame(cbind(Year = test.years, AUC = AUC.scores))

# Create and save plot
theme_set(theme_bw())
partB_plot <- ggplot(data = AUC.data, mapping = aes(x=Year, y=AUC)) + geom_point()

ggsave(plot=partB_plot, file='partB_plot.pdf', height=5, width=5)


###########################################
# C) Performance and Calibration Plots

# 1) Predict force.pointed from pre-stop variables using Naive Bayes
# Divide into test and training data by year; standardize numeric variables
train_pt <- sqf.data %>% filter(complete.cases(sqf.data)) %>% filter(year < 2011)
test_pt <- sqf.data %>% filter(complete.cases(sqf.data)) %>% filter(year == 2011)

train_pt <- train_pt %>% mutate(suspect.height = standardize(suspect.height),
                          suspect.weight = standardize(suspect.weight),
                          suspect.age = standardize(suspect.age),
                          observation.period = standardize(observation.period))

test_pt <- test_pt %>% mutate(suspect.height = standardize(suspect.height),
                        suspect.weight = standardize(suspect.weight),
                        age = suspect.age,
                        suspect.age = standardize(suspect.age),
                        observation.period = standardize(observation.period))

# Fit model on training using naive bayes
mod_pt <- naive_bayes(force.pointed ~ precinct + location.housing +  
                          additional.report + additional.investigation + additional.sights +
                          additional.proximity + additional.evasive + additional.associating +
                          additional.direction + additional.highcrime + additional.time + 
                          stopped.bc.object + stopped.bc.bulge + stopped.bc.desc + stopped.bc.violent +
                          stopped.bc.casing + stopped.bc.lookout + stopped.bc.drugs + stopped.bc.clothing +
                          stopped.bc.furtive + suspect.age + suspect.build + suspect.sex +
                          suspect.height + suspect.weight + inside + observation.period +
                          officer.uniform + radio.run + day + month + time.period, 
                      data = train_pt)

# Generate predicted probabilities for test set
test_pt$predicted.probability <- predict(mod_pt, newdata = test_pt, type = "prob")[, 2]



# Performance plot
plot.data <- test_pt %>% arrange(desc(predicted.probability)) %>% 
    mutate(numstops = row_number(), percent.outcome = cumsum(force.pointed)/sum(force.pointed),
           stops = numstops/n()) %>% select(stops, percent.outcome)

# Create and save plot
theme_set(theme_bw())
p <- ggplot(data=plot.data, aes(x=stops, y=percent.outcome)) 
p <- p + geom_line()
p <- p + scale_x_log10('\nPercent of stops', limits=c(0.003, 1), breaks=c(.003,.01,.03,.1,.3,1), 
                       labels=c('0.3%','1%','3%','10%','30%','100%'))
p <- p + scale_y_continuous("Percent of times weapon pointed", limits=c(0, 1), labels=scales::percent)

ggsave(plot=p, file='performance_pt.pdf', height=5, width=5)

# Calibration plot
plot.data <- test_pt %>% mutate(calibration = round(100*predicted.probability)) %>% 
    group_by(calibration) %>% summarize(model.estimate = mean(predicted.probability),
                                        numstops = n(),
                                        empirical.estimate = mean(force.pointed))

# Create and save plot
p <- ggplot(data = plot.data, aes(y=empirical.estimate, x=model.estimate))
p <- p + geom_point(alpha=0.5, aes(size=numstops))
p <- p + scale_size_area(guide='none', max_size=15)
p <- p + geom_abline(intercept=0, slope=1, linetype="dashed")
p <- p + scale_y_log10('Empirical probability \n', limits=c(.001,1), breaks=c(.001,.003,.01,.03,.1,.3,1), 
                       labels=c('0.1%','0.3%','1%','3%','10%','30%','100%'))
p <- p + scale_x_log10('\nModel estimated probability', limits=c(.001,1), breaks=c(.001,.003,.01,.03,.1,.3,1), 
                       labels=c('0.1%','0.3%','1%','3%','10%','30%','100%'))

ggsave(plot=p, file='calibration_pt.pdf', height=5, width=5)

#2)Predict arrested with logistic regression
# Train-test split by year.
train_arrested <-  sqf.data %>% filter(complete.cases(sqf.data)) %>% filter(year < 2012) %>% mutate(suspect.height = standardize(suspect.height),
                                                                                                    suspect.weight = standardize(suspect.weight),
                                                                                                    suspect.age = standardize(suspect.age),
                                                                                                    observation.period = standardize(observation.period))
test_arrested <- sqf.data %>% filter(complete.cases(sqf.data)) %>% filter(year == 2012) %>% mutate(suspect.height = standardize(suspect.height),
                                                                                                   suspect.weight = standardize(suspect.weight),
                                                                                                   suspect.age = standardize(suspect.age),
                                                                                                   observation.period = standardize(observation.period))

model_arrested <- glm(arrested ~ precinct + location.housing +  
                          additional.report + additional.investigation + additional.sights +
                          additional.proximity + additional.evasive + additional.associating +
                          additional.direction + additional.highcrime + additional.time + 
                          stopped.bc.object + stopped.bc.bulge + stopped.bc.desc + stopped.bc.violent +
                          stopped.bc.casing + stopped.bc.lookout + stopped.bc.drugs + stopped.bc.clothing +
                          stopped.bc.furtive + suspect.age + suspect.build + suspect.sex +
                          suspect.height + suspect.weight + inside + observation.period +
                          officer.uniform + radio.run + day + month + time.period, 
                      data = train_arrested, family = 'binomial')

# Generate predicted probabilities for test set
test_arrested$predicted.probability <- predict(model_arrested, newdata = test_arrested, type = 'response')

### MAKE PLOTS ###
# make performance plot
plot.data <- test_arrested %>% arrange(desc(predicted.probability)) %>% 
    mutate(numstops = row_number(), percent.outcome = cumsum(arrested)/sum(arrested),
           stops = numstops/n()) %>% select(stops, percent.outcome)

# create and save plot
theme_set(theme_bw())
p <- ggplot(data=plot.data, aes(x=stops, y=percent.outcome)) 
p <- p + geom_line()
p <- p + scale_x_log10('\nPercent of stops', limits=c(0.003, 1), breaks=c(.003,.01,.03,.1,.3,1), 
                       labels=c('0.3%','1%','3%','10%','30%','100%'))
p <- p + scale_y_continuous("Percent of being arrested", limits=c(0, 1), labels=scales::percent)
p
ggsave(plot=p, file='performance_plot_arrestd.pdf', height=5, width=5)

# Calibration plot
plot.data <- test_arrested %>% mutate(calibration = round(100*predicted.probability)) %>% 
    group_by(calibration) %>% summarize(model.estimate = mean(predicted.probability),
                                        numstops = n(),
                                        empirical.estimate = mean(arrested))

# Create and save plot
p <- ggplot(data = plot.data, aes(y=empirical.estimate, x=model.estimate))
p <- p + geom_point(alpha=0.5, aes(size=numstops))
p <- p + scale_size_area(guide='none', max_size=15)
p <- p + geom_abline(intercept=0, slope=1, linetype="dashed")
p <- p + scale_y_log10('Empirical probability \n', limits=c(.001,1), breaks=c(.001,.003,.01,.03,.1,.3,1), 
                       labels=c('0.1%','0.3%','1%','3%','10%','30%','100%'))
p <- p + scale_x_log10('\nModel estimated probability', limits=c(.001,1), breaks=c(.001,.003,.01,.03,.1,.3,1), 
                       labels=c('0.1%','0.3%','1%','3%','10%','30%','100%'))

ggsave(plot=p, file='calibration_arrested.pdf', height=5, width=5)

#############################
# 3) Predict frisked using naive bayes
set.seed(911)

full <- sqf.data %>%
  mutate(suspect.height = standardize(suspect.height),
    suspect.weight = standardize(suspect.weight),
    suspect.age = standardize(suspect.age),
    observation.period = standardize(observation.period),
    suspected.crime=as.character(suspected.crime))
full$id <- seq(nrow(full))

train <- full %>% sample_n(nrow(full)/2, replace=FALSE)
test <- full %>% filter(!(id %in% train$id))

frisk.naive <- naive_bayes(frisked ~ precinct + location.housing +  
                          additional.report + additional.investigation + additional.sights +
                          additional.proximity + additional.evasive + additional.associating +
                          additional.direction + additional.highcrime + additional.time + 
                          stopped.bc.object + stopped.bc.bulge + stopped.bc.desc + stopped.bc.violent +
                          stopped.bc.casing + stopped.bc.lookout + stopped.bc.drugs + stopped.bc.clothing +
                          stopped.bc.furtive + suspect.age + suspect.build + suspect.sex +
                          suspect.height + suspect.weight + inside + observation.period +
                          officer.uniform + radio.run + day + month + time.period,
                          data=train)

# Predictions
test$pred.prob <- predict(frisk.naive, newdata=test, type='prob')[,2]


# Performance Plot
plot.data <- test %>% arrange(desc(pred.prob)) %>% 
    mutate(numstops = row_number(), percent.outcome = cumsum(frisked)/sum(frisked),
           stops = numstops/n()) %>% select(stops, percent.outcome)

# create and save plot
p <- ggplot(data=plot.data, aes(x=stops, y=percent.outcome)) +
  geom_line() + 
  scale_x_log10('\nPercent of stops', limits=c(0.003, 1), breaks=c(.003,.01,.03,.1,.3,1), labels=c('0.3%','1%','3%','10%','30%','100%')) +
  scale_y_continuous("Percent of stops w/ frisk", limits=c(0, 1), labels=scales::percent) +
  theme_bw()

ggsave(plot=p, file='performance_plot_frisked.pdf', height=5, width=5)

# Calibration plot
plot.data <- test %>% mutate(calibration = round(100*pred.prob)) %>% 
    group_by(calibration) %>%
    summarize(model.estimate = mean(pred.prob),
      numstops = n(),
      empirical.estimate = mean(frisked))

# Create and save plot
p <- ggplot(data = plot.data, aes(y=empirical.estimate, x=model.estimate)) +
  geom_point(alpha=0.5, aes(size=numstops)) +
  scale_size_area(guide='none', max_size=15) +
  geom_abline(intercept=0, slope=1, linetype="dashed") +
  scale_y_log10('Empirical probability \n', limits=c(.001,1), breaks=c(.001,.003,.01,.03,.1,.3,1), labels=c('0.1%','0.3%','1%','3%','10%','30%','100%')) +
  scale_x_log10('\nModel estimated probability', limits=c(.001,1), breaks=c(.001,.003,.01,.03,.1,.3,1), labels=c('0.1%','0.3%','1%','3%','10%','30%','100%')) +
  theme_bw()

ggsave(plot=p, file='calibration_frisked.pdf', height=5, width=5)

