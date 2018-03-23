# Import libraries.
library('ggplot2') # Visualisation.
library('ggthemes') # Visualisation.
library('scales') # Visualisation.
library('dplyr') # Data manipulation.
library('mice') # Imputation.
library('randomForest') # Classification algorithm.

# Set new working directory.
setwd("C://Users//Tim//Rpractice//Kaggle")

# Read in datasets.
train <- read.csv("train.csv", stringsAsFactors=F)
test <- read.csv("test.csv", stringsAsFactors=F)

# Check data.
str(train)
str(test)

# Combine the data.
full <- bind_rows(train, test) # Bind training and test data.

# Feature engineering.
# Get title from passenger names.
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

# Show title counts by sex.
table(full$Sex, full$Title)

# Combine very rare titles into a single column.
rare_title <- c('Dona', 'Lady', 'the Countess', 'Capt',
		    'Col', 'Don', 'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle, ms and mme accordingly.
full$Title[full$Title == 'Mlle'] <- 'Miss'
full$Title[full$Title == 'Ms'] <- 'Miss'
full$Title[full$Title == 'Mme'] <- 'Mrs'
full$Title[full$Title %in% rare_title] <- 'Rare Title'

# Show title counts by sex.
table(full$Sex, full$Title)

# Grab surnames from passenger name.
full$Surname <- sapply(full$Name, function(x) strsplit(x, split = '[,.]')[[1]][1])

# How many unique surnames
cat(paste('We have <b>', nlevels(factor(full$Surname)), 
'</b> unique surnames. I would be interested to infer ethnicity based on surname --- another time.'))

# Do families sink or swim together?
# Create a family size variable including the passenger themselves.
full$Fsize <- full$SibSp + full$Parch + 1

# Create a family variable.
full$Family <- paste(full$Surname, full$Fsize, sep='_')

# Use ggplot2 to visualise the relationship between family size and survival.
ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()

# Split families into discrete sizes.
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

# Plot family size by survival in mosaic plot.
mosaicplot(table(full$FsizeD, full$Survived), main='Family Size by Survival', shade=TRUE)

# Extract data from cabin column.
full$Deck <- factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))

# Dealing with missing values.
# Fill passenger 62 and 830 on embarked column.
full[c(62, 830), 'Embarked']

# Get rid of missing passenger ID's. %>% passes the values to the df on the left.
embark_fare <- full %>%
  filter(PassengerId != 62 & PassengerId != 830)

# Use ggplot2 to visualise embarkment, passenger class and median fare.
ggplot(embark_fare, aes(x=Embarked, y=Fare, fill=factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80),
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

# Fix some rows.
# Since their fair was only 80$ for first class it's likely wrong.
full$Embarked[c(62, 830)] <- 'C'

# Show row 1044.
full[1044,]

# Visualise all class 3 and embarked S samples.
ggplot(full[full$Pclass == '3' & full$Embarked == 'S',],
       aes(x=Fare)) +
  geom_density(fill='#99d6ff', alpha=0.4) +
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()

# Replace missing fare values with median value.
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm=TRUE)

# Creating predictions.
# Show number of NaNs.
sum(is.na(full$Age))

# Fill missing values with multiple imputation using chained equations.
# Make variables into factors.
factor_vars <- c('Passenger_id', 'Pclass', 'Sex', 'Embarked',
                 'Title', 'Surname', 'Family', 'FsizeD')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

# Set a random seed.
set.seed(129)

# Perform mice imputation, excluding certain, less than useful vars.
mice_mod <- mice(full[, !names(full) %in% c('PassengerId', 'Name', 'Ticket', 'Cabin', 'Family', 'Surname', 'Survived'
                                            )], method='rf')

# Save output
mice_output <- complete(mice_mod)

# Plot age distributions.
par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Age of Original Data',
     col='darkgreen', ylim=c(0, 0.04))
hist(mice_output$Age, freq=F, main='Age: MICE output', 
     col='lightgreen', ylim=c(0, 0.04))

# Replace age in original data with age from mice model.
full$Age <- mice_output$Age

# Show number of missing vals.
sum(is.na(full$Age))

# More feature engineering.
# Look at the relationship between age and survival.
ggplot(full[1:891,], aes(Age, fill=factor(Survived))) +
  geom_histogram() +
  facet_grid(.~Sex) +
  theme_few()

# Create another column to indicate whether child or adult.
full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'

# Show counts.
table(full$Child, full$Survived)

# Adding mother variable.
full$Mother <- 'Not Mother'
full$Mother[full$Sex == 'female' & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'Mother'

# Show counts.
table(full$Mother, full$Survived)

# Finish by factorising our two variables.
full$Child <- factor(full$Child)
full$Mother <- factor(full$Mother)
md.pattern(full)
# Predictions.
# Split into train and test sets.
train <- full[1:891,]
test <- full[892:1309,]

# Building the model.
# Set a random seed.
set.seed(754)

# Build the model
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch +
                           Fare + Embarked + Title + FsizeD + Child + Mother,
                         data=train)

# Show model error.
plot(rf_model, ylim=c(0, 0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)




