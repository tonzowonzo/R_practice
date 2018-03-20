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



















