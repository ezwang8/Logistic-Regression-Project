
adult <- read.csv('adult_sal.csv')
head(adult)

# Use dplyr and double-check head and summary
library(dplyr)
adult <- select(adult,-X)
head(adult)
str(adult)
summary(adult)

# Use table() to check out the frequency of the type_employer column.
table(adult$type_employer)

# Combine these two smallest groups into a single group called "Unemployed". 
unemp <- function(job){
  job <- as.character(job)
  if (job=='Never-worked' | job=='Without-pay'){
    return('Unemployed')
  }else{
    return(job)
  }
}

adult$type_employer <- sapply(adult$type_employer,unemp)
table(adult$type_employer)

# Combine State and Local gov jobs into a category called SL-gov and 
# combine self-employed jobs into a category called self-emp.
group_emp <- function(job){
  if (job=='Local-gov' | job=='State-gov'){
    return('SL-gov')
  }else if (job=='Self-emp-inc' | job=='Self-emp-not-inc'){
    return('self-emp')
  }else{
    return(job)
  }
}

adult$type_employer <- sapply(adult$type_employer,group_emp)
table(adult$type_employer)

# Use table() to look at the marital column
table(adult$marital)

group_marital <- function(mar){
  mar <- as.character(mar)
  
  # Not-Married
  if (mar=='Separated' | mar=='Divorced' | mar=='Widowed'){
    return('Not-Married')
    
    # Never-Married   
  }else if(mar=='Never-married'){
    return(mar)
    
    #Married
  }else{
    return('Married')
  }
}

adult$marital <- sapply(adult$marital,group_marital)
table(adult$marital)

# Check the country column using table()
table(adult$country)

# Group these countries together
Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

group_country <- function(ctry){
  if (ctry %in% Asia){
    return('Asia')
  }else if (ctry %in% North.America){
    return('North.America')
  }else if (ctry %in% Europe){
    return('Europe')
  }else if (ctry %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')      
  }
}

adult$country <- sapply(adult$country,group_country)

# Use table() to confirm the groupings
table(adult$country)

# Check the str() of adult again.
str(adult)
adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)
str(adult)

# We're missing data, so we must install a package.
# Install and load the Amelia package.
library(Amelia)

# Convert any cell with a '?' or a ' ?' value to a NA value.
adult[adult == '?'] <- NA

# Using table() on a column with NA values should now not display 
# those NA values, instead you'll just see 0 for ?.
table(adult$type_employer)

adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)
adult$occupation <- sapply(adult$occupation,factor)

# Try to use missmap
missmap(adult)

# The missmap is used to detect all the missing values
# Edit the map by removing the ylabels
missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

# Use na.omit() to omit NA data from the adult data frame.
adult <- na.omit(adult)
str(adult)

# Use missmap() to check that all the NA values were in fact dropped.
missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

# Check the str() of the data.
str(adult)

# Use ggplot2 to create a histogram of ages, colored by income.
library(ggplot2)
library(dplyr)

ggplot(adult,aes(age)) + geom_histogram(aes(fill=income),color='black',binwidth=1) + theme_bw()

# Plot a histogram of hours worked per week
ggplot(adult,aes(hr_per_week)) + geom_histogram() + theme_bw()

# Rename the country column to region column to better reflect the factor levels.
names(adult)[names(adult)=="country"] <- "region"
str(adult)

# Create a barplot of region with the fill color defined by income class.
ggplot(adult,aes(region)) + geom_bar(aes(fill=income),color='black')+theme_bw()+
theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Split the data into a train and test set using the caTools library

# Check adult
head(adult)

# Import Library
library(caTools)

# Set a random see so your "random" results are the same as this notebook
set.seed(101) 

# Split up the sample, basically randomly assigns a booleans to a new column "sample"
sample <- sample.split(adult$income, SplitRatio = 0.70) # SplitRatio = percent of sample==TRUE

# Training Data
train = subset(adult, sample == TRUE)

# Testing Data
test = subset(adult, sample == FALSE)

# Explore the glm() function with help(glm).
help(glm)

# Train a glm() model on the training data set, pass the 
# argument family=binomial(logit) into the glm function.
model = glm(income ~ ., family = binomial(logit), data = train)

# Check the model summary
summary(model)

# Use new.model <- step(your.model.name) 
# to use the step() function to create a new model.
new.step.model <- step(model)

# Check the new.model by using summary()
summary(new.step.model)

# Create a confusion matrix using the predict function with 
# type='response' as an argument inside of that function.
test$predicted.income = predict(model, newdata=test, type="response")

table(test$income, test$predicted.income > 0.5)

# Check the accuracy of the model
(6372+1423)/(6372+1423+548+872)

# Calculate other measures of performance like, recall or precision.

# recall
6732/(6372+548)

# precision
6732/(6372+872)
