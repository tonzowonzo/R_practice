# Week 1 R course
# Objects.
a <- 1
b <- 2
c <- 3

# Operator.
a * c
a - b

# Functions.
# We need to use parenthesis.
ls() # This is a function.

# Log func.
log(8)
log(a)

# Exp
exp(1)
log(exp(1))

# Help system.
help("log")
?log

# Arguments of log
args(log)

# Change args
log(x=2, base=2)

# Sum to 1000.
n <- 1000
n*(n+1)/2

# Log base 10
log10(sqrt(100))

# Data types.
class(ls) # Function

# Dataframes
# Like tables and very useful because of merging etc
library(dslabs)
data("murders")
class(murders)
str(murders) # Structure of dataframe
head(murders) # Head of df


# Clear memory
gc()