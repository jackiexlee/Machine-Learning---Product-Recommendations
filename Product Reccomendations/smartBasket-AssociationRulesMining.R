#install.packages("arules")
#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("readxl")

library(arules)
library(arulesViz)
library(tidyr)
library(dplyr)
library(readxl)
library(skimr)

## Data loading

trans <- read_excel(file.choose(), sheet=2) ##"sheet=2" since this file has 2 sheets, will tell R to analyze sheet 2
colnames(trans) ##looking at transaction column

#data engineering stage
#transforming column names
colnames(trans) = tolower(gsub(" ", "", colnames(trans)))

head(trans)
structure(trans) ##check number of rows
#skim(trans)

# We need only an order identifier and the items for that order
# The same item repeats in an order so we need to remove that as well - unique drops duplicates.
trans <- trans %>% select(order, description) %>% unique()  ##removing duplicates
trans <- split(trans$description, trans[,"order"]) ##looking at order column
head(trans)

#Converting this to a transactions object - reshape descriptions 'wide' with respect to Order #
trans_basket = as(trans, "transactions")
inspect(trans_basket[0:5])

#This data is now fit for running association rules.
rules <- apriori(trans_basket,parameter=list(supp = 0.01, conf=0.75)) ##apiori -> H{A,B} then {H Butter} -> lift & confidence. Min support is 0.01 and confidence is 0.75 -> these are hyperparameters you specify yourself
inspect(rules[1:5]) ##inspecting our rules. 

rules <- sort(rules,by="lift",decreasing=TRUE) ##sort rules by lift by decreasing order
inspect(rules) 

# I can also look at only rules that contain a particular item I care about on the LHS.
inspect(subset(rules, items %in% "Toor Dal")) ##run this to have a rule that goes with "Toor Dal" inside

# Let's visualize our rules to explore them more easily.
# The best plot to use is often a colored scatter plot (the default)
# Draw a box around some points and click inspect to see what rules they are.
# Make sure you click "end" to terminate the interaction, else you can't get your console back!
#plot(rules,jitter=4,interactive=TRUE) # not working with new version of R



