#Installing and loading packages

requiredPackages = c("tidyverse","stats","ggforce","knitr","kableExtra","DataExplorer",
                     "reshape2","arules","arulesViz","arulesCBA","arulesSequences","psych","pastecs","caret","fastDummies") 

for(i in requiredPackages){if(!require(i,character.only = TRUE)) install.packages(i)} 
for(i in requiredPackages){library(i,character.only = TRUE)}

# Loading up the data
data_full <- read.csv("dataset.csv",stringsAsFactors = F)
round(nrow(data)/nrow(data_full),3)*100
###########################
######### EDA
###########################

# Looking for NA values
plot_missing(data_full,group = list(Good = 0.05, OK = 0.4, Bad = 0.8, Remove = 1))

# Removing NA observations
data <- data_full[complete.cases(data_full),]


# Ploting descriptive statistics
temp <- t(round(stat.desc(data[]),2))[,c(1,3,4,5,6,8,9)]
kable(temp,caption = "Table 2. Summary statistics of the dataset")%>% 
  kable_styling(latex_options="scale_down",bootstrap_options = c("striped", "hover"))


# Plotting distributions of ordinal and categorical variables

# Histograms of ordinal variables
gather(data[,c(1,4,5,7,9,10)]) %>% 
  ggplot(., aes(value)) + 
  geom_histogram(aes(y =..density..), 
                 col="black", 
                 fill="lightblue", 
                 alpha=.2) + 
  geom_density(col="cyan3")+
  facet_wrap(~key, scales = 'free') + theme_minimal()


# Barplots of categorical variables
gather(data[,c(2,3,6,8,11,12,13,14)]) %>% 
  ggplot(., aes(value)) + 
  geom_bar(aes(x =value),
           position = "dodge",
           col="black", 
           fill="lightblue", 
           alpha=.2)+
  facet_wrap(~key, scales = 'free') + theme_minimal()

# Transofrming variables 

# Categorical
categorical <- c("Sex","Marital.Status","Occupation","Dual.income","Householder.status",
                 "Type.of.home","Ethnic.classification","Most.spoken.language")

data_edit <- data 

data_edit$Sex <- as.factor(data_edit$Sex)
data_edit$Marital.Status <- as.factor(data_edit$Marital.Status)
data_edit$Occupation <- as.factor(data_edit$Occupation)
data_edit$Dual.income <- as.factor(data_edit$Dual.income)
data_edit$Householder.status <- as.factor(data_edit$Householder.status)
data_edit$Type.of.home <- as.factor(data_edit$Type.of.home)
data_edit$Ethnic.classification <- as.factor(data_edit$Ethnic.classification)
data_edit$Most.spoken.language <- as.factor(data_edit$Most.spoken.language)
 
# install.packages("fastDummies")
# library(fastDummies)
results <- fastDummies::dummy_cols(data_edit, 
                                   select_columns = categorical,remove_selected_columns = T)

# Ordinal data
ordinal <- c("Annual.Income","Age","Education","Years.living.in.the.area",
             "Persons.in.household","Persons.in.household...18")

data_edit2 <- results
data_edit2$Annual.Income <- ifelse(data_edit2$Annual.Income > median(data_edit2$Annual.Income),1,0)
data_edit2$Age <- ifelse(data_edit2$Age > median(data_edit2$Age),1,0)
data_edit2$Education <- ifelse(data_edit2$Education > median(data_edit2$Education),1,0)
data_edit2$Years.living.in.the.area <- ifelse(data_edit2$Years.living.in.the.area >= median(data_edit2$Years.living.in.the.area),1,0)
data_edit2$Persons.in.household <- ifelse(data_edit2$Persons.in.household > median(data_edit2$Persons.in.household),1,0)
data_edit2$Persons.in.household...18 <- ifelse(data_edit2$Persons.in.household...18 > median(data_edit2$Persons.in.household...18),1,0)

data_edit2 <- fastDummies::dummy_cols(data_edit2, 
                                      select_columns = ordinal,remove_selected_columns = T)
data_edit2 <- as.data.frame(data_edit2)

## Association rule learning

data_edit3 <- data_edit2

data_edit3 <- apply(data_edit2, 2, as.logical)
trans <- as(data_edit3, "transactions")

summary(trans)
size(trans) 
length(trans)
       

itemFrequencyPlot(trans, type="relative", main="Item Frequency",horiz = T, cex.names=0.45)

rules.trans<-apriori(trans, parameter=list(supp=0.2, conf=0.8,minlen=2)) 
rules.by.conf<-sort(rules.trans, by="confidence", decreasing=TRUE) 
inspect(head(rules.by.conf,n=50))
rules.by.lift<-sort(rules.trans, by="lift", decreasing=TRUE) 
inspect(head(rules.by.lift,n=80))
rules.by.count<- sort(rules.trans, by="count", decreasing=TRUE) 
inspect(head(rules.by.count))
rules.by.supp<-sort(rules.trans, by="support", decreasing=TRUE) 
inspect(head(rules.by.supp))


rules.trans.high_income<-apriori(trans, parameter=list(supp=0.1, conf=0.5,minlen=2),
                                 appearance = list(default="lhs",rhs="Annual.Income_1"), control = list (verbose=F))
rules.by.conf.high_income<-sort(rules.trans.high_income, by="confidence", decreasing=TRUE) 
inspect(head(rules.by.conf.high_income,n=50))



image(trans)

plot(rules.trans, method="matrix", measure="lift")
plot(rules.trans) 
plot(rules.trans, measure=c("support","lift"), shading="confidence")
plot(rules.trans, shading="order", control=list(main="Two-key plot"))
subrules <- sample(rules.trans,30)
plot(subrules, method="grouped") # this one has a great potential
plot(subrules, method="graph") # this one too
plot(subrules, method="graph", control=list(type="items"))
plot(subrules, method="paracoord", control=list(reorder=TRUE))

