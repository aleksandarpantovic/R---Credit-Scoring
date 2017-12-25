library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(magrittr)
library(caTools)
library(data.table)

data <- read_excel("~/Downloads/Sec.xlsx")


######################  Converting columns to factors  #################################

unique.vals <- NULL
counter <-1
for (i in names(data)){
  unique.vals[counter]<- data %>% dplyr::select(i) %>% unique() %>% count() %>% pull(n)
  counter<-counter+1
}

names(unique.vals)<- names(data)

tmp<-unique.vals[unique.vals<15]   #converting to factor columns with less than 15 unique values
data <- mutate_each(data, funs(factor), names(tmp))


################################# Splitting Sample ######################################

set.seed(100)
split <- sample.split(data$gb_flag, SplitRatio = 0.70) #Splitting data into train and testing

train = subset(data, split == TRUE)
test = subset(data, split == FALSE)

train<-as.data.frame(train)
train$gb_flag<-as.numeric(train$gb_flag)-1



################################# Fine Classing #########################################

numeric<-names(Filter(is.numeric, train))

fineClassing<-list()
for (num in 1:length(numeric)){
fineClassing[[numeric[num]]]<-fc.num(train,'gb_flag',numeric[num])
  
}
fineClassing

################################# Correlation Analysis ###################################





################################# Coarse Classing ########################################









