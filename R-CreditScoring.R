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
train$gb_flag<-as.numeric(train$gb_flag)-1


fineClassing<-list()
for (num in 1:length(numeric)){
  fineClassing[[numeric[num]]]<-fc.num(train,'gb_flag',numeric[num])
  
}

IV_table<-sapply(lapply(fineClassing, '[',10),max) %>% as.data.frame()
names(IV_table)<-'IV_raw'
IV_table$variable<-rownames(IV_table)
rownames(IV_table)<-NULL
IV_table<-arrange(IV_table,desc(IV_raw))

################################# Correlation Analysis ###################################

correlation <- cor(Filter(is.numeric,train),method = "spearman",use = 'complete') %>% abs() %>% as.data.frame()

corr_all<-data.frame()
for (i in 1:nrow(correlation)){
proba<-NULL
proba$var1<-rownames(correlation)
proba$corr<-correlation[,i]
proba$var2<-names(correlation[i]) 
proba<- as.data.frame(proba)
corr_all<-rbind(corr_all,proba)
}
corr_all<-corr_all[!corr_all['var1']==corr_all['var2'],]

high_corr<-(corr_all[corr_all[, "corr"]>0.6 & corr_all[, "corr"]<1, ])
high_corr<-high_corr %>% merge(IV_table, by.x='var1',by.y='variable') %>% rename(iv1=IV_raw) %>%
  merge(IV_table, by.x='var2',by.y='variable') %>% rename(iv2=IV_raw) %>% arrange(desc(iv1),var1)
high_corr$diff<-high_corr$iv1-high_corr$iv2

high_corr<-mutate(high_corr,drop_var = case_when(diff >=0 ~ var2, 
                      diff < 0 ~ var1))

stay<-IV_table$variable

for(i in 1:nrow(IV_table)){
  drop<-high_corr$var2[high_corr$diff>=0 & high_corr$var1==stay[i]]
  stay<-stay[!stay %in% drop]
}

forCoarse<-filter(IV_table,variable %in% stay & IV_raw>0.02)
raw_corr<- cor(select(train, stay),method = "spearman",use = 'complete') %>% abs

################################# Coarse Classing ########################################
rm(drop,i,num,split,tmp,proba,high_corr,stay,corr_all)

coarse_start<-fineClassing[forCoarse$variable]

for (i in length(coarse_start)){
coarse_start[[i]]$group<-1:nrow(as.data.frame(coarse_start[i]))
}









