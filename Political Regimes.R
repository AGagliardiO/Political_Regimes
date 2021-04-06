# 0) Initial download of necesary packages 


if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)      
if (!require(lubridate)) install.packages('lubridate')
library(lubridate)
if (!require(utils)) install.packages('utils')
library(utils)
if (!require(stats)) install.packages('stats')
library(stats)

# 1) Download, wrangling and consolidation of economic and political data  

# 1.1 Download from original sources

if (!require(readxl)) install.packages('readxl')
library(readxl)

url1 <- "https://freedomhouse.org/sites/default/files/Country_and_Territory_Ratings_and_Statuses_FIW1973-2019.xls"
download.file(url1, "./Data_FHouse.xls", method = "curl")
Data_FHouse <- read_excel("Data_FHouse.xls", sheet = "Country Ratings, Statuses ")

url2 <- "https://www.imf.org/external/pubs/ft/weo/2019/02/weodata/WEOOct2019all.xls"
download.file(url2, "./Data_IMF.xls", method = "curl")
Data_IMF <- read.delim("./Data_IMF.xls")


# 1.2 Cleaning of the both databases and transformation to tidy format

# Economic

Economic_Data <- Data_IMF %>% 
select(-WEO.Country.Code,-WEO.Subject.Code,-Subject.Notes,-Country.Series.specific.Notes,-Estimates.Start.After, - ISO) %>% 
gather("Year","Amount", - Country, - Subject.Descriptor, - Units, - Scale)
Economic_Data$Year <- str_remove(Economic_Data$Year,"X") 

# Political

seq_data <- seq(1,139,3)
Political_Data <- Data_FHouse[c(-1,-2),]  
Political_Data <- Political_Data[,seq_data]
seq_years <- c(1, seq(2,137,3))
years <- Data_FHouse[1,seq_years]
colnames(Political_Data) <- years 
Political_Data <- Political_Data %>% 
rename("1984" = `Nov.1983-Nov.1984`, "1985" = `Nov.1984-Nov.1985`,"1986" = `Nov.1985-Nov.1986`, "1987" = `Nov.1986-Nov.1987`, "1988" = `Nov.1987-Nov.1988`, "1989" = `Nov.1988-Dec.1989`, "1983" = `Aug.1982-Nov.1983`, "Country" = `Year(s) Under Review`, "1981"= `Jan.1981-Aug. 1982`) %>% 
gather(key = "Year", value = "Grade", - Country)
rm(seq_data,years,seq_years)


# 1.3 Consolidation of both datasets through a common variable (Unity)

Economic_Data$Unity <- paste(Economic_Data$Country,Economic_Data$Year)
Political_Data$Unity <- paste(Political_Data$Country,Political_Data$Year)
Consolidated_Data <- left_join(Economic_Data,Political_Data, by = "Unity")
 
# 1.4 Cleaning of the resulting dataset

Consolidated_Data$Amount <- str_remove(Consolidated_Data$Amount,"n/a")
Consolidated_Data$Amount <- as.numeric(Consolidated_Data$Amount)
Consolidated_Data$Grade <- str_remove(Consolidated_Data$Grade,"-")
Consolidated_Data$Grade <- as.factor(Consolidated_Data$Grade)
Consolidated_Data <- Consolidated_Data %>% filter(Amount != "") %>% filter(Grade != "") %>%
select(-Year.y,-Country.y,-Scale,-Country.x,-Year.x)

#2) Data Analysis

# 2.1) Filtering of non - comparable data

Consolidated_Data <-  Consolidated_Data  %>% 
filter(Units %in% c("Percent of GDP","Percent change"))

# 2.2) Cuantitaive analysis of variables

Predictor_Analysis <-  Consolidated_Data %>%
  group_by(Subject.Descriptor,Grade) %>%
  summarize(Amount = mean(Amount)) %>%
  spread(key = Grade, value = Amount) %>%
  mutate("MaxVar" = (abs((max(F,NF,PF) - min(F,NF,PF))/min(F,NF,PF)*100)))  %>%
  arrange(desc(MaxVar))


# 2.3) Graphical Analysis of Variables

# Transformation from tidy to spread format

Consolidated_Data_spread <- Consolidated_Data  %>%
  dplyr::filter(Subject.Descriptor %in% c("Current account balance","General government primary net lending/borrowing",
                                          "Inflation, average consumer prices","General government net debt", 
                                          "General government revenue", "General government total expenditure")) %>%
  select(Unity,Grade,Subject.Descriptor,Amount) %>%
  spread(key = Subject.Descriptor, value = Amount)


# Boxplot

labels_boxplot <- as_labeller(c(`Current account balance` = "External Balance", `General government primary net lending/borrowing` = "Fiscal Balance", `Inflation, average consumer prices` = "Inflation", `General government net debt` = "Goverment Debt", `General government revenue` = "Revenue", `General government total expenditure` = "Expenditure"))

Boxplot <- Consolidated_Data %>%
  dplyr::filter(Subject.Descriptor %in% c("Current account balance",
                                          "General government primary net lending/borrowing",
                                          "Inflation, average consumer prices","General government net debt",
                                          "General government revenue", "General government total expenditure")) %>%
  ggplot(aes(Grade,Amount), col = Grade) +
  geom_boxplot() + ylim(-10,50) + ylab("Percent Points") + xlab("Regime Type" ) +
  facet_grid(. ~ Subject.Descriptor, labeller = labels_boxplot)
Boxplot
rm(labels_boxplot)

# Scatter Plots

Scatter_Debt_Deficit <- Consolidated_Data_spread %>% 
ggplot(aes(x = `General government net debt`, y = `General government primary net lending/borrowing`, alpha = 0.1, colour = Grade)) + 
geom_point() + xlim(-20,100) + ylim(-20,20) + 
ggtitle("Goverment Debt vs Goverment Deficit") + 
theme(plot.title =element_text(hjust = 0.5)) +
ylab("Primary Fiscal Balance") + xlab("Goverment Debt")
Scatter_Debt_Deficit

Scatter_Revenue_Expenditure <- Consolidated_Data_spread %>% 
ggplot(aes(x = `General government revenue`, y = `General government total expenditure`, alpha = 0.1, colour = Grade)) + 
geom_point() + xlim(-20,30) + ylim(-0,30) + ylab("Goverment Expenditures") + 
xlab("Goverment Revenues") + ggtitle("Goverment Expenditures vs Goverment Revenues") + 
theme(plot.title =element_text(hjust = 0.5)) 
Scatter_Revenue_Expenditure

Scatter_CA_Inflation <- Consolidated_Data_spread %>% 
ggplot(aes(x = `Current account balance`, y = `Inflation, average consumer prices`, alpha = 0.1, colour = Grade)) + 
geom_point() + xlim(-20,20) + ylim(-0,20) + ggtitle("Inflation vs External Balance" ) + 
ylab("Inflation") + xlab("External Balance") + theme(plot.title = element_text(hjust = 0.5))
Scatter_CA_Inflation


# 3) Machine Learning Analysis 

f (!require(caret)) install.packages('caret')
library(caret)
if (!require(rpart)) install.packages('rpart')
library(rpart)
if (!require(randomForest)) install.packages('randomForest')
library(randomForest)
if (!require(grid)) install.packages('grid')
library(grid)
if (!require(gridExtra)) install.packages('gridextra')
library(gridExtra)
if (!require(ggpubr)) install.packages('ggpubr')
library(ggpubr)

# 3.1) A new vertion of the dataset is created, containig only the countries and year with all the selected variables available
  
Reduced_Data <- Consolidated_Data_spread %>%
  rename("External" = `Current account balance`, 
         "Inflation" = `Inflation, average consumer prices`,
         "Debt" = `General government net debt`, 
         "Deficit" = `General government primary net lending/borrowing`,
         "Revenue" = `General government revenue`, 
         "Expenditure" = `General government total expenditure`) %>%
  drop_na("External","Debt","Deficit") %>% 
  select(-Unity) %>% dplyr::filter(Grade != "")
Reduced_Data$Grade <- as.factor(as.character(Reduced_Data$Grade))


# 3.2) Partition into training set and test set

set.seed(1)
test_index <- createDataPartition(Reduced_Data$Grade,times = 1, p = 0.2, list = FALSE)
train_set <- Reduced_Data[-test_index,]
test_set <- Reduced_Data[test_index,]

# 3.3) Aplication of Machine Learning algorithms


# 3.3.1) Clasification tree Model

set.seed(1)
train_rpart_tuned   <-  train(Grade ~ .,data = train_set, method = "rpart", 
                              tuneGrid = data.frame(cp = seq(0.001, 0.01, 0.001)))
plot(train_rpart_tuned, main = "Clasification Tree Parameter Tunning (II)")

predict_rpart <-  predict(train_rpart_tuned, test_set, type = "raw" )
Results_rpart <-  confusionMatrix(predict_rpart,test_set$Grade)
Overall_rpart <-  Results_rpart$overall[["Accuracy"]]
Table_rpart   <-  Results_rpart$table

# 3.3.2) Random Forest Model

set.seed(1)
train_rf_tuned   <- train(Grade ~ ., data = train_set, method = "rf", 
                          tuneGrid = data.frame(mtry = seq(1, 3, 1)))
plot(train_rf_tuned, main = "Random Forest Parameter Tuning (II)")

predict_rf <-  predict(train_rf_tuned, test_set, type = "raw" )
Results_rf <- confusionMatrix(predict_rf,test_set$Grade)
Table_rf   <-  Results_rf$table
Overall_rf  <-  Results_rf$overall[["Accuracy"]]

# 3.3.3)  kNN Model 

set.seed(1)
train_knn   <-  train(Grade ~ ., data = train_set, method = "knn", tuneGrid = data.frame(k = seq(1, 10, 2)))
predict_knn <-  predict(train_knn, test_set, type = "raw" )
Results_knn      <- confusionMatrix(predict_knn,test_set$Grade)
Overall_knn  <-  Results_knn$overall[["Accuracy"]]


## 3.4) Effective relevance of variables

var_rpart <- plot(varImp(train_rpart_tuned))
var_knn   <- plot(varImp(train_knn_tuned))
var_rf    <- plot(varImp(train_rf_tuned))
Variable_chart <- ggarrange(var_rpart,var_rf,var_knn, 
                            labels = c("Clasification Tree","Random Forest","kNN"), hjust = -0.5)
annotate_figure(Variable_chart, 
                top = text_grob("Relative Importance of variables, by ML model", size = 15))
rm(var_knn,var_rf,var_rpart)

