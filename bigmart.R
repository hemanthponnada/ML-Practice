
#Loading the required packages
library(data.table)
library(dplyr)
library(ggplot2)
library(caret)
library(corrplot)
library(xgboost)
library(cowplot)

#Loading the datasets
train<-read.table("Train.txt", sep=",", header = TRUE)
test<-read.table("Test.txt", sep = ",", header = TRUE)

dim(train)
dim(test)
names(train)
names(test)

str(train)
str(test)


test[,Item_Outlet_Sales:=NA]
test$Item_Outlet_Sales<-NA

combi<-rbind(train, test)

dim(combi)


ggplot(train)+geom_histogram(aes(train$Item_Outlet_Sales), binwidth=100, fill="darkgreen")+ xlab("Item_Outlet_Sales")


p1 = ggplot(combi) + geom_histogram(aes(Item_Weight), binwidth = 0.5, fill = "blue") 
p2 = ggplot(combi) + geom_histogram(aes(Item_Visibility), binwidth = 0.005, fill = "blue") 
p3 = ggplot(combi) + geom_histogram(aes(Item_MRP), binwidth = 1, fill = "blue") 
plot_grid(p1, p2, p3, nrow = 1) # plot_grid() from cowplot package


ggplot(combi)+geom_bar(aes(Item_Fat_Content))

combi$Item_Fat_Content[combi$Item_Fat_Content == "LF"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "low fat"] = "Low Fat" 
combi$Item_Fat_Content[combi$Item_Fat_Content == "reg"] = "Regular"

ggplot(combi)+geom_bar(aes(Item_Fat_Content), fill="coral1")

p4=ggplot(combi, aes(x=Item_Type))+geom_bar()+ geom_text(stat='count', aes(label=..count..), vjust=-1)+theme(axis.text.x = element_text(angle = 45, hjust = 1))


p5=ggplot(combi, aes(x=Outlet_Identifier)) +geom_bar()+geom_text(stat = 'count', aes(label=..count..), vjust=-1)+ theme(axis.text.x = element_text(angle = 45, hjust = 1))

p6=ggplot(combi, aes(x=Outlet_Size)) +geom_bar()+geom_text(stat = 'count', aes(label=..count..), vjust=-1)+ theme(axis.text.x = element_text(angle = 45, hjust = 1))

p7=ggplot(combi, aes(x=Outlet_Establishment_Year)) +geom_bar()+geom_text(stat = 'count', aes(label=..count..), vjust=-1)+ theme(axis.text.x = element_text(angle = 45, hjust = 1))

p8=ggplot(combi, aes(x=Outlet_Type)) +geom_bar()+geom_text(stat = 'count', aes(label=..count..), vjust=-1)+ theme(axis.text.x = element_text(angle = 45, hjust = 1))



train = combi[1:nrow(train),] # extracting train data from the combined data


p9=ggplot(train)+geom_point(aes(Item_Weight ,Item_Outlet_Sales), colour="violet", alpha=0.3)+ theme(axis.title = element_text(size = 8.5))

p10= ggplot(train)+geom_point(aes(Item_Visibility ,Item_Outlet_Sales), colour="violet", alpha=0.3)+ theme(axis.title = element_text(size = 8.5))

p11= ggplot(train)+geom_point(aes(Item_MRP ,Item_Outlet_Sales), colour="violet", alpha=0.3)+ theme(axis.title = element_text(size = 8.5))


second_row_2=plot_grid(p10,p11, ncol = 2)

plot_grid(p9, second_row_2, nrow = 2)


p12=ggplot(train)+geom_violin(aes(Item_Type, Item_Outlet_Sales), fill="magenta")+theme(axis.text.x = element_text(angle = 45, hjust=1), axis.text = element_text(size = 6),axis.title = element_text(size=8.5))

p13=ggplot(train)+geom_violin(aes(Item_Fat_Content , Item_Outlet_Sales), fill="magenta")+theme(axis.text.x = element_text(angle = 45, hjust=1), axis.text = element_text(size = 6),axis.title = element_text(size=8.5))


p14=ggplot(train)+geom_violin(aes(Outlet_Identifier , Item_Outlet_Sales), fill="magenta")+theme(axis.text.x = element_text(angle = 45, hjust=1), axis.text = element_text(size = 6),axis.title = element_text(size=8.5))  

second_row_3 = plot_grid(p13, p14, ncol = 2)
plot_grid(p12, second_row_3, ncol = 1)


ggplot(train) + geom_violin(aes(Outlet_Size, Item_Outlet_Sales), fill = "magenta")


p15=ggplot(train)+geom_violin(aes(Outlet_Location_Type , Item_Outlet_Sales), fill="magenta")+theme(axis.text.x = element_text(angle = 45, hjust=1), axis.text = element_text(size = 6),axis.title = element_text(size=8.5))  

p16 = ggplot(train) + geom_violin(aes(Outlet_Type, Item_Outlet_Sales), fill = "magenta") 
plot_grid(p15, p16, ncol = 1)


sum(is.na(combi$Item_Weight))

missing_index=which(is.na(combi$Item_Weight))

for (i in missing_index){
  item=combi$Item_Identifier[i]
  combi$Item_Weight[i]=mean(combi$Item_Weight[combi$Item_Identifier==item], na.rm = TRUE)
}

sum(is.na(combi$Item_Weight))

ggplot(combi) + geom_histogram(aes(Item_Visibility), bins = 100)

zero_index = which(combi$Item_Visibility == 0)
for(i in zero_index){    
  item = combi$Item_Identifier[i]  
  combi$Item_Visibility[i] = mean(combi$Item_Visibility[combi$Item_Identifier == item], na.rm = T)
  }



perishable = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")



non_perishable = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene", "Household", "Soft Drinks")


combi$Item_Type_new <- ifelse(combi$Item_Type %in% perishable, "perishable",
                              ifelse(combi$Item_Type %in% non_perishable, "non_perishable",
                                     "not_sure"))

table(combi$Item_Type, substr(combi$Item_Identifier, 1, 2))



combi[,Item_category <- substr(combi$Item_Identifier, 1, 2)]


combi$Item_Category<-substr(combi$Item_Identifier,1,2)


combi$Item_Fat_Content[combi$Item_Category == "NC"] = "Non-Edible" 

combi$Outlet_Years <- (2019 - combi$Outlet_Establishment_Year)


combi[,price_per_unit_wt := Item_MRP/Item_Weight]

combi$price_per_unit_wt<-combi$Item_MRP/combi$Item_Weight


combi[,Item_MRP_clusters := ifelse(Item_MRP < 69, "1st",
                                   ifelse(Item_MRP >= 69 & Item_MRP < 136, "2nd",
                                          ifelse(Item_MRP >= 136 & Item_MRP < 203, "3rd", "4th")))]

combi$Item_MRP_Clusters<-ifelse(combi$Item_MRP < 69, "1st",
                                ifelse(combi$Item_MRP >= 69 & combi$Item_MRP < 136, "2nd",
                                       ifelse(combi$Item_MRP >= 136 & combi$Item_MRP < 203, "3rd", "4th")))


combi$Outlet_Size<-ifelse(combi$Outlet_Size=="Small", 0,
                          ifelse(combi$Outlet_Size=="Medium",1,2))

combi$Outlet_Location_Type<-ifelse(combi$Outlet_Location_Type=="Tier 3",0,
                                   ifelse(combi$Outlet_Location_Type=="Tier 2",1,2))

ohe = dummyVars("~.", data = combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")], fullRank = T) 
ohe_df = data.table(predict(ohe, combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")])) 
combi = cbind(combi[,"Item_Identifier"], ohe_df)


ix <- which(c("Item_Identifier", "Outlet_Establishment_Year","Item_Type") %in% rownames(combi)) 

dum=data.frame(model.matrix("~.", data =combi[-ix] ))
















