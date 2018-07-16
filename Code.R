library(ggplot2)

testSamples = read.csv("/Users/Caius/Documents/Knowledge/Data sets/datasciencetest-master/testSamples.csv")
transData = read.csv("/Users/Caius/Documents/Knowledge/Data sets/datasciencetest-master/transData.csv")


#######################
# Data Transformation #
#######################


userData = as.data.frame(cbind(testSamples$sample_id,matrix(0,nrow(testSamples),4)))
names(userData) = c("sample_id","revenue","addRebill","rebillCount","chargebackCount")

# table containing all users that had a transaction and the revenue they generated
userRevenue = aggregate(list(revenue = transData$transaction_amount), by=list(sample_id = transData$sample_id), FUN = sum)

for (i in userRevenue$sample_id)                # filling in userData
{                                              
  userData$revenue[i] = userRevenue$revenue[match(i,userRevenue$sample_id)]
  userData[i,4] = length(transData[(transData$sample_id == i) & (as.character(transData$transaction_type) == "REBILL"),1])
  userData[i,5] = length(transData[(transData$sample_id == i) & (as.character(transData$transaction_type) == "CHARGEBACK"),1])
  print(i)
}

# addRebill = 1 if user had at least 1 additionnal and is 0 otherwise
userData$addRebill = as.numeric(userData$rebillCount > 0)

userData = merge(testSamples,userData, by="sample_id")      # joining userData and testSamples



########################
# Exploratory Analysis #
########################


groups = as.data.frame(table(userData$test_group))

pcGroup = ggplot(groups, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  ggtitle("Distribution between test and control group")+
  scale_fill_manual(values=c("#4583B6", "#56B4E9"), 
                    name="Group",
                    labels=c("Control group", "Test group")) 
pcGroup


rebilldTest = as.data.frame(table(userData$addRebill[userData$test_group == 1]))
rebilldCtrl = as.data.frame(table(userData$addRebill[userData$test_group == 0]))


pcTest = ggplot(rebilldTest, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  ggtitle("Distribution of rebilled users for test group") +
  scale_fill_manual(values=c("#4583B6", "#56B4E9"), 
                    name="Group",
                    labels=c("Not rebilled", "Rebilled"))
pcTest


pcCtrl = ggplot(rebilldCtrl, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  ggtitle("Distribution of rebilled users for control group") +
  scale_fill_manual(values=c("#4583B6", "#56B4E9"), 
                    name="Group",
                    labels=c("Not rebilled", "Rebilled"))
pcCtrl


avgRevenue =  data.frame(group = factor(c("Test","Control"), levels=c("Test","Control")),
                         AverageRevenue = c(mean(userData$revenue[userData$test_group == 1]),
                                            mean(userData$revenue[userData$test_group == 0])))

ggplot(data=avgRevenue, aes(x=group, y=AverageRevenue, fill=group)) + geom_bar(stat="identity") +
  ggtitle("Avergage revenue by group") 


ggplot(userData[userData$test_group == 1,], aes(x=revenue)) + geom_histogram(binwidth=40) +
  ggtitle("Distribution of revenue users for test group") 
  
ggplot(userData[userData$test_group == 0,], aes(x=revenue)) + geom_histogram(binwidth=40) +
  ggtitle("Distribution of revenue for control group") 



########################################
# Statistical Analysis and Conclusions #
########################################

#####
# 1 #
#####

prop.test(sum(userData$test_group),length(userData$test_group))


#####
# 2 #
#####

prop.test(c(sum(userData$addRebill[userData$test_group == 1]),sum(userData$addRebill[userData$test_group == 0])),
          c(length(userData$addRebill[userData$test_group == 1]),length(userData$addRebill[userData$test_group == 0])),
          alternative = "greater")


#####
# 3 #
#####

wilcox.test(userData$revenue[userData$test_group == 1],userData$revenue[userData$test_group == 0],
            alternative = "greater")

t.test(userData$revenue[userData$test_group == 1],userData$revenue[userData$test_group == 0],
       alternative = "greater")


#####
# 4 #
#####

sum(userData$chargebackCount[userData$test_group == 1])/sum(userData$rebillCount[userData$test_group == 1])
sum(userData$chargebackCount[userData$test_group == 0])/sum(userData$rebillCount[userData$test_group == 0])


