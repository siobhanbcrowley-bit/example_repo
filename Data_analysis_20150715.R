######################################################
## Patterson et al - Code to create score variables
##
##	This script provides code for calculating the scores
##	presented in Patterson et al. (under review).
##
##		2016-09-26
######################################################

#required libraries
library(reshape)
library(ggplot2)
library(stringr)

#Set working directory to location of file
setwd("")

data = read.csv(file="Analysis_data.csv", header=T)


########################################################
## Data processing
########################################################

#Need to reshape the data into long format
DR_long = melt(subset(data, select=c("ResponseID", grep("DR_", names(data), value=T))), id.vars="ResponseID")
IB_long = melt(subset(data, select=c("ResponseID", grep("IB_", names(data), value=T))), id.vars="ResponseID")
IB_long1 = subset(IB_long, IB_long$variable %in% c("IB_title","IB_text", "IB_text2", "IB_text3")==FALSE)
DR_long1 = subset(DR_long, DR_long$variable!="DR_title")


DR_long1$variable = gsub("DR_", "", DR_long1$variable)
names(DR_long1)[3] = "DR"
IB_long1$variable = gsub("IB_", "", IB_long1$variable)
names(IB_long1)[3] = "IB"

#Merge DR (risk of disease spread) and IB (impact on business) datasets
datalong = merge(DR_long1, IB_long1, by=c("ResponseID", "variable"), all=T)

#Reverse IB, so that higher values = more critical (time until hurts business)
datalong$IB_rev = 5 - datalong$IB

#Add group for movement type
datalong$mvgroup = factor(gsub("_[[:digit:]]*", "", as.character(datalong$variable)))

#Create aggregate data - mean, sd, se by movement 
dataagg = aggregate(subset(datalong, select=c("DR", "IB_rev")), list(datalong$variable), mean, na.rm=T)
dataaggsd = aggregate(subset(datalong, select=c("DR", "IB_rev")), list(datalong$variable), sd, na.rm=T)
names(dataaggsd)[2:3] = c("DR_sd", "IB_rev_sd")

dataagg1 = merge(dataagg, dataaggsd, by="Group.1")
dataagg1$DR_se = dataagg1$DR_sd/sqrt(colSums(!is.na((data[,which(names(data) %in% paste("DR_", dataagg1$Group.1, sep=""))]))))
dataagg1$IB_rev_se = dataagg1$IB_rev_sd/sqrt(colSums(!is.na((data[,which(names(data) %in% paste("IB_", dataagg1$Group.1, sep=""))]))))

#Add group for movement type
dataagg1$mvgroup = factor(gsub("_[[:digit:]]*", "", as.character(dataagg1$Group.1)))

#create reversed IB scores in the original data
for (i in paste("IB_", dataagg1$Group.1, sep="")) {
  data$a = 5 - data[,which(names(data)==i)]
  names(data)[which(names(data)=="a")] = paste(i, "rev", sep="_")
}


#add column with number that corresponds to movement question
dataagg1$MovmtNumber = gsub(".*_", "", dataagg1$Group.1)

dataagg1$MovmtNumberContin = 1:30



########################################################
## Creating Scores
########################################################


#Based on Reviewer feedback: present the average reversed IB score on the graph along with the consensus score for Risk of Disease Spread Presentation of the RDS variable will be changed to indicate the \% majority saying the top two categories (some or high risk = above 50) or the bottom two categories (no or low risk = below 50). The table will include bucketed reverse IB scores based on placement of the means on the rated scale: 3-4 (48 hours to a week); 2.5-3 (7 days to 14 days); 2 - 2.5 (14 days to 21 days); 1-2 (21 days to 60 days). 


#create new variables for each DR rating that codes 1 as "high" (3 or 4) and 0 as "low" risk (1 or 2)
for (i in paste("DR_", dataagg1$Group.1, sep="")) {
  data$a = ifelse(data[,which(names(data)==i)] > 2, 1, 0)
  names(data)[which(names(data)=="a")] = paste(i, "phigh", sep="_")
}

#create function to calculate percent of 1's for each variable
percentof1s = function(x) {round((sum(x, na.rm=T)/sum(complete.cases(x)))*100, 2)}

#calculate for each new variable
dataagg_p = apply(subset(data, select=grep("phigh", names(data), value=T)), 2, percentof1s)


#add to aggregated dataset
dataagg1$Percent_DR_High = dataagg_p[1:30]
dataagg1$checknames = names(dataagg_p)[1:30]


#create a new variable for IB means that codes which above category they fall into:
dataagg1$IB_rev_cat = ifelse(dataagg1$IB_rev > 3.01, "2 to 7 days",
                             ifelse(dataagg1$IB_rev > 2.51 & dataagg1$IB_rev < 3.01, "7 to 14 days", 
                                    ifelse(dataagg1$IB_rev > 2.01 & dataagg1$IB_rev < 2.51, "14 to 21 days",
                                           ifelse(dataagg1$IB_rev >= 1 & dataagg1$IB_rev < 2.01, "21 to 60 days", NA))))



########################################################
## Figure 1
########################################################

#Set colors for plot and adjust text labels so they don't overlap
colors = c("#d7191c","#fdae61", "#ffffbf", "#abdda4", "#2b83ba")
dataagg1$Percent_DR_high_nudge = dataagg1$Percent_DR_High
dataagg1$Percent_DR_high_nudge[1] = dataagg1$Percent_DR_high_nudge[1]-3
dataagg1$Percent_DR_high_nudge[21] = dataagg1$Percent_DR_high_nudge[21]-3.3
dataagg1$Percent_DR_high_nudge[6] = dataagg1$Percent_DR_high_nudge[6]+3
dataagg1$Percent_DR_high_nudge[10] = dataagg1$Percent_DR_high_nudge[10]-3


#Create static figure
ggplot(dataagg1, aes(x=Percent_DR_High, y=IB_rev)) + coord_cartesian(ylim=c(1,4), xlim=c(0,105)) +  labs(title="Expert Consensus on Risk of Disease Spread and Average Time to Impact on Business", x="Percent majority consensus of risk of disease spread", y="Average time until negative impact on business") +  geom_vline(xintercept=c(25,75), color="gray") + geom_hline(yintercept=c(2, 2.5, 3), lty=2, color="gray") + geom_errorbar(aes(ymin=IB_rev-IB_rev_se, ymax=IB_rev+IB_rev_se, color=dataagg1$mvgroup), width=1) + geom_point(aes(x= dataagg1$Percent_DR_High, y=dataagg1$IB_rev, shape=mvgroup, fill=mvgroup), alpha=.75, size=8.5) + scale_shape_manual(values = c(21:25), name="Movement Type", labels=c("Equipment", "General", "Genetic", "Harvest", "Person"))  + scale_fill_manual(name="Movement Type", labels=c("Equipment", "General", "Genetic", "Harvest", "Person"), values=colors)  + scale_color_manual(name="Movement Type", labels=c("Equipment", "General", "Genetic", "Harvest", "Person"), values=rep("darkgray", 5)) + scale_y_continuous(breaks = c(1, 2, 3, 4), labels=rev(c("< 48 hours",  "7 days",  "21 days", "> 60 days"))) + geom_text(aes(x=dataagg1$Percent_DR_high_nudge, y=dataagg1$IB_rev, label=dataagg1$MovmtNumberContin), size=4) + annotate("text", x=12.5, y=1.2, label="Majority Low", size=4) + annotate("text", x=50, y=1.2, label="Unclear Consensus", size=4) + annotate("text", x=90, y=1.2, label="Majority High", size=4)  + theme_classic() 



########################################################
## Information for Table
########################################################

dataagg1$Category = ifelse(dataagg1$mvgroup=="equip", "Equipment", ifelse(dataagg1$mvgroup=="general", "General", ifelse(dataagg1$mvgroup=="genetic", "Genetic", ifelse(dataagg1$mvgroup=="harvest", "Harvest", "Person"))))
dataagg1$NumCat = paste(dataagg1$Category, dataagg1$MovmtNumberContin, sep=" ")

#Create one column for bin (high, even, low), one for % majority in that bin

#Mirror scores to match figure - percent majority low or high
dataagg1$Placement = ifelse(dataagg1$Percent_DR_High > 25 & dataagg1$Percent_DR_High < 75, "Unclear Consensus", 
                            ifelse(dataagg1$Percent_DR_High <= 25, "Low", 
                                   ifelse(dataagg1$Percent_DR_High >= 75, "High", NA)))

scores1 = subset(dataagg1, select=c("Percent_DR_High", "NumCat", "Percent_DR_High", "Placement", "IB_rev_cat"))
names(scores1) = c("remove", "Movement", "Percent High RDS", "Binned Risk of Disease Spread", "Time to Negative Business Impact")

#order the factors
scores1$`Binned Risk of Disease Spread` = factor(scores1$`Binned Risk of Disease Spread`, levels=c("Low", "Unclear Consensus", "High"))
scores1$`Time to Negative Business Impact` = factor(scores1$`Time to Negative Business Impact`, levels=c("14 to 21 days", "7 to 14 days", "2 to 7 days"))


scores1[order(scores1$`Binned Risk of Disease Spread`, scores1$`Time to Negative Business Impact`, scores1$`Percent High RDS`, decreasing=T) ,-1]


