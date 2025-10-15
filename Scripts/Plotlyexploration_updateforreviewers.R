#Plotly for Patterson graphs

#Load packages
library(plotly)
library(ggplot2)


#Read in data
data= read.csv(file="Data/Data_for_Plotly.csv", header=T)
movements = read.csv(file="Data/Movements_for_plotly.csv", header=T)


#Ggplot
colors = c("#d7191c","#fdae61", "#ffffbf", "#abdda4", "#2b83ba")

gnotext <- ggplot(data, aes(x=Percent_DR_High, y=IB_rev)) + coord_cartesian(ylim=c(1,4), xlim=c(0,105)) +  labs(title="Expert Consensus on Risk of Disease Spread and Average Time to Impact on Business", x="Percent majority consensus of risk of disease spread", y="Average time until negative impact on business") +  geom_vline(xintercept=c(25,75), color="gray") + geom_hline(yintercept=c(2, 2.5, 3), lty=2, color="gray") + geom_errorbar(aes(ymin=IB_rev-IB_rev_se, ymax=IB_rev+IB_rev_se, color=data$mvgroup), width=1, show.legend = FALSE) + geom_point(aes(x= data$Percent_DR_High, y=data$IB_rev, shape=mvgroup, fill=mvgroup), alpha=.75, size=5) + scale_shape_manual(values = c(21:25), name="Movement Type", labels=c("Equipment", "General", "Genetic", "Harvest", "Person"))  + scale_fill_manual(name="Movement Type", labels=c("Equipment", "General", "Genetic", "Harvest", "Person"), values=colors)  + scale_color_manual(name="Movement Type", labels=c("Equipment", "General", "Genetic", "Harvest", "Person"), values=rep("darkgray", 5)) + scale_y_continuous(breaks = c(1, 2, 3, 4), labels=rev(c("< 48 hours",  "7 days",  "21 days", "> 60 days"))) + annotate("text", x=12.5, y=1.2, label="Majority Low", size=4) + annotate("text", x=50, y=1.2, label="Unclear Consensus", size=4) + annotate("text", x=90, y=1.2, label="Majority High", size=4)  + theme_classic() 


#build plotly
p = plotly_build(gnotext)

#examing the graph structure
str(p)

#find the sections that deal with the markers
grep("marker", p$x$data)

ifelse(p$x$data[[8]]$y[1] > 3.01, "2 to 7 days", ifelse(p$x$data[[8]]$y[1] > 2.51 & p$x$data[[8]]$y[1] < 3.01, "7 to 14 days", ifelse(p$x$data[[8]]$y[1] > 2.01 & p$x$data[[8]]$y[1]< 2.51, "14 to 21 days", ifelse(p$x$data[[8]]$y[1] >= 1 & p$x$data[[8]]$y[1] < 2.01, "21 to 60 days", NA))))

#Edit text labels that appear when hovering over a point
names(p$x$data[[8]])
p$x$data[[8]]$name = "Equipment"
p$x$data[[8]]$text = c(paste("Feed onto production sites","<br>Consensus High Disease Spread: ", round(p$x$data[[8]]$x[1],0), "%", "<br>Business Impact: ",ifelse(p$x$data[[8]]$y[1] > 3.01, "2 to 7 days", ifelse(p$x$data[[8]]$y[1] > 2.51 & p$x$data[[8]]$y[1] < 3.01, "7 to 14 days", ifelse(p$x$data[[8]]$y[1] > 2.01 & p$x$data[[8]]$y[1]< 2.51, "14 to 21 days", ifelse(p$x$data[[8]]$y[1] >= 1 & p$x$data[[8]]$y[1] < 2.01, "21 to 60 days", NA)))), sep=""), paste("Supplies onto production sites","<br>Consensus High Disease Spread: ", round(p$x$data[[8]]$x[2],0), "%","<br>Business Impact: ", ifelse(p$x$data[[8]]$y[2] > 3.01, "2 to 7 days", ifelse(p$x$data[[8]]$y[2] > 2.51 & p$x$data[[8]]$y[2] < 3.01, "7 to 14 days", ifelse(p$x$data[[8]]$y[2] > 2.01 & p$x$data[[8]]$y[2]< 2.51, "14 to 21 days", ifelse(p$x$data[[8]]$y[2] >= 1 & p$x$data[[8]]$y[2] < 2.01, "21 to 60 days", NA)))), sep=""), paste("Shared equipment onto production sites","<br>Consensus High Disease Spread: ", round(p$x$data[[8]]$x[3],0), "%","<br>Business Impact: ", ifelse(p$x$data[[8]]$y[3] > 3.01, "2 to 7 days", ifelse(p$x$data[[8]]$y[3] > 2.51 & p$x$data[[8]]$y[3] < 3.01, "7 to 14 days", ifelse(p$x$data[[8]]$y[3] > 2.01 & p$x$data[[8]]$y[3]< 2.51, "14 to 21 days", ifelse(p$x$data[[8]]$y[3] >= 1 & p$x$data[[8]]$y[3] < 2.01, "21 to 60 days", NA)))), sep=""), paste("Contracted or shared livestock trucks onto production sites","<br>Consensus High Disease Spread: ", round(p$x$data[[8]]$x[4],0), "%","<br>Business Impact: ", ifelse(p$x$data[[8]]$y[4] > 3.01, "2 to 7 days", ifelse(p$x$data[[8]]$y[4] > 2.51 & p$x$data[[8]]$y[4] < 3.01, "7 to 14 days", ifelse(p$x$data[[8]]$y[4] > 2.01 & p$x$data[[8]]$y[4]< 2.51, "14 to 21 days", ifelse(p$x$data[[8]]$y[4] >= 1 & p$x$data[[8]]$y[4] < 2.01, "21 to 60 days", NA)))), sep=""), paste("Dedicated livestock trucks among company production sites","<br>Consensus High Disease Spread: ", round(p$x$data[[8]]$x[5],0), "%","<br>Business Impact: ", ifelse(p$x$data[[8]]$y[5] > 3.01, "2 to 7 days", ifelse(p$x$data[[8]]$y[5] > 2.51 & p$x$data[[8]]$y[5] < 3.01, "7 to 14 days", ifelse(p$x$data[[8]]$y[5] > 2.01 & p$x$data[[8]]$y[5]< 2.51, "14 to 21 days", ifelse(p$x$data[[8]]$y[5] >= 1 & p$x$data[[8]]$y[5] < 2.01, "21 to 60 days", NA)))), sep=""))


names(p$x$data[[9]])
p$x$data[[9]]$name = "General"
p$x$data[[9]]$text = c(paste("Weaned pigs to offsite nursery, <br> wean to finish, or finishing <br> (single source)","<br>Consensus High Disease Spread: ", round(p$x$data[[9]]$x[1],0), "%", "<br>Business Impact: ", ifelse(p$x$data[[9]]$y[1] > 3.01, "2 to 7 days", ifelse(p$x$data[[9]]$y[1] > 2.51 & p$x$data[[9]]$y[1] < 3.01, "7 to 14 days", ifelse(p$x$data[[9]]$y[1] > 2.01 & p$x$data[[9]]$y[1]< 2.51, "14 to 21 days", ifelse(p$x$data[[9]]$y[1] >= 1 & p$x$data[[9]]$y[1] < 2.01, "21 to 60 days", NA)))), sep=""), 
paste("Finishing pigs direct to slaughter","<br>Consensus High Disease Spread: ", round(p$x$data[[9]]$x[2],0), "%", "<br>Business Impact: ",ifelse(p$x$data[[9]]$y[2] > 3.01, "2 to 7 days", ifelse(p$x$data[[9]]$y[2] > 2.51 & p$x$data[[9]]$y[2] < 3.01, "7 to 14 days", ifelse(p$x$data[[9]]$y[2] > 2.01 & p$x$data[[9]]$y[2]< 2.51, "14 to 21 days", ifelse(p$x$data[[9]]$y[2] >= 1 & p$x$data[[9]]$y[2] < 2.01, "21 to 60 days", NA)))), sep=""),
paste("Replacement gilts into a sow unit","<br>Consensus High Disease Spread: ", round(p$x$data[[9]]$x[3],0), "%", "<br>Business Impact: ",ifelse(p$x$data[[9]]$y[3] > 3.01, "2 to 7 days", ifelse(p$x$data[[9]]$y[3] > 2.51 & p$x$data[[9]]$y[3] < 3.01, "7 to 14 days", ifelse(p$x$data[[9]]$y[3] > 2.01 & p$x$data[[9]]$y[3]< 2.51, "14 to 21 days", ifelse(p$x$data[[9]]$y[3] >= 1 & p$x$data[[9]]$y[3] < 2.01, "21 to 60 days", NA)))), sep=""),
paste("Weaned pigs to offsite nursery,<br> wean to finish, or finishing <br> (multiple sources)","<br>Consensus High Disease Spread: ", round(p$x$data[[9]]$x[4],0), "%", "<br>Business Impact: ", ifelse(p$x$data[[9]]$y[4] > 3.01, "2 to 7 days", ifelse(p$x$data[[9]]$y[4] > 2.51 & p$x$data[[9]]$y[4] < 3.01, "7 to 14 days", ifelse(p$x$data[[9]]$y[4] > 2.01 & p$x$data[[9]]$y[4]< 2.51, "14 to 21 days", ifelse(p$x$data[[9]]$y[4] >= 1 & p$x$data[[9]]$y[4] < 2.01, "21 to 60 days", NA)))), sep=""),
paste("Feeder pigs to finishing (e.g. from nursery to finishing)","<br>Consensus High Disease Spread: ", round(p$x$data[[9]]$x[5],0), "%", "<br>Business Impact: ", ifelse(p$x$data[[9]]$y[5] > 3.01, "2 to 7 days", ifelse(p$x$data[[9]]$y[5] > 2.51 & p$x$data[[9]]$y[5] < 3.01, "7 to 14 days", ifelse(p$x$data[[9]]$y[5] > 2.01 & p$x$data[[9]]$y[5]< 2.51, "14 to 21 days", ifelse(p$x$data[[9]]$y[5] >= 1 & p$x$data[[9]]$y[5] < 2.01, "21 to 60 days", NA)))), sep=""),
paste("Cull sows and boars direct to slaughter","<br>Consensus High Disease Spread: ", round(p$x$data[[9]]$x[6],0), "%", "<br>Business Impact: ", ifelse(p$x$data[[9]]$y[6] > 3.01, "2 to 7 days", ifelse(p$x$data[[9]]$y[6] > 2.51 & p$x$data[[9]]$y[6] < 3.01, "7 to 14 days", ifelse(p$x$data[[9]]$y[6] > 2.01 & p$x$data[[9]]$y[6]< 2.51, "14 to 21 days", ifelse(p$x$data[[9]]$y[6] >= 1 & p$x$data[[9]]$y[6] < 2.01, "21 to 60 days", NA)))), sep=""),
paste("Off  size and cull pigs, sows and boars <br> to sale barn/buying station","<br>Consensus High Disease Spread: ", round(p$x$data[[9]]$x[7],0), "%", "<br>Business Impact: ",ifelse(p$x$data[[9]]$y[7] > 3.01, "2 to 7 days", ifelse(p$x$data[[9]]$y[7] > 2.51 & p$x$data[[9]]$y[7] < 3.01, "7 to 14 days", ifelse(p$x$data[[9]]$y[7] > 2.01 & p$x$data[[9]]$y[7]< 2.51, "14 to 21 days", ifelse(p$x$data[[9]]$y[7] >= 1 & p$x$data[[9]]$y[7] < 2.01, "21 to 60 days", NA)))), sep=""),
paste("Off size and cull pigs,<br> sows and boars from sale barn<br> /buying station to slaughter","<br>Consensus High Disease Spread: ", round(p$x$data[[9]]$x[8],0), "%", "<br>Business Impact: ", ifelse(p$x$data[[9]]$y[8] > 3.01, "2 to 7 days", ifelse(p$x$data[[9]]$y[8] > 2.51 & p$x$data[[9]]$y[8] < 3.01, "7 to 14 days", ifelse(p$x$data[[9]]$y[8] > 2.01 & p$x$data[[9]]$y[8]< 2.51, "14 to 21 days", ifelse(p$x$data[[9]]$y[8] >= 1 & p$x$data[[9]]$y[8] < 2.01, "21 to 60 days", NA)))), sep=""),
paste("Feeder pigs from sale barn to production site","<br>Consensus High Disease Spread: ", round(p$x$data[[9]]$x[9],0), "%", "<br>Business Impact: ", ifelse(p$x$data[[9]]$y[9] > 3.01, "2 to 7 days", ifelse(p$x$data[[9]]$y[9] > 2.51 & p$x$data[[9]]$y[9] < 3.01, "7 to 14 days", ifelse(p$x$data[[9]]$y[9] > 2.01 & p$x$data[[9]]$y[9]< 2.51, "14 to 21 days", ifelse(p$x$data[[9]]$y[9] >= 1 & p$x$data[[9]]$y[9] < 2.01, "21 to 60 days", NA)))), sep=""),
paste("Dead stock to offsite disposal (landfill, rendering, etc.)","<br>Consensus High Disease Spread: ", round(p$x$data[[9]]$x[10],0), "%", "<br>Business Impact: ",ifelse(p$x$data[[9]]$y[10] > 3.01, "2 to 7 days", ifelse(p$x$data[[9]]$y[10] > 2.51 & p$x$data[[9]]$y[10] < 3.01, "7 to 14 days", ifelse(p$x$data[[9]]$y[10] > 2.01 & p$x$data[[9]]$y[10]< 2.51, "14 to 21 days", ifelse(p$x$data[[9]]$y[10] >= 1 & p$x$data[[9]]$y[10] < 2.01, "21 to 60 days", NA)))), sep=""),
paste("Manure to field application offsite","<br>Consensus High Disease Spread: ", round(p$x$data[[9]]$x[11],0), "%", "<br>Business Impact: ", ifelse(p$x$data[[9]]$y[11] > 3.01, "2 to 7 days", ifelse(p$x$data[[9]]$y[11] > 2.51 & p$x$data[[9]]$y[11] < 3.01, "7 to 14 days", ifelse(p$x$data[[9]]$y[11] > 2.01 & p$x$data[[9]]$y[11]< 2.51, "14 to 21 days", ifelse(p$x$data[[9]]$y[11] >= 1 & p$x$data[[9]]$y[11] < 2.01, "21 to 60 days", NA)))), sep=""))




p$x$data[[10]]$name = "Genetic"
p$x$data[[10]]$text = c(paste("Replacement gilts and boars <br>into production system isolation","<br>Consensus High Disease Spread: ", round(p$x$data[[10]]$x[1],0), "%", "<br>Business Impact: ", ifelse(p$x$data[[10]]$y[1] > 3.01, "2 to 7 days", ifelse(p$x$data[[10]]$y[1] > 2.51 & p$x$data[[10]]$y[1] < 3.01, "7 to 14 days", ifelse(p$x$data[[10]]$y[1] > 2.01 & p$x$data[[10]]$y[1]< 2.51, "14 to 21 days", ifelse(p$x$data[[10]]$y[1] >= 1 & p$x$data[[10]]$y[1] < 2.01, "21 to 60 days", NA)))), sep=""),
paste("Replacement gilts and boars onto production site","<br>Consensus High Disease Spread: ", round(p$x$data[[10]]$x[2],0), "%", "<br>Business Impact: ",ifelse(p$x$data[[10]]$y[2] > 3.01, "2 to 7 days", ifelse(p$x$data[[10]]$y[2] > 2.51 & p$x$data[[10]]$y[2] < 3.01, "7 to 14 days", ifelse(p$x$data[[10]]$y[2] > 2.01 & p$x$data[[10]]$y[2]< 2.51, "14 to 21 days", ifelse(p$x$data[[10]]$y[2] >= 1 & p$x$data[[10]]$y[2] < 2.01, "21 to 60 days", NA)))), sep=""),
paste("Semen into a production system (breeding herd)","<br>Consensus High Disease Spread: ", round(p$x$data[[10]]$x[3],0), "%", "<br>Business Impact: ", ifelse(p$x$data[[10]]$y[3] > 3.01, "2 to 7 days", ifelse(p$x$data[[10]]$y[3] > 2.51 & p$x$data[[10]]$y[3] < 3.01, "7 to 14 days", ifelse(p$x$data[[10]]$y[3] > 2.01 & p$x$data[[10]]$y[3]< 2.51, "14 to 21 days", ifelse(p$x$data[[10]]$y[3] >= 1 & p$x$data[[10]]$y[3] < 2.01, "21 to 60 days", NA)))), sep=""))

p$x$data[[11]]$name = "Harvest"
p$x$data[[11]]$text = c(paste("Fresh carcasses to offsite processing","<br>Consensus High Disease Spread: ",round(p$x$data[[11]]$x[1],0), "%" , "<br>Business Impact: ", ifelse(p$x$data[[11]]$y[1] > 3.01, "2 to 7 days", ifelse(p$x$data[[11]]$y[1] > 2.51 & p$x$data[[11]]$y[1] < 3.01, "7 to 14 days", ifelse(p$x$data[[11]]$y[1] > 2.01 & p$x$data[[11]]$y[1]< 2.51, "14 to 21 days", ifelse(p$x$data[[11]]$y[1] >= 1 & p$x$data[[11]]$y[1] < 2.01, "21 to 60 days", NA)))), sep=""),
paste("Raw inedibles (byproducts) from harvest<br> site to further processing","<br>Consensus High Disease Spread: ", round(p$x$data[[11]]$x[2],0), "%", "<br>Business Impact: ", ifelse(p$x$data[[11]]$y[2] > 3.01, "2 to 7 days", ifelse(p$x$data[[11]]$y[2] > 2.51 & p$x$data[[11]]$y[2] < 3.01, "7 to 14 days", ifelse(p$x$data[[11]]$y[2] > 2.01 & p$x$data[[11]]$y[2]< 2.51, "14 to 21 days", ifelse(p$x$data[[11]]$y[2] >= 1 & p$x$data[[11]]$y[2] < 2.01, "21 to 60 days", NA)))), sep=""), 
paste("Rendered inedibles from harvest site to further processing","<br>Consensus High Disease Spread: ", round(p$x$data[[11]]$x[3],0), "%", "<br>Business Impact: ", ifelse(p$x$data[[11]]$y[3] > 3.01, "2 to 7 days", ifelse(p$x$data[[11]]$y[3] > 2.51 & p$x$data[[11]]$y[3] < 3.01, "7 to 14 days", ifelse(p$x$data[[11]]$y[3] > 2.01 & p$x$data[[11]]$y[3]< 2.51, "14 to 21 days", ifelse(p$x$data[[11]]$y[3] >= 1 & p$x$data[[11]]$y[3] < 2.01, "21 to 60 days", NA)))), sep=""),
paste("Finished products to distributing", "<br>Consensus High Disease Spread: ", round(p$x$data[[11]]$x[4],0), "%", "<br>Business Impact: ", ifelse(p$x$data[[11]]$y[4] > 3.01, "2 to 7 days", ifelse(p$x$data[[11]]$y[4] > 2.51 & p$x$data[[11]]$y[4] < 3.01, "7 to 14 days", ifelse(p$x$data[[11]]$y[4] > 2.01 & p$x$data[[11]]$y[4]< 2.51, "14 to 21 days", ifelse(p$x$data[[11]]$y[4] >= 1 & p$x$data[[11]]$y[4] < 2.01, "21 to 60 days", NA)))), sep=""), 
paste("Fresh products to point of service", "<br>Consensus High Disease Spread: ", round(p$x$data[[11]]$x[5],0), "%", "<br>Business Impact: ",ifelse(p$x$data[[11]]$y[5] > 3.01, "2 to 7 days", ifelse(p$x$data[[11]]$y[5] > 2.51 & p$x$data[[11]]$y[5] < 3.01, "7 to 14 days", ifelse(p$x$data[[11]]$y[5] > 2.01 & p$x$data[[11]]$y[5]< 2.51, "14 to 21 days", ifelse(p$x$data[[11]]$y[5] >= 1 & p$x$data[[11]]$y[5] < 2.01, "21 to 60 days", NA)))), sep=""),
paste("Ready to eat products to point of service", "<br>Consensus High Disease Spread: ", round(p$x$data[[11]]$x[6],0), "%", "<br>Business Impact: ", ifelse(p$x$data[[11]]$y[6] > 3.01, "2 to 7 days", ifelse(p$x$data[[11]]$y[6] > 2.51 & p$x$data[[11]]$y[6] < 3.01, "7 to 14 days", ifelse(p$x$data[[11]]$y[6] > 2.01 & p$x$data[[11]]$y[6]< 2.51, "14 to 21 days", ifelse(p$x$data[[11]]$y[6] >= 1 & p$x$data[[11]]$y[6] < 2.01, "21 to 60 days", NA)))), sep=""))

p$x$data[[12]]$name = "Person"
p$x$data[[12]]$text = c(paste("Employees onto, off and/or between production site(s)","<br>Consensus High Disease Spread: ", round(p$x$data[[12]]$x[1],0), "%", "<br>Business Impact: ", ifelse(p$x$data[[12]]$y[1] > 3.01, "2 to 7 days", ifelse(p$x$data[[12]]$y[1] > 2.51 & p$x$data[[12]]$y[1] < 3.01, "7 to 14 days", ifelse(p$x$data[[12]]$y[1] > 2.01 & p$x$data[[12]]$y[1]< 2.51, "14 to 21 days", ifelse(p$x$data[[12]]$y[1] >= 1 & p$x$data[[12]]$y[1] < 2.01, "21 to 60 days", NA)))), sep=""),
paste("Routine service providers <br>(e.g. plumbers, electricians, etc.)<br> onto, off and/or between sites","<br>Consensus High Disease Spread: ", round(p$x$data[[12]]$x[2],0), "%", "<br>Business Impact: ",ifelse(p$x$data[[12]]$y[2] > 3.01, "2 to 7 days", ifelse(p$x$data[[12]]$y[2] > 2.51 & p$x$data[[12]]$y[2] < 3.01, "7 to 14 days", ifelse(p$x$data[[12]]$y[2] > 2.01 & p$x$data[[12]]$y[2]< 2.51, "14 to 21 days", ifelse(p$x$data[[12]]$y[2] >= 1 & p$x$data[[12]]$y[2] < 2.01, "21 to 60 days", NA)))), sep=""), 
paste("Veterinarians onto, off and/or between sites","<br>Consensus High Disease Spread: ", round(p$x$data[[12]]$x[3],0), "%", "<br>Business Impact: ", ifelse(p$x$data[[12]]$y[3] > 3.01, "2 to 7 days", ifelse(p$x$data[[12]]$y[3] > 2.51 & p$x$data[[12]]$y[3] < 3.01, "7 to 14 days", ifelse(p$x$data[[12]]$y[3] > 2.01 & p$x$data[[12]]$y[3]< 2.51, "14 to 21 days", ifelse(p$x$data[[12]]$y[3] >= 1 & p$x$data[[12]]$y[3] < 2.01, "21 to 60 days", NA)))), sep=""),
paste("Vaccination crews into, off and/or between sites", "<br>Consensus High Disease Spread: ", round(p$x$data[[12]]$x[4],0), "%", "<br>Business Impact: ", ifelse(p$x$data[[12]]$y[4] > 3.01, "2 to 7 days", ifelse(p$x$data[[12]]$y[4] > 2.51 & p$x$data[[12]]$y[4] < 3.01, "7 to 14 days", ifelse(p$x$data[[12]]$y[4] > 2.01 & p$x$data[[12]]$y[4]< 2.51, "14 to 21 days", ifelse(p$x$data[[12]]$y[4] >= 1 & p$x$data[[12]]$y[4] < 2.01, "21 to 60 days", NA)))), sep=""), 
 paste("Commercial crews onto, off and/or<br> between sites (e.g. manure haulers,<br> feed trucks, livestock haulers)", "<br>Consensus High Disease Spread: ", round(p$x$data[[12]]$x[5],0), "%", "<br>Business Impact: ", ifelse(p$x$data[[12]]$y[5] > 3.01, "2 to 7 days", ifelse(p$x$data[[12]]$y[5] > 2.51 & p$x$data[[12]]$y[5] < 3.01, "7 to 14 days", ifelse(p$x$data[[12]]$y[5] > 2.01 & p$x$data[[12]]$y[5]< 2.51, "14 to 21 days", ifelse(p$x$data[[12]]$y[5] >= 1 & p$x$data[[12]]$y[5] < 2.01, "21 to 60 days", NA)))), sep=""))


#Edit labels and legend groups for error bar legends
p$x$data[[3]]$name = "Equipment"
p$x$data[[4]]$name = "General"
p$x$data[[5]]$name = "Genetic"
p$x$data[[6]]$name = "Harvest"
p$x$data[[7]]$name = "Person"

p$x$data[[3]]$showlegend = FALSE
p$x$data[[4]]$showlegend = FALSE
p$x$data[[5]]$showlegend = FALSE
p$x$data[[6]]$showlegend = FALSE
p$x$data[[7]]$showlegend = FALSE


#Edit what is shown on hover
p$x$layout$hovermode = "closest"
p$x$data[[8]]$hoverinfo = "text"
p$x$data[[9]]$hoverinfo = "text"
p$x$data[[10]]$hoverinfo = "text"
p$x$data[[11]]$hoverinfo = "text"
p$x$data[[12]]$hoverinfo = "text"
p$x$data[[3]]$hoverinfo = "none"
p$x$data[[4]]$hoverinfo = "none"
p$x$data[[5]]$hoverinfo = "none"
p$x$data[[6]]$hoverinfo = "none"
p$x$data[[7]]$hoverinfo = "none"

p

#Save output as .html
htmlwidgets::saveWidget(as_widget(p), "Pattersonetal_FMD_FigureInteractive.html")


