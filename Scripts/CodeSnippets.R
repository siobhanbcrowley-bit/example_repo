###########################
# Random code snippets 
############################

# Load Packages
install.packages("pacman")
pacman::p_load(ggplot2, nlme, mass, dplyr)
pacman::p_load_gh("dill/beyonce")


#Load Packages
library(ggplot2)
library(nlme)
library(mass)
library(dplyr)
library(devtools)
devtools::install_github("dill/beyonce")
library(beyonce)



#Base R
data <- aggregate(subset(as.data.frame(Titanic), as.data.frame(Titanic)$Sex=="Female")$Freq, list( subset(as.data.frame(Titanic), as.data.frame(Titanic)$Sex=="Female")$Class, subset(as.data.frame(Titanic), as.data.frame(Titanic)$Sex=="Female")$Survived), sum)

reshape(data, timevar="Group.1", idvar="Group.2", direction="wide")

pacman::p_load(dplyr, tidyr)

#dplyr
data <- as.data.frame(Titanic) %>% 
          filter(Sex=="Female") %>% 
          group_by(Class, Survived) %>% 
          summarize(count = sum(Freq))

#tidyr
data %>% 
  spread(key=Class, value=count)



