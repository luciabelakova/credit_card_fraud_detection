# =============================================================================
#
#
# This code concerns a case study about the effect of lockdown on pollution 
# India, measured in the form of Air Quality Index, or AQI
getRversion() # [1] ‘4.0.3’

#  
#
# =============================================================================
# NOTES
# • 
# • 
# • 
#
#
# =============================================================================
# --- global variables ---

# 
wk.dir <- getwd() 
# [1] "C:/Users/lucib/OneDrive/Documents/R/Research Project/belakova.lucia.research_project"

# =============================================================================
# ---- libraries ----

# 
install.packages("data.table")
install.packages("ranger")
install.packages("caret")
install.packages("scales")
install.packages("caTools")
install.packages("randomForest")
install.packages("e1071", dependencies = TRUE)
install.packages("pROC")
install.packages("ROSE")

#  
library(data.table)
library(ranger)
library(caret)
library(scales)
library(caTools)
library(randomForest)
library(e1071)
library(pROC)
library(ROSE)


# =============================================================================
# --- folder management ---

# 
# let's make some folders in the working directory that I've set up
folder.names <- c("1.data", "2.visualizations")

# and add a loop for same name control
for(i in 1:length(folder.names)){ 
  if(file.exists(folder.names[i]) == FALSE){
    dir.create(folder.names[i])
  } 
}


# now we can make a path for the results of our code in the future
path.data <- paste(wk.dir, "/", folder.names[1], "/", sep = "")
path.visualizations <- paste(wk.dir, "/", folder.names[2], "/", sep = "")


# ==== end =================================================================

