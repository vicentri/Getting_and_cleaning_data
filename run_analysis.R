# run_analysis
# This R code reads and load dataset into datatable
#

setwd("C:/Users/Eli/Documents/datasciencecoursera/gacd/course_project")
library(httr) 

#download required data

site <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
lfile <- "data.zip"
	## download.file(site, lfile, method="curl") SSL failed lot of times using http
download.file(site, lfile)

#unzip data, create subdirectories to work in

source_dir <- "UCI HAR Dataset"
results_dir <- "results"
if(!file.exists(source_dir)){
	unzip(lfile, list = FALSE, overwrite = TRUE)
} 
if(!file.exists(results_dir)){
	dir.create(results_dir)
} 

#read and create data.frame using specific function

getdatatables <- function (filename,cols = NULL){

	wfile <- paste(source_dir,filename,sep="/")
	mydata <- data.frame()
	if(is.null(cols)){
		mydata <- read.table(wfile,sep="",stringsAsFactors=F)
	} else {
		mydata <- read.table(wfile,sep="",stringsAsFactors=F, col.names= cols)
	}
	mydata ## Check data
}

# execute getdatatables
features <- getdatatables("features.txt")

# specific function to create database, require variables (axis)

buildmydb <- function(wvar, features){

	data_type <- getdatatables(paste(wvar,"/","subject_",wvar,".txt",sep=""),"id")
	y_data <- getdatatables(paste(wvar,"/","y_",wvar,".txt",sep=""),"activity")
	x_data <- getdatatables(paste(wvar,"/","X_",wvar,".txt",sep=""),features$V2)
	return (cbind(data_type,y_data,x_data))
}

#execute buildmydb for testing

test <- buildmydb("test", features)
train <- buildmydb("train", features)

# Function to save results using the directory already created

saving_res <- function (wdata, fname){
	wfile <- paste(results_dir, "/", fname,".csv" ,sep="")
	write.csv(wdata, fname)
}

# Assignment tasks

#1) Merges the training and the test sets to create one data set.
library(plyr)
data <- rbind(train, test)
data <- arrange(data, id)

#2) Extracts only the measurements on the mean and standard deviation for each measurement. 
mean_and_std <- data[,c(1,2,grep("std", colnames(data)), grep("mean", colnames(data)))]
saving_res(mean_and_std,"mean_and_std")

#3) Convert activity names to name the activities in the data set
activity_labels <- getdatatables("activity_labels.txt")

#4) Label the data set with descriptive variable names. 
data$activity <- factor(data$activity, levels=activity_labels$V1, labels=activity_labels$V2)

#5) Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
tidy_dataset <- ddply(mean_and_std, .(id, activity), .fun=function(x){ colMeans(x[,-c(1:2)]) })
colnames(tidy_dataset)[-c(1:2)] <- paste(colnames(tidy_dataset)[-c(1:2)], "_mean", sep="")
saving_res(tidy_dataset,"tidy_dataset")
