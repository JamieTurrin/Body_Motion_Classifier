# James Turrin
# March 2016

# This will classify a person's activity (laying, sitting, standing, walking,
# walking downstairs, walking upstairs) based on body-motion data obtained
# from a Samsung Galaxy smartphone's accelerometer and gyroscope.

# Data come from Univ. of California Irvine Machine Learning Lab
# 79 variables of body motion, measured for 30 subjects and 6 activities,
# yields 180 observations each of 79 variables.
# Activities are: laying, sitting, standing, walking, walkupstairs, walkdownstairs

# Classification is accomplished by correlating observations with a reference spectra
# Reference spectra are derived by computing an average of each variable for each
# activity type.

################################################################################

# UCI HAR Dataset folder contains:

# activity_labels.txt: with the activites labeled 1 thru 6

# features.txt: lists the 561 features calculated using the data, but does not
#               contain data, just the feature names

# features_info.txt: contains some info regarding the features,including the set
#               of variables (mean, stdev, etc.) that were computed for each.

# README.txt: more complete info regarding dataset, including brief descriptions
#               of the files in the 'test' and 'train' folders.

# 'Test/Train' folders: Each contains subject_test, X_test, y_test files.

# subject_test/train.txt: contains the ID of the human subject, an integer from
#                   1-30 in a single row of data.

# X_test/train.txt: Processed data to be analyzed by this program

# y_test/train.txt: An integer from 1-6 that indicates the activity. This is a
#                   single row of data.

# Inertial Signals Folder: contains raw data produced by accelerometer and gyro,
#                           not for use by this program.

###############################################################################
# CREATE WORKING DIRECTORY THEN DOWNLOAD AND UNZIP THE DATA

setwd("C:/users/james/desktop/")

# create directory for project to hold data and output
if(!file.exists("./Body Motion Data")){dir.create("./Body Motion Data")}

setwd("./Body Motion Data")

# download zip file of data if necessary
if(!file.exists("project_data.zip")){
    
    # location of data
    url<-"http://archive.ics.uci.edu/ml/machine-learning-databases/00240/UCI HAR Dataset.zip"
   
    download.file(url,destfile="project_data.zip")  # download it...

    unzip("project_data.zip") # unzip the data file
    }  

################################################################################
# READ DATA FROM 'TEST' FOLDER AND COMBINE INTO SINGLE DATAFRAME

# read 'features' into dataframe
features<-read.table("UCI HAR Dataset/features.txt",col.names=c("number","feature"),
                     colClasses=c("integer","character"))

# read test subject_IDs into dataframe
test_ids<-read.table("UCI HAR Dataset/test/subject_test.txt",col.names="ID") 

# read activity number into dataframe
test_act_num<-read.table("UCI HAR Dataset/test/y_test.txt",col.names="activity_number")

# create new variable in test_act_num dataframe to hold activity description
for(i in 1:nrow(test_act_num)){
    if(test_act_num$activity_number[i]==1) test_act_num$activity[i]<-"WALKING"
    if(test_act_num$activity_number[i]==2) test_act_num$activity[i]<-"WALKING_UPSTAIRS"
    if(test_act_num$activity_number[i]==3) test_act_num$activity[i]<-"WALKING_DOWNSTAIRS"
    if(test_act_num$activity_number[i]==4) test_act_num$activity[i]<-"SITTING"
    if(test_act_num$activity_number[i]==5) test_act_num$activity[i]<-"STANDING"
    if(test_act_num$activity_number[i]==6) test_act_num$activity[i]<-"LAYING"
}

library(data.table)  # load data.table package to use fread() function.
# fast-read processed data into dataframe 
# column names come from 'features' dataframe
X_test<-fread("UCI HAR Dataset/test/X_test.txt",col.names=features$feature)  

# merge subject_ID, activity_number, and test data into a single dataframe
test_data<-cbind(test_ids,test_act_num,X_test)

# delete extraneous dataframe objects to free memory:
rm(test_ids); rm(test_act_num); rm(X_test)


################################################################################
# READ DATA FROM 'TRAIN' FOLDER AND COMBINE INTO SINGLE DATAFRAME

# read training subject_IDs into dataframe
train_ids<-read.table("UCI HAR Dataset/train/subject_train.txt",col.names="ID")

# read activity number into dataframe
train_act_num<-read.table("UCI HAR Dataset/train/y_train.txt",col.names="activity_number")

# create new variable in train_act_num dataframe to hold activity description
for(i in 1:nrow(train_act_num)){
    if(train_act_num$activity_number[i]==1) train_act_num$activity[i]<-"WALKING"
    if(train_act_num$activity_number[i]==2) train_act_num$activity[i]<-"WALKING_UPSTAIRS"
    if(train_act_num$activity_number[i]==3) train_act_num$activity[i]<-"WALKING_DOWNSTAIRS"
    if(train_act_num$activity_number[i]==4) train_act_num$activity[i]<-"SITTING"
    if(train_act_num$activity_number[i]==5) train_act_num$activity[i]<-"STANDING"
    if(train_act_num$activity_number[i]==6) train_act_num$activity[i]<-"LAYING"
}

# fast-read processed data into dataframe 
# column names come from 'features' dataframe
print('')
print('READING DATA...BE PATIENT')
print('')
X_train<-fread("UCI HAR Dataset/train/X_train.txt",col.names=features$feature)  

# merge subject_ID, activity_number, and train data into a single dataframe
train_data<-cbind(train_ids,train_act_num,X_train)

# delete extraneous dataframe objects to free memory:
rm(train_ids); rm(train_act_num); rm(X_train)
###############################################################################
# MERGE AND ORDER THE DATA

# merge test dataframe and training dataframes into a single dataframe
data<-rbind(test_data,train_data)

# order the data by ID and activity number
data<-data[order(data$ID,data$activity_number),]

###############################################################################
# commands to limit data set to variable means and st. devs.

names_list<-names(data) # get list of all variable names

# get indices of variables that are 'means'
mean_indices<-grep("mean()",names_list)

# get indices of variables that are 'std'
std_indices<-grep("std()",names_list)

# combine indices of mean and std variable names into 1 vector 
# include columns 1,2,3 as well (ID,activity_number,activity)
mean_std_indices<-c(1,2,3,mean_indices,std_indices)

# order the vector of indices
mean_std_indices<-mean_std_indices[order(mean_std_indices)]

#subset data so only variables that are mean or std are present.
data<-data[,mean_std_indices] 

##############################################################################
# split data and compute averages of variables by ID and activity
# and transpose the dataframe, swapping rows and columns
# so data are oriented correctly in output.

# split data by Id and activity
s<-split(data,list(data$ID,data$activity))

# calc mean for each combination of Id and activity
means<-lapply(s, function(x) colMeans(x[4:82]))

# convert list to dataframe
means<-as.data.frame(means)

col_names<-names(means) #get column names
row_names<-row.names(means) # get row names

# transpose rows and columns, so variables are columns and ID/activity are rows
means<-transpose(means) 

# use as.data.frame() to name the rows with the old column names
means<-as.data.frame(means,row.names=col_names)

# column names become the old row names
names(means)<-row_names

###############################################################################
# commands to format data so it looks nice when written to table

col_names<-names(means) # get new column names

widths<-numeric(80) # create vector to hold column widths to format output
library(stringr)  # load package to use str_pad() function

for(i in 1:79){ # set width according to width of column name or data values
    if(nchar(col_names[i])>nchar(means[1,i])) 
    {widths[i+1]<-nchar(col_names[i])}
    else if(nchar(col_names[i])<=nchar(means[1,i])) 
    {widths[i+1]<-nchar(means[1,i])
    # pad column name with whitespace if necessary to match width of data
    col_names[i]<-str_pad(col_names[i],widths[i+1],side="left")}}

widths[1]<-22 # set first width to match width of longest variable name

# pad first variable name with extra whitespace for proper placement
# to the right of the row names
col_names[1]<-str_pad(col_names[1],40,side="left")

names(means)<-col_names # put new names back into dataframe

if(!file.exists("Body_Motion_Means.txt")){
    # write to fixed-width table so we can see data to be classified in a nice format
    print('')
    print('WRITING TIDY DATA TO TEXT FILE FOR VIEWING')
    print('')
    library(gdata) # load gdata package to use write.fwf() function
    write.fwf(means,file="Body_Motion_Means.txt",
          width=widths,rownames=TRUE)
}

############# LOAD THE TIDY DATA ###########################################

data<-read.table("Body_Motion_Means.txt",stringsAsFactors = FALSE)

########## CREATE REFERENCE SPECTRA FOR EACH ACTIVITY ##########################

# get 1st 15 rows of data, these will be used to construct spectrum for LAYING 
lay<-data[1:15,1:79]
# calc mean of each variable to produce spectrum for LAYING
lay_spect<-colMeans(lay)

# get rows 31-45, these will be used to construct spectrum for SITTING
sit<-data[31:45,1:79]
sit_spect<-colMeans(sit)

# get rows 61-75, these will be used to construct spectrum for STANDING
stand<-data[61:75,1:79]
stand_spect<-colMeans(stand)

# get rows 91-105, these will be used to construct spectrum for WALKING
walk<-data[91:105,1:79]
walk_spect<-colMeans(walk)

# get rows 121-135, these will be used to construct spectrum for WALKING DOWNSTAIRS
down<-data[121:135,1:79]
down_spect<-colMeans(down)

# get rows 151-165, these will be used to construct spectrum for WALKING UPSTAIRS
up<-data[151:165,1:79]
up_spect<-colMeans(up)

lay_spectrum<-numeric(); sit_spectrum<-numeric(); stand_spectrum<-numeric()
walk_spectrum<-numeric(); down_spectrum<-numeric(); up_spectrum<-numeric()

for(i in 1:79){ #place values into vectors
    lay_spectrum[i]<-lay_spect[i]
    sit_spectrum[i]<-sit_spect[i]
    stand_spectrum[i]<-stand_spect[i]
    walk_spectrum[i]<-walk_spect[i]
    down_spectrum[i]<-down_spect[i]
    up_spectrum[i]<-up_spect[i]
}

# PLOT REFERENCE SPECTRA 
plot(lay_spectrum,main="LAYING Spectrum",type="l")
plot(sit_spectrum,main="SITTING Spectrum",type="l")
plot(stand_spectrum,main="STANDING Spectrum",type="l")
plot(walk_spectrum,main="WALKING Spectrum",type="l")
plot(down_spectrum,main="WALKING DOWNSTAIRS Spectrum",type="l")
plot(up_spectrum,main="WALKING UPSTAIRS Spectrum",type="l")

plot(lay_spectrum,main="Reference Spectra",type="l",ylab="")
lines(sit_spectrum,col="red")
lines(stand_spectrum,col="blue")
lines(walk_spectrum,col="green")
lines(down_spectrum,col="purple")
lines(up_spectrum,col="orange")
legend_text<-c("LAYING","SITTING",'STANDING','WALKING','DOWNSTAIRS','UPSTAIRS')
legend("top",legend_text,col=c("black","red","blue","green","purple","orange"),
       lty=1,cex=0.5)


# create function to cache reference spectra
cache_spectra<-function(lay_spectrum,sit_spectrum,stand_spectrum,
                        walk_spectrum,down_spectrum,up_spectrum){
    laysp<<-lay_spectrum # cache spectra
    sitsp<<-sit_spectrum
    stndsp<<-stand_spectrum
    wlksp<<-walk_spectrum
    dwnsp<<-down_spectrum
    upsp<<-up_spectrum
}
# call function to cache reference spectra
cache_spectra(lay_spectrum,sit_spectrum,stand_spectrum,
              walk_spectrum,down_spectrum,up_spectrum)


############ CREATE SPECTRA FOR TEST DATA ######################################

test_lay<-data[16:30,1:79] # rows 16-30 for LAYING test data
test_sit<-data[46:60,1:79] # rows 46-60 for SITTING test data
test_stand<-data[76:90,1:79] # rows 76-90 for STANDING test data
test_walk<-data[106:120,1:79] # rows 106-120 for WALKING test data
test_down<-data[136:150,1:79] # rows 136-150 for WALKING DOWNSTAIRS test data
test_up<-data[166:180,1:79] # rows 166-180 for WALKING UPSTAIRS test data

############## FUNCTION TO CORRELATE SPECTRA ###################################

classify_activity<-function(test_spectrum){
    coeffs<-numeric()
    # correlate the test spectrum w/cached reference spectrum
    coeffs[1]<-cor(test_spectrum,laysp)
    coeffs[2]<-cor(test_spectrum,sitsp)
    coeffs[3]<-cor(test_spectrum,stndsp)
    coeffs[4]<-cor(test_spectrum,wlksp)
    coeffs[5]<-cor(test_spectrum,dwnsp)
    coeffs[6]<-cor(test_spectrum,upsp)
    coeffs # return correlation coefficients
}

################################################################################

# vectors to use as columns in output dataframe
actual=character(length=90) 
prediction=character(length=90)
laycoef=numeric(length=90); sitcoef=numeric(length=90); standcoef=numeric(length=90)
walkcoef=numeric(length=90); wlkdncoef=numeric(length=90); wlkupcoef=numeric(length=90)

# create empty dataframe to hold results
output<-data.frame(actual,prediction,laycoef,sitcoef,standcoef,walkcoef,
                   wlkdncoef,wlkupcoef,stringsAsFactors = FALSE) 
flag=1

for (j in 1:6){
    for (k in 1:15){# extract test spectra, change to numeric, place actual activity in output dataframe
        if (j==1) {spect<-test_lay[k,1:79];class(spect)<-"numeric";output$actual[flag]="LAYING"}
        if (j==2) {spect<-test_sit[k,1:79];class(spect)<-"numeric";output$actual[flag]="SITTING"}
        if (j==3) {spect<-test_stand[k,1:79];class(spect)<-"numeric";output$actual[flag]="STANDING"}
        if (j==4) {spect<-test_walk[k,1:79];class(spect)<-"numeric";output$actual[flag]="WALKING"}
        if (j==5) {spect<-test_down[k,1:79];class(spect)<-"numeric";output$actual[flag]="WALKING DOWNSTAIRS"}
        if (j==6) {spect<-test_up[k,1:79];class(spect)<-"numeric";output$actual[flag]="WALKING UPSTAIRS"}
        
        # send test spectrum to classify_activity() function for classification
        result<-classify_activity(spect)
        
        # put predicted activity in output dataframe
        if(which.max(result)==1) output$prediction[flag]="LAYING"
        if(which.max(result)==2) output$prediction[flag]="SITTING"
        if(which.max(result)==3) output$prediction[flag]="STANDING"
        if(which.max(result)==4) output$prediction[flag]="WALKING"
        if(which.max(result)==5) output$prediction[flag]="WALKING DOWNSTAIRS"
        if(which.max(result)==6) output$prediction[flag]="WALKING UPSTAIRS"
        
        output$laycoef[flag]=result[1] # put corr. coeffs. in output dataframe
        output$sitcoef[flag]=result[2]
        output$standcoef[flag]=result[3]
        output$walkcoef[flag]=result[4]
        output$wlkdncoef[flag]=result[5]
        output$wlkupcoef[flag]=result[6]
        
        flag=flag+1 # iterate flag
    }}

################################################################################
# RESULTS AND ACCURACY

# print(output[,1:2]) #print results to see how well the program performed.

table1<-table(output[,1],output[,2]) # table of results
names(dimnames(table1))<-list('  ACTUAL CLASSES',' PREDICTED CLASSES')
print(table1)

# LAYING: all correctly classified!
# SITTING: all correctly classified!
# STANDING: all correctly classified!
# WALKING: 2 misclassified
# WALKING DOWNSTAIRS: 3 misclassified
# WALKING UPSTAIRS: all correctly classfied!
# total of 5 misclassifications out of 90 observations, 94.44% success rate.

setwd("../")
