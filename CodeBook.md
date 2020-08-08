# Getting and Cleaning Data : Course Project - Code Book

The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. 

The goal is to prepare tidy data that can be used for later analysis.

#Data

The data for this project can be downloaded through the following link: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

Full description of the data can be found here: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

# Processing

Step 1: Merge training and test sets to create one data set

Step 2: Extract only measurements on mean and standard deviation

Step 3: Use descriptive activities names for activity measurements

Step 4: Appropriately Label the Dataset with Descriptive Variable Names

Step 5: Create tidy data set with average of each variable, by activity, by subject


# Variables

features = feature names for X_test, X_train

activities = six activity codes and corresponding activity names

subject_train = train set subject number, 1-30

X_train = train set feature measurements

y_train = train set activity, coded 1-6

subject_test = test set subject number, 1-30

X_test = test set feature measurements

y_test = test set activity, coded 1-6

train = subject_train, y_train, X_train combined, by columns

test = subject_test, y_test, X_test combined, by columns

allData = test, train, combined by rows

allNames = set of names corresponding to allData, not yet applied

meanStdColumns = subject, activity, and all features with mean or standard deviation

reducedSet = allData subsetted with meanStdColumns

reducedNames = allNames subsetted with meanStdColumns, renamed to R conventions; applied to reducedSet

tidydataset = data set with average of each variable, by activity, by subject
