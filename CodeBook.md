In the program run_analysis.R I used the data collected from the accelerometers from the Samsung Galaxy S smartphone. Full description of the data can be found at the site http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones .

Briefly, the experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, there were captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The following files are available for the train and test data. Their descriptions are equivalent. 
X_train.txt - values of all measurements
y_train.txt - training activity labels (values from 1 to 6)
subject_train.txt - Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

In the R script the data above was read and merged together. Only the measurements on the mean and standard deviation for each measurement were selected.
The additional files activity_labels.txt and features.txt were used for labeling the data set with descriptive activity names and selecting the measurements.