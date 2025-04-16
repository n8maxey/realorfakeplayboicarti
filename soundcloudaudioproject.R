```{r}
# Clear the environment
rm(list = ls())

# Set the working directory to the folder containing your audio files
setwd("C:/Users/natem/OneDrive/Desktop/sound audio project/")

# Load required libraries
library(tuneR)       # For audio file input/output
library(seewave)     # For signal processing and spectral analysis
library(caret)       # For machine learning
library(randomForest) # For Random Forest
library(e1071)       # For SVM
library(class)       # For KNN
library(rpart)       # For Decision Trees

# Set the file paths for the fake and real audio folders
fake_audio_folder <- "C:/Users/natem/OneDrive/Desktop/sound audio project/fakeplayboi"
real_audio_folder <- "C:/Users/natem/OneDrive/Desktop/sound audio project/realplayboi"

# Get the list of MP3 audio files from both folders
fake_files <- list.files(fake_audio_folder, pattern = "\\.mp3$", full.names = TRUE)
real_files <- list.files(real_audio_folder, pattern = "\\.mp3$", full.names = TRUE)

# Initialize an empty list to store features and labels
features_list <- list()

# Function to extract features from each MP3 file
extract_features <- function(audio_file, label) {
  # Check if the file exists
  if (!file.exists(audio_file)) {
    message("File not found: ", audio_file)
    return(NULL)
  }
  
  # Try reading the MP3 audio file
  audio <- tryCatch({
    message("Reading file: ", audio_file)  # Debugging line
    readMP3(audio_file)
  }, error = function(e) {
    message("Error reading file: ", audio_file, " Error: ", e$message)
    return(NULL)
  })
  
  if (is.null(audio)) return(NULL)  # Skip if audio file can't be read
  
  # Convert to mono if audio has more than one channel
  if (audio@stereo) {
    audio <- mono(audio, which = "both")  # Converts to mono by averaging channels
  }
  
  # Extract MFCC features
  mfcc_features <- tryCatch({
    message("Extracting MFCC from: ", audio_file)  # Debugging line
    melfcc(audio)
  }, error = function(e) {
    message("Error extracting MFCC from: ", audio_file, " Error: ", e$message)
    return(NULL)
  })
  
  if (is.null(mfcc_features)) return(NULL)  # Skip if MFCC extraction fails
  
  # Convert MFCC features to a data frame and add the label
  feature_df <- data.frame(mfcc_features)
  feature_df$label <- label
  
  return(feature_df)
}

# Process each fake audio file individually (label = 1 for fake)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/fakeplayboi/1.mp3", 1)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/fakeplayboi/10.mp3", 1)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/fakeplayboi/2.mp3", 1)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/fakeplayboi/3.mp3", 1)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/fakeplayboi/4.mp3", 1)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/fakeplayboi/5.mp3", 1)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/fakeplayboi/6.mp3", 1)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/fakeplayboi/7.mp3", 1)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/fakeplayboi/8.mp3", 1)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/fakeplayboi/9.mp3", 1)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/fakeplayboi/Cobra.mp3", 1)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/fakeplayboi/Do The Most.mp3", 1)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/fakeplayboi/Outro - tay royce (Playboi Carti feat. Travis Scott).mp3", 1)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/fakeplayboi/Playboi Carti- System.mp3", 1)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/fakeplayboi/Playboi Carti - GOAT.mp3", 1)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/fakeplayboi/Playboi Carti - IYKWIK (slowed & reverb).mp3", 1)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/fakeplayboi/Playboi_Carti_BOSSMAN.mp3", 1)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/fakeplayboi/R0T.mp3", 1)

# Process each real audio file individually (label = 0 for real)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/realplayboi/[Playboi Carti] Playboi Carti - Beno! (Official Audio).mp3", 0)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/realplayboi/[Playboi Carti] Playboi Carti - Control (Official Audio).mp3", 0)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/realplayboi/[Playboi Carti] Playboi Carti - Die4Guy (Official Audio).mp3", 0)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/realplayboi/[Playboi Carti] Playboi Carti - F33l Lik3 Dyin (Official Audio).mp3", 0)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/realplayboi/[Playboi Carti] Playboi Carti - ILoveUIHateU (Official Audio).mp3", 0)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/realplayboi/[Playboi Carti] Playboi Carti - Jump Out The House (Official Audio).mp3", 0)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/realplayboi/[Playboi Carti] Playboi Carti - King Vamp (Official Audio).mp3", 0)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/realplayboi/[Playboi Carti] Playboi Carti - Meh (Official Audio).mp3", 0)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/realplayboi/[Playboi Carti] Playboi Carti - New N3on (Official Audio).mp3", 0)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/realplayboi/[Playboi Carti] Playboi Carti - New Tank (Official Audio).mp3", 0)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/realplayboi/[Playboi Carti] Playboi Carti - NoSl33p (Official Audio).mp3", 0)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/realplayboi/[Playboi Carti] Playboi Carti - Not Playing (Official Audio).mp3", 0)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/realplayboi/[Playboi Carti] Playboi Carti - On That Time (Official Audio).mp3", 0)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/realplayboi/[Playboi Carti] Playboi Carti - Over (Official Audio).mp3", 0)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/realplayboi/[Playboi Carti] Playboi Carti - Place (Official Audio).mp3", 0)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/realplayboi/[Playboi Carti] Playboi Carti - Punk Monk (Official Audio).mp3", 0)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/realplayboi/[Playboi Carti] Playboi Carti - Rockstar Made (Official Audio).mp3", 0)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/realplayboi/[Playboi Carti] Playboi Carti - Sky (Official Audio).mp3", 0)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/realplayboi/[Playboi Carti] Playboi Carti - Slay3r (Official Audio).mp3", 0)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/realplayboi/[Playboi Carti] Playboi Carti - Stop Breathing (Official Audio).mp3", 0)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/realplayboi/[Playboi Carti] Playboi Carti - Vamp Anthem (Official Audio).mp3", 0)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/realplayboi/[Playboi Carti] Playboi Carti ft. Future - TeenX (Official Audio).mp3", 0)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/realplayboi/[Playboi Carti] Playboi Carti ft. Kanye West - Go2DaMoon (Official Audio).mp3", 0)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/realplayboi/[Playboi Carti] Playboi Carti ft. Kid Cudi - M3tamorphosis (Official Video).mp3", 0)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/realplayboi/20.mp3", 0)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/realplayboi/21.mp3", 0)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/realplayboi/22.mp3", 0)
features_list[[length(features_list) + 1]] <- extract_features("C:/Users/natem/OneDrive/Desktop/sound audio project/realplayboi/23.mp3", 0)

# Remove NULL values from the list (in case there were files that failed to process)
features_list <- features_list[!sapply(features_list, is.null)]

# Combine all features into one data frame
features_df <- do.call(rbind, features_list)

# Check if features_df is empty
if (nrow(features_df) == 0) {
  stop("No features were extracted. Please check your files.")
}

# Remove any rows with missing values
features_df <- na.omit(features_df)

# Ensure label column has at least two unique values
if (length(unique(features_df$label)) < 2) {
  stop("Insufficient unique values in the label column for binary classification.")
}

# Export features to a CSV file
write.csv(features_df, "extracted_features.csv", row.names = FALSE)
message("Features have been successfully exported to 'extracted_features.csv'.")

# Split the data into training and test sets
set.seed(123)  # Set seed for reproducibility
trainIndex <- createDataPartition(features_df$label, p = 0.8, list = FALSE)

# Split data based on indices
trainData <- features_df[trainIndex, ]
testData <- features_df[-trainIndex, ]

# Train the models

# Random Forest
rf_model <- randomForest(label ~ ., data = trainData, ntree = 100)
print(rf_model)

# Logistic Regression
log_reg_model <- glm(label ~ ., data = trainData, family = binomial)
print(log_reg_model)
# Support Vector Machine (SVM)
svm_model <- svm(label ~ ., data = trainData)
print(svm_model)

# K-Nearest Neighbors (KNN)
knn_model <- knn(train = trainData[, -ncol(trainData)], test = testData[, -ncol(testData)], cl = trainData$label, k = 5)
print(knn_model)

# Ensure label column in trainData is a factor
trainData$label <- as.factor(trainData$label)
testData$label <- as.factor(testData$label)


# Decision Tree
dt_model <- rpart(label ~ ., data = trainData)
print(dt_model)



# Re-run prediction after verifying structure and levels
predictions <- predict(dt_model, testData, type = "class")


# Make predictions on the test data for each model

# Random Forest predictions
rf_predictions <- predict(rf_model, testData)

# Logistic Regression predictions
log_reg_predictions <- predict(log_reg_model, testData, type = "response")
log_reg_predictions <- factor(ifelse(log_reg_predictions > 0.5, 1, 0), levels = c(0, 1))

# SVM predictions
svm_predictions <- predict(svm_model, testData)

# KNN predictions
knn_predictions <- knn(train = trainData[, -ncol(trainData)], test = testData[, -ncol(testData)], cl = trainData$label, k = 5)

# Decision Tree predictions
dt_predictions <- predict(dt_model, testData, type = "class")

# Evaluate the models
# Convert both predictions and actual labels to factors with the same levels
log_reg_predictions <- factor(log_reg_predictions, levels = unique(testData$label))
testData$label <- factor(testData$label, levels = unique(testData$label))

# Check levels to ensure they match
levels(log_reg_predictions)
levels(testData$label)

# Now create the confusion matrix
print(confusionMatrix(log_reg_predictions, testData$label))

# Confusion Matrix for Random Forest
cat("Random Forest Confusion Matrix:\n")
print(confusionMatrix(rf_predictions, testData$label))

# Confusion Matrix for Logistic Regression
cat("Logistic Regression Confusion Matrix:\n")
print(confusionMatrix(log_reg_predictions, testData$label))

# Confusion Matrix for SVM
cat("SVM Confusion Matrix:\n")
print(confusionMatrix(svm_predictions, testData$label))

# Confusion Matrix for KNN
cat("KNN Confusion Matrix:\n")
print(confusionMatrix(knn_predictions, testData$label))

# Confusion Matrix for Decision Tree
cat("Decision Tree Confusion Matrix:\n")
print(confusionMatrix(dt_predictions, testData$label))


```