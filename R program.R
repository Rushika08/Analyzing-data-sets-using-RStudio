# Install and load the readxl and ggplot2 package
install.packages("readxl")
install.packages("ggplot2")
library(readxl)
library(ggplot2)

#Loading the data set into R studio

# Specify the path to your Excel file
excel_file <- 'D:\\Downloads\\lung cancer\\cancer patient data sets.xlsx'

# Read the Excel file into a data frame
data <- read_excel(excel_file)


#------------------------------------------------------------------------------

# Plotting the age values of patients


# Converting the 'Age' data into numeric format
dataAge <- as.numeric(data$Age)

# Create a frequency table for 'Age'
age_frequency <- table(dataAge)

# Convert the frequency table to a data frame
age_frequency_df <- as.data.frame(age_frequency)

#debug and identify any issues
#debug(ggplot)

# Create a bar chart
print(ggplot(age_frequency_df, aes(x = dataAge, y = Freq)) +
        geom_bar(stat = "identity", fill = "skyblue", color = "black") +
        labs(title = "Age Frequency Bar Chart", x = "Age", y = "Frequency") +
        theme_minimal())


#-----------------------------------------------------------------------------

# Plotting the Dust Allergy levels of patients


# Converting the 'Dust Allergy' data into numeric format
dataDustAllergy <- as.numeric(data$`Dust Allergy`)

# Create a frequency table for 'Dust Allergy'
dustAllergy_frequency <- table(dataDustAllergy)

# Convert the frequency table to a data frame
dustAllergy_frequency_df <- as.data.frame(dustAllergy_frequency)

# Create a bar chart for 'Dust Allergy'
print(ggplot(dustAllergy_frequency_df, aes(x = dataDustAllergy, y = Freq)) +
        geom_bar(stat = "identity", fill = "salmon", color = "black") +
        labs(title = "Dust Allergy Frequency Bar Chart", x = "Dust Allergy Level", y = "Frequency") +
        theme_minimal())


#-------------------------------------------------------------------------------------------------------------

# Plotting the smoking levels of patients


# Converting the 'Smoking' data into numeric format
dataSmoking <- as.numeric(data$Smoking)

# Create a frequency table for 'Smoking'
smoking_frequency <- table(dataSmoking)

# Convert the frequency table to a data frame
smoking_frequency_df <- as.data.frame(smoking_frequency)

# Create a bar chart for 'Smoking'
print(ggplot(smoking_frequency_df, aes(x = dataSmoking, y = Freq)) +
        geom_bar(stat = "identity", fill = "lightgreen", color = "black") +
        labs(title = "Smoking Frequency Bar Chart", x = "Smoking Level", y = "Frequency") +
        theme_minimal())


#----------------------------------------------------------------------------------------------

# Plotting the chronic lung disease levels of patiets


# Converting the 'Chronic Lung Disease' data into numeric format
dataChronicLungDisease <- as.numeric(data$`chronic Lung Disease`)

# Create a frequency table for 'Chronic Lung Disease'
chronicLungDisease_frequency <- table(dataChronicLungDisease)

# Convert the frequency table to a data frame
chronicLungDisease_frequency_df <- as.data.frame(chronicLungDisease_frequency)

# Create a bar chart for 'Chronic Lung Disease'
print(ggplot(chronicLungDisease_frequency_df, aes(x = dataChronicLungDisease, y = Freq)) +
        geom_bar(stat = "identity", fill = "coral", color = "black") +
        labs(title = "Chronic Lung Disease Frequency Bar Chart", x = "Chronic Lung Disease Level", y = "Frequency") +
        theme_minimal())


#----------------------------------------------------------------------------------------------------------------------

# Plotting the Severity levels of the patients


# Create a frequency table for 'Level'
level_frequency <- table(data$Level)

# Convert the frequency table to a data frame
level_frequency_df <- as.data.frame(level_frequency)

# Create a pie chart for 'Level'
print(ggplot(level_frequency_df, aes(x = "", y = Freq, fill = Var1)) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        coord_polar("y", start = 0) +
        labs(title = "Level Frequency Pie Chart", fill = "Level") +
        theme_minimal() +
        theme(legend.position = "right"))


#----------------------------------------------------------------------------

# Plotting the severity level of heavily smoking patients


# Filter patients with smoking level greater than 5
filtered_data <- data[data$Smoking > 5, ]

# Create a frequency table for 'Level' in the filtered data
level_frequency_filtered <- table(filtered_data$Level)

# Convert the frequency table to a data frame
level_frequency_filtered_df <- as.data.frame(level_frequency_filtered)

# Create a pie chart for 'Level' in the filtered data
print(ggplot(level_frequency_filtered_df, aes(x = "", y = Freq, fill = Var1)) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        coord_polar("y", start = 0) +
        labs(title = "Severity Level Distribution for Patients with Smoking Level > 5", fill = "Level") +
        theme_minimal() +
        theme(legend.position = "right"))


#------------------------------------------------------------------------------------------------------------

# Plotting the severity level patients who are highly allergic to dust and who
# live within a heavily air polluted environment


# Filter patients with Dust Allergy level and Air Pollution level both greater than 5
filtered_data <- data[data$`Dust Allergy` > 5 & data$`Air Pollution` > 5, ]

# Create a frequency table for 'Level' in the filtered data
level_frequency_filtered <- table(filtered_data$Level)

# Convert the frequency table to a data frame
level_frequency_filtered_df <- as.data.frame(level_frequency_filtered)

# Create a pie chart for 'Level' in the filtered data
print(ggplot(level_frequency_filtered_df, aes(x = "", y = Freq, fill = Var1)) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        coord_polar("y", start = 0) +
        labs(title = "Severity Level Distribution with Dust Allergy and Air Pollution > 5", fill = "Level") +
        theme_minimal() +
        theme(legend.position = "right"))


#----------------------------------------------------------------------------------------------------------------

# Printing the mean values of each attribute


attributes <- c('Air Pollution', 'Alcohol use', 'Dust Allergy', 'Genetic Risk',
                'chronic Lung Disease', 'Balanced Diet', 'Obesity', 'Smoking',
                'Passive Smoker', 'Chest Pain', 'Fatigue', 'Weight Loss',
                'Shortness of Breath', 'Wheezing', 'Swallowing Difficulty',
                'Clubbing of Finger Nails', 'Frequent Cold', 'Dry Cough', 'Snoring')

# Create an empty data frame to store rounded mean values
mean_values_df <- data.frame(Attribute = character(), Mean_Value = numeric(), stringsAsFactors = FALSE)

# Calculate rounded mean values (rounded up) for each attribute
for (attribute in attributes) {
  mean_value <- mean(data[[attribute]])
  rounded_mean_value <- ceiling(mean_value)
  mean_values_df <- rbind(mean_values_df, data.frame(Attribute = attribute, Mean_Value = rounded_mean_value))
}

# Print the rounded mean values data frame
print(mean_values_df)


#--------------------------------------------------------------------------------------------------------------

# Plotting the mean values of each attribute


# Create a bar chart
print(ggplot(mean_values_df, aes(x = Attribute, y = Mean_Value)) +
    geom_bar(stat = "identity", fill = "skyblue", color = "black") +
    labs(title = "Mean Values of Attributes", x = "Attribute", y = "Mean Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)))  # Rotate x-axis labels for better readability


#------------------------------------------------------------------------------------------------------------------

# Calculating and Plotting the mean values separately based on the severity level
# of the patients

# Create an empty data frame to store rounded mean values and severity levels
mean_values_df <- data.frame(Attribute = character(), Severity = character(), Mean_Value = numeric(), stringsAsFactors = FALSE)

# Calculate rounded mean values (rounded up) for each attribute and each severity level
for (attribute in attributes) {
  for (severity_level in c('Low', 'Medium', 'High')) {
    mean_value <- mean(data[data$Level == severity_level, ][[attribute]])
    rounded_mean_value <- ceiling(mean_value)
    mean_values_df <- rbind(mean_values_df, data.frame(Attribute = attribute, Severity = severity_level, Mean_Value = rounded_mean_value))
  }
}

# Create a bar chart
print(ggplot(mean_values_df, aes(x = Attribute, y = Mean_Value, fill = Severity)) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    labs(title = "Mean Values of Attributes by Severity Level", x = "Attribute", y = "Mean Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)))  # Rotate x-axis labels for better readability
