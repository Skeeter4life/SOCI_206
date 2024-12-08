library(ggplot2)

race <- readLines("C:\\Users\\Andrew\\Downloads\\Race.txt")
duration <- readLines("C:\\Users\\Andrew\\Documents\\SOCI206\\Duration.txt")
units <- readLines("C:\\Users\\Andrew\\Documents\\SOCI206\\Units.txt")
sentence_type <- readLines("C:\\Users\\Andrew\\Documents\\SOCI206\\Sentence_Type.txt")
crime <- readLines("C:\\Users\\Andrew\\Documents\\SOCI206\\Crime.txt")

#I have the headers in the data.
Data <- data.frame(Race = race[-1], Duration = duration[-1], Units = units[-1], Sentence_Type = sentence_type[-1], Crime = crime[-1])


Data <- Data[!is.na(Data[["Units"]]) & Data[["Units"]] != "", ]


Data <- Data[Data[["Sentence_Type"]] == "Prison", ]


Data[["Race"]] <- ifelse(Data[["Race"]] == "White", "Majority", "Minority")


Data[["Duration"]] <- as.character(Data[["Duration"]])  

# Handle "Natural Life" and other special cases
Data[["Duration_in_Years"]] <- ifelse(Data[["Units"]] == "Natural Life", 100,  
                                      ifelse(Data[["Units"]] == "Year(s)", as.numeric(Data[["Duration"]]),  
                                             ifelse(Data[["Units"]] == "Months", as.numeric(Data[["Duration"]]) / 12,  
                                                    ifelse(Data[["Units"]] == "Days", as.numeric(Data[["Duration"]]) / 365, NA))))  


Data[["Duration"]] <- NULL
Data[["Units"]] <- NULL

print(Data)

Data_cleaned <- Data[Data[["Duration_in_Years"]] <= 1000, ]

anova_result <- aov(Duration_in_Years ~ Race, data = Data_cleaned)
summary(anova_result)


