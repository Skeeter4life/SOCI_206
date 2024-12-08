library(ggplot2)
library(vcd)

race <- readLines("C:\\Users\\Andrew\\Downloads\\Race.txt")
plea_result <- readLines("C:\\Users\\Andrew\\Downloads\\Plea.txt")

data <- data.frame(Race = race, PleaResult = plea_result)

data[["Race"]] <- ifelse(data[["Race"]] == "White", "Majority", "Minority")


data <- data[!data[["PleaResult"]] %in% c("BFW", "Case Dismissed", "Charge Reversed", "Charge Vacated", "CHARGE_DISPOSITION", "Death Suggested-Cause Abated", "Nolle On Remand", "Nolle Prosecution", "SOL", "SOLW", "Superseded by Indictment", "WOWI"), ]


data[["PleaResult"]] <- ifelse(data[["PleaResult"]] %in% c("Plea Of Guilty", "Plea of Guilty - Amended Charge", "Plea of Guilty - Lesser Included"), 
                               "Guilty Plea", "Not Guilty Plea")

contingency_table <- table(data[["Race"]], data[["PleaResult"]])
print(contingency_table)


chi_sq_test <- chisq.test(contingency_table)
print(chi_sq_test)


contingency_df <- as.data.frame(contingency_table)
colnames(contingency_df) <- c("Race", "PleaResult", "Count")