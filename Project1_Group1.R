install.packages("readxl")
library(readxl)
main_Dataset <- read_excel("F:/AIUB  All Sem/Aiub Sem - 10/INTRODUCTION TO DATA SCIENCE [A]/Mid/Lab/Project/Project_1/Midterm_Dataset_Section_A.xlsx")
print(main_Dataset, n = Inf, width = Inf)

which(is.na(main_Dataset), arr.ind = TRUE)
colSums(is.na(main_Dataset))

omitInstances_Dataset <- na.omit(main_Dataset)
which(is.na(omitInstances_Dataset), arr.ind = TRUE)
colSums(is.na(omitInstances_Dataset))

meanMode_Dataset <- main_Dataset
meanMode_Dataset$Age[is.na(meanMode_Dataset$Age)] <- mean(meanMode_Dataset$Age, na.rm = TRUE)
get_mode <- function(x) {
  names(which.max(table(na.omit(x))))
}
meanMode_Dataset$Gender[is.na(meanMode_Dataset$Gender)] <- get_mode(meanMode_Dataset$Gender)
meanMode_Dataset$`Item Purchased`[is.na(meanMode_Dataset$`Item Purchased`)] <- get_mode(meanMode_Dataset$`Item Purchased`)
meanMode_Dataset$`Frequency of Purchases`[is.na(meanMode_Dataset$`Frequency of Purchases`)] <- get_mode(meanMode_Dataset$`Frequency of Purchases`)
colSums(is.na(meanMode_Dataset))

medianMode_Dataset <- main_Dataset
medianMode_Dataset$Age[is.na(medianMode_Dataset$Age)] <- median(medianMode_Dataset$Age, na.rm = TRUE)
medianMode_Dataset$Gender[is.na(medianMode_Dataset$Gender)] <- get_mode(medianMode_Dataset$Gender)
medianMode_Dataset$`Item Purchased`[is.na(medianMode_Dataset$`Item Purchased`)] <- get_mode(medianMode_Dataset$`Item Purchased`)
medianMode_Dataset$`Frequency of Purchases`[is.na(medianMode_Dataset$`Frequency of Purchases`)] <- get_mode(medianMode_Dataset$`Frequency of Purchases`)
colSums(is.na(meanMode_Dataset))

install.packages("naniar")
library(naniar)
vis_miss(main_Dataset)
gg_miss_var(main_Dataset)

table(meanMode_Dataset$Gender)
minority_class <- meanMode_Dataset[meanMode_Dataset$Gender == "Male", ]
majority_class <- meanMode_Dataset[meanMode_Dataset$Gender == "Female", ]
set.seed(123)
undersampled_majority <- majority_class[sample(1:nrow(majority_class), size = nrow(minority_class)), ]
undersampled_Dataset <- rbind(minority_class, undersampled_majority)
table(undersampled_Dataset$Gender)

set.seed(123)
oversampled_minority <- minority_class[sample(1:nrow(minority_class), size = nrow(majority_class), replace = TRUE), ]
oversampled_Dataset <- rbind(majority_class, oversampled_minority)
table(oversampled_Dataset$Gender)

install.packages("dplyr")
library(dplyr)
sum(duplicated(meanMode_Dataset))
duplicateRemoved_Dataset <- distinct(meanMode_Dataset)
sum(duplicated(duplicateRemoved_Dataset))

filter(duplicateRemoved_Dataset, Age <= 45)

numToCat_Dataset <- meanMode_Dataset %>%
  mutate(Age = case_when(
    Age <= 30 ~ "Young",
    Age <= 50 ~ "Middle-aged",
    TRUE ~ "Senior"
  ))
print(numToCat_Dataset)

catToNum_Dataset <- meanMode_Dataset %>%
  mutate(`Subscription Status` = case_when(
    `Subscription Status` == "Yes" ~ 1,
    `Subscription Status` == "No" ~ 0
  ))
print(catToNum_Dataset[10:12])

normalized_Dataset <- meanMode_Dataset
minMax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
normalized_Dataset$`Previous Purchases` <- minMax(normalized_Dataset$`Previous Purchases`)
print(normalized_Dataset[12:14])

boxplot(duplicateRemoved_Dataset$Age,
        ylab = "Age"
)
Filter_Age <- filter(duplicateRemoved_Dataset, Age > 100)
print(Filter_Age)
outliersRemoved_Dataset <- duplicateRemoved_Dataset[!(duplicateRemoved_Dataset$Age %in% c(256, 235)), ]
boxplot(outliersRemoved_Dataset$Age,
        ylab = "Age"
)

vars <- c("Age", "Purchase Amount (USD)", "Review Rating", "Previous Purchases")
summary(outliersRemoved_Dataset[vars])
