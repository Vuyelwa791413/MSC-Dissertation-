
ModelData <- read_csv("Documents/WITS/Data/LoanDefault_LTFS_AV/train_LTFS.csv")
ModelData <- as.data.frame(ModelData)
str(ModelData)

# Set the seed for reproducibility
set.seed(123)

#1. Split the data into training (70%) and testing (30%) sets
splitIndex <- sample.split(ModelData$UniqueID, SplitRatio = 0.7)
training_data <- ModelData[splitIndex, ]
testing_data <- ModelData[!splitIndex, ]

####### 1.2 Data check ########

#Assume NA is Other and replace missing with Other -> If loan was taken out we assume they are employed
training_data$Employment.Type[is.na(training_data$Employment.Type)] <- "Other"
testing_data$Employment.Type[is.na(testing_data$Employment.Type)] <- "Other"

# Create a new bin variable based on Employment.Type
training_data$Employment.Type.Code <- ifelse(training_data$Employment.Type == "Self employed", 2,
                                             ifelse(training_data$Employment.Type == "Salaried", 1, 
                                                    ifelse(training_data$Employment.Type == "Other", 0, NA)))

testing_data$Employment.Type.Code <- ifelse(testing_data$Employment.Type == "Self employed", 2,
                                            ifelse(testing_data$Employment.Type == "Salaried", 1, 
                                                   ifelse(testing_data$Employment.Type == "Other", 0, NA)))

#1.2.2 PERFORM_CNS.SCORE description and score range aswell as bin the scores

score_ranges <- training_data %>%
  group_by(PERFORM_CNS.SCORE.DESCRIPTION) %>%
  summarise(
    min_score = min(PERFORM_CNS.SCORE),
    max_score = max(PERFORM_CNS.SCORE)
  )
# Print the result
print(score_ranges)


score_ranges_ <- ModelData %>%
  group_by(PERFORM_CNS.SCORE.DESCRIPTION) %>%
  summarise(
    min_score = min(PERFORM_CNS.SCORE),
    max_score = max(PERFORM_CNS.SCORE)
  )
# Print the result
print(score_ranges_)





training_data$PERFORM_CNS.SCORE.CODE <- ifelse(training_data$PERFORM_CNS.SCORE.DESCRIPTION == "No Bureau History Available", 1,
                                               ifelse(training_data$PERFORM_CNS.SCORE.DESCRIPTION == "Not Scored: More than 50 active Accounts found", 1,
                                                      ifelse(training_data$PERFORM_CNS.SCORE.DESCRIPTION == "Not Scored: Only a Guarantor", 1,
                                                             ifelse(training_data$PERFORM_CNS.SCORE.DESCRIPTION == "Not Scored: Sufficient History Not Available", 1,
                                                                    ifelse(training_data$PERFORM_CNS.SCORE.DESCRIPTION == "Not Scored: No Activity seen on the customer (Inactive)", 1,
                                                                           ifelse(training_data$PERFORM_CNS.SCORE.DESCRIPTION == "Not Scored: Not Enough Info available on the customer", 1,
                                                                                  ifelse(training_data$PERFORM_CNS.SCORE.DESCRIPTION == "Not Scored: No Updates available in last 36 months", 1, 
                                                                                         ifelse(training_data$PERFORM_CNS.SCORE.DESCRIPTION == "M-Very High Risk", 2,
                                                                                                ifelse(training_data$PERFORM_CNS.SCORE.DESCRIPTION == "L-Very High Risk", 2,
                                                                                                       ifelse(training_data$PERFORM_CNS.SCORE.DESCRIPTION == "K-High Risk", 3,
                                                                                                              ifelse(training_data$PERFORM_CNS.SCORE.DESCRIPTION == "J-High Risk", 3, 
                                                                                                                     ifelse(training_data$PERFORM_CNS.SCORE.DESCRIPTION == "I-Medium Risk", 4,
                                                                                                                            ifelse(training_data$PERFORM_CNS.SCORE.DESCRIPTION == "H-Medium Risk", 4, 
                                                                                                                                   ifelse(training_data$PERFORM_CNS.SCORE.DESCRIPTION == "G-Low Risk", 5,
                                                                                                                                          ifelse(training_data$PERFORM_CNS.SCORE.DESCRIPTION == "F-Low Risk", 5,
                                                                                                                                                 ifelse(training_data$PERFORM_CNS.SCORE.DESCRIPTION == "E-Low Risk", 5, 
                                                                                                                                                        ifelse(training_data$PERFORM_CNS.SCORE.DESCRIPTION == "D-Very Low Risk", 6, 
                                                                                                                                                               ifelse(training_data$PERFORM_CNS.SCORE.DESCRIPTION == "C-Very Low Risk", 6, 
                                                                                                                                                                      ifelse(training_data$PERFORM_CNS.SCORE.DESCRIPTION == "B-Very Low Risk", 6, 
                                                                                                                                                                             ifelse(training_data$PERFORM_CNS.SCORE.DESCRIPTION == "A-Very Low Risk", 6, NA))))))))))))))))))))



testing_data$PERFORM_CNS.SCORE.CODE <- ifelse(testing_data$PERFORM_CNS.SCORE.DESCRIPTION == "No Bureau History Available", 1,
                                              ifelse(testing_data$PERFORM_CNS.SCORE.DESCRIPTION == "Not Scored: More than 50 active Accounts found", 1,
                                                     ifelse(testing_data$PERFORM_CNS.SCORE.DESCRIPTION == "Not Scored: Only a Guarantor", 1,
                                                            ifelse(testing_data$PERFORM_CNS.SCORE.DESCRIPTION == "Not Scored: Sufficient History Not Available", 1,
                                                                   ifelse(testing_data$PERFORM_CNS.SCORE.DESCRIPTION == "Not Scored: No Activity seen on the customer (Inactive)", 1,
                                                                          ifelse(testing_data$PERFORM_CNS.SCORE.DESCRIPTION == "Not Scored: Not Enough Info available on the customer", 1,
                                                                                 ifelse(testing_data$PERFORM_CNS.SCORE.DESCRIPTION == "Not Scored: No Updates available in last 36 months", 1, 
                                                                                        ifelse(testing_data$PERFORM_CNS.SCORE.DESCRIPTION == "M-Very High Risk", 2,
                                                                                               ifelse(testing_data$PERFORM_CNS.SCORE.DESCRIPTION == "L-Very High Risk", 2,
                                                                                                      ifelse(testing_data$PERFORM_CNS.SCORE.DESCRIPTION == "K-High Risk", 3,
                                                                                                             ifelse(testing_data$PERFORM_CNS.SCORE.DESCRIPTION == "J-High Risk", 3, 
                                                                                                                    ifelse(testing_data$PERFORM_CNS.SCORE.DESCRIPTION == "I-Medium Risk", 4,
                                                                                                                           ifelse(testing_data$PERFORM_CNS.SCORE.DESCRIPTION == "H-Medium Risk", 4, 
                                                                                                                                  ifelse(testing_data$PERFORM_CNS.SCORE.DESCRIPTION == "G-Low Risk", 5,
                                                                                                                                         ifelse(testing_data$PERFORM_CNS.SCORE.DESCRIPTION == "F-Low Risk", 5,
                                                                                                                                                ifelse(testing_data$PERFORM_CNS.SCORE.DESCRIPTION == "E-Low Risk", 5, 
                                                                                                                                                       ifelse(testing_data$PERFORM_CNS.SCORE.DESCRIPTION == "D-Very Low Risk", 6, 
                                                                                                                                                              ifelse(testing_data$PERFORM_CNS.SCORE.DESCRIPTION == "C-Very Low Risk", 6, 
                                                                                                                                                                     ifelse(testing_data$PERFORM_CNS.SCORE.DESCRIPTION == "B-Very Low Risk", 6, 
                                                                                                                                                                            ifelse(testing_data$PERFORM_CNS.SCORE.DESCRIPTION == "A-Very Low Risk", 6, NA))))))))))))))))))))


#1.2.3 Convert CREDIT.HISTORY.LENGTH and AVERAGE.ACCT.AGE to numeric months

# Function to convert "xyrs ymon" to total months
convert_to_months <- function(x) {
  # Extract years and months using regular expressions
  years <- as.numeric(sub("([0-9]+)yrs.*", "\\1", x))
  months <- as.numeric(sub(".* ([0-9]+)mon", "\\1", x))
  # Calculate total months
  total_months <- (years * 12) + months
  return(total_months)
}


# Apply the function to the duration column

training_data$CREDIT.HISTORY.LENGTH.MONTHS <- convert_to_months(training_data$CREDIT.HISTORY.LENGTH)
training_data$AVERAGE.ACCT.AGE.MONTHS <- convert_to_months(training_data$AVERAGE.ACCT.AGE)

testing_data$CREDIT.HISTORY.LENGTH.MONTHS <- convert_to_months(testing_data$CREDIT.HISTORY.LENGTH)
testing_data$AVERAGE.ACCT.AGE.MONTHS <- convert_to_months(testing_data$AVERAGE.ACCT.AGE)

#1.2.4 Convert Date variables to numeric year and months for date of birth and disbursal date respectively - Disbursal date was all in 2018

training_data$Disbursal.Month <- as.numeric(substr(training_data$DisbursalDate, 4, 5))
testing_data$Disbursal.Month <- as.numeric(substr(testing_data$DisbursalDate, 4, 5))


# Function to convert the year suffix to a full four-digit year
convert_year <- function(Date.of.Birth) {
  # Extract the last two digits of the year
  year_suffix <- as.numeric(str_sub(Date.of.Birth, -2))
  
  # Convert to full year
  full_year <- ifelse(year_suffix <= 20, 2000 + year_suffix, 1900 + year_suffix)
  
  return(full_year)
}

# Apply the function to the 'date_char' column
training_data$Birth.Year <-  as.numeric(sapply(training_data$Date.of.Birth, convert_year))
testing_data$Birth.Year <-  as.numeric(sapply(testing_data$Date.of.Birth, convert_year))


# Create a new bin variable based on Date of Birth 
training_data$DOB_IND <- ifelse(training_data$Birth.Year <= 1959, 1,
                                ifelse(training_data$Birth.Year >= 1960 & training_data$Birth.Year <= 1969, 2, 
                                       ifelse(training_data$Birth.Year >= 1970 & training_data$Birth.Year <= 1979, 3, 
                                              ifelse(training_data$Birth.Year >= 1980 & training_data$Birth.Year <= 1989, 4, 
                                                     ifelse(training_data$Birth.Year >= 1990, 5, NA)))))

testing_data$DOB_IND <- ifelse(testing_data$Birth.Year <= 1959, 1,
                               ifelse(testing_data$Birth.Year >= 1960 & testing_data$Birth.Year <= 1969, 2, 
                                      ifelse(testing_data$Birth.Year >= 1970 & testing_data$Birth.Year <= 1979, 3, 
                                             ifelse(testing_data$Birth.Year >= 1980 & testing_data$Birth.Year <= 1989, 4, 
                                                    ifelse(testing_data$Birth.Year >= 1990, 5, NA)))))


#create new variables - Combine Primary and Secondary accounts to create Total Accounts

training_data$TOT.NO.OF.ACCTS=training_data$PRI.NO.OF.ACCTS  + training_data$SEC.NO.OF.ACCTS
training_data$TOT.ACTIVE.ACCTS=training_data$PRI.ACTIVE.ACCTS + training_data$SEC.ACTIVE.ACCTS
training_data$TOT.OVERDUE.ACCTS=training_data$PRI.OVERDUE.ACCTS + training_data$SEC.OVERDUE.ACCTS
training_data$TOT.CURRENT.BALANCE=training_data$PRI.CURRENT.BALANCE + training_data$SEC.CURRENT.BALANCE
training_data$TOT.SANCTIONED.AMOUNT=training_data$PRI.SANCTIONED.AMOUNT + training_data$SEC.SANCTIONED.AMOUNT
training_data$TOT.DISBURSED.AMOUNT=training_data$PRI.DISBURSED.AMOUNT + training_data$SEC.DISBURSED.AMOUNT
training_data$TOT.INSTAL.AMT=training_data$PRIMARY.INSTAL.AMT + training_data$SEC.INSTAL.AMT

testing_data$TOT.NO.OF.ACCTS=testing_data$PRI.NO.OF.ACCTS  + testing_data$SEC.NO.OF.ACCTS
testing_data$TOT.ACTIVE.ACCTS=testing_data$PRI.ACTIVE.ACCTS + testing_data$SEC.ACTIVE.ACCTS
testing_data$TOT.OVERDUE.ACCTS=testing_data$PRI.OVERDUE.ACCTS + testing_data$SEC.OVERDUE.ACCTS
testing_data$TOT.CURRENT.BALANCE=testing_data$PRI.CURRENT.BALANCE + testing_data$SEC.CURRENT.BALANCE
testing_data$TOT.SANCTIONED.AMOUNT=testing_data$PRI.SANCTIONED.AMOUNT + testing_data$SEC.SANCTIONED.AMOUNT
testing_data$TOT.DISBURSED.AMOUNT=testing_data$PRI.DISBURSED.AMOUNT + testing_data$SEC.DISBURSED.AMOUNT
testing_data$TOT.INSTAL.AMT=testing_data$PRIMARY.INSTAL.AMT + testing_data$SEC.INSTAL.AMT



#Binning for Total current balance 

percentiles <- quantile(training_data$TOT.CURRENT.BALANCE, probs = seq(0, 1, by = 0.1))

print(percentiles)



training_data$TOT.CUR.BALANCE.BIN <- ifelse(training_data$TOT.CURRENT.BALANCE < 0, 1,
                                            ifelse(training_data$TOT.CURRENT.BALANCE == 0 , 2, 
                                                   ifelse(training_data$TOT.CURRENT.BALANCE > 0 & training_data$TOT.CURRENT.BALANCE < 18334, 3, 
                                                          ifelse(training_data$TOT.CURRENT.BALANCE >= 18334 & training_data$TOT.CURRENT.BALANCE < 66901, 4,
                                                                 ifelse(training_data$TOT.CURRENT.BALANCE >= 66901 & training_data$TOT.CURRENT.BALANCE < 319221, 5,
                                                                        ifelse(training_data$TOT.CURRENT.BALANCE >= 319221, 6, NA))))))



testing_data$TOT.CUR.BALANCE.BIN <- ifelse(testing_data$TOT.CURRENT.BALANCE < 0, 1,
                                           ifelse(testing_data$TOT.CURRENT.BALANCE == 0 , 2, 
                                                  ifelse(testing_data$TOT.CURRENT.BALANCE > 0 & testing_data$TOT.CURRENT.BALANCE < 18334, 3, 
                                                         ifelse(testing_data$TOT.CURRENT.BALANCE >= 18334 & testing_data$TOT.CURRENT.BALANCE < 66901, 4,
                                                                ifelse(testing_data$TOT.CURRENT.BALANCE >= 66901 & testing_data$TOT.CURRENT.BALANCE < 319221, 5,
                                                                       ifelse(testing_data$TOT.CURRENT.BALANCE >= 319221, 6, NA))))))


#remove variables where binning was created etc.
training_data <- subset(training_data, select = -c(Date.of.Birth,Birth.Year,DisbursalDate,CREDIT.HISTORY.LENGTH,AVERAGE.ACCT.AGE,PERFORM_CNS.SCORE.DESCRIPTION, PERFORM_CNS.SCORE,Employment.Type,SEC.NO.OF.ACCTS, SEC.ACTIVE.ACCTS, SEC.OVERDUE.ACCTS, 
                                                   SEC.CURRENT.BALANCE, SEC.SANCTIONED.AMOUNT, SEC.DISBURSED.AMOUNT, SEC.INSTAL.AMT, PRI.NO.OF.ACCTS, PRI.ACTIVE.ACCTS, PRI.OVERDUE.ACCTS, PRI.CURRENT.BALANCE, PRI.SANCTIONED.AMOUNT, PRI.DISBURSED.AMOUNT, PRIMARY.INSTAL.AMT))

testing_data <- subset(testing_data, select = -c(Date.of.Birth,Birth.Year,DisbursalDate,CREDIT.HISTORY.LENGTH,AVERAGE.ACCT.AGE,PERFORM_CNS.SCORE.DESCRIPTION, PERFORM_CNS.SCORE,Employment.Type,SEC.NO.OF.ACCTS, SEC.ACTIVE.ACCTS, SEC.OVERDUE.ACCTS, 
                                                 SEC.CURRENT.BALANCE, SEC.SANCTIONED.AMOUNT, SEC.DISBURSED.AMOUNT, SEC.INSTAL.AMT, PRI.NO.OF.ACCTS, PRI.ACTIVE.ACCTS, PRI.OVERDUE.ACCTS, PRI.CURRENT.BALANCE, PRI.SANCTIONED.AMOUNT, PRI.DISBURSED.AMOUNT, PRIMARY.INSTAL.AMT))

str(training_data)


###############################################################################################################
#2.1 Outliers
boxplot(training_data$disbursed_amount) #Has an outlier - Max Value is an outlier
boxplot(training_data$Deposit) #Has an outlier - Max Value is an outlier
boxplot(training_data$asset_cost) #Has an outlier - Max Value is an outlier
boxplot(training_data$ltv) #Has an outlier - Min Value is an outlier     
boxplot(training_data$PRI.NO.OF.ACCTS) # no Outlier -> Updated to "Has an outlier - Max Value is an outlier"
boxplot(training_data$PRI.ACTIVE.ACCTS) # no Outlier -> Updated to "Has an outlier - Max Value is an outlier"
boxplot(training_data$PRI.OVERDUE.ACCTS) # no Outlier -> Updated to "Has an outlier - Max Value is an outlier"
boxplot(training_data$PRI.CURRENT.BALANCE) #Has an outlier - Max Value is an outlier
boxplot(training_data$PRI.SANCTIONED.AMOUNT) #Has an outlier - Max Value is an outlier
boxplot(training_data$PRI.DISBURSED.AMOUNT) #Has an outlier - Max Value is an outlier
boxplot(training_data$PRIMARY.INSTAL.AMT) #Has an outlier - Max Value is an outlier
boxplot(training_data$SEC.INSTAL.AMT)  #Has an outlier - Max Value is an outlier
boxplot(training_data$SEC.NO.OF.ACCTS) # no Outlier -> Updated to "Has an outlier - Max Value is an outlier"
boxplot(training_data$SEC.ACTIVE.ACCTS) # no Outlier -> Updated to "Has an outlier - Max Value is an outlier"
boxplot(training_data$SEC.OVERDUE.ACCTS) # no Outlier -> Updated to "Has an outlier - Max Value is an outlier"
boxplot(training_data$SEC.CURRENT.BALANCE) #Has an outlier - Max Value is an outlier
boxplot(training_data$SEC.SANCTIONED.AMOUNT) #Has an outlier - Max Value is an outlier
boxplot(training_data$SEC.DISBURSED.AMOUNT) #Has an outlier - Max Value is an outlier
boxplot(training_data$NEW.ACCTS.IN.LAST.SIX.MONTHS) # no Outlier -> Updated to "Has an outlier - Max Value is an outlier"
boxplot(training_data$DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS) # no Outlier -> Updated to "Has an outlier - Max Value is an outlier"
boxplot(training_data$AVERAGE.ACCT.AGE.MONTHS) #Has an outlier - Max Value is an outlier
boxplot(training_data$CREDIT.HISTORY.LENGTH.MONTHS) # Has an Outlier -> Updated to "Has an outlier - Max Value is an outlier"
boxplot(training_data$NO.OF_INQUIRIES) # no Outlier -> Updated to "Has an outlier - Max Value is an outlier"

#New variables created 
boxplot(training_data$TOT.CURRENT.BALANCE) 
boxplot(training_data$TOT.SANCTIONED.AMOUNT) 
boxplot(training_data$TOT.DISBURSED.AMOUNT) 
boxplot(training_data$TOT.NO.OF.ACCTS) 
boxplot(training_data$TOT.ACTIVE.ACCTS) 
boxplot(training_data$TOT.OVERDUE.ACCTS) 
boxplot(training_data$TOT.INSTAL.AMT) 

###################################################################################################################################################################

#2.1 Capping Outliers 

disbursed_amount_Sorted <- sort(training_data$disbursed_amount, decreasing = TRUE)
disbursed_amount <- disbursed_amount_Sorted[7]
print(disbursed_amount)

asset_cost_Sorted <- sort(training_data$asset_cost, decreasing = TRUE)
asset_cost <- asset_cost_Sorted[8]
print(asset_cost)


TOT.NO.OF.ACCTS_Sorted <- sort(training_data$TOT.NO.OF.ACCTS, decreasing = TRUE)
TOT.NO.OF.ACCTS <- TOT.NO.OF.ACCTS_Sorted[5]
print(TOT.NO.OF.ACCTS)

TOT.ACTIVE.ACCTS_Sorted <- sort(training_data$TOT.ACTIVE.ACCTS, decreasing = TRUE)
TOT.ACTIVE.ACCTS <- TOT.ACTIVE.ACCTS_Sorted[2]
print(TOT.ACTIVE.ACCTS)


TOT.SANCTIONED.AMOUNT_Sorted <- sort(training_data$TOT.SANCTIONED.AMOUNT, decreasing = TRUE)
TOT.SANCTIONED.AMOUNT <- TOT.SANCTIONED.AMOUNT_Sorted[2]
print(TOT.SANCTIONED.AMOUNT)

TOT.DISBURSED.AMOUNT_Sorted <- sort(training_data$TOT.DISBURSED.AMOUNT, decreasing = TRUE)
TOT.DISBURSED.AMOUNT <- TOT.DISBURSED.AMOUNT_Sorted[2]
print(TOT.DISBURSED.AMOUNT)

TOT.INSTAL.AMT_Sorted <- sort(training_data$TOT.INSTAL.AMT, decreasing = TRUE)
TOT.INSTAL.AMT <- TOT.INSTAL.AMT_Sorted[10]
print(TOT.INSTAL.AMT)

AVERAGE.ACCT.AGE.MONTHS_Sorted <- sort(training_data$AVERAGE.ACCT.AGE.MONTHS, decreasing = TRUE)
AVERAGE.ACCT.AGE.MONTHS <- AVERAGE.ACCT.AGE.MONTHS_Sorted[3]
print(AVERAGE.ACCT.AGE.MONTHS)

CREDIT.HISTORY.LENGTH.MONTHS_Sorted <- sort(training_data$CREDIT.HISTORY.LENGTH.MONTHS, decreasing = TRUE)
CREDIT.HISTORY.LENGTH.MONTHS <- CREDIT.HISTORY.LENGTH.MONTHS_Sorted[5]
print(CREDIT.HISTORY.LENGTH.MONTHS)



variables_to_cap_UP <- c("disbursed_amount", "asset_cost", "TOT.NO.OF.ACCTS", "TOT.ACTIVE.ACCTS","TOT.SANCTIONED.AMOUNT","TOT.DISBURSED.AMOUNT","TOT.INSTAL.AMT","AVERAGE.ACCT.AGE.MONTHS","CREDIT.HISTORY.LENGTH.MONTHS")


##Final Capping Variables
Capp_Values <- data.frame(
  disbursed_amount, 
  asset_cost, 
  #Deposit,
  TOT.NO.OF.ACCTS,
  TOT.ACTIVE.ACCTS, 
  TOT.SANCTIONED.AMOUNT,
  TOT.DISBURSED.AMOUNT, 
  TOT.INSTAL.AMT,
  AVERAGE.ACCT.AGE.MONTHS,
  CREDIT.HISTORY.LENGTH.MONTHS
)

view(Capp_Values)

####Transfer Data
training_data_Cap <- training_data

for (variable in variables_to_cap_UP) {
  
  training_data_Cap[[variable]] <- ifelse(training_data_Cap[[variable]] > Capp_Values[[variable]], Capp_Values[[variable]], training_data_Cap[[variable]])
  
}

#Apply to Test set
testing_data_Cap <- testing_data

for (variable in variables_to_cap_UP) {
  
  testing_data_Cap[[variable]] <- ifelse(testing_data_Cap[[variable]] > Capp_Values[[variable]], Capp_Values[[variable]], testing_data_Cap[[variable]])
  
}

# Cap Lower bound
ltv_Sorted <- sort(training_data$ltv, decreasing = FALSE)
ltv <- ltv_Sorted[2]
print(ltv)




Capp_ValuesL <- data.frame(
  ltv
  
)

variables_to_cap_LOW <- c("ltv")

for (variable in variables_to_cap_LOW) {
  
  training_data_Cap[[variable]] <- ifelse(training_data_Cap[[variable]] < Capp_ValuesL[[variable]], Capp_ValuesL[[variable]], training_data_Cap[[variable]])
  
}

#Apply to Test set
for (variable in variables_to_cap_LOW) {
  
  testing_data_Cap[[variable]] <- ifelse(testing_data_Cap[[variable]] < Capp_ValuesL[[variable]], Capp_ValuesL[[variable]], testing_data_Cap[[variable]])
  
}

str(testing_data_Cap)
###################################################################################################################################################################
###################################################################################################################################################################

#Store Data
training_data_Capped <- training_data_Cap
###################################################################################################################################################################

##########Convert to Factors###########
TrainData_Balanced$PERFORM_CNS.SCORE.CODE <- factor(TrainData_Balanced$PERFORM_CNS.SCORE.CODE, levels = c(6, 5, 4, 3, 2,1), ordered = TRUE)
TrainData_Balanced$DOB_IND <- factor(TrainData_Balanced$DOB_IND,ordered = TRUE)
TrainData_Balanced$TOT.CUR.BALANCE.BIN <- factor(TrainData_Balanced$TOT.CUR.BALANCE.BIN,levels = c(6, 5, 4, 3, 2,1),ordered = TRUE)

str(TrainData_Balanced)

# Convert variables to factors for Training

training_data_Cap$PERFORM_CNS.SCORE.CODE <- factor(training_data_Cap$PERFORM_CNS.SCORE.CODE, ordered = TRUE)
training_data_Cap$DOB_IND <- factor(training_data_Cap$DOB_IND,ordered = TRUE)
training_data_Cap$TOT.CUR.BALANCE.BIN <- factor(training_data_Cap$TOT.CUR.BALANCE.BIN,ordered = TRUE)
training_data_Cap$Employment.Type.Code <- factor(training_data_Cap$Employment.Type.Code)
training_data_Cap$loan_default <- factor(training_data_Cap$loan_default)
training_data_Cap$State_ID <- factor(training_data_Cap$State_ID)
training_data_Cap$manufacturer_id <- factor(training_data_Cap$manufacturer_id)
training_data_Cap$branch_id <- factor(training_data_Cap$branch_id)
training_data_Cap$MobileNo_Avl_Flag <- factor(training_data_Cap$MobileNo_Avl_Flag)
training_data_Cap$Aadhar_flag <- factor(training_data_Cap$Aadhar_flag)
training_data_Cap$PAN_flag <- factor(training_data_Cap$PAN_flag)
training_data_Cap$VoterID_flag <- factor(training_data_Cap$VoterID_flag)
training_data_Cap$Driving_flag <- factor(training_data_Cap$Driving_flag)
training_data_Cap$Passport_flag <- factor(training_data_Cap$Passport_flag)
training_data_Cap$supplier_id <- factor(training_data_Cap$supplier_id)
training_data_Cap$Current_pincode_ID <- factor(training_data_Cap$Current_pincode_ID)
training_data_Cap$Employee_code_ID <- factor(training_data_Cap$Employee_code_ID)

# Convert variables to factors for Testing
testing_data_Cap$PERFORM_CNS.SCORE.CODE <- factor(testing_data_Cap$PERFORM_CNS.SCORE.CODE, ordered = TRUE)
testing_data_Cap$DOB_IND <- factor(testing_data_Cap$DOB_IND,ordered = TRUE)
testing_data_Cap$TOT.CUR.BALANCE.BIN <- factor(testing_data_Cap$TOT.CUR.BALANCE.BIN,ordered = TRUE)
testing_data_Cap$Employment.Type.Code <- factor(testing_data_Cap$Employment.Type.Code)
testing_data_Cap$loan_default <- factor(testing_data_Cap$loan_default)
testing_data_Cap$State_ID <- factor(testing_data_Cap$State_ID)
testing_data_Cap$manufacturer_id <- factor(testing_data_Cap$manufacturer_id)
testing_data_Cap$branch_id <- factor(testing_data_Cap$branch_id)
testing_data_Cap$MobileNo_Avl_Flag <- factor(testing_data_Cap$MobileNo_Avl_Flag)
testing_data_Cap$Aadhar_flag <- factor(testing_data_Cap$Aadhar_flag)
testing_data_Cap$PAN_flag <- factor(testing_data_Cap$PAN_flag)
testing_data_Cap$VoterID_flag <- factor(testing_data_Cap$VoterID_flag)
testing_data_Cap$Driving_flag <- factor(testing_data_Cap$Driving_flag)
testing_data_Cap$Passport_flag <- factor(testing_data_Cap$Passport_flag)
testing_data_Cap$supplier_id <- factor(testing_data_Cap$supplier_id)
testing_data_Cap$Current_pincode_ID <- factor(testing_data_Cap$Current_pincode_ID)
testing_data_Cap$Employee_code_ID <- factor(testing_data_Cap$Employee_code_ID)

#Remove Mobile Flag as it is only one value "1"
training_data_Cap <- subset(training_data_Cap, select = -c(MobileNo_Avl_Flag)) 
testing_data_Cap <- subset(testing_data_Cap, select = -c(MobileNo_Avl_Flag)) 

###################################################################################################################################################################
###################################################################################################################################################################
########################### Feature Importance ###########################


#xgboost - only numeric 

######Gradient Boosting MAchine - gbm does not currently handle categorical variables with more than 1024 levels
training_dataset_ <- subset(training_data_Cap, select = -c(UniqueID,TOT.CURRENT.BALANCE,Current_pincode_ID,supplier_id,Employee_code_ID)) 
str(training_dataset_)

training_dataset_$loan_default  <- as.numeric(as.character(training_dataset_$loan_default))

#str(training_dataset_)
GBM_Model <-gbm(loan_default ~ ., data=training_dataset_, distribution = "bernoulli") 
summary(GBM_Model)

importance_matrix <- summary(GBM_Model)
str(importance_matrix)

# Filter variables with rel.inf > 0.30

ImportantVariables <- importance_matrix[importance_matrix$rel.inf > 0.30, ]
print(ImportantVariables)

rm(training_dataset_)

###################################################
#Select variables that are selected by feature reduction 
importance_matrix$var <- gsub("([A-Za-z])([0-9])", "\\1.\\2", importance_matrix$var)
ImportantVariables <- importance_matrix[importance_matrix$rel.inf > 0.30, ]

# View the updated importance_matrix
print(ImportantVariables)
view(ImportantVariables)


selected_features <- ImportantVariables$var
view(selected_features)

V16 <- c("loan_default")
V17 <- c("UniqueID")

selected_features_ <- rbind(selected_features, V16)
selected_features_ <- cbind(selected_features_, V16)
selected_features_ <- rbind(selected_features_, V17)
selected_features_ <- cbind(selected_features_, V17)
view(selected_features_)


# Step 2: Subset dataset1 to keep only columns in selected_features
Training_Filtered_data <- training_data_Capped[, colnames(training_data_Capped) %in% selected_features_]

# Step 3: View the filtered dataset
view(Training_Filtered_data)
str(Training_Filtered_data) 

############ Apply to Test set


# Step 2: Subset dataset1 to keep only columns in selected_features
Test_Filtered_data <- testing_data_Cap[, colnames(testing_data_Cap) %in% selected_features_]

# Step 3: View the filtered dataset
view(Test_Filtered_data)
str(Test_Filtered_data) 



#########################################################################################################################################################################
######################### Create Dummy #######################################
##############################################################################

Training_Filtered_data$Employment.Type.Code <- factor(Training_Filtered_data$Employment.Type.Code)
Training_Filtered_data$loan_default <- factor(Training_Filtered_data$loan_default)
Training_Filtered_data$manufacturer_id <- factor(Training_Filtered_data$manufacturer_id)


Training_Filtered_data$loan_default  <- as.numeric(as.character(Training_Filtered_data$loan_default))

# Create a dummyVars object
Train_dummies <- dummyVars(~ ., data = Training_Filtered_data)

# Use the predict function to create dummy variables
training_dataset_ONEHOT <- predict(Train_dummies, newdata = Training_Filtered_data)

# Convert the result to a data frame
training_dataset_ONEHOT <- as.data.frame(training_dataset_ONEHOT)

Training_Filtered_data$loan_default <- factor(Training_Filtered_data$loan_default)
str(Training_Filtered_data)
#rm(Training_Filtered_data) #Uncomment and delete dataset 

# Display the result
view(training_dataset_ONEHOT)
str(training_dataset_ONEHOT)

training_dataset_ONEHOT$loan_default <- factor(training_dataset_ONEHOT$loan_default)
prop.table(table(training_dataset_ONEHOT$loan_default))

################################################################################################
#Dealing with Variables that are have multiple factors(Frequency Encoding)
training_dataset_ONEHOT$branch_id <- factor(training_dataset_ONEHOT$branch_id)


Training_Encoding_Final <- training_dataset_ONEHOT %>%
  group_by(branch_id) %>%
  mutate(Frequency = n() / nrow(training_dataset_ONEHOT)) %>%
  ungroup()

str(training_dataset_ONEHOT)
# Compute frequency of each category
freq_table <- Training_Encoding_Final %>%
  count(branch_id) %>%
  mutate(Frequency = n / sum(n)) %>%
  select(branch_id, Frequency)

# Merge frequency back into the dataframe
Training_Encoding_Final <- Training_Encoding_Final %>%
  left_join(freq_table, by = "branch_id") %>%
  select(-branch_id)  # Drop the original factor variable if needed


Training_Encoding_Final <- Training_Encoding_Final %>%
  select(-Frequency.y) %>%  # Remove duplicate column
  rename(Frequency = Frequency.x)  # Rename the correct column

# View transformed dataframe

str(Training_Encoding_Final)

names(Training_Encoding_Final)[names(Training_Encoding_Final) == "Frequency"] <- "BranchID_Freq"




#############################################
#######Apply on Test Set


Test_Filtered_data$loan_default  <- as.numeric(as.character(Test_Filtered_data$loan_default))
Test_Filtered_data$branch_id  <- as.numeric(as.character(Test_Filtered_data$branch_id))
Test_Filtered_data$PERFORM_CNS.SCORE.CODE  <- as.numeric(as.character(Test_Filtered_data$PERFORM_CNS.SCORE.CODE))
Test_Filtered_data$DOB_IND  <- as.numeric(as.character(Test_Filtered_data$DOB_IND))
str(Test_Filtered_data)

# Create a dummyVars object
Test_dummies <- dummyVars(~ ., data = Test_Filtered_data)

# Use the predict function to create dummy variables
Test_dataset_ONEHOT <- predict(Test_dummies, newdata = Test_Filtered_data)

# Convert the result to a data frame
Test_dataset_ONEHOT <- as.data.frame(Test_dataset_ONEHOT)
Test_dataset_ONEHOT$loan_default <- factor(Test_dataset_ONEHOT$loan_default)

Test_Filtered_data$loan_default <- factor(Test_Filtered_data$loan_default)
#rm(Test_Filtered_data) #Uncomment and delete dataset 

# Display the result
view(Test_dataset_ONEHOT)
str(Test_dataset_ONEHOT)

#############################
#Dealing with Variables that are have multiple factors (Frequency Encoding)
Test_dataset_ONEHOT$branch_id <- factor(Test_dataset_ONEHOT$branch_id)


Test_Encoding_Final <- Test_dataset_ONEHOT %>%
  group_by(branch_id) %>%
  mutate(Frequency = n() / nrow(Test_dataset_ONEHOT)) %>%
  ungroup()

str(Test_dataset_ONEHOT)
# Compute frequency of each category
freq_table <- Test_Encoding_Final %>%
  count(branch_id) %>%
  mutate(Frequency = n / sum(n)) %>%
  select(branch_id, Frequency)

# Merge frequency back into the dataframe
Test_Encoding_Final <- Test_Encoding_Final %>%
  left_join(freq_table, by = "branch_id") %>%
  select(-branch_id)  # Drop the original factor variable if needed


Test_Encoding_Final <- Test_Encoding_Final %>%
  select(-Frequency.y) %>%  # Remove duplicate column
  rename(Frequency = Frequency.x)  # Rename the correct column

# View transformed dataframe

str(Test_Encoding_Final)
names(Test_Encoding_Final)[names(Test_Encoding_Final) == "Frequency"] <- "BranchID_Freq"


#########################################################################################################################################################################
#########################################################################################################################################################################
################ 4. Scaling & normalising Data ################

include_vars <- c("disbursed_amount", "ltv","TOT.SANCTIONED.AMOUNT","TOT.DISBURSED.AMOUNT")  

# Calculate mean and sd from training data
train_means <- sapply(Training_Encoding_Final[include_vars], mean)
train_sds   <- sapply(Training_Encoding_Final[include_vars], sd)


# Normalize training data
TrainScaled <- Training_Encoding_Final
TrainScaled[include_vars] <- scale(Training_Encoding_Final[include_vars],
                                   center = train_means,
                                   scale = train_sds)

# Normalize test data using training mean and sd
TestScaled <- Test_Encoding_Final
TestScaled[include_vars] <- scale(Test_Encoding_Final[include_vars],
                                  center = train_means,
                                  scale = train_sds)

TestScaled$loan_default <- factor(TestScaled$loan_default)


prop.table(table(TrainScaled$loan_default))

###################################################################################################################################################################
###################################################################################################################################################################
################ 4. Balance Dataset - SMOTE ################


str(TrainScaled)
TrainScaled <- as.data.frame(TrainScaled)
TestScaled <- as.data.frame(TestScaled)
training_dataset <- TrainScaled

training_dataset <- training_dataset %>%
  mutate(across(.cols = c(manufacturer_id.45,DOB_IND,manufacturer_id.48,manufacturer_id.49,manufacturer_id.51,manufacturer_id.67,manufacturer_id.86,manufacturer_id.120,manufacturer_id.145,manufacturer_id.152,manufacturer_id.153,Employment.Type.Code.0,
                          Employment.Type.Code.1,Employment.Type.Code.2,PERFORM_CNS.SCORE.CODE,DOB_IND,BranchID_Freq,
                          DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS,NO.OF_INQUIRIES,CREDIT.HISTORY.LENGTH.MONTHS,CREDIT.HISTORY.LENGTH.MONTHS,Disbursal.Month,TOT.ACTIVE.ACCTS,TOT.OVERDUE.ACCTS), as.factor)) # Count Variables 



str(training_dataset)


# Function to split dataset and apply SMOTE iteratively
apply_smote_in_chunks <- function(dataset, target_col, chunk_ratio = 0.01, seed = 123) {
  set.seed(seed)
  
  remaining_data <- dataset  # Copy of dataset to modify
  balanced_data_list <- list()  # Store SMOTE results
  
  #splitIndex <- sample.split(training_dataset$UniqueID, SplitRatio = 0.01)
  while (nrow(remaining_data) > 0) {
    # Split a small portion
    splitIndex <- sample.split(remaining_data$UniqueID, SplitRatio = chunk_ratio)
    small_chunk <- remaining_data[splitIndex, ]
    remaining_data <- remaining_data[!splitIndex, ]  # Update remaining data
    
    # Apply SMOTE
    if (nrow(small_chunk) > 0) {  # Ensure non-empty batch
      smote_chunk <- SmoteClassif(as.formula(paste(target_col, "~ .")), small_chunk, dist = "HVDM")
      balanced_data_list[[length(balanced_data_list) + 1]] <- smote_chunk
    }
    
    # Print progress
    print(paste("Processed", length(balanced_data_list), "chunks, remaining:", nrow(remaining_data)))
  }
  
  # Combine all SMOTE-processed chunks
  final_balanced_data <- rbindlist(balanced_data_list, use.names = TRUE, fill = TRUE)
  
  return(final_balanced_data)
}

# Run function
TrainData_Balanced <- apply_smote_in_chunks(training_dataset, "loan_default")

TrainData_Balanced <- as.data.frame(TrainData_Balanced)
str(TrainData_Balanced)

prop.table(table(TrainData_Balanced$loan_default))

###Store Data  
Final_Train_Dataset <- TrainData_Balanced
str(Final_Train_Dataset)

#rm(TrainData_Balanced) # Uncomment and delete dataset

Final_Train_Dataset <- Final_Train_Dataset %>%
  mutate(across(.cols = c(manufacturer_id.45, DOB_IND, manufacturer_id.48, manufacturer_id.49, 
                          manufacturer_id.51, manufacturer_id.67, manufacturer_id.86, manufacturer_id.120, 
                          manufacturer_id.145, manufacturer_id.152, manufacturer_id.153, Employment.Type.Code.0,
                          Employment.Type.Code.1, Employment.Type.Code.2, PERFORM_CNS.SCORE.CODE, DOB_IND, 
                          BranchID_Freq, DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS, NO.OF_INQUIRIES, 
                          CREDIT.HISTORY.LENGTH.MONTHS, Disbursal.Month, TOT.ACTIVE.ACCTS, TOT.OVERDUE.ACCTS), 
                ~ as.numeric(as.character(.))))
str(Final_Train_Dataset)


str(TestScaled)

##################################################################################################################################################################
#DATA FIX. 
#The test Dataset does not have 152, it has 156 instead. The train dataset does not have 156 it has 152. 
Final_Train_Dataset$manufacturer_id.156  <- 0
TestScaled$manufacturer_id.152  <- 0

##################################################################################################################################################################
###################################################################################################################################################################

#Final Dataset to use

Final_TRAINING_Dataset <- subset(Final_Train_Dataset, select = -c(UniqueID)) 
Final_TEST_Dataset <- subset(TestScaled, select = -c(UniqueID)) 

Final_TRAINING_Dataset <- as.data.frame(Final_TRAINING_Dataset)
Final_TEST_Dataset <- as.data.frame(Final_TEST_Dataset)

str(Final_TRAINING_Dataset)
str(Final_TEST_Dataset)

Final_TRAINING_Dataset <- as.data.frame(Final_TRAINING_Dataset)
Final_TEST_Dataset <- as.data.frame(Final_TEST_Dataset)


##################################################################################################################################################################
###################################################################################################################################################################
#Logistic Regression


Logistic <- glm(loan_default ~ ., data = Final_TRAINING_Dataset, family = "binomial")
Logistic <- update(Logistic, . ~ . - manufacturer_id.156)
Logistic <- update(Logistic, . ~ . - TOT.SANCTIONED.AMOUNT)


summary(Logistic) #step(Logistic) #takes time to run 

pred_probs <- predict(Logistic, newdata = Final_TEST_Dataset, type = "response")
pred_classes <- ifelse(pred_probs > 0.5, 1, 0)

# Convert to factors for caret
actual <- factor(Final_TEST_Dataset$loan_default)
predicted_factor <- factor(pred_classes)

library(caret)

# Ensure both are factors and have the same levels
actual <- factor(Final_TEST_Dataset$loan_default, levels = c(0, 1))
predicted_factor <- factor(pred_classes, levels = c(0, 1))

# confusion Matrix
conf_matrix <- caret::confusionMatrix(predicted_factor, actual, positive = "1")

conf_matrix

# Access metrics
accuracy  <- conf_matrix$overall['Accuracy']
precision <- conf_matrix$byClass['Precision']
recall    <- conf_matrix$byClass['Recall']
f1        <- conf_matrix$byClass['F1']

accuracy
precision
recall
f1


### ROC CURVE

library(pROC)

# Convert factor to numeric (0/1)
actual_numeric <- as.numeric(as.character(actual))  # or use Final_TEST_Dataset$loan_default directly if it's numeric

# Re-run ROC and AUC
roc_obj <- roc(actual_numeric, pred_probs)
auc_value <- pROC::auc(roc_obj)

auc_value

#PLOT
plot(roc_obj, col = "blue", main = "ROC Curve")
legend("bottomright", legend = paste("AUC =", round(auc_value, 3)), col = "blue", lwd = 2)

###################################################################################################################################################################
###################################################################################################################################################################
#Neural Network

TrainData <-Final_TRAINING_Dataset
TestData <-Final_TEST_Dataset

TrainData <- as.data.frame(TrainData)
TestData <- as.data.frame(TestData)

##FINAL MODEL
set.seed(42)
neuralnetwork <- neuralnetwork(
  X = TrainData[,-15],
  y = as.numeric(as.character(TrainData$loan_default)),
  hidden.layers = c(12),
  regression = FALSE,
  standardize = TRUE,
  activ.functions = "relu",      # match nnet
  learn.rates = 0.01,
  L2 = 0.001,
  optim.type = "adam",
  n.epochs = 100,
  batch.size = 150,
  drop.last = FALSE,
  val.prop = 0,                     # don't use validation split
  verbose = FALSE,
  random.seed = 42
)



pred_probs_list <- predict(neuralnetwork, TestData[, -15], type = "probabilities")
pred_probs <- pred_probs_list$probabilities[,2]

# Convert probabilities to class labels (assuming binary classification, 0 or 1)

pred_labels <- ifelse(pred_probs > 0.5, 1, 0)


# Ensure actual labels are numeric or factor
actual_labels <- Final_TEST_Dataset$loan_default

# If needed, convert both to the same type
actual_labels <- as.factor(actual_labels)
pred_labels <- as.factor(pred_labels)

### Calculate confusion matrix
conf_matrix9 <- caret::confusionMatrix(pred_labels, actual_labels, positive = "1")
conf_matrix9
f1        <- conf_matrix9$byClass['F1']
f1

#ROC CURVE
roc(actual_labels, pred_probs)
roc_obj <- roc(actual_labels, pred_probs)
auc_value <- pROC::auc(roc_obj)
print(auc_value)

#PLOT
plot(roc_obj, col = "blue", main = "ROC Curve")
legend("bottomright", legend = paste("AUC =", round(auc_value, 3)), col = "blue", lwd = 2)


neuralweights(neuralnetwork) 

# Plot model
plot(neuralnetwork)


###############################################################
#svm - Support Vector Machines
###############################################################

###############Kernlab

library(kernlab)

# Exclude response variable
X <- Final_TRAINING_Dataset[, setdiff(names(Final_TRAINING_Dataset), "loan_default")]

# Identify zero-variance columns
zero_var_cols <- names(X)[apply(X, 2, function(col) var(as.numeric(col), na.rm = TRUE) == 0)]

# Drop zero-variance columns
X_clean <- X[, setdiff(names(X), zero_var_cols)]

# Combine back with response
Train_clean <- cbind(X_clean, loan_default = Final_TRAINING_Dataset$loan_default)


# Train a radial (RBF) SVM      --- FINAL MODEL--- 
svm_rbf <- ksvm(loan_default ~ ., 
                data = Train_clean,
                type = "C-svc",
                kernel = "rbfdot",   # radial basis kernel
                C = 1,
                kpar = "automatic",  # gamma parameter
                prob.model = TRUE)


# Inspect the model
svm_rbf

# Predict class labels
pred_rbf <- predict(svm_rbf, Final_TEST_Dataset, type = "response")
pred_rbf <- as.factor(pred_rbf)

# Predict probabilities
prob_rbf <- predict(svm_rbf, Final_TEST_Dataset, type = "probabilities")[,2]


actual_Label_svm <- Final_TEST_Dataset$loan_default
actual_Label_svm <- as.factor(actual_Label_svm)


### Confusion matrix

conf_matrix_rbf <- caret::confusionMatrix(pred_rbf, actual_Label_svm, positive = "1")
conf_matrix_rbf



accuracy  <- conf_matrix_rbf$overall['Accuracy']
precision <- conf_matrix_rbf$byClass['Precision']
recall    <- conf_matrix_rbf$byClass['Recall']
f1        <- conf_matrix_rbf$byClass['F1']

accuracy
precision
recall
f1

# ROC CURVE
roc_obj <- roc(actual_Label_svm, prob_rbf)
auc_value <- pROC::auc(roc_obj)
pROC::auc(roc_obj)
#PLOT
plot(roc_obj, col = "blue", main = "ROC Curve")
legend("bottomright", legend = paste("AUC =", round(auc_value, 3)), col = "blue", lwd = 2)


#######################################################################################################
#######################################################################################################
####### Descision Tree #######


dt_model2 <- rpart(
  loan_default ~ ., 
  data = Final_TRAINING_Dataset, 
  method = "class",
  control = rpart.control(minsplit = 10, cp = 0.001, maxdepth = 10)
)



# Predict probabilities
dt_pred_prob <- predict(dt_model2, Final_TEST_Dataset, type = "prob")
# Predict class labels
dt_pred_class <- predict(dt_model2, Final_TEST_Dataset, type = "class")
dt_pred_class <- factor(dt_pred_class)

dt_pred_probs_list <- predict(dt_model2, Final_TEST_Dataset[, -15], type = "prob")
dt_pred_prob <- dt_pred_probs_list[,2]

dt_pred_labels <- ifelse(dt_pred_prob > 0.5, 1, 0)
dt_pred_labels <- factor(dt_pred_labels)

Actual <- factor(Final_TEST_Dataset$loan_default)

#Confusion Matrix
conf_matrixDT <- caret::confusionMatrix(dt_pred_labels, Actual, positive = "1")
conf_matrixDT



accuracy  <- conf_matrixDT$overall['Accuracy']
precision <- conf_matrixDT$byClass['Precision']
recall    <- conf_matrixDT$byClass['Recall']
f1        <- conf_matrixDT$byClass['F1']

accuracy
precision
recall
f1

# ROC CURVE
roc(Final_TEST_Dataset$loan_default, dt_pred_prob[,2])

roc_obj <- roc(Actual, dt_pred_prob[,2])
auc_value <- pROC::auc(roc_obj)

#PLOT
plot(roc_obj, col = "blue", main = "ROC Curve")
legend("bottomright", legend = paste("AUC =", round(auc_value, 3)), col = "blue", lwd = 2)


### Tree plot

# Nice plot
rpart.plot(dt_model2, type = 2, extra = 104, fallen.leaves = TRUE)

printcp(dt_model2)     # see complexity parameter table
plotcp(dt_model2)      # plot cross-validation error

dt_pruned <- prune(dt_model2, cp = dt_model2$cptable[which.min(dt_model2$cptable[,"xerror"]), "CP"])
rpart.plot(dt_pruned, type = 2, extra = 104)

dt_model2$variable.importance

#########################################################################
######################### Random Forest #################################

set.seed(42)  

#Final Model 
set.seed(42) 
rf_model <- randomForest(
  loan_default ~ ., 
  data = Final_TRAINING_Dataset,
  ntree = 500,        # number of trees
  mtry = 5,           # number of variables tried at each split
  importance = TRUE   # store variable importance
)

print(rf_model)

importance(rf_model)
varImpPlot(rf_model)

# Probabilities
rf_pred_prob <- predict(rf_model, Final_TEST_Dataset, type = "prob")

# Predicted classes
rf_pred_class <- predict(rf_model, Final_TEST_Dataset, type = "response")


Actual<-factor(Final_TEST_Dataset$loan_default)
rf_pred_class <- factor(rf_pred_class)

#Confusion Matrix 
conf_matrixRF <- caret::confusionMatrix(rf_pred_class, Actual, positive = "1")
conf_matrixRF

accuracy  <- conf_matrixRF$overall['Accuracy']
precision <- conf_matrixRF$byClass['Precision']
recall    <- conf_matrixRF$byClass['Recall']
f1        <- conf_matrixRF$byClass['F1']

accuracy
precision
recall
f1


#Roc Curve 
library(pROC)

roc_obj <- roc(Final_TEST_Dataset$loan_default, rf_pred_prob[,2])
auc_value <- pROC::auc(roc_obj)
roc_obj

plot(roc_obj, col = "blue", main = "ROC Curve")
legend("bottomright", legend = paste("AUC =", round(auc_value, 3)), col = "blue", lwd = 2)


################################################################################################################################################
################################################################################################################################################
############ XGBOOST

library(xgboost)
library(caret)
library(pROC)


# Ensure loan_default is numeric

train_set <-Final_TRAINING_Dataset
Test_set <-Final_TEST_Dataset

Test_set <- as.data.frame(Test_set)
train_set <- as.data.frame(train_set)

train_set$loan_default <- as.numeric(as.character(train_set$loan_default))
Test_set$loan_default <- as.numeric(as.character(Test_set$loan_default))

# Separate features and target
x_train <- as.matrix(train_set[, -which(names(train_set) == "loan_default")])
y_train <- train_set$loan_default

x_test <- as.matrix(Test_set[, -which(names(Test_set) == "loan_default")])
y_test <- Test_set$loan_default

# Ensure both train and test matrices have identical column names
colnames(x_test) <- colnames(x_train)

# Convert both train and test to xgb.DMatrix with same column names
# Create DMatrix after syncing column names
dtrain <- xgb.DMatrix(data = x_train, label = y_train)
dtest <- xgb.DMatrix(data = x_test, label = y_test)




set.seed(42)

#FINAL Model
xgb_model1 <- xgboost(
  data = dtrain,
  objective = "binary:logistic",
  nrounds = 1000,
  eta = 0.05,
  max_depth = 6,
  min_child_weight = 5,
  subsample = 0.8,
  colsample_bytree = 0.8,
  gamma = 1,
  lambda = 1,
  alpha = 0,
  eval_metric = "auc",
  verbose = 0
)

# Predict
xgb_probs <- predict(xgb_model1, dtest)


xgb_probs <- predict(xgb_model1, x_test)
roc_obj <- roc(y_test, xgb_probs, levels = c("0", "1"), direction = "<")
plot(roc_obj, print.thres = "best", print.auc = TRUE, col = "darkblue", main = "XGBoost ROC")

best_thresh <- coords(roc_obj, "best", ret = "threshold", best.method = "youden")
print(best_thresh)


# Ensure threshold is a scalar
best_thresh_val <- as.numeric(best_thresh)


xgb_pred_labels <- ifelse(xgb_probs > 0.5, 1, 0)

# Ensure factors have the same levels in the same order
xgb_pred_labels <- factor(xgb_pred_labels, levels = c(0, 1))
y_test_factor <- factor(y_test, levels = c(0, 1))
str(xgb_pred_labels)
str(y_test_factor)

length(xgb_pred_labels)
length(y_test_factor)

# Generate confusion matrix
conf_matrix_xgboost<-caret::confusionMatrix(xgb_pred_labels, y_test_factor, positive = "1")
conf_matrix_xgboost
roc_obj

accuracy  <- conf_matrix_xgboost$overall['Accuracy']
precision <- conf_matrix_xgboost$byClass['Precision']
recall    <- conf_matrix_xgboost$byClass['Recall']
f1        <- conf_matrix_xgboost$byClass['F1']

accuracy
precision
recall
f1


#ROC CURVE 
roc_obj <- roc(y_test, xgb_probs)
auc_value <- pROC::auc(roc_obj)


#PLOT
plot(roc_obj, col = "blue", main = "ROC Curve")
legend("bottomright", legend = paste("AUC =", round(auc_value, 3)), col = "blue", lwd = 2)


###########################################################################################################
###########################################################################################################
########## KNN ####


Final_TEST_Dataset_num<-Final_TEST_Dataset
Final_TEST_Dataset_num$loan_default<-as.numeric(Final_TEST_Dataset_num$loan_default)

Final_TRAINING_Dataset_num<-Final_TRAINING_Dataset
Final_TRAINING_Dataset_num$loan_default<-as.numeric(Final_TRAINING_Dataset_num$loan_default)

train_x <- Final_TRAINING_Dataset_num[, -which(names(Final_TRAINING_Dataset_num) == "loan_default")]
test_x  <- Final_TEST_Dataset_num[, -which(names(Final_TEST_Dataset_num) == "loan_default")]
train_y <- Final_TRAINING_Dataset_num$loan_default

# Ensure target is a factor
train_y <- as.factor(train_y)

# 
set.seed(42)
knnF_model <- FNN::knn(
  train = train_x,
  test  = test_x,
  cl    = train_y,
  k = 400,
  prob = TRUE
)

actual_labels <- as.factor(Final_TEST_Dataset_num$loan_default)
pred_labels <- knnF_model

conf_matrix_KnnF <- caret::confusionMatrix(pred_labels, actual_labels)
conf_matrix_KnnF

f1        <- conf_matrix_KnnF$byClass['F1']
f1


prob_attr <- attr(knnF_model, "prob")

pred_probs <- ifelse(pred_labels == "1", prob_attr, 1 - prob_attr)

# Make sure actual labels are numeric (0,1) or factor with levels c(0,1)
actual_labels <- factor(Final_TEST_Dataset_num$loan_default)

# Compute ROC and AUC
roc_obj <- roc(actual_labels, pred_probs)
auc_value <- pROC::auc(roc_obj)
auc_value

#PLOT
plot(roc_obj, col = "blue", main = "ROC Curve")
legend("bottomright", legend = paste("AUC =", round(auc_value, 3)), col = "blue", lwd = 2)


#########################################################################################################################################################
#########################################################################################################################################################
##Gaussian Mixture Models (GMM)


library(mclust)

# Remove target variable
data_unsupervised <- Final_TRAINING_Dataset[, -which(names(Final_TRAINING_Dataset) == "loan_default")]



# Fit Gaussian Mixture Model
gmm_model <- Mclust(data_unsupervised)

# Cluster assignments (predicted cluster for each observation)
cluster_assignments <- gmm_model$classification


# Cross-tabulate clusters vs actual labels
table(Cluster = cluster_assignments, Actual = Final_TRAINING_Dataset$loan_default)

Final_TRAINING_Dataset$loan_default <- as.numeric(as.character(Final_TRAINING_Dataset$loan_default))
# Identify which cluster corresponds to "1"
cluster_mean <- tapply(Final_TRAINING_Dataset$loan_default, cluster_assignments, mean)
cluster_mean


# Map clusters: if mean default rate > 0.5, call it "1", else "0"
pred_classes <- ifelse(cluster_mean[cluster_assignments] > 0.5, 1, 0)

table(pred_classes)
table(Final_TRAINING_Dataset$loan_default)


library(caret)

conf_matrix_GMM <- caret::confusionMatrix(
  factor(pred_classes),
  factor(Final_TRAINING_Dataset$loan_default),
  positive = "1"
)

conf_matrix_GMM

f1        <- conf_matrix_GMM$byClass['F1']
f1



library(pROC)

# Extract probability that each record belongs to each cluster
cluster_probs <- gmm_model$z  

# Identify the “default” cluster = highest mean default rate
default_cluster <- which.max(cluster_mean)

# Extract probabilities of belonging to that cluster
prob_default <- cluster_probs[, default_cluster]

# Compute AUC

# Compute ROC and AUC
roc_obj <- pROC::roc(as.numeric(Final_TRAINING_Dataset$loan_default), prob_default)
auc_value <- pROC::auc(roc_obj)

# Print AUC
auc_value

plot(roc_obj, col = "blue", lwd = 2,
     main = paste("GMM ROC Curve (AUC =", round(auc_value, 3), ")"))


#PLOT
plot(roc_obj, col = "blue", main = "ROC Curve")
legend("bottomright", legend = paste("AUC =", round(auc_value, 3)), col = "blue", lwd = 2)





