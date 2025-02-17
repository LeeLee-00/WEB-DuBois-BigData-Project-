library(openxlsx)

### Cleaning 2020 Voting Data ----

urlToRead <- "https://www2.census.gov/programs-surveys/cps/tables/p20/585/table04b.xlsx"
testFrame <- read.xlsx(urlToRead)

# Change State NA to the state name
testFrame[6:15, 1] <- "US"
testFrame[17:26, 1] <- "ALABAMA"
testFrame[28:37, 1] <- "ALASKA"
testFrame[39:48, 1] <- "ARIZONA"
testFrame[50:59, 1] <- "ARKANSAS"
testFrame[61:70, 1] <- "CALIFORNIA"
testFrame[72:81, 1] <- "COLORADO"
testFrame[83:92, 1] <- "CONNECTICUT"
testFrame[94:103, 1] <- "DELAWARE"
testFrame[105:114, 1] <- "DISTRICT OF COLUMBIA"
testFrame[116:125, 1] <- "FLORIDA"
testFrame[127:136, 1] <- "GEORGIA"
testFrame[138:147, 1] <- "HAWAII"
testFrame[149:158, 1] <- "IDAHO"
testFrame[160:169, 1] <- "ILLINOIS"
testFrame[171:180, 1] <- "INDIANA"
testFrame[182:191, 1] <- "IOWA"
testFrame[193:202, 1] <- "KANSAS"
testFrame[204:213, 1] <- "KENTUCKY"
testFrame[215:224, 1] <- "LOUISIANA"
testFrame[226:235, 1] <- "MAINE"
testFrame[237:246, 1] <- "MARYLAND"
testFrame[248:257, 1] <- "MASSACHUSETTS"
testFrame[259:268, 1] <- "MICHIGAN"
testFrame[270:279, 1] <- "MINNESOTA"
testFrame[281:290, 1] <- "MISSISSIPPI"
testFrame[292:301, 1] <- "MISSOURI"
testFrame[303:312, 1] <- "MONTANA"
testFrame[314:323, 1] <- "NEBRASKA"
testFrame[325:334, 1] <- "NEVADA"
testFrame[336:345, 1] <- "NEW HAMPSHIRE"
testFrame[347:356, 1] <- "NEW JERSEY"
testFrame[358:367, 1] <- "NEW MEXICO"
testFrame[369:378, 1] <- "NEW YORK"
testFrame[380:389, 1] <- "NORTH CAROLINA"
testFrame[391:400, 1] <- "NORTH DAKOTA"
testFrame[402:411, 1] <- "OHIO"
testFrame[413:422, 1] <- "OKLAHOMA"
testFrame[424:433, 1] <- "OREGON"
testFrame[435:444, 1] <- "PENNSYLVANIA"
testFrame[446:455, 1] <- "RHODE ISLAND"
testFrame[457:466, 1] <- "SOUTH CAROLINA"
testFrame[468:477, 1] <- "SOUTH DAKOTA"
testFrame[479:488, 1] <- "TENNESSEE"
testFrame[490:499, 1] <- "TEXAS"
testFrame[501:510, 1] <- "UTAH"
testFrame[512:521, 1] <- "VERMONT"
testFrame[523:532, 1] <- "VIRGINIA"
testFrame[534:543, 1] <- "WASHINGTON"
testFrame[545:554, 1] <- "WEST VIRGINIA"
testFrame[556:565, 1] <- "WISCONSIN"
testFrame[567:576, 1] <- "WYOMING"

# Removing unneeded rows and columns
testFrame <- testFrame[-577:-583, ]
testFrame <- testFrame[-1:-4, ]
testFrame <- testFrame[ , c(1:3, 5, 10)]

# Adding column names
colnames(testFrame) <- c("State", "SexRaceHispanic", "Population", 
                         "Registered", "Voted")

# Convert State and SexRaceHispanic to factors
testFrame$State <- as.factor(testFrame$State)
testFrame$SexRaceHispanic <- as.factor(testFrame$SexRaceHispanic)

#Converting Columns to Numeric
testFrame$Population <- as.numeric(testFrame$Population)
testFrame$Registered <- as.numeric(testFrame$Registered)
testFrame$Voted <- as.numeric(testFrame$Voted)

# Reset row numbers
rownames(testFrame) = NULL