# Set work directory for local system
setwd("C:\\Users\\stack\\Dropbox\\Berkeley\\Spring_2018\\Lab 1\\Cancer_EDA\\w203.4_lab1")

# Import car
library(car)

# Import Data 
df <- read.csv("cancer.csv")

summary(df)

# Derive outcome CaseRate from abgAnnCount to normalize for population

df["CaseRate"] <- (df$avgAnnCount/df$popEst2015) * 100000

df["RaceCat"] <- apply(df[,c("PctBlack", "PctAsian", "PctWhite", "PctOtherRace")], 1, function(x) {
    check = max(x)
    if (check == x[1]) {
      return <- "Black"
    } else if (check == x[2]) {
        return <- "Asian"
    } else if (check == x[3]) {
      return <-"White"
    } else {
      return <-"Other"
    }
})

barplot(prop.table(table(df$RaceCat)), ylim=c(0,1),
        ylab="% Frequency", xlab="Race Category",
        main="Distribution of Race")


# Create groupings of columns I want to look at
outcome <- c("Geography", "CaseRate", "deathRate")

pop <- c("popEst2015", "BirthRate")

age <- c("MedianAge", "MedianAgeFemale", "MedianAgeMale")

household <- c("AvgHouseholdSize", "PercentMarried")

race <- c("PctBlack", "PctAsian", "PctWhite", "PctOtherRace")

###########################
# Population vs. outcomes #
###########################
df_pop <- df[df$CaseRate <= 2000,c(outcome,pop)]

summary(df_pop)

# Case Rate
scatterplotMatrix(data=df_pop, ~CaseRate + popEst2015 + BirthRate,
                  diagonal="histogram")

cor(df_pop[, c("CaseRate", "popEst2015", "BirthRate")])

### Note - Don't see anything of interest here

# Death Rate - NEED TO ADJUST SOMEHOW
scatterplotMatrix(data=df_pop, ~deathRate + popEst2015 + BirthRate,
                  diagonal="histogram")
cor(df_pop[, c("deathRate", "popEst2015", "BirthRate")])


####################
# Age vs. outcomes #
####################

df_age <- df[df$CaseRate <= 2000,c(outcome,age)]
df_age2 <- df[df$CaseRate > 2000,c(outcome,age)]

summary(df_age)

# Excluding clear outliers because median age > 100 is implausible
# Because of this, looking at MedianAge separately from individual sexes
scatterplotMatrix(data=df_age[df_age$MedianAge < 100, ], ~CaseRate + MedianAge,
                  diagonal="histogram")

cor(df_age[df_age$MedianAge < 100, c("CaseRate", "MedianAge")])

scatterplotMatrix(data=df_age, ~CaseRate + MedianAgeFemale + MedianAgeMale,
                  diagonal="histogram")

cor(df_age[, c("CaseRate", "MedianAgeFemale", "MedianAgeMale")])

# Death rate
scatterplotMatrix(data=df_age[df_age$MedianAge < 100, ], ~deathRate + MedianAge,
                  diagonal="histogram")

cor(df_age[df_age$MedianAge < 100, c("deathRate", "MedianAge")])

cor(df_age[, c("deathRate", "MedianAgeFemale", "MedianAgeMale")])

# When we restrict the same to case rates < .02 (Poor way of balancing for outliers)
# there's a strong correlation between age and case rates, which is to be expected 
# because the incidence rate of cancer naturally grows as people get older.
# 
# Interestingly, the death rate doesn't have a strong correlation with 
# Median ages.
#
# The characteristics of the case rates > .02 should be examined further

###########################
# Households vs. outcomes #
###########################

df_house <- df[df$CaseRate <= 2000,c(outcome, household)]

summary(df_house)

scatterplotMatrix(data=df_house, ~CaseRate + AvgHouseholdSize + PercentMarried,
                  diagonal="histogram")

cor(df_house[, c("CaseRate", "AvgHouseholdSize", "PercentMarried")])

# Aversage household size has a slight negative relationship with the CaseRate, while
# PercentMarried has a slight postiive relationship, which is a bit interesting.

#####################
# Race vs. outcomes #
#####################

df_race <- df[df$CaseRate <= 2000,c(outcome, race)]


summary(df_race)

scatterplotMatrix(data=df_race, ~CaseRate + PctBlack + PctAsian + PctWhite + PctOtherRace,
                  diagonal="histogram")

cor(df_race[, c("CaseRate", "PctBlack", "PctAsian", "PctWhite", "PctOtherRace")])

scatterplotMatrix(data=df_race, ~deathRate + PctBlack + PctAsian + PctWhite + PctOtherRace,
                  diagonal="histogram")

cor(df_race[, c("deathRate", "PctBlack", "PctAsian", "PctWhite", "PctOtherRace")])

# Make a function to bin the % Races to groups of 25% and boxplot
bplot_race <- function(race, dep, racec, rate) {
  race_bin = cut(race, breaks = c(0, 25, 50, 75, 100), label = 
                 c("LOW", "LOW-MID", "MID-HIGH", "HIGH"))
  
  boxplot(dep ~ race_bin, data=df_race, 
          main = paste("Percent ", racec, " Split by 25%"),
          ylab=paste(rate, " Rate"))
}

# Black
bplot_race(df_race$PctBlack,df_race$CaseRate, "Black", "Case")
bplot_race(df_race$PctBlack,df_race$deathRate, "Black", "Death")

# Asian
bplot_race(df_race$PctAsian,df_race$CaseRate,"Asian", "Case")
bplot_race(df_race$PctAsian,df_race$deathRate, "Asian", "Death")

# White
bplot_race(df_race$PctWhite,df_race$CaseRate, "White", "Case")
bplot_race(df_race$PctWhite,df_race$deathRate, "White", "Death")

# Other
bplot_race(df_race$PctOtherRace,df_race$CaseRate, "Other Race", "Case")
bplot_race(df_race$PctOtherRace,df_race$deathRate, "Other Race", "Death")

# PctBlack in a commuity doesn't seem to have a significant relationship,
# while PctAsian seems to have a negative relationship. PctWhite is the only race
# with a positive relationship, and PctOtherRace has the most significant negative
# relationship
#
# Asian and Black have higher standard deviations than Asian or PctOtherRace

summary(df$RaceCat)
cat("Mean Death Rate in Black communities: ", mean(df[df$RaceCat == "Black", ]$deathRate))
cat("Mean Death Rate in White communities: ", mean(df[df$RaceCat == "White", ]$deathRate))
cat("Mean Death Rate in Asian communities: ", mean(df[df$RaceCat == "Asian", ]$deathRate))
cat("Mean Death Rate in communities other race: ", mean(df[df$RaceCat == "Other", ]$deathRate))

cat("Mean Incidence Rate in Black communities: ", 
    mean(df[df$RaceCat == "Black" & df$CaseRate < 2000, ]$CaseRate))
cat("Mean Incidence Rate in White communities: ", 
    mean(df[df$RaceCat == "White" & df$CaseRate < 2000, ]$deathRate))
cat("Mean Incidence Rate in Asian communities: ", 
    mean(df[df$RaceCat == "Asian" & df$CaseRate < 2000, ]$deathRate))
cat("Mean Incidence Rate in communities other race: ", 
    mean(df[df$RaceCat == "Other" & df$CaseRate < 2000, ]$deathRate))

test <- function(race) {
  x <- df[df$CaseRate <= 2000 & df$RaceCat == race, ]
  
  cat("Correlation between Death Rate and Incidence Rate for ", race, " Communities",
      cor(x$CaseRate, x$deathRate))
}

test("Black")
test("Asian")
test("White")
test("Other")


x <- names(df)[!names(df) %in% c("deathRate", "X")]

vars <- c()
cors <- c()

for (i in x) { 
  if (class(df[ ,i]) == "numeric") {
    vars <- c(vars,i)
    cors <- c(cors,round(cor(df$deathRate,df[ ,i]),digits = 4))
  }
}

cor_df = data.frame(vars,cors)
cor_df <- cor_df[order(abs(cor_df$cors),decreasing=T),]
