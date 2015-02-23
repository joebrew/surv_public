

# Read in # what does skip and stingAsFactors do?
setwd("/home/benbrew/Documents/tony_b/public_data")
race <- read.csv("RaceGender.csv", skip = 6, stringsAsFactors = FALSE)
pop <- read.csv("SchoolPop.csv", skip = 5, stringsAsFactors = FALSE)
lunch <- read.csv("FreeLunch.csv", skip = 5, stringsAsFactors = FALSE)

################################
# Clean up pop

# First, check out the structure of each variable
str(pop)

# Total Membership is our column of interest, but it has problems
# (commas in the numbers, and it's not numeric)
# So, we'll make a better column:

# remove commas, #here you're taking out commas and replacing them with nothing right?
pop$totmem <- gsub(",", "", pop$TOTAL.MEMBERSHIP)

# make numeric
pop$totmem <- as.numeric(pop$totmem)

# Let's do the same (remove commas and make numeric)
# for all the other grade columns
for (i in c("PK", "K", paste0("X", 1:12))){
  pop[,i ] <- 
    as.numeric(gsub(",", "", pop[, i]))
}


# Check out the structure to see if it worked
str(pop)

# Go through each school, and see if they have more 
# prek, elem, mid, or high school students
pop$type <- NA
for (i in 1:nrow(pop)){
  pk <- sum(pop[i, "PK"], na.rm = TRUE)
  elem <- sum(as.numeric(pop[i,c("K", "X1", "X2", "X3", "X4", "X5")]), na.rm = TRUE)
  mid <- sum(as.numeric(pop[i, c("X6", "X7", "X8")]), na.rm = TRUE)
  high <- sum(as.numeric(pop[i, c("X9", "X10", "X11", "X12")]), na.rm = TRUE)
  
  types <- c(pk, elem, mid, high)
  type_names <- c("pk", "elem", "mid", "high")
  pop$type[i] <-  type_names[which.max(types)][1] #I don't really understand what you're doing here
}

# Remove unecessary columns
pop <- pop[,c("DISTRICT.NAME","SCHL..", "SCHOOL.NAME", "totmem", "type")]

# Rename some columns
names(pop) <- c("district","school_number", "school", "totmem", "type")

# Check out our cleaned up dataframe
head(pop)


################################
# Clean up lunch

# remove rows with "District record" in it
lunch <- lunch[which(lunch$School.Name != "DISTRICT RECORD"),]

# Fix the terrible column names
names(lunch)
names(lunch) <- c("district_number", "district", "school_number", "school",
                  "totmem", "free", "reduced", "provision2", "direct_cert")
head(lunch)

# Check out the structure of each column
str(lunch)

# Loop through a few of them to remove commas and make numeric
for (i in c("totmem", "free", "reduced", "provision2", "direct_cert")){
  
  # get rid of commas and make numeric each column mentioned above
    temp <- as.numeric(gsub(",", "", lunch[,i])) 
    
    # in these same columns, fill all the NAs with 0
    temp[which(is.na(temp))] <- 0
    
    # Once the column is fixed, put it back in its place
    lunch[,i] <- temp 
}
# I don't understand why we need a temp column
head(lunch)

# Make a free/reduced total
lunch$free_reduced <- lunch$free + lunch$reduced

# Make a free/reduced percentage
lunch$per_fr <- lunch$free_reduced / lunch$totmem * 100

#take out unnecessary columns 

lunch <- lunch[,c("district", "school_number", "school", "totmem", "free", "reduced",
                  "free_reduced", "per_fr")]

# Check out your fine cleaning work
head(lunch)


################################
# Clean up race
head(race)

#rename columns 

names(race) <- c("district1", "district", "school_number", "school", "grade", "white", "black",
                 "hispanic","asian", "hawian", "native", "multi", "female", "male", "total" )

#remove commas #!!!!!!!!!! DON'T MAKE GRADE NUMERIC - IT HAS THINGS LIKE K AND PK
for (i in c("school_number", "white", "black", "hispanic", "asian", "hawian", "native", "multi", 
            "female", "male", "total")){
  race[,i ] <- 
    as.numeric(gsub(",", "", race[,i]))
}

#structure of each column 
str(race)

#remove uneeded rows

race <- race[,c("district", "school_number", "school", "grade", "white", "black", "hispanic", "asian", "hawian",
                "native", "multi", "female", "male", "total")]

###################################################
#Not sure what to do with this. do we just want totals for each school, in that case
#we could use the school total row at the end of each school, but none of them have a 
#unique name. And honestly, I suck at writing loops. Can you help me with this?
library(dplyr)

# Group race together by school (don't need by grade)
race_by_school <- race %>%
  group_by(school, district, school_number) %>%
  summarise(total_from_race = sum(total, na.rm = TRUE),
            total_white = sum(white, na.rm = TRUE),
            total_black = sum(black, na.rm = TRUE),
            total_hispanic = sum(hispanic, na.rm = TRUE),
            total_asian = sum(asian, na.rm = TRUE))

# NOW We've got 3 datasets, each with one row = one school
# MERGE all three datasets together by school number

pop1 <- pop[,c("district", "school", "totmem", "type")]

pop1 <- pop1$
pop1 <- pop1 %>% 
  group_by(school, type) %>%
  

df <- left_join(x = lunch, 
                y = race_by_school)

pop1$totmempop <- pop1$totmem; pop1$totmem <- NULL
df <- left_join(x = df,
                y = pop1)

# Clean up df a bit
df <- df[,c("district","school_number","school", "totmem", "type", "total_from_race", 
            "total_white", "total_black", "total_hispanic", "total_asian",
            "free_reduced", "per_fr")] 

#write df as csv

write.csv(df, "/home/benbrew/Documents/tony_b/public_schools.csv")

# Get total students, schools, etc. by countonyty
by_county <- pop %>%
group_by(district) %>% 
  summarise(total_schools = n(),
            total_students = sum(totmem, na.rm = TRUE),
            total_schools_pk = length(type[which(type == "pk")]),
            total_schools_elem = length(type[which(type == "elem")]),
            total_schools_mid = length(type[which(type == "mid")]),
            total_schools_high = length(type[which(type == "high")]))

# Get racial breakdown by county
by_county2 <- race %>%
  group_by(district, grade) %>%
  summarise(white_students = sum(white, na.rm = TRUE),
            black_students = sum(black, na.rm = TRUE),
            hispanic_students = sum(hispanic, na.rm = TRUE))


