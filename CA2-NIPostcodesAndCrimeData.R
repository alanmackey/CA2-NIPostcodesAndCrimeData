# Read the file, we use file.choose to avoid directory issues. Alternatively we could 
# assume the working directory and just hardcode the file name. To simplify future processing 
# we will convert emtpy fields to NA and prevent factorisation at this point 
# Section 1 (a) (i)
NIPostcodes <- read.csv(file.choose(), header=FALSE, na.strings=c("", " ", "NA"), 
                        stringsAsFactors = FALSE)
# (ii) Look at the structure of the data frame that we have read in
str(NIPostcodes)
summary(NIPostcodes) # Not requested but useful
# (iii) Show the first 10 rows of the data frame
head(NIPostcodes, 10)

# Section 1 (b)
# Add a suitable title for each attribute of the data.
headers <- c('Organisation','Sub-building', 'Building',
             'Number', 'Thorfare', 'Alt-Thorfare', 'Secondary-Thorfare', 'Locality',
             'Townland', 'Town', 'County', 'Postcode', 'x-coordinates', 'y-coordinates',
             'Primary-Key')
colnames(NIPostcodes) <- headers
head(NIPostcodes, 10)

# Section 1 (c)
# This was handled when the file was read
# It is best to keep the values at this point but make them NA as we dont know what values 
# are important or not and what can be imputed

# Section 1 (d)
# Show the sum and mean of the values with NA
sumna_postcodes <- sapply(NIPostcodes, function(x) sum(is.na(x)))
sumna_postcodes
meanna_postcodes <- sapply(NIPostcodes, function(x) mean(is.na(x)))
meanna_postcodes

# For the whole dataset (not required)
sum(is.na(NIPostcodes))
mean(is.na(NIPostcodes))

# Section 1 (e)
# Modify the County attribute to be a categorising factor
NIPostcodes$County <- as.factor(NIPostcodes$County)
str(NIPostcodes$County)
levels(NIPostcodes$County)

# Section 1 (f)
# Move the primary key identifier to the start of the dataset
NIPostcodes <- NIPostcodes[, c(15,1:14)]
str(NIPostcodes)
head(NIPostcodes)


# Section 1 (g) 
# Create a new dataset called Limavady_data. Store within it only information that has locality, 
# townland and town containing the name Limavady. Store this information in an external csv file 
# called Limavady.
limavady_data <- subset(NIPostcodes, grepl("LIMAVADY", NIPostcodes$Locality) 
                        & grepl("LIMAVADY", NIPostcodes$Townland) & grepl("LIMAVADY", NIPostcodes$Town))
str(limavady_data)
head(limavady_data)
write.csv(limavady_data, "Limavady.csv")

#If 'any' was required
#limavady_data <- subset(NIPostcodes, grepl("LIMAVADY", NIPostcodes$Locality) 
#                        & grepl("LIMAVADY", NIPostcodes$Townland) & grepl("LIMAVADY", NIPostcodes$Town))

# Section 1 (h) 
# Save the modified NIPostcode dataset in a csv file called 
write.csv(NIPostcodes, file = "CleanNIPostcodeData.csv")


#############################################################
###                 Section 2                             ###
#############################################################

# Section 2 (a) 
# Amalgamate all the crime csv files   ######
CSV_Files <- list.files(path = 'NI Crime Data', recursive = TRUE, 
                        pattern = "*northern-ireland-street.csv", full.names = TRUE)
AllNICrimeData <- do.call(rbind, lapply(CSV_Files, read.csv,header = TRUE, 
                        stringsAsFactors = FALSE))
str(AllNICrimeData)
head(AllNICrimeData, 10)

nrow(AllNICrimeData) # Count and Show the number of rows in the data frame

ncol(AllNICrimeData) # Count and Show the number of columns in the data frame

write.csv(AllNICrimeData, file = "NI Crime Data/AllNICrimeData.csv", row.names = FALSE)

# Section 2 (b) 
#  Modify the structure of the newly created AllNICrimeData csv file and remove
# the following attributes: CrimeID, Reported by, Falls within, LSOA code, LSOA name,
# last outcome and context. Show the structure of the modified file

head(AllNICrimeData)
AllNICrimeData <- AllNICrimeData[, c(2,5,6,7,10)]
head(AllNICrimeData)
str(AllNICrimeData)
# if required we now write the file back using the amended dataframe
write.csv(AllNICrimeData, file = "NI Crime Data/AllNICrimeData.csv", row.names = FALSE)

# Section 2 (c)
# Factorise the Crime type attribute. Show the modified structure
AllNICrimeData$Crime.type <- as.factor(AllNICrimeData$Crime.type)
str(AllNICrimeData)

# Section 2 (d)
# Modify the AllNICrimeData dataset so that the Location attribute contains only a
# street name. For example, the attribute value “On or near Westrock Square” should
# be modified to only contain “Westrock Square”. Modify the resultant empty location
# attributes with a suitable identifier.
AllNICrimeData$Location <- gsub("On or near ", "", AllNICrimeData$Location)
AllNICrimeData[AllNICrimeData$Location == "", "Location"] <- NA 

str(AllNICrimeData)
head(AllNICrimeData)

# Section 2 (e) 
# Create a function called find_a_postcode that takes as an input each location
# attribute from AllNICrimeData and finds a suitable postcode value from the
# postcode dataset. Use the CleanNIPostcodeData dataset you created in section 1
# as the reference data to find postcodes. If there are several postcodes discovered
# with the same location, choose the most popular postcode for that location. Store
# the output from the find_a_postcode function in a suitably named variable. Show
# the structure and number of values in this variable


library(data.table)     # Use data.table for better reads and better with large data sets
postcodes <- fread("CleanNIPostcodeData.csv", select = c(7,14))   # We just need the two columns
setDT(AllNICrimeData) # Convert All Crime to data.table by reference to speed things up

# Select rows where location is not NA and then pass to sample 1000 of them
random_crime_sample <- AllNICrimeData[!is.na(AllNICrimeData$Location),][sample(.N, 1000)]
head(random_crime_sample)
str(random_crime_sample)

# function to find a postcode based on a location that is passed. Returns a charater postcode or
# NA if none are found. Progress bar is put here as we are using sapply(), it is really bad programming 
# practice as the function is for a simgle location and normally this would be within a for loop. 
# Would remove this in production, of course the tables have a global contect too so 
find_a_postcode <- function(location) {

  ctr <<- ctr+1  # <<- keeps context between calls
  setTxtProgressBar(pb, ctr)

# Using data.table chaining. The first part does a select of the number of rows in J for a match 
# to location in i and then grouped by Postcode.
# The output is chained to selected only rows where there is values, ie. not NA
# The third selects the postcode where N is max ie. most number of rows in original table
# The check for NA is redundant because of how the random_crime_sample is done but left for completeness 
  if (!is.na(location)) {
    postcode_match <- 
      postcodes[grepl(toupper(location), toupper(postcodes$Thorfare)), 
                .N, by=Postcode][!is.na(Postcode)][which.max(N), Postcode]
      } else {
    postcode_match <- NA
  }
# It is possible that there was no rows so we replace a character(0) with an NA
  if (length(postcode_match) == 0) {
    postcode_match <- NA
  }
  
   return(postcode_match)
}


#Setup for a progress bar because this is very slow and gives confidence that it is progressing
pb <- txtProgressBar(min = 0, max = nrow(random_crime_sample), style = 3)
ctr <-0

# Use sapply to call find_a_postcode for each location in random_crime_sample. 'simplify = TRUE' gets 
# it to return the simplest form avoiding a list 
random_crime_sample$postcode <- sapply(random_crime_sample$Location, find_a_postcode, simplify = TRUE)
head(random_crime_sample, 10)
str(random_crime_sample)
nrow(random_crime_sample)

close(pb) # Close the progress bar

# Assume that the count of rows was how many now have a postcode added
rows_with_postcodes <-nrow(random_crime_sample[!is.na(postcode)])
rows_with_postcodes

# Write using fwrite (used with data.table), much quiker than write.csv
fwrite(random_crime_sample, "random_crime_sample.csv")

# As required although not really necessary, we could just use the random_crime_sample
# directly, probably legacy of changes to asssignment
updated_random_sample <- random_crime_sample

# Extract rows where postcode contains 'BT1'
chart_data <- updated_random_sample[ grepl( "BT1",postcode)] 
chart_data <- chart_data[order(postcode, Crime.type)] # Now sort by postcode and crime.type
summary(chart_data$Crime.type)  # Sumamry of crime.type column 
summary(chart_data)             # Summary of complete dataframe
head(chart_data)

install.packages("ggplot2")   
install.packages("ggthemes")
library(ggplot2)              # ggplot base library
library(ggthemes)             # some aditional themes
theme_set(theme_base())       # set base theme for tick marks, scale, etc 
theme_update(plot.title = element_text(hjust = 0.5))      # Centre the title
pl <-ggplot(data=chart_data, aes(x=Crime.type)) # Set up the data and aesthethics layers
pl1 <-pl+geom_bar(fill="blue") + ggtitle("Bar plot of Crimes in Postcodes starting with BT1") +
   labs(y="Number of Crimes", x = "Type of Crime")
pl1+theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Make labels diagonal to fit


# In case the interpetation of (g) was incorrect and the ordering of crime.type was not on 
# the crimes themseleves but the frequency this is easily down using the forcats library 
# and making the fill use the fct_infreq() function
install.packages("forcats")
library(forcats)
pl <- ggplot(chart_data, aes(x = fct_infreq(Crime.type), fill = fct_infreq(Crime.type))) + 
  geom_bar(width = 1, colour = "black", show.legend = FALSE) + xlab("Var1")
pl1 <- pl + ggtitle("Bar plot of Crimes in Postcodes starting with BT1") +
  labs(y="Number of Crimes", x = "Type of Crime")
pl1+theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Just using standard R plotting
#plot.new()
#crimes <- table(chart_data$Crime.type)
#crimes
#barplot(crimes, main= "Bar plot of Crimes in Postcodes starting with BT1",
#        xlab = "Crime Type", ylab = "No of Crimes", las=2)

