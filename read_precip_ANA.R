# Extracts and organizes precipitation data from files
# provided by the Brazilian National Water Agency (ANA)

library(lubridate) # package for date manipulation
library(tools) # for using the "file_path_sans_ext" function

# 1) Select files to be processed. All files should be on the same input folder

# set input and output folders. EDIT PATHS TO YOUR WORKFLOW.
in.folder <- "GoogleDrive/EcoDyn/People/Joao/teste_read_precip/"
out.folder <- "GoogleDrive/EcoDyn/People/Joao/teste_read_precip/"

# list all csv files in the specified folder
in.files <- list.files(in.folder, pattern = "csv$", full.names=T)

# extract names without path and extension
in.names <- basename(file_path_sans_ext(in.files))

for (f in c(1:length(in.files))){
  
  # read the csv data
  in.data <- read.csv(in.files[f], skip = 15, header = T, sep = ";", dec = ",", as.is = T)
  
  # Convert date field ("Data") into an actual date
  in.data$Data <- parse_date_time(in.data$Data,orders = "d/m/Y" )
  
  # Selects only observations with consistency level
  # (΅NivelConsistencia) = 1. Level 2 is supposedly more
  # trustworthy, but always lags behind several years
    in.data <- in.data[in.data$NivelConsistencia ==1,]
  
  # Get the number of rows of the csv table
  nrows <- nrow(in.data)
  
  # 2) Extracting and stacking observations
  
  # The main quirk of the ANA data is that daily values are stored as sequential columns, 
  # and each row of the table corresponds to a month. All months have 31 days, and 
  # impossible dates are just empty. Also, entire months(rows) can be missing. The only safe way 
  # to extract values is to loop over each value, extract it, and date it.
  
  # Starting empty data frame that will receive precipitation and date values
  # It is a bad practice to grow dataframes inside a loop, but this is lightweight enough
  # that it makes little difference in performance
  st.data <- data.frame(dt = as.Date(NA), precip = as.numeric(NA))
  
  # This loop will take a row of daily values for a given month, create a vector of corresponding dates,
  # stack them side by side as columns in a data frame, and append them to main st.data output dataframe
  for(r in c(1:nrows)){ 
    rowdate <- in.data$Data[r] # get the date that defines the month for a given row
    rowmonth <- month(rowdate) # extract just the month
    rowyear <- year(rowdate) # extract the year
    d.prec <- unlist(as.vector(in.data[r,14:44])) # extract daily values
    d.date <- unlist(as.Date(paste(rowyear,rowmonth,c(1:31),sep="-"))) # creates actual dates
    st.data <- rbind(st.data, data.frame(dt = d.date,precip = d.prec)) # stacks values and dates and add to dataframe
  }

  # All date rows have 31 values, resulting in some impossible dates (e.g. 31st of february)  
  # They will become NAs, so we can just remove the rows with NA dates
  
  na.rows <- which(is.na(st.data$dt))
  st.data <- st.data[-(na.rows),]
    
  # create a new name for the resulting file
  out.name <- paste(out.folder,in.names[f],"_daily.csv", sep="")

  # save the resulting file
  write.csv(st.data,file=out.name,quote=F,row.names=F)

}




