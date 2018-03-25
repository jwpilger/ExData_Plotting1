#
plot1 <- function(power_df,project_folder) {
    plot_file <- file.path(project_folder,paste("plot1.png",sep=""))
    png(file=plot_file,width=480, height=480,units="px")
    par(mar=rep(2,4))
    hist(power_df$Global_active_power, main = paste("Global Active Power"), col="red", xlab="Global Active Power (kilowatts)")
    dev.off()   # close graphics device
    return(plot_file)
}

# define required libs and source files
library(RCurl)
library(dplyr)
library(data.table)
source("downloadfile.R")
#source("power_consumption_plot.R")

#rm(list = ls())

#define variables
power_data_start_date <- "2007-02-01"   # select data between these dates
power_data_end_date   <- "2007-02-02"
Url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
script_r <-  tools::file_path_sans_ext(basename(r_source_name()))
project_folder <- "Power_Consumption"    # set to  "" if script name to be used
#project_folder <- script_r # set to script name if single script is used
log(script_r,"#Coursera Exploratory Data Analysis course project")
log(script_r,"#R script: ",script_r,".R"," Author: John Pilger Date: ",date())

log(script_r,"#Individual household electric power consumption ")
log(script_r,"[#1] Download power consumption files using downloadfile function ")
log(script_r,"#Coursera Exploratory Data Analysis course project")

rc <- downloadfile(Url,script_r,project_folder)    # download file
file_name <- rc[1,2]    #  path name + file name of downloaded file
rc_down <- rc[1,1]      #  return code 0 = successful
project_folder_name <- rc[1,3]  # project folder where was it placed?


# The power consumption dataset has 2,075,259 rows and 9 columns. 
# First calculate a rough estimate of how much memory the dataset will require in memory before reading into R.
# We will only be using data from the dates 2007-02-01 and 2007-02-02. 
#  convert the Date and Time variables to Date/Time classes in R using the 𝚜𝚝𝚛𝚙𝚝𝚒𝚖𝚎()  and 𝚊𝚜.𝙳𝚊𝚝𝚎() functions.  
    
    power_data_rows <- 2075259  # from file documentation
    log(script_r,"Projected size of power_data object frame (MB): ",(power_data_rows * 9 * 8)/(1024*1024))
    colclass_data <- c("character","character","numeric","numeric",
                   "numeric","numeric","numeric","numeric","numeric")
    if (rc_down == 0) {  # file was retrived
    log(script_r,"[#2] Create data frame, calculate power consumption object space required ") 
    power_data <- read.table(rc[1,2], header=TRUE,nrows=power_data_rows,sep =";",
                       colClasses= colclass_data,na="?")
            }
     log(script_r,"Power consumption data raw data rows: ", nrow(power_data)) 
     log(script_r,"Power consumption data raw data frame size (MB): ",object.size(power_data)/(1024*1024))
     
# combine date/time as characters, convert date and time variables to Date/Time class 
     power_data$Date_time <- paste(power_data$Date,power_data$Time,sep=" ")
     power_data$Date <- as.Date(power_data$Date, format="%d/%m/%Y")
     power_data_subset <- power_data[(power_data$Date >= power_data_start_date) & (power_data$Date<= power_data_end_date),]
     rm(power_data) # remove raw data input object

     log(script_r,"[#3] Select raw data input by date filter ",power_data_start_date," to ",power_data_end_date)  
     log(script_r,"Power consumption data (subset) rows after date selection: ", nrow(power_data_subset))
    
     power_data_subset$Time <- strptime(power_data_subset$Date_time, "%d/%m/%Y %H:%M:%S")
    
    log(script_r,"[#4] Data prep complete - ready for visualization ")

# plot dataframe in project folder
    
    plot_ds <- plot1(power_data_subset,project_folder_name)
    log(script_r,"[#5] ",plot_ds," complete and written to project folder")
    
    
    log(script_r,"[#6] Project complete")
