        ## Install and load packages
        print("Install and loading packages if necessary ...")
        pkgs_to_be_used <- c("data.table","dplyr")
        sapply(pkgs_to_be_used,
               function(pkg) 
                       if(pkg %in% rownames(installed.packages()) == FALSE){
                               print(paste("Installing",pkg,"..."))
                               install.packages(pkg)
                       }
        )        
        sapply(pkgs_to_be_used,
               function(pkgs) 
                       for (pkg in pkgs) {
                               print(paste("Loading",pkg,"..."))
                               library(pkg,character.only = TRUE)
                       }
        )

        
        ## Unzip files from url to a data directory in your working directory
        print("Unzipping files if necessary ...")
        # Check for data directory
        if(!file.exists("data")){
                dir.create("data")
        }
        
        # check for zip file
        if(!file.exists(".\\data\\household_power_consumption.zip")){
                zipfileurl <-
                        "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
                download.file(zipfileurl,
                              destfile = ".\\data\\household_power_consumption.zip")
                
                # timestamp of download
                dateDownLoaded <- date()
                
                # unpack downloaded zip file
                unzip(zipfile=".\\data\\household_power_consumption.zip",
                      exdir=".\\data")
                
        }

        # We will only be using data from the dates 2007-02-01 and 2007-02-02. 
        print("Reading records from FEB 2007 only ...")
        # Use lapply to download data, extract data.frame created in list
        feb_data<- data.frame(lapply(".//data//household_power_consumption.txt",
                           function(x) 
                                   fread(x,
                                         header= TRUE,
                                         na.strings="?",
                                         sep = ";",
                                         data.table=TRUE
                                         ) 
                           [Date %in% c("1/2/2007","2/2/2007"),]
                           )[[1]]
        )
        
         # Use read.table,slower
        # feb_data<- read.table(".//data//household_power_consumption.txt",
        #                         header= TRUE,
        #                         na.strings="?",
        #                         sep = ";"
        # )
        # 
        # feb_data <- feb_data[feb_data$Date %in% c("1/2/2007","2/2/2007"),]
        
        # Convert Date and Time varialbles to Date/Time types
        print("Convert Date and Time varialbles to Date/Time types ...")
        feb_data$Date <- as.Date(feb_data$Date ,"%d/%m/%Y")
        feb_data$Time <- strptime(feb_data$Time,"%H:%M:%S")
        
        # Create a graphics device file for a png type
        print(paste("Creating png graphics device on",".\\data\\plot1.png","..."))
        png(filename = ".\\data\\plot1.png",
            width = 480, height = 480, units = "px", pointsize = 12,
            bg = "white", res = NA, family = "", restoreConsole = TRUE,
            type = c("windows", "cairo", "cairo-png"), antialias = "default")
        
        # Plot 1
        print("Plotting Global Active Power ...")
        hist(feb_data$Global_active_power,
             main = "Global Active Power",
             col = "red",
             xlab = "Global Active Power (kilowats)",
             breaks = 12)
        
        # rest graphics device to screen
        print(paste("Removing png graphics device on",".\\data\\plot1.png","..."))
        dev.off()
        