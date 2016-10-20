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
        
        
        
        # Prepare to union Sub_metering data
        ss1 <- select(feb_data,Date,Time,Sub_metering_1)
        names(ss1)[names(ss1)=="Sub_metering_1"] <- "metering"
        
        ss2 <- select(feb_data,Date,Time,Sub_metering_2)
        names(ss2)[names(ss2)=="Sub_metering_2"] <- "metering"
        
        ss3 <- select(feb_data,Date,Time,Sub_metering_3)
        names(ss3)[names(ss3)=="Sub_metering_3"] <- "metering"
        
        
        # Union of sub meter data
        sub_data <- union_all(ss1, ss2)
        sub_data <- union_all(sub_data, ss3)
        
        # Convert Date and Time varialbles to Date/Time types
        print("Convert Date and Time varialbles to Date/Time types ...")
        # Convert time to inlcude date for plot
        sub_data$Time <- strptime(paste(sub_data$Date,sub_data$Time),"%d/%m/%Y %H:%M:%S")
        
        # Create factors for plotting
        m = gl(3,2880,8640,c("sm1","sm2","sm3"))

        # Create a graphics device file for a png type
        print(paste("Creating png graphics device on",".\\data\\plot3.png","..."))
        png(filename = ".\\data\\plot3.png",
            width = 480, height = 480, units = "px", pointsize = 12,
            bg = "white", res = NA, family = "", restoreConsole = TRUE,
            type = c("windows", "cairo", "cairo-png"), antialias = "default")
        
        # Create empty plot
        plot(sub_data$Time,
             sub_data$metering,
             type = "n",
             xlab = "",
             ylab = "Energy sub metering"
        )
        
        # Plot submetering 1 reading
        points(sub_data[m=="sm1","Time"],
               sub_data[m=="sm1","metering"],
               type="l",
               col="black")
        
        # Plot submetering 2 reading
        points(sub_data[m=="sm2","Time"],
               sub_data[m=="sm2","metering"],
               type="l",
               col="red")
        
        # Plot submetering 3 reading
        points(sub_data[m=="sm3","Time"],
               sub_data[m=="sm3","metering"],
               type="l",
               col="blue")
         # Legend in upper right corner
        legend("topright",
               cex = 0.9,
               pch = NA,
               lty = 1,
               lwd = 1,
               col = c("black","red","blue"),
               legend = c("Sub_metering_1",
                          "Sub_metering_2",
                          "Sub_metering_3")
        )
        # rest graphics device to screen
        print(paste("Removing png graphics device on",".\\data\\plot3.png","..."))
        dev.off()
        