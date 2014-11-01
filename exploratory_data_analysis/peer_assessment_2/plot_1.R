######### PEER ASSESSMENT 2: PLOT 1 ###############


MakeData <- function() {
    # Reads the raw data and creates a list data with get and set attributes to
    # fetch and set data
    #
    # Returns:
    #   A list of consisting of get and set attributes with required data
    
    data <- NULL
    set <- function(y) {
        raw.data <<- readRDS(y)
        data <- NULL
    }
    get <- function() raw.data
    setRequiredData <- function(required.data) data <<- required.data
    getRequiredData <- function() data
    # Return a list
    list(set = set,
         get = get,
         setRequiredData = setRequiredData,
         getRequiredData = getRequiredData)
}

CacheData <- function(made.data, data.file.path, ...) {
    # As the file is large, we cache the data and load from the cache if it
    # exists
    #
    # Args:
    #   made.data: The list obtained from MakeData function
    #   data.file.path: Path to the data file
    
    data <- made.data$getRequiredData()
    if(!is.null(data)) {
        message("getting cached data")
        return(data)
    }
    message("caching data")
    made.data$set(data.file.path)
    raw.data <- made.data$get()
    data <- aggregate(Emissions ~ year, data=raw.data, FUN=sum)
    made.data$setRequiredData(data)
    data
}

main <- function() {
    # Using the data in the assignment, plot the graph required for "Figure 1"
    # This is the plot of Total P2.5 emisions v/s Year
    if (!exists("made.NEI.data.plot1")){
        made.NEI.data.plot1 <- MakeData()
    }
    
    data.path <- "data"
    NEI.data.path <- file.path(data.path, "summarySCC_PM25.rds")
    aggregate.NEI <- CacheData(made.NEI.data.plot1, NEI.data.path)
    
    figures.path <- "figures"
    if (!file.exists(figures.path)){
        dir.create(figures.path)
    }
    
    # png default is 480x480
    png(file.path(figures.path, "plot1.png"))
    
    plot(Emissions ~ year,
         data=aggregate.NEI,
         type="o",
         xlab="Year",
         ylab=expression(paste("Total ",PM[2.5], " emissions")),
         main=expression(paste("Total ",PM[2.5], " emissions per year")),
         col.main="dark red")
    
    dev.off()   
}

main()