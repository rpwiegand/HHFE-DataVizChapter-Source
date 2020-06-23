# ------------------------------------------------------------------
#   This R source code provides the data visualizations for the
#   2020 Human Factors and Ergonomics chapter on Data Visualization.
#   
#     Sumanta Pattanaik   University of Central Florida
#     R. Paul Wiegand     Winthrop University
#
# ------------------------------------------------------------------

library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(plyr)
library(reshape2)
library(MASS)


# ------------------------------------------------------------------
#   Data Loading Functions
# ------------------------------------------------------------------

# Get the data from the CSV and format it like we need it.
getDLTimeSeriesData <- function(filename='./data/88.csv') {
  # Needed to map the short-hand values for the methods into longer phrases
  methodMap <- c("D"="Direct", "M"="MIMO", "R"="Recursive")
  
  # Read the data file
  allData <- read.csv(filename, header=T)
  
  # Take the wide-form data and put it into statistical long form.
  # Also, add several columns that we will need for other purposes.
  meltedData <- mutate(melt(allData, 
                            id.vars=c("X", "exptid", "id", "sclass", "t_train", "t_eval")),
                       CaseID=factor(id),
                       Class=factor(methodMap[sclass]),
                       MethodAndClass=factor(paste(variable,Class,sep=" & ")),
                       MethodAndID=factor(paste(variable,id,sep=" & ")))
  
  # Rank order the values for each method and sensor ID group
  meltedData <- ddply(meltedData, .(MethodAndID), transform, Rank = rank(value))
  
  return(meltedData)
}


# Filter out just the name you want, make sure all yobs in the range
# have counts (could be 0).
getCorrectedFilteredSSNNameData <- function(filename='./data/ssnNames.csv', 
                                            babyName=c('Kendall')) {
  
  # Read the data and find the min/max decades of birth in the data
  ssnNames <- read.csv(filename)
  decadeBreakMin <- round(min(ssnNames$yob)/10,0)*10
  decadeBreakMax <- round(max(ssnNames$yob)/10,0)*10
  
  nameSubset <- filter(ssnNames, name %in% babyName)
  for (year in min(nameSubset$yob):max(nameSubset$yob)) {
    # Deal with missing women
    if (dim(filter(nameSubset, sex=='F', yob==year))[1] == 0)
      nameSubset <- rbind(nameSubset, data.frame(name=babyName,
                                                 sex='F',
                                                 count=0,
                                                 yob=year))
    # Deal with missing men
    if (dim(filter(nameSubset, sex=='M', yob==year))[1] == 0)
      nameSubset <- rbind(nameSubset, data.frame(name=babyName,
                                                 sex='M',
                                                 count=0,
                                                 yob=year))
  }
  
  return(list(nameSubset, decadeBreakMin, decadeBreakMax))
}


# Produce simulated data for visualizing the pre-attentive
# affects for various features.
generatePAFeatures <- function(numPoints, feature) {
  # Correct any wonky feature request
  FeatureTypeLevels <- c("Color", "Shape", "Size", "Color+Shape")
  if (!(feature %in% FeatureTypeLevels))
    feature <- "Color+Shape"
  
  # Basic setup all feature sets need
  XPosition   <- runif(numPoints)
  YPosition   <- runif(numPoints)
  FeatureType <- factor(rep(feature,numPoints),
                        levels=FeatureTypeLevels,
                        ordered=T)
  
  # Initialize each of the features to be all one thing
  Color <- factor(rep("A", numPoints), levels=c("A","B"), ordered=T)
  Shape <- factor(rep("A", numPoints), levels=c("A","B"), ordered=T)
  Size  <- factor(rep("A", numPoints), levels=c("A","B"), ordered=T)
  
  # Feature-specific override
  if (feature == "Color")
    Color[1] = "B"
  
  else if (feature == "Shape")
    Shape[1] = "B"
  
  else if (feature == "Size")
    Size[1] = "B"
  
  # If Color+Shape, fill first half of colors with B, second
  # half of shapes with B, then make sure precisely 1 is B in both.
  else {
    lowerRange <- 1:floor(numPoints/2)
    upperRange <- (floor(numPoints/2)+1):numPoints
    Color[lowerRange] <- "B"
    Shape[upperRange] <- "B"
    Shape[1] <- "B"
  }

  return(data.frame(XPosition, YPosition, FeatureType, Color, Shape, Size))
}


# ------------------------------------------------------------------
#   Section 2 Plots
# ------------------------------------------------------------------

# Make a heatmap where one axis are the sensors and the other is the folded
# categories of error measure and strategy
makeDLTimeSeriesHeatmapPlot <- function(dataFilename="./data/88.csv", 
                                        outputPDF=FALSE) {
  dataset <- getDLTimeSeriesData(dataFilename)
  p <- ggplot(dataset, aes(CaseID, MethodAndClass)) +
    geom_tile(aes(fill=factor(Rank)), color="white") +
    scale_fill_manual(values=c("steelblue","gray","firebrick"),
                      name="",
                      labels=c("Best", "Middle", "Worst"),
                      guide = guide_legend(reverse = TRUE) ) +
    geom_hline(yintercept=c(3.5,6.5,9.5), size=1, color="white") +
    ylab("") + xlab("Sensors") +
    theme_bw() +
    theme(text=element_text(family="Times", size=24),
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_text(size=25, hjust=1),#angle=60, hjust=1),
          axis.text.x = element_blank(),
          legend.text = element_text(size=24)) 
  
  # Produce the plot in RStudio
  print(p)
  
  # If the caller wants to produce a PDF, do so
  if (outputPDF) {
    pdfFilename = "./figures/sect2-DLTimeSeries.pdf"
    ggsave(pdfFilename, width=18, height=8)
    system(paste('open', pdfFilename))
  }
}


# Make a fixed area plot to show how the ratio of girls to boys changed
# over time for a given name.
compareSSNNamesSexFixedAreaPlot <- function(filename='./data/ssnNames.csv', 
                                            babyName=c('Kendall'),
                                            outputPDF=F) {
  # Get SSN data associated with the selected name
  ssnDataReturn <- getCorrectedFilteredSSNNameData(filename, babyName)
  nameData <- ssnDataReturn[[1]]
  decadeBreakMin <- ssnDataReturn[[2]]
  decadeBreakMax <- ssnDataReturn[[3]]
  
  # Set the breaks at the decade marks
  yearBreaks <- seq(from=decadeBreakMin, to=decadeBreakMax, by=10)
  
  p <- ggplot(nameData, aes(x=yob, y=count, fill=sex)) + 
    geom_hline(yintercept=0.5, size=0.75, color="darkgray") +
    geom_area(position='fill', alpha=0.4) +
    scale_fill_brewer(palette="Set1", name="Sex") +
    scale_x_continuous(breaks=yearBreaks) +
    xlab("Year of Birth") +
    ylab(paste("Ratio of Babies with the Name",babyName)) +
    ggtitle(paste("Men vs. Women Named ",babyName, ", 1910-2018", sep='')) +
    theme(text=element_text(family="Times", size=22)) +
    annotate("segment", x=1992.5, y=0.5, xend=2000, yend=0.6) + 
    annotate("text",x=2000,y=0.6, hjust=0, size=6, label="Born 1992")
  
  # Produce the plot in RStudio
  print(p)
  
  # If the caller wants to produce a PDF, do so
  if (outputPDF) {
    filename = paste('./figures/sect2-',tolower(babyName),'1.pdf',sep='')
    ggsave(filename, width=13, height=6)
    system(paste('open',filename))
  }
  
  return(p)
}


# Make a line plot showing the counts for how many girls and boys were
# named with a given name over time.
compareSSNNamesSexCountLinePlot <- function(filename='./data/ssnNames.csv', 
                                            babyName=c('Kendall'),
                                            outputPDF=F,
                                            doPlot=T) {
  
  # Get SSN data associated with the selected name
  ssnDataReturn <- getCorrectedFilteredSSNNameData(filename, babyName)
  nameData <- ssnDataReturn[[1]]
  decadeBreakMin <- ssnDataReturn[[2]]
  decadeBreakMax <- ssnDataReturn[[3]]

  # Set the breaks at the decade marks
  yearBreaks <- seq(from=decadeBreakMin, to=decadeBreakMax, by=10)
  
  p <- ggplot(nameData, aes(x=yob, y=count, color=sex)) + 
    geom_line(size=1.5) +
    scale_color_brewer(palette="Set1", name="Sex") +
    scale_x_continuous(breaks=yearBreaks) +
    xlab("Year of Birth") +
    ylab(paste("Number of Babies with the Name",babyName)) +
    ggtitle(paste("The Rise of Women Named ", babyName, ", 1910-2018",sep='')) +
    theme(text=element_text(family="Times", size=18))
  
  # Produce the plot in RStudio, if the user wants
  if (doPlot)
    print(p)
  
  # If the caller wants to produce a PDF, do so 
  if (outputPDF) {
    filename = paste('./figures/sect2-',tolower(babyName),'2.pdf',sep='')
    ggsave(filename, width=13, height=6)
    system(paste('open',filename))
  }
  
  # Return the plot for later annotation ...
  return(list(p, nameData))
}


# Make a line plot showing the counts for how many girls and boys were
# named with a given name over time, including some very specific annotations.
compareSSNNamesSexCountLinePlotWithAnnotations <- function(filename='./data/ssnNames.csv', 
                                                           babyName=c('Kendall'),
                                                           outputPDF=F) {
  
  # Get the line plot object  
  plotResults <- compareSSNNamesSexCountLinePlot(filename, babyName, F, F)
  p <- plotResults[[1]]
  ssnNames <- plotResults[[2]]

  # Store some key years for marking annotations later
  mCount93 <- filter(ssnNames, name %in% babyName, yob==1993, sex=='M')$count
  fCount95 <- filter(ssnNames, name %in% babyName, yob==1995, sex=='F')$count
  fCount07 <- filter(ssnNames, name %in% babyName, yob==2007, sex=='F')$count   
  
  # Add annotations to the line plot
  p <- p +  
    annotate("segment", x=1992.5, y=mCount93, xend=1982, yend=1100) + 
    annotate("text",x=1982,y=1100,hjust=1, size=6, label="Character 'Kendall Hart'\nappears first on All My Children") +
    annotate("segment", x=1994.5, y=fCount95, xend=1982, yend=1600) + 
    annotate("text",x=1982,y=1600,hjust=1, size=6, label="'Kendall Hart' leaves the show") +
    annotate("segment", x=2006.5, y=fCount07, xend=1992, yend=fCount07+10) + 
    annotate("text",x=1992,y=fCount07+10,hjust=1, size=6, label="Keeping Up with the Kardashians begins") +
    annotate("text", x=1986, y=240, hjust=0, size=6, label="General rise in\ngender-neutral\nnaming in 80s/90s")
  
  # Produce the plot in RStudio
  print(p)
  
  # If the caller wants to produce a PDF, do so
  if (outputPDF) {
    filename = paste('./figures/sect2-',tolower(babyName),'3.pdf',sep='')
    ggsave(filename, width=13, height=6)
    system(paste('open',filename))
  }
}


# Make a small multiples plot to show the ratio of girls to boys for a
# variety of gender neutral names over time.
smallmultSSNNamesSexFixedAreaPlot <- function(filename='./data/ssnNames.csv', 
                                              babyName=c('Kendall', 'Morgan', 'Riley',
                                                         'Taylor', 'Sidney',
                                                         'Indiana'),#,  'Clarke', 'Tyler'),
                                              outputPDF=F) {
  # Get SSN data associated with the selected name
  ssnDataReturn <- getCorrectedFilteredSSNNameData(filename, babyName)
  nameData <- ssnDataReturn[[1]]
  decadeBreakMin <- ssnDataReturn[[2]]
  decadeBreakMax <- ssnDataReturn[[3]]
  
  # Set the breaks at the decade marks
  #yearBreaks <- seq(from=decadeBreakMin, to=decadeBreakMax, by=10)
  yearBreaks <- c(1980,2000)
  
  p <- ggplot(nameData, aes(x=yob, y=count, fill=sex)) + 
    geom_hline(yintercept=0.5, size=0.5, color="gray") +
    geom_vline(xintercept=1980, size=0.75, linetype='dotted', color="darkgray") +
    geom_vline(xintercept=2000, size=0.75, linetype='dotted', color="darkgray") +
    geom_area(position='fill', alpha=0.4) +
    facet_wrap(name ~ .) +
    scale_fill_brewer(palette="Set1", name="Sex") +
    scale_x_continuous(breaks=yearBreaks) +
    xlab("") +
    ylab("") +
    ggtitle(paste("Gender Neutral Names Grew in 80's and 90's", sep='')) +
    theme_bw() + 
    theme(text=element_text(family="Times", size=22),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          #panel.margin = unit(c(0,0,0,0), "lines"),
          panel.grid.minor = element_blank(), 
          axis.line = element_blank())

  # Produce the plot in RStudio
  print(p)
  
  # If the caller wants to produce a PDF, do so
  if (outputPDF) {
    filename = paste('./figures/sect2-',tolower(babyName),'4.pdf',sep='')
    ggsave(filename, device="pdf", width=13, height=6)
    system(paste('open',filename))
  }
}


# Make an annotated line plot showing how the murder rate has evolved over time
# in the US.
crimeContextLinePlot <- function(filename='./data/fbi-crime-1996-2015.csv', 
                                 outputPDF=F) {
  crime <- read.csv(filename, header=T)
  
  p<-  ggplot(arrange(crime, Year), aes(x=Year, y=Murder.and..nonnegligent..manslaughter..rate.)) +
    geom_line(size=1.35, col="darkblue") +
    geom_point(size=4, shape=21, fill="white", color="darkblue") +
    ylab("Murder & Manslaughter Rate in U.S (per 100K people)") +
    theme(text=element_text(family="Times", size=16)) +
    geom_hline(yintercept=7.7, linetype="dashed", color="black") +
    annotate("text",2005,7.75,vjust=0,label="Murder Rate of Bolivia, 2011", color="black") +        
    geom_hline(yintercept=3.7, linetype="dashed", color="black") +
    annotate("text",2005,3.75,vjust=0,label="Murder Rate of Chile, 2011", color="black")  +
    annotate("text", 1996, 4.5, hjust=0, label="The U.S. Murder Rate was the lowest in the last\n100 years in 1955, when it was 4.5 per 100K people") +
    ggtitle("Murder in the U.S. is at an Historic Low")
  
  # Produce the plot in RStudio
  print(p)
  
  # If the caller wants to produce a PDF, do so
  if (outputPDF) {
    filename = './figures/sect2-crime.pdf'
    ggsave(filename, width=13, height=6)
    system(paste('open',filename))
  }
}



# ------------------------------------------------------------------
#   Section 3 Plots
# ------------------------------------------------------------------

# This function generates the plot for showing pre-attentive processing
# for color, shape, and size.
preattentiveFeatures <- function(numPoints=30,
                                 outputPDF=F) {
  # Get the each dataset and glue them all together into one
  psDataset <- rbind(generatePAFeatures(numPoints, "Color"),
                     generatePAFeatures(numPoints, "Shape"),
                     generatePAFeatures(numPoints, "Size"),
                     generatePAFeatures(numPoints, "Color+Shape"))
 
  # Create the plot
  p <- ggplot(psDataset, aes(x=XPosition, y=YPosition, 
                             color=Color, shape=Shape, size=Size)) +
    geom_point() +
    scale_color_manual(values= c("darkgray", "firebrick")) +
    scale_shape_manual(values  = c(16,17)) +
    scale_size_manual(values = c(3,5)) +
    facet_wrap(FeatureType ~ .) +
    xlab("") + ylab("") + guides(fill=FALSE) +
    theme_bw() +
    theme(text=element_text(family="Times", size=16),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          legend.position="none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  # Produce the plot in RStudio
  print(p)
  
  # If the caller wants to produce a PDF, do so
  if (outputPDF) {
    filename = './figures/sect3-preattentive.pdf'
    ggsave(filename, width=8, height=6)
    system(paste('open',filename))
  }
}


badMarriageRateExample <- function(filename='./data/marriage-rates.csv',
                                   outputPDF=F) {
  # Read the marriage rate data and format it
  marriage = read.csv(filename,header=T)
  marriage = mutate(marriage,
                  Year=factor(Year,
                              levels=c(2008,2009,2010,2011,2012), 
                              ordered=T),
                  Education=factor(Education, 
                                   levels=c("ltHS","HS","SomeCol","gtCol"), 
                                   ordered=T))
  
  # Create the labels we'll use on the axis
  myLabels=c("Less than\nhigh school", 
             "High school\ngraduate", 
             "Some\ncollege", 
             "Bachelor's\ndegree or\nmore")

  p <- ggplot(marriage, aes(x=Education, 
                            y=NewMarriageRate, 
                            fill=Year)) +
    scale_fill_brewer(palette="Set1", name="") +
    scale_x_discrete(labels=myLabels) +
    geom_bar(stat="identity", position="dodge", color="white") +
    ylab("") + ggtitle("New Marriage Rate By Education") +
    theme_bw() +
    theme(text=element_text(family="Times", size=16),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  # Produce the plot in RStudio
  print(p)
  
  # If the caller wants to produce a PDF, do so
  if (outputPDF) {
    filename = './figures/sect3-badmarriagerate.pdf'
    ggsave(filename, width=8, height=6)
    system(paste('open',filename))
  }
}


betterMarriageRateExample <- function(filename='./data/marriage-rates.csv',
                                   outputPDF=F) {
  # Read the marriage rate data and format it
  marriage = read.csv(filename,header=T)
  marriage = mutate(marriage,
                    Year=factor(Year,
                                levels=c(2008,2009,2010,2011,2012), 
                                ordered=T),
                    Education=factor(Education, 
                                     levels=c("ltHS","HS","SomeCol","gtCol"), 
                                     ordered=T))
  
  # Create the labels we'll use on the axis
  myLabels=c("Less than\nhigh school", 
             "High school\ngraduate", 
             "Some\ncollege", 
             "Bachelor's\ndegree or\nmore")
  
  # Create a special pallete to highlight the bars we like.
  myPalette=c(rep("darkgray",3),"darkgoldenrod")
  
  p<- ggplot(marriage, aes(x=Year, 
                           y=NewMarriageRate, 
                           fill=Education)) +
    scale_fill_manual(values=myPalette, labels=myLabels, name="") +
    #scale_x_discrete(labels=myLabels) +
    geom_bar(stat="identity", position="dodge", color="white") +
    ylab("") + ggtitle("New Marriage Rate By Education") +
    theme_bw() +
    theme(text=element_text(family="Times", size=16),
          legend.position = "bottom",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())
  # Produce the plot in RStudio
  print(p)
  
  # If the caller wants to produce a PDF, do so
  if (outputPDF) {
    filename = './figures/sect3-bettermarriagerate.pdf'
    ggsave(filename, width=8, height=6)
    system(paste('open',filename))
  }
}

