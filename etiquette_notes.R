# Section 1 - Organising scripts into sections ----


# Outline test 1 ----

# Use four hyphens like above to crate outlines (headings) for easy navigation using topleft icon.

# You can also go to 'Edit/Folding/Collapse all' to collapse all sections

# At the start of each script include the following

# Introduction ----

# Author statement (what does this script do?), author(s) names, contact details and date.




# Analysing vertebrate population change based on the Living Planet Index
# Data available from http://www.livingplanetindex.org/

# Gergana Daskalova ourcodingclub(at)gmail.com
# 25-04-2017

# Libraries ----
library(tidyr)  # Formatting data for analysis
library(dplyr)  # Manipulating data
library(ggplot2)  # Visualising results
library(readr)  # Manipulating data

# Libraries: What packages are you using for this script? Example above


# Defining functions ----
# A custom ggplot2 function
theme.LPI <- function(){
  theme_bw()+
    theme(axis.text.x=element_text(size=12, angle=45, vjust=1, hjust=1),
          axis.text.y=element_text(size=12),
          axis.title.x=element_text(size=14, face="plain"),             
          axis.title.y=element_text(size=14, face="plain"),             
          panel.grid.major.x=element_blank(),                                          
          panel.grid.minor.x=element_blank(),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.y=element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size=20, vjust=1, hjust=0.5),
          legend.text = element_text(size=12, face="italic"),          
          legend.title = element_blank(),                              
          legend.position=c(0.9, 0.9))
}

# Functions: Are you using any functions written by you and/or others? Define them here.



# Set the working directory ----

setwd("/Users/etz/Desktop/Data Science/Git Repository/CC-etiquette")


# Import data ----
LPI <- read.csv("LPIdata_CC.csv")


# The sections of your analysis ----


# Formatting data ----
LPI2 <- gather(LPI, "year", "abundance", 9:53)  # Transforming the data from wide to long format, some blank cells may disappear
# gather function requires tidyr package
LPI2$year <- parse_number(LPI2$year)  # Do you see awkward Xs before all the years? This gets rid of them.
names(LPI2)  # Check what the different variables are called
names(LPI2) <- tolower(names(LPI2))  # Make all variable names lower case

# When manipulating data it's always good check if the variables have stayed how we want them
# Use the str() function
str(LPI2)

# Abundance is a character variable, when it should be numeric, let's fix that
LPI2$abundance <- as.numeric(LPI2$abundance)

# Calc summary stats for each biome in the LPI database ----
levels(LPI2$biome)  # list all biomes

LPI_biome_summ <- LPI2 %>%  # use of pipe operator
  group_by(biome) %>%  # Group by biome
  summarise(populations = n())  # Create columns, number of populations

# Visualising the number of populations in each biome with ggplot2 package ---- 
(barplot <- ggplot(LPI_biome_summ, aes(biome, color = biome, y = populations)) + geom_bar(stat = "identity") +  
   theme.LPI() +                     # Use of personal theme function
   ylab("Number of populations") +
   xlab("Biome") +
   theme(legend.position = "none"))  # Removal of legend for simplicity


# The outputs of your analysis ----

# good practice to save image outputs in a subdirectory of your working directory, e.g. img/ 
# Remember that you will have to create the folder img manually before saving plots to it

png(file="img/biome_pop.png", width = 1000, height = 2000)  # Note that png() uses pixel values for width and height
ggplot(LPI_biome_summ, aes(biome, color = biome, y = populations)) + geom_bar(stat = "identity") +
  theme.LPI() +
  ylab("Number of populations") +
  xlab("Biome") +
  theme(legend.position = "none")
dev.off()  # This tells R you are done with the plotting and it can save the file

pdf(file="img/biome_pop.pdf",  width = 13.33, height = 26.66)  # pdf() uses inches
ggplot(LPI_biome_summ, aes(biome, color = biome, y = populations)) + geom_bar(stat = "identity") +
  theme.LPI() +
  ylab("Number of populations") +
  xlab("Biome") +
  theme(legend.position = "none")
dev.off()



# Section 2 - Following a coding syntax etiquette ----

# Naming files and objects ----

# LPI_analysis_Apr_2017.R  # Alright.

# yet_another_script.R  # Bad. Took me hours to find the file when I needed it one year later.

# Object names
avg_clicks  # Good.
avg.clicks  # Acceptable.
avg_Clicks  # Not okay.

# Function names
calculate.avg.clicks  # This is what we are aiming for.
CalculateAvgClicks  # Not that bad, but mixing capital and lowercase letters can lead to typos
calculate_avg_clicks , calculateAvgClicks  # Bad. The convention is that functions are defined using dots, not underscores.


# Spaceing ----

# Place spaces around all infix operators (=, +, -, <-, etc.)

# : and :: don’t need spaces around them and one should not add spaces when 
# defining coordinate systems in spatial objects.

x <- 1:10  # Good
base::get  # Good
dplyr::select  # When you use `package_name::function_name` in your code like the 
# example here, this means you are calling the function `select()` from the 
# package `dplyr` - this way of using functions works without having loaded the 
# package beforehand using `library(dplyr)`, but it's not very commonly used, since it's longer.

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
# Here we are creating an imaginary object with a geographical projection commonly used for the UK

# Don’t place a space before left parentheses, except in a function call

# Good
if (debug) do(x)
plot(x, y)

# Bad
if(debug)do(x)
plot (x, y)


# Do not place spaces around code in parentheses or square brackets (unless there’s a comma)

# Good
if (debug) do(x)
diamonds[5, ]

# Bad
if ( debug ) do(x)  # No spaces around debug
x[1,]   # Needs a space after the comma
x[1 ,]  # Space goes after comma not before


# Inline commenting: If you are commenting inline with code, place two spaces after the code, followed by #, a single space and then your text

# Calculating summary statistics for each biome in the Living Planet Index database
# No need to copy and run this code now, this just illustrates comments
LPI_biome_summ <- LPI2 %>%
  group_by(biome) %>%  # Group by biome
  summarise(populations = n(),   # Create columns, number of populations
            mean_study_length_years = mean(lengthyear),  # mean study length
            max_lat = max(decimal_latitude),  # max latitude
            min_lat = min(decimal_latitude),  # max longitude
            dominant_sampling_method = names(which.max(table(sampling_method))),  # modal sampling method
            dominant_units = names(which.max(table(units))))  # modal unit type


# Curly braces ----

# An opening curly brace should never go on its own line and should always be followed by a new line. 

# A closing curly brace should always go on its own line, unless it’s followed by else. Always indent the code inside curly braces


# Good

if (y < 0 && debug) {
  message("Y is negative")
}

if (y == 0) {
  log(x)
} else {
  y ^ x
}

# Bad

if (y < 0 && debug)
{message("Y is negative")}

if (y == 0) {
  log(x)
}
else {
  y ^ x
}



# Line length ----

# The official convention is to limit your code to 80 characters per line  >>>>

# When using pipes from the dplyr package, keep the pipe operator %>% at the end 
# of the line and continue your pipe on a new line.

# Just an example of what a pipe could look like, no need to run the code at this stage.
LPI_long <- LPI_long %>%
  group_by(., genus_species_id) %>%  # group rows so that each group is one population
  mutate(., maxyear = max(year), minyear = min(year)) %>%  # Create columns for the first and most recent years that data was collected
  mutate(., lengthyear = maxyear-minyear) %>%  # Create a column for the length of time data available
  mutate(., scalepop = (pop-min(pop))/(max(pop)-min(pop))) %>%  # Scale population trend data
  filter(., is.finite(scalepop)) %>%
  filter(., lengthyear > 5) %>%  # Only keep rows with more than 5 years of data
  ungroup(.)  # Remove any groupings you've greated in the pipe, not entirely necessary but it's better to be safe


# When using ggplot2, keep the + at the end of the line and continue adding on layers on a new line


# Just an example of what the code could look like, no need to run the code at this stage.
(vulture_scatter <- ggplot(vultureITCR, aes (x = year, y = abundance, colour = Country.list)) +
    geom_point(size = 2) +                                                
    geom_smooth(method = lm, aes(fill = Country.list)) +                    
    theme_my_own() +
    scale_fill_manual(values = c("#EE7600", "#00868B")) +               
    scale_colour_manual(values = c("#EE7600", "#00868B"),               
                        labels = c("Croatia", "Italy")) +                 
    ylab("Griffon vulture abundance\n") +                             
    xlab("\nYear"))



# Indentation ----

# If a command runs over multiple lines, indent the second line to where the definition starts


# Again, just an example, don't run this, it won't work!
ggplot()+geom_hline(yintercept=0,linetype="dotted",colour="darkgrey")+
  geom_line(data=cwa.sub, aes(x=Season,y=Total.Concentration),size=2,alpha=0.2)+
  geom_ribbon(data=preds2, aes(x=Season, ymin=ploBT, ymax=phiBT), fill="#3cd0ea", alpha=0.3)+
  geom_line(data=preds2,aes(x=Season,y=Total.ConcentrationBT),colour="#3cd0ea",size=3)+theme_bw()+ylab("Minimum Sea Ice Concentration")+xlab("Season")+annotate("text",x=2012,y=0.4,label=paste0("p = ",round(pval.cwa.sub,4)),size=6)+theme(legend.title=element_text(size=20,face="plain",hjust=1),legend.text=element_text(size=18,angle=45),legend.position="bottom",legend.key =element_blank(),axis.title.x=element_text(size=20,margin=margin(20,0,0,0)),axis.title.y=element_text(size=20,margin=margin(0,20,0,0)),axis.text=element_text(size=16),panel.grid.minor=element_blank(),panel.grid.major=element_blank())



# The second version is much easier to read and there is no need to keep scrolling left and right.

(plot <- ggplot() +
    geom_hline(yintercept = 0, linetype = "dotted", colour = "darkgrey") +
    geom_line(data = cwa.sub, aes(x = Season, y = Total.Concentration), size = 2, alpha = 0.2) +
    geom_ribbon(data = preds2, aes(x = Season, ymin = ploBT, ymax = phiBT), fill = "#3cd0ea", alpha = 0.3) +
    geom_line(data = preds2, aes(x = Season, y = Total.ConcentrationBT), colour = "#3cd0ea", size = 3) +
    theme_bw() +
    labs(y = "Minimum Sea Ice Concentration", x = "Season") +
    annotate("text", x = 2012, y = 0.4, label = paste("p = ", round(pval.cwa.sub,4)), size = 6) +
    theme(legend.title = element_text(size = 20, face = "plain", hjust = 1),
          legend.text = element_text(size = 18, angle = 45),
          legend.position = "bottom",
          legend.key = element_blank(),
          axis.title.x = element_text(size = 20, margin = margin(20,0,0,0)),
          axis.title.y = element_text(size = 20, margin = margin(0,20,0,0)),
          axis.text = element_text(size=16),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()))
