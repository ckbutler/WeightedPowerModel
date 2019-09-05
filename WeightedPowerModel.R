# Meta Data ===================================================================
# Title: Static analysis of the Weighted Power Model
# Author(s): Christopher K. Butler
# Created: December 2018
# Last update: 2019-09-05
#
# Full description:
#
#	The Weighted Power Model (WPM) has four input variables:
# Actors, Capabilities, Position, and Salience.
#
#	The variable "Actors" is a list of the actors by name (or abbreviation).
#
#	"Capabilities" are a measure of Power. Higher numbers mean more capabilities.
#	Additionally, an actor who has capabilities of 6 compared to an actor who 
#	has capabilities of 2 is three times stronger than the weaker actor.
#	The program transforms Capabilites into "power" such that the sum of power
#	over all Actors is 100%.
#
#	"Position" is a measure of Preferences. For the WPM, this is thought of as a
#	left-right spectrum anchored by two opposing sides. Which side is left or
#	right doesn't matter, but the positions of the other actors does matter. For
#	example, an actor exactly halfway between the two ends is presumed to be
#	indifferent (or neutral) between the two anchor actors. Intermediate values
#	closer to one end mean that actor has a stronger preference for that anchor
#	actor's way of thinking. The program transforms Position (to a new variable
#	called "position") to be between 0 and 100 if the input values don't already
#	conform to that norm.
#
#	"Salience" is another measure of Preference. Specifically, it is a measure of
#	how much an actor cares about the issue being modeled by the WPM relative to
#	other issues. The input variable needs to be on a 1 to 100 scale, representing
#	the percentage of importance of this issue relative to all other issues on the
#	actor's plate. Values outside this range are removed, resulting in fewer
#	actors in the analysis. The program transforms Salience (to a new variable
#	called "salience") to be in decimal form (i.e., as a proportion).
#
# The program then calculates the "weighted_power" of each actor (which is
# simply power times salience). Additional outputs identify the "median
# position" and the "relative power" of each side of the issue space.
###

###
# NOTE about making and opening your own data files for analysis. There need to
# be four columns of information: Actors, Capabilities, Position, and Salience.
# (See, for example, "WPM-input-template.csv".) Each column needs a header
# matching (exactly, including capitalization) these names. The columns need not
# be in this order, but all four must exist. You may have additional columns,
# but this program will ignore them. Your file can be plain text (such as comma-
# or tab-delimited) or an Excel spreadsheet (XLS or XLSX). For plain text files,
# you need to know whether it is comma or tab delimited; the extension (CSV or
# TAB) doesn't always tell you and many CSV files are, in fact, tab-delimited.
###

# Working Directory ===========================================================
# Check the working directory:
getwd()

# Set working directory (where your data files are) for your computer:
# setwd("")

# Clean up workspace and detach non-basic packages ============================
rm(list = ls(all = TRUE))
if (!is.null(names(sessionInfo()$otherPkgs))) {
  invisible(lapply(
    paste0('package:', names(sessionInfo()$otherPkgs)),
    detach,
    character.only = TRUE,
    unload = TRUE
  ))}
graphics.off()

# Libraries ===================================================================
# Load necessary libraries
# If you don't have a library, use the install.package("") command in the Console.
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
ggplotTheme <-
  theme_classic() + theme(text = element_text(size = 12),
                     axis.text = element_text(colour = "black"))
theme_set(ggplotTheme)
library(viridis)

# Load the Data ===============================================================
# Open the input file (df = "Data Frame"):
# Example:
# Open a comma-delimited or Comma Separated Value file:
df <- read_csv("USSR-USA-1948-BdM1998.csv")

# Check and transform the data ================================================
# List the column labels (variable names):
names(df)

# Summarize the input variables:
summary(df)

# Check Salience values:
df$Salience[df$Salience<1] <- NA
df$Salience[is.nan(df$Salience)] <- NA
df$Salience[df$Salience>100] <- NA

# Remove any actors with missing (NA) values anywhere in the data frame:
df <- na.omit(df)

# Transform Salience from percent to proportion:
df$salience <- df$Salience/100

# Transform Capabilities to Power:
df$power <- df$Capabilities/sum(df$Capabilities)*100

# Transform Position to be 0 to 100:
rng <- max(df$Position) - min(df$Position)
df$position <- (df$Position - min(df$Position))/rng*100
rm(rng)

# Calculate weighted power:
df$weighted_power <- df$power * df$salience

# Analyze the Data ============================================================
# Sort by position and pull out only variables used in analysis:
df_sorted <- subset(df[order(df$position),], 
                    select = c("Actors", "power", "position", "salience", "weighted_power")
)

# Find the median actor by position according to weighted-power:
issueSpace <- df_sorted %>% 
  group_by(position) %>%
  summarize(
    power_by_position = sum(weighted_power)
  )
issueSpace$cumulativePower <- cumsum(issueSpace$power_by_position)
flag <- 0
medianPosition <- list()
for(i in seq_along(issueSpace$cumulativePower)){
  if(issueSpace$cumulativePower[i]>sum(df$weighted_power)/2 & flag==0){
    medianPosition[i] <- 1
    flag <- 1
  }
  else{
    medianPosition[i] <- 0
  }
}
issueSpace$medianPosition <- strtoi(medianPosition)
# The strtoi() function converts a string to an integer.
rm(flag, i, medianPosition)

# issueSpace$medianPosition above is a dummy variable indicating No/Yes.
# mp is the value of the median position in the 0-100 issue space:
mp <- issueSpace$position * issueSpace$medianPosition
mp <- max(mp)

median_actors <- df_sorted$Actors[df_sorted$position == mp]

anchor_left <- df_sorted$Actors[df_sorted$position == 0]
anchor_right <- df_sorted$Actors[df_sorted$position == 100]

power_left  <- sum(df$weighted_power[df$position < 50])
power_right <- sum(df$weighted_power[df$position > 50])
power_median <- as.integer(issueSpace$power_by_position[issueSpace$medianPosition==1])

prob_left_win <- as.integer(power_left / (power_left + power_right) * 100)
prob_right_win <- as.integer(power_right / (power_left + power_right) * 100)

# Print Output of Analysis ====================================================
print("================================ Begin Output ================================", quote = FALSE)
# Anchors
print("The actor(s) anchoring the 'left' end of the issue space are:", quote = FALSE)
print(paste(anchor_left, collapse = ", "), quote = FALSE)
print("", quote = FALSE)
print("The actor(s) anchoring the 'right' end of the issue space are:", quote = FALSE)
print(paste(anchor_right, collapse = ", "), quote = FALSE)
print("", quote = FALSE)
# Median
print("The 'median position' is the value along the issue space that would result from negotiations.", quote = FALSE)
print(paste("The median position for this analysis is:", mp), quote = FALSE)
print("The following actor(s) are at the median position:", quote = FALSE)
print(paste(median_actors, collapse = ", "), quote = FALSE)
print(paste("The percent of power at the median position is:", power_median,"%"), quote = FALSE)
print("", quote = FALSE)
# War
print("If the actors were to fight rather than negotiate (and assuming that those at", quote = FALSE)
print("position 50 remain neutral), the following probabilities of winning obtain:", quote = FALSE)
print(paste("The probability that the 'left' side wins is:", prob_left_win,"%"), quote = FALSE)
print(paste("The probability that the 'right' side wins is:", prob_right_win,"%"), quote = FALSE)

# Generate plots ==============================================================
# Transform position to interger for figures:
df_sorted$integerPosition <- as.integer(df_sorted$position)

# Identify the anchor actors who have the highest weighted power if there is more than one:
if (length(anchor_left)==1) {
  caption_left <- anchor_left
} else {
  df_left <- subset(df_sorted, position==0, select = c(Actors, weighted_power))
  df_left$keep <- 0
  df_left$keep[df_left$weighted_power==max(df_left$weighted_power)] <- 1
  caption_left <-  df_left$Actors[df_left$keep==1]
}
if (length(anchor_right)==1) {
  caption_right <- anchor_right
} else {
  df_right <- subset(df_sorted, position==0, select = c(Actors, weighted_power))
  df_right$keep <- 0
  df_right$keep[df_right$weighted_power==max(df_right$weighted_power)] <- 1
  caption_right <-  df_right$Actors[df_right$keep==1]
}

# Autogenerate a caption for the plot:
plotCaption <-
  paste(
      "Issue space defined by\n",
      caption_left,
      "on the left and\n",
      caption_right,
      "on the right."
  )

# Graphics of power distribution, demonstrating importance of salience
rawPower <- ggplot(df_sorted[order(df_sorted$salience, decreasing = TRUE),],
       aes(x = integerPosition, y = power, alpha = salience)) +
  geom_col() + 
  labs(
    x = "Position", 
    y = "Raw Power",
    alpha = "Salience",
    caption = plotCaption
  )

# Graphics of weighted power distribution (basic)
WPM <- ggplot(df_sorted,
       aes(x = integerPosition, y = weighted_power, fill = Actors)) +
  geom_col() + 
  scale_fill_viridis_d(option = "plasma") + 
  theme(legend.position = "none") +
  labs(
    x = "Position", 
    y = "Weighted Power",
    caption = plotCaption
  )

# Graphics of weighted power distribution coded by actor with legend
WPM_with_Legend <- ggplot(df_sorted,
       aes(x = integerPosition, y = weighted_power, fill = Actors)
       ) +
  geom_col() + 
  theme(legend.position = "bottom") +
  scale_fill_viridis_d(option = "plasma") + 
  labs(
    x = "Position", 
    y = "Weighted Power",
    fill = NULL,
    caption = plotCaption
  )

print("", quote = FALSE)
print("Reached end of script. Analysis complete.", quote = FALSE)
print("", quote = FALSE)
print("To show plots, use one of the following commands in the Console:", quote = FALSE)
print("     print(rawPower)", quote = FALSE)
print("     print(WPM)", quote = FALSE)
print("     print(WPM_with_Legend)", quote = FALSE)

# ggsave("rawPower.png", plot = rawPower)
# ggsave("WPM.png", plot = WPM)
# ggsave("WPM_with_Legend.png", plot = WPM_with_Legend)
