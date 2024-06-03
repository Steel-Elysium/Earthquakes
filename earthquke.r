# imported libaries
library(stringr)

# Made By Steel Elysium


#Globals
md_earthquake_data <<- data.frame()
ml_earthquake_data <<- data.frame()
mb_earthquake_data <<- data.frame()

get_file_data <- function(file_name) {
  # This will take in a csv file name and set a global Data Frame for the sheet
  # After this will output all the data into the type Globals

  raw_earthquake_data <- read.csv(file_name, header = TRUE, sep = ",")

  # Splitting up into all the common magTypes

  md_earthquake_data <<- subset(raw_earthquake_data, magType == "md")
  ml_earthquake_data <<- subset(raw_earthquake_data, magType == "ml")
  mb_earthquake_data <<- subset(raw_earthquake_data, magType == "mb")

}

bar_graph_magitude <- function(lower, upper, data, name, incrament = 0.5,  color = "blue", dense = 0) {
  # This will take all the data from the md_earthquake_data and output the
  # magnitude into a bar chart

  bar_labels <- c()
  bar_count <- c()

  # This will range from 0-10 by increments of 0.5

  for(count in seq(lower, upper, by = incrament)){
    rng <- c(paste(count, "-", count + incrament)) # this adds range varible
    occure <- c(length(which(data$mag >= count &
      data$mag < (count + incrament))
    ))
    bar_labels <- append(bar_labels, rng)
    bar_count <- append(bar_count, occure)
  }
  setwd
  barplot(bar_count,
    names.arg = bar_labels,
    border = "black",
    col = color,
    main = paste("Earthquake of ", name, " type Magnitudes"),
    density = dense)
}

get_file_data("all_month.csv")

jpeg("ml.jpeg", quality = 75)
bar_graph_magitude(-2, 5.5, ml_earthquake_data, "ml")
dev.off()
jpeg("md.jpeg", quality = 75)
bar_graph_magitude(-1, 4.5, md_earthquake_data, "md")
dev.off()
jpeg("mb.jpeg", quality = 75)
bar_graph_magitude(3.4, 5.6, mb_earthquake_data, "mb", incrament = 0.1)
dev.off()