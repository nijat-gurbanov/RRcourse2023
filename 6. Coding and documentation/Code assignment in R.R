
# Set the working directory -----------------------------------------------

# Sets the path to the parent directory of RR classes
main_wd <- "C:/Users/Nicat/Desktop/GIT/RRcourse2023/6. Coding and documentation"
setwd(main_wd)


# Importing libraries -----------------------------------------------------
# install.packages("readxl")
library(readxl)
# install.packages("stringr")
library(stringr)
# install.packages("dplyr")
library(dplyr)
# install.packages("Hmisc")
library(Hmisc)


source("Functions.R")

# Importing data ----------------------------------------------------------

# Import data from the O*NET database, at ISCO-08 occupation level.
# The original data uses a version of SOC classification, but the data we load 
# here are already cross-walked to ISCO-08 using: 
# https://ibs.org.pl/en/resources/occupation-classifications-crosswalks-from-onet-soc-to-isco/

# The O*NET database contains information for occupations in the USA, including
# the tasks and activities typically associated with a specific occupation.

task_data <- read.csv("Data\\onet_tasks.csv")

# Description of variables:
    # isco08 - variable is for occupation codes
    # the t_* variables are specific tasks conducted on the job

# Read employment data from Eurostat
# These datasets include quarterly information on the number of workers in specific
# 1-digit ISCO occupation categories. (Check here for details: 
# https://www.ilo.org/public/english/bureau/stat/isco/isco08/)

df_vector <- c()
for(i in 1:9) {
  assign(paste0("isco", i), 
         read_excel("Data\\Eurostat_employment_isco.xlsx", sheet = paste0("ISCO", i)))
  df_vector <- append(df_vector, paste0("isco", i))
}


# Data preperation --------------------------------------------------------

# We will focus on three countries, but perhaps we could clean this code to allow it
# to easily run for all the countries in the sample?

# This will calculate worker totals in each of the chosen countries.
countries <- c("Belgium", "Spain", "Poland")

for (i in 1:length(countries)) {
  assign(paste0("total_", countries[i]), 
         calc_totals(df_vector, countries[i]))
}

# Let's merge all these datasets. We'll need a column that stores the occupation categories:
# and this gives us one large file with employment in all occupations.
all_data <- data.frame()

for (i in 1:9) {
  isco_object <- get(df_vector[i])
  isco_object$ISCO <- i
  all_data <- rbind(all_data, isco_object)
}

# We have 9 occupations and the same time range for each, so we an add the totals by
# adding a vector that is 9 times the previously calculated totals
columns <- c()

for (i in 1:length(countries)) {
  columns <- append(columns, paste0("total_", countries[i]))
} 

for (col in columns) {
  all_data[[col]] <- rep(get(columns[i]), each = 9)
}

# And this will give us shares of each occupation among all workers in a period-country
columns <- c()

for (i in 1:length(countries)) {
  columns <- append(columns, paste0("share_", countries[i]))
} 

for (i in 1:length(countries)) {
  country <- countries[i]
  column <- columns[i]
  total_column <- paste0("total_", country)
  all_data[[column]] <- all_data[[country]] / all_data[[total_column]]
}

# Now let's look at the task data. We want the first digit of the ISCO variable only
task_data$isco08_1dig <- str_sub(task_data$isco08, 1, 1) %>% as.numeric()

# And we'll calculate the mean task values at a 1-digit level
# (more on what these tasks are below)

aggdata <- aggregate(task_data[, !colnames(task_data) %in% "isco08"],
  by = list(task_data$isco08_1dig),
  FUN = mean, na.rm = TRUE
)


# Non-routine cognitive analytical tasks ----------------------------------

# We'll be interested in tracking the intensity of Non-routine cognitive analytical tasks
# Using a framework reminiscent of the work by David Autor.

# These are the ones we're interested in:
# Non-routine cognitive analytical
# 4.A.2.a.4 Analyzing Data or Information
# 4.A.2.b.2	Thinking Creatively
# 4.A.4.a.1	Interpreting the Meaning of Information for Others

# Let's combine the data.
combined <- left_join(all_data, aggdata, by = c("ISCO" = "isco08_1dig"))

# Traditionally, the first step is to standardise the task values using weights
# defined by share of occupations in the labour force. This should be done separately
# for each country. Standardisation -> getting the mean to 0 and std. dev. to 1.
# Let's do this for each of the variables that interests us:

# first, second and third task items
variables <- c("t_4A2a4", "t_4A2b2", "t_4A4a1")

for (variable in variables) {
  for (country in countries) {
    share_column <- paste0("share_", country)
    std_column <- paste0("std_", country, "_", variable)
    
    temp_mean <- wtd.mean(combined[[variable]], combined[[share_column]])
    temp_sd <- wtd.var(combined[[variable]], combined[[share_column]]) %>% sqrt()
    combined[[std_column]] <- (combined[[variable]] - temp_mean) / temp_sd
  }
}

# The next step is to calculate the `classic` task content intensity, i.e.
# how important is a particular general task content category in the workforce
# Here, we're looking at non-routine cognitive analytical tasks, as defined
# by David Autor and Darron Acemoglu:

for (country in countries) {
  std_column_1 <- paste0("std_", country, "_t_4A2a4")
  std_column_2 <- paste0("std_", country, "_t_4A2b2")
  std_column_3 <- paste0("std_", country, "_t_4A4a1")
  NRCA_column <- paste0(country, "_NRCA")
  
  combined[[NRCA_column]] <- combined[[std_column_1]] + combined[[std_column_2]] + combined[[std_column_3]]
}

# And we standardise NRCA in a similar way.
for (country in countries) {
  NRCA_column <- paste0(country, "_NRCA")
  share_column <- paste0("share_", country)
  std_column <- paste0("std_", country, "_NRCA")
  
  temp_mean <- wtd.mean(combined[[NRCA_column]], combined[[share_column]])
  temp_sd <- wtd.var(combined[[NRCA_column]], combined[[share_column]]) %>% sqrt()
  combined[[std_column]] <- (combined[[NRCA_column]] - temp_mean) / temp_sd
}

# Finally, to track the changes over time, we have to calculate a country-level mean
# Step 1: multiply the value by the share of such workers.
combined$multip_Spain_NRCA <- (combined$std_Spain_NRCA * combined$share_Spain)
combined$multip_Belgium_NRCA <- (combined$std_Belgium_NRCA * combined$share_Belgium)
combined$multip_Poland_NRCA <- (combined$std_Poland_NRCA * combined$share_Poland)

# Step 2: sum it up (it basically becomes another weighted mean)
agg_Spain <- aggregate(combined$multip_Spain_NRCA,
  by = list(combined$TIME),
  FUN = sum, na.rm = TRUE
)
agg_Belgium <- aggregate(combined$multip_Belgium_NRCA,
  by = list(combined$TIME),
  FUN = sum, na.rm = TRUE
)
agg_Poland <- aggregate(combined$multip_Poland_NRCA,
  by = list(combined$TIME),
  FUN = sum, na.rm = TRUE
)

# We can plot it now!
plot(agg_Poland$x, xaxt = "n")
axis(1, at = seq(1, 40, 3), labels = agg_Poland$Group.1[seq(1, 40, 3)])

plot(agg_Spain$x, xaxt = "n")
axis(1, at = seq(1, 40, 3), labels = agg_Spain$Group.1[seq(1, 40, 3)])

plot(agg_Belgium$x, xaxt = "n")
axis(1, at = seq(1, 40, 3), labels = agg_Belgium$Group.1[seq(1, 40, 3)])


# If this code gets automated and cleaned properly,
#  you should be able to easily add other countries as well as other tasks.
# E.g.:

# Routine manual
# 4.A.3.a.3	Controlling Machines and Processes
# 4.C.2.d.1.i	Spend Time Making Repetitive Motions
# 4.C.3.d.3	Pace Determined by Speed of Equipment
