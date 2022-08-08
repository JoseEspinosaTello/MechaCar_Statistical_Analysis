library("dplyr")

# Deliverable 1

#read in csv file and create dataframe
MechaCar_df <- read.csv(file = "MechaCar_mpg.csv", check.names = F, stringsAsFactors = F)
head(MechaCar_df)

#create linear model
model <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, MechaCar_df)

#summarize linear model
summary(model) 


# Deliverable 2
arize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI), .groups = 'keep') 

lot_summary <- suspension_coil_df %>% group_by(Manufacturing_Lot) %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI), .groups = 'keep')

# Deliverable 3
# read csv and create a dataframe
suspension_coil_df <- read.csv(file = "Suspension_Coil.csv", check.names = F, stringsAsFactors = F)

#creates a statistical summary
total_summary <- suspension_coil_df %>% summ

#select random 50 from total population
sample <- suspension_coil_df %>% sample_n(50) 

# compare means of population with population mean
t.test(suspension_coil_df$PSI, mu = 1500) 

# sample of population compared with population mean of 1500 PSI
t.test(sample$PSI, mu = 1500) 

# T-Test for Lot 1
t.test(subset(sample$PSI, sample$Manufacturing_Lot == "Lot1"), mu=1500)


#T-Test for Lot 2
t.test(subset(sample$PSI, sample$Manufacturing_Lot == "Lot2"), mu=1500)

# T-Test for Lot 3
t.test(subset(sample$PSI, sample$Manufacturing_Lot == "Lot3"),mu=1500)
