#clear all lists
rm(list=ls())

#check if package is installed, if not, install it; returns boolean
require(tidyverse) || install.packages("tidyverse")

path = getwd()

database = read.csv(file = paste0(path, "/Salaries.csv"), header = TRUE, stringsAsFactors = FALSE)

database$BasePayInt = as.integer(database$BasePay)
database$OvertimePayInt = as.integer(database$OvertimePay)

overtime.workers = length(data.overtime$EmployeeName)
percent.of.overtime.workers = overtime.workers / length(database$EmployeeName) * 100


#find hourly salary:
#The 2,087-hour divisor must be used for almost all civilian Federal employees 
#in an executive agency, including employees under the General Schedule (GS), and most other employees, 
#unless excluded by law.

database = database %>%
  mutate(hourly.pay = BasePayInt / 2087) %>%
  filter(hourly.pay > 0)

data.overtime = database %>%
  filter(OvertimePay != "0.0")


data.overtime = data.overtime %>%
  group_by(JobTitle) %>%
  summarise(average.hourly.pay = mean(hourly.pay))

#show most overtimers by hourly pay
p = ggplot()
p = p + geom_bar(data = data.overtime, aes(x = average.hourly.pay, fill = JobTitle), stat = "bin")



