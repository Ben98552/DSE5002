library(dplyr)
library(tidyr)
library(ggplot2)

#import necessary data
r_project_data = read.csv('GitHub/DSE5002/DSE5002/R_Project/r_project_data.csv')

#split all US based employees from original data frame into a new data frame
us_data <- r_project_data[r_project_data$employee_residence == 'US', ]

#split all non-US based employees from original data frame into a new data frame
non_us_data <- r_project_data[r_project_data$employee_residence != 'US', ]

#Creates the average of salaries based on company size
salary_by_comp_size <- tapply(r_project_data$salary_in_usd, r_project_data$company_size, mean)


#Creates bar plot to show average salary based on company size worldwide
ggplot(r_project_data,
       aes(x = company_size, y= salary_in_usd, fill= company_size)) +
         geom_bar(stat = 'summary', fun= 'mean')+
         labs(x = "Company Size",
              y = 'Salary',
              title = 'Salaries based on Company Size Worldwide')

#Creates the average of salaries based on company size in US
us_salary_by_comp_size <- tapply(us_data$salary_in_usd, us_data$company_size, mean)

#Creates bar plot to show average salary based on company size in US
ggplot(us_data,
       aes(x = company_size, y= salary_in_usd, fill= company_size)) +
  geom_bar(stat = 'summary', fun= 'mean')+
  labs(x = "Company Size",
       y = 'Salary',
       title = 'Salaries based on Company Size in US')

#Creates the average of salaries based on company size in non-US
non_us_salary_by_comp_size <- tapply(non_us_data$salary_in_usd, non_us_data$company_size, mean)

#Creates bar plot to show average salary based on company size in non-US
ggplot(non_us_data,
       aes(x = company_size, y= salary_in_usd, fill= company_size)) +
  geom_bar(stat = 'summary', fun= 'mean')+
  labs(x = "Company Size",
       y = 'Salary',
       title = 'Salaries based on Company Size for Non-US Employees')

#Creates a new column to turn from multiple variables down to a two variable column
r_project_data <- r_project_data %>%
  mutate(residence_group= ifelse(employee_residence== "US", "US", "Non-US"))

#Take out all employment types that are not Fulltime
FT_employees <- r_project_data[r_project_data$employment_type == 'FT',]

#Creates dataframe for just large companies
large_comp <- FT_employees[FT_employees$company_size== 'L', ]

#Creates dataframe for just medium companies
med_comp <- FT_employees[FT_employees$company_size== 'M', ]

#Creates dataframe for just small companies
small_comp <- FT_employees[FT_employees$company_size== 'S', ]

#Creates boxplot that signifies the salaries of US and Non-Us based employees by experience level for large companies
options(scipen=999)
ggplot(large_comp, aes(x=residence_group, y=salary_in_usd, fill = experience_level)) +
  geom_boxplot() +
  labs(x='Employee Residence', y = 'Salary (USD in Thousands)', title = 'Salaries of Residence by Experience for Large Companies') +
  facet_grid( ~ experience_level)

#Creates boxplot that signifies the salaries of US and Non-Us based employees by experience level for medium companies
options(scipen=999)
ggplot(med_comp, aes(x=residence_group, y=salary_in_usd, fill = experience_level)) +
  geom_boxplot() +
  labs(x='Employee Residence', y = 'Salary (USD in Thousands)', title = 'Salaries of Residence by Experience for Medium Companies') +
  facet_grid( ~ experience_level)

#Creates boxplot that signifies the salaries of US and Non-Us based employees by experience level for small companies
options(scipen=999)
ggplot(small_comp, aes(x=residence_group, y=salary_in_usd, fill = experience_level)) +
  geom_boxplot() +
  labs(x='Employee Residence', y = 'Salary (USD in Thousands)', title = 'Salaries of Residence by Experience for Small Companies') +
  facet_grid( ~ experience_level)



