############################################# Project on "Lego" Database ###########################################

# A) colors.csv
setwd("C:\\Users\\user\\Downloads\\Unfinished-Work-Directory\\Intellipaat\\Project 2\\Datasets")
getwd()

# Loading Pacakges
library(ggplot2)
library(readr)
##install.packages("tidyverse")
library(tidyverse,warn.conflicts = FALSE)
library(RColorBrewer)

# a.Load the 'colors.csv' file and get the number of unique colors
colors_data = read.csv("colors.csv")
head(colors_data)
colors_names = length(unique(colors_data$name))
paste("Number of Unique colors in Lego sets =",colors_names) 

# b.In the 'rgb'  column, add '#' as the prefix to all the values
colors_data = colors_data%>%mutate(rgb=paste0("#",str_trim(rgb)))
new_colors_data = colors_data$rgb
names(new_colors_data) = new_colors_data

# c.Make a bar-plot to get the count of colors in 'is_trans' column
colors_data %>%group_by(is_trans)%>%summarize(c=n())%>% 
  ggplot(aes(x=is_trans,y=c,fill=is_trans))+geom_bar(stat="identity")




# B)	sets.csv:
sets_data = read.csv("sets.csv")
head(sets_data)
str(sets_data)

# a.From the 'sets' dataframe create a new dataframe-'part_year', where the first column is 'year' & second column is  'avg_parts', which basically has the values for average number of parts per year
part_year = sets_data%>%select(year,num_parts)%>%group_by(year)%>%summarise(avg_parts=mean(num_parts))

# b.Create a line plot on 'part_year' where, 'avg_parts' would be on y-axis & 'year' would be on x-axis.  Add geom_point() into the same plot
ggplot(data = part_year,aes(x=year,y=avg_parts))+
  geom_line(col="palegreen4",size=1)+
  geom_point(col="darkgoldenrod",size=2) + 
  labs(title = "Average Parts per year")

# c.From the 'sets' dataframe create a new dataframe-'theme_year', where the first column is 'year' & second column is  'theme_count', which gives the count of unique themes for each year
theme_year = sets_data%>%select(year,theme_id)%>%group_by(year)%>%summarise(theme_count=length(unique(theme_id)))

# d.Create a line plot on 'theme_year' where, 'theme_count' would  be on  y-axis & 'year' would be on x-axis.  Add geom_point() into the same plot
ggplot(data = theme_year,aes(x=year,y=theme_count))+
  geom_line(col="palegreen4",size=1)+
  geom_point(col="darkgoldenrod",size=2) + 
  labs(title = "Themes used per year")




# C)	parts.csv & part_categories.csv
part_categories = read.csv("part_categories.csv")    
parts = read.csv("parts.csv")
str(part_categories)
str(parts)

# a.Change column names: i.Change the name of 2nd column in 'part_categories' to 'part_category_name'
colnames(part_categories)[2] = "part_category_name"

# ii.Change the name of 2nd column in 'parts' to 'part_name'
colnames(parts)[2] = "part_name"

# iii.Change the name of 2nd column in 'colors' to 'color_name'
colnames(colors_data)[2] = "color_name"

# b.Left Join 'part_categories' to 'parts' where "id" equals "part_cat_id" and store the result in 'part_cat'
part_cat = part_categories%>%left_join(parts,by=c("id"="part_cat_id"))
head(part_cat)

# c.Create a bar-plot which would give the number of parts under each category. Categories should be on y-axis and number of parts should be on x-axis. The bars should be arranged in decreasing order of 'No of parts'
parts_per_cat = part_cat%>%select(part_category_name,part_num)%>%
  group_by(part_category_name)%>%
  summarise(parts_under_cat=length(unique(part_num)))%>%
  arrange(desc(parts_under_cat))

ggplot(data = parts_per_cat,aes(x=reorder(part_category_name,parts_under_cat),y=parts_under_cat,fill=part_category_name)) + coord_flip() + geom_bar(stat="identity") + theme(legend.position = "none") +
  labs(title="Parts under Category",x="Category",y="No of Parts")


# Joining 'part_cat' with 'inventory_parts' & 'colors'
inventory_parts = read.csv("inventory_parts.csv")

part_color = part_cat%>%left_join(inventory_parts,by="part_num")%>%
  left_join(colors_data,by=c("color_id"="id"))

partsp_col = part_color%>%select(color_name,rgb,part_name)%>%
  group_by(color_name,rgb)%>%summarise(part_per_color=length(unique(part_name)))%>%
  arrange(desc(part_per_color))%>%head(100)
head(part_color)




# D) themes.csv:
library(ggrepel) 
themes = read.csv("themes.csv")

# a.Change column names: i.	Change the name of 2nd column in 'themes' to 'theme_name'
colnames(themes)[2] = "theme_name"

# ii.	Change the name of 2nd column in 'sets' to 'set_name'
colnames(sets_data)[2] = "set_name"

# iii.	Remove the 3rd column from 'themes' and store it back to 'themes'
themes = themes[,-3]

# b.Left join 'themes' on 'sets' where "id" equals "theme_id" and  store the result in  'set_themes'
set_themes = themes %>%left_join(sets_data,by=c("id"="theme_id"))

# c.Create a bar plot which would give the top 10 logo sets with maximum part counts. 'Set name' would be on y-axis and 'Parts count' would be on x-axis. The bars should be arranged in decreasing order of 'Parts count'
set_count<-set_themes%>%
  group_by(set_name)%>%
  summarise(parts_count=sum(num_parts))%>%
  arrange(desc(parts_count))

head(set_count,10) %>% 
  ggplot(aes(x=reorder(set_name,parts_count),y=parts_count,fill=set_name)) + 
  geom_bar(stat = "identity")+coord_flip() + theme(legend.position = "none") +
  labs(title = "Top 10 logo sets with maximum part counts",x="Parts count",y="Set Name")

# d.Build a tree map which depicts the top 50 themes based  on 'set counts'
##install.packages("treemap")
themes_per_set = set_themes%>%
  select(theme_name,set_num)%>%
  group_by(theme_name)%>%
  summarise(set_cnt=length(unique(set_num)))%>%
  arrange(desc(set_cnt))%>%head(50)

##require(treemap)
library(treemap)
treemap(themes_per_set,
        index="theme_name",
        vSize="set_cnt",
        type="index",
        fontsize.labels=7,
        palette=new_colors_data,
        title="Lego Themes Based on Set Counts")

