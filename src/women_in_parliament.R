
# Install packages
install.packages("ggplot2", dependencies = TRUE)
install.packages("plotly", dependencies = TRUE)
library(ggplot2)
library(plotly)

# Load and check data
data <- read.csv("data/Viz5_August_Female_Political_Representation.csv")
region_data <- read.csv("data/all.csv")

region_data
data

colnames(data)
range(data$Year) # Range: [1997, 2019]

##### Graphing average global proportions by year #####

# Create new empty data frame to plot average global proportions 
len <- diff(range(data$Year))
yearly_global_averages <- data.frame("Year" = rep(NA, len), "Average_Proportion" = rep(NA, len))
yearly_global_averages

# Populate data frame by looping through years
year = min(data$Year)
for(i in 1:len){
  # Update year
  yearly_global_averages$Year[i] = year
  year = year + 1
  
  # Find average proportion in that year
  temp_subset <- subset(data, Year == yearly_global_averages$Year[i])
  yearly_global_averages$Average_Proportion[i] = mean(temp_subset$Proportion.of.seats.held.by.women.in.national.parliaments...., na.rm=T)
}

# Graph scatterplot
p <- ggplot(data=yearly_global_averages, mapping = aes(x=Year, y=Average_Proportion)) +
  geom_point()+
  ggtitle("Global Proportions of Women in Government Positions")+
  ylab("Average Global Proportion")+
  xlab("Year")

p
ggplotly(p)


##### Graphing and comparing select countries and regions #####

# Merge data frames
data <- merge(data, region_data, by.x="Country.Name", by.y = "name", all.x=TRUE, all.y=F)
data <- merge(data, yearly_global_averages, by="Year")

# Subset the data in any way you'd like. Here are the 7 countries with the highest GDPs
countries_with_highest_GDPs <- subset(data, Country.Name == "United States" | 
                                        Country.Name == "China" |
                                        Country.Name == "Japan" |
                                        Country.Name == "Germany" |
                                        Country.Name == "India" |
                                        Country.Name == "United Kingdom" |
                                        Country.Name == "France"
                                      )

# Clean up the data slightly (the merging step did not map rows perfectly)
countries_with_highest_GDPs$sub.region = ifelse (countries_with_highest_GDPs$Country.Name == "United States", "Americas", countries_with_highest_GDPs$sub.region)
countries_with_highest_GDPs$sub.region = ifelse (countries_with_highest_GDPs$Country.Name == "United Kingdom", "Northern Europe", countries_with_highest_GDPs$sub.region)

# Plot data and add styles
p <- ggplot(data=countries_with_highest_GDPs, mapping = aes(x=Year, y=Proportion.of.seats.held.by.women.in.national.parliaments...., shape=Country.Name, color=sub.region)) +
  geom_point(na.rm=T, size=3, alpha = .8) +
  scale_shape_manual(values=seq(0,9)) +
  geom_line(data=countries_with_highest_GDPs,aes(x=Year,y=Average_Proportion), colour="gray")+
  ggtitle("Proportions of Women in Government Positions")+
  ylab("Average Proportion") +
  xlab("Year") +
  theme(axis.title.x = element_text(margin = margin(t = 20)))+
  theme(axis.title.y = element_text(margin = margin(r = 20)))
p <- style(p, showlegend = FALSE, traces = 8:14)

ggplotly(p)