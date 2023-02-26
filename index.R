### Summary Statistics

#reading data from csv file
data <- read.csv("HP_Checkouts_by_Title.csv")
head(data)

#average checkouts by check out type
library(dplyr)
data%>%
  group_by(CheckoutType)%>%
  summarise(avg_checkouts = mean(Checkouts))

#Year with most checkouts
data%>%
  group_by(CheckoutYear)%>%
  summarise(Checkouts = sum(Checkouts))%>%
  arrange(desc(Checkouts))

#Most/least number of checkout by Month
data%>%
  group_by(CheckoutMonth)%>%
  summarise(Checkouts = sum(Checkouts))%>%
  arrange(desc(Checkouts))%>%
  arrange(desc(CheckoutMonth))

#average checkouts by Material Type
data%>%
  group_by(MaterialType)%>%
  summarise(avg_checkout = mean(Checkouts))

#summarizing  the Checkouts by CheckoutType
data%>%
  group_by(Publisher)%>%
  summarise(avg_checkouts = mean(Checkouts))%>%
  arrange(desc(avg_checkouts))


### Data Visualization

#### Number of Checkouts by Material Type for each Year

# Create a new column for year
library(ggplot2)
# Aggregate data by MaterialType and Year
data_agg <- aggregate(Checkouts ~ MaterialType + CheckoutYear, data, sum)

# Create line graph
ggplot(data_agg, aes(x = CheckoutYear, y = Checkouts, color = MaterialType)) +
  geom_line() +
  labs(title = "Number of Books Checked Out by Material Type",
       x = "Year",
       y = "Number of Checkouts",
       color = "Material Type") +
  scale_x_continuous(breaks = seq(2012, 2022, 1)) 

#### Number of Checkouts by Publisher for each Year

#filtering data for publisher Arthur A.Levin Books, Pottermore, Scholastic, Salamandra and Seizansha.
data2 <- data%>%
  filter(Publisher %in% c("Arthur A. Levine Books,", "Pottermore",
                          "Scholastic,", "Salamandra,", "Seizansha,"))
# Aggregate data by MaterialType and Year
data_agg <- aggregate(Checkouts ~ Publisher + CheckoutYear, data2, sum)

# Create line graph
ggplot(data_agg, aes(x = CheckoutYear, y = Checkouts, color = Publisher)) +
  geom_line() +
  labs(title = "Number of Books Checked Out by Different publisher",
       x = "Year",
       y = "Number of Checkouts",
       color = "Material Type") +
  scale_x_continuous(breaks = seq(2012, 2022, 1)) 

#### Distribution of Checkout by Year

#distribution of checkout by year
data$CheckoutYear <- as.factor(data$CheckoutYear)
data2 <- data[data$Checkouts < 300,]
data2%>%
  group_by(CheckoutYear)%>%
  summarise(Checkouts = sum(Checkouts))%>%
  ggplot(aes(x = reorder(CheckoutYear, -Checkouts), y = Checkouts, fill = CheckoutYear)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Checkouts by Year", x = "Year") +
  theme(legend.position = "none")
