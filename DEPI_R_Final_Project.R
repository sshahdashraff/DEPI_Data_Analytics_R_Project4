library(dplyr)
library(ggplot2)
library(tidyr)

rm=(list=ls())
hotel_bookings <- read.csv("C:/Users/shash/OneDrive/Desktop/hotel_bookings.csv")
View(hotel_bookings)

# Handle missing values
# Impute 'children' with median
hotel_bookings$children[is.na(hotel_bookings$children)] <- median(hotel_bookings$children, na.rm = TRUE)

# Impute 'country' with the most frequent value
most_frequent_country <- hotel_bookings %>% count(country) %>% top_n(1, n) %>% pull(country)
hotel_bookings$country[is.na(hotel_bookings$country)] <- most_frequent_country

# Replace 'agent' missing values with 0
hotel_bookings$agent[is.na(hotel_bookings$agent)] <- 0

# Drop 'company' column due to too many missing values
hotel_bookings <- hotel_bookings %>% select(-company)

# Remove rows with 'distribution_channel' equal to "undefined"
hotel_bookings <- hotel_bookings %>%
  filter(distribution_channel != "Undefined")


# 1. Which distribution channel generates the highest revenue?
revenue_per_channel <- hotel_bookings %>%
  group_by(distribution_channel) %>%
  summarize(total_revenue = sum(adr * (stays_in_weekend_nights + stays_in_week_nights), na.rm = TRUE))

# Create the pie chart
ggplot(revenue_per_channel, aes(x = "", y = total_revenue, fill = distribution_channel)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.title = element_blank()) +
  ggtitle("Total Revenue by Distribution Channel") +
  scale_fill_brewer(palette = "Set3") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = scales::dollar(total_revenue)), position = position_stack(vjust = 0.5))

# 2. What is the relationship between lead time and cancellations?
lead_cancellation <- hotel_bookings %>%
  group_by(lead_time) %>%
  summarize(cancellation_rate = mean(is_canceled, na.rm = TRUE))

# Plot lead time vs cancellation rate
ggplot(lead_cancellation, aes(x = lead_time, y = cancellation_rate)) +
  geom_line(color="#7ae582",size=2) +
  ggtitle("Lead Time vs Cancellation Rate") +
  theme_minimal()

# 3. How do special requests impact ADR?
special_requests_adr <- hotel_bookings %>%
  group_by(total_of_special_requests) %>%
  summarize(avg_adr = mean(adr, na.rm = TRUE))

# Plot special requests vs ADR
ggplot(special_requests_adr, aes(x = total_of_special_requests, y = avg_adr,
    fill = factor(total_of_special_requests))) +
  geom_bar(stat = "identity") +
  ggtitle("Special Requests vs Average Daily Rate (ADR)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")

# 4. Revenue by month
revenue_by_month <- hotel_bookings %>%
  group_by(arrival_date_month) %>%
  summarize(total_revenue = sum(adr * (stays_in_weekend_nights + stays_in_week_nights),
                                na.rm = TRUE))

# Plot revenue by month
ggplot(revenue_by_month, aes(x = reorder(arrival_date_month, -total_revenue),
  y = total_revenue, fill = arrival_date_month)) +
  geom_bar(stat = "identity") +
  ggtitle("Total Revenue by Month") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")

# 5. Length of stay vs ADR
length_of_stay_adr <- hotel_bookings %>%
  mutate(length_of_stay = stays_in_weekend_nights + stays_in_week_nights) %>%
  group_by(length_of_stay) %>%
  summarize(avg_adr = mean(adr, na.rm = TRUE))

# Plot length of stay vs ADR
ggplot(length_of_stay_adr, aes(x = length_of_stay, y = avg_adr)) +
  geom_line(color = "#7ae582", size = 2) +  # Increase the size argument
  ggtitle("Length of Stay vs Average Daily Rate (ADR)") +
  theme_minimal()

# 6. Room type preference for repeat guests
room_repeat_guests <- hotel_bookings %>%
  filter(is_repeated_guest == 1) %>%
  count(assigned_room_type)

# Plot room types for repeat guests
ggplot(room_repeat_guests, aes(x = assigned_room_type, y = n,
                               fill = assigned_room_type)) +
  geom_bar(stat = "identity") +
  ggtitle("Room Types Preferred by Repeat Guests") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")

# 7. Revenue by guest type
guest_type_revenue <- hotel_bookings %>%
  group_by(customer_type) %>%
  summarize(total_revenue = sum(adr * (stays_in_weekend_nights + stays_in_week_nights),
                                na.rm = TRUE))

# Plot revenue by guest type
ggplot(guest_type_revenue, aes(x = reorder(customer_type, -total_revenue),
                               y = total_revenue, fill = customer_type)) +
  geom_bar(stat = "identity") +
  ggtitle("Total Revenue by Guest Type") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  coord_flip() +
  xlab("Customer Type") +  # Add x-axis label for clarity
  ylab("Total Revenue")  # Add y-axis label for clarity

# 8. Country of origin impact on bookings
country_bookings <- hotel_bookings %>%
  count(country, sort = TRUE) %>%
  top_n(10)

# Plot top 10 countries
ggplot(country_bookings, aes(x = reorder(country, n), y = n, fill = country)) +
  geom_bar(stat = "identity") +
  ggtitle("Top 10 Countries by Number of Bookings") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")




















# 9. Hotel type (city vs resort) impact on cancellations
hotel_cancellation_rate <- hotel_bookings %>%
  group_by(hotel) %>%
  summarize(cancellation_rate = mean(is_canceled, na.rm = TRUE))

# Plot cancellation rates by hotel type
ggplot(hotel_cancellation_rate, aes(x = hotel, y = cancellation_rate, fill = hotel)) +
  geom_bar(stat = "identity") +
  ggtitle("Cancellation Rate by Hotel Type (City vs Resort)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")