library(tidyverse)
library(tidyr)
library(readr)
library(dplyr)
library(data.table)
#library(ggplot2)
library(vroom)
library(forcats)
install.packages("plotly")
library(plotly)
setwd("C:\\Users\\vgp_1\\OneDrive\\Desktop\\google_data analysis\\project1")
myfolder = "archive_11"
csv_fileloads <- list.files(path = myfolder, pattern ="*.csv", full.names=T) 
csv_fileloads
combined_csv = lapply(csv_fileloads, read_csv)
view(combined_csv[1]) # there are 23 tibbles for each file in archive11
aave_coin <- combined_csv[1]
BNB_coin <- combined_csv[2]
BTC_coin <- combined_csv[3]
ADA_coin <- combined_csv[4]
LINK <- combined_csv[5]
ATOM_coin<- combined_csv[6]
CRO_coin <- combined_csv[7]
DOGE_coin <- combined_csv[8]
EOS_coin <- combined_csv[9]
Ethereum_coin <- combined_csv[10]
miota_coin <- combined_csv[11]
LTC_coin <- combined_csv[12]
XMR_coin <- combined_csv[13]
XEM_coin <- combined_csv[14]
DOT_coin <- combined_csv[15]
SOL_coin <- combined_csv[16]
XLM_coin <- combined_csv[17]
USDT_coin <- combined_csv[18]
TRX_coin <- combined_csv[19]
UNI_coin <- combined_csv[20]
USDC_coin <- combined_csv[21]
WBTC_coin<- combined_csv[22]
XRP_coin <- combined_csv[23]
bitcoin_df<-bind_rows(aave_coin, BNB_coin, BTC_coin, ADA_coin, LINK, ATOM_coin, CRO_coin, DOGE_coin, EOS_coin, Ethereum_coin, miota_coin, LTC_coin, XMR_coin, XEM_coin, DOT_coin, SOL_coin, XLM_coin, USDT_coin, TRX_coin, UNI_coin, USDC_coin, WBTC_coin, XRP_coin)
view(bitcoin_df)
#create summary of the data frame
summary(bitcoin_df)
#separate date value into new columns with month, year, day
new_bitcoin_df <- bitcoin_df %>% mutate_at(vars(Date), funs(year, month, day))
#here there is a group by for year and month for each year and month avg of all numerical columns is calculated.
new_avg_bitcoin_values<-new_bitcoin_df %>% group_by(year, month, Name) %>% summarise(avg_marketcap = format(mean(Marketcap), nsmall =2), avghigh= format(mean(High), nsmall=2), avgLow=format(mean(Low), nsmall =2), avgOpen=format(mean(Open), nsmall=2), avgClose=format(mean(Close), nsmall=2), avgVol= format(mean(Volume), nsmall=2))

view(new_avg_bitcoin_values)
#sorting the new avg values in the new_avg_bitcoin_values dataframe.
df_sorted <- arrange(new_avg_bitcoin_values,desc(year))
view(df_sorted)
vroom_write(df_sorted, "df_bitcoin_sorted.csv")
#filtered_bitcoin_2021 <- df_sorted %>% filter(year == 2021 & month %in% c(1:12))
#view(filtered_bitcoin_2021)
#ggplot(data = filtered_bitcoin_2021, aes(x = Name, y = month))+
#  geom_line(mapping = )+
#  facet_wrap(~ month)+
#  theme(axis.text.x= element_text(angle=45))+
#  labs(title = "avg market value for each year and month")

#filter a subsection
##filtered_bitcoin_2021<-df_sorted %>% dplyr::filter(year ==2021 )
#getting the highest avg_marketcap values and the lowest.
##min_cap <- min(as.numeric(filtered_bitcoin_2021$avg_marketcap), na.rm=TRUE)
##max_cap <- max(as.numeric(filtered_bitcoin_2021$avg_marketcap), na.rm = TRUE)
# add the column to the data frame for plotting. tag each row as highest or lowest marketcap
##df_bitcoin_2021 <- filtered_bitcoin_2021 %>% group_by(month) %>%
##  mutate(cap_type = if_else(row_number() == which.max(avg_marketcap),"Highest","Normal")) %>% 
##  ungroup()
##view(df_bitcoin_2021)
#plotting
##ggplot(df_bitcoin_2021, aes(x = Name, y = avg_marketcap, fill = cap_type))+ 
##  geom_bar(stat = "identity", show.legend = TRUE)+
##  theme(axis.text.x= element_text(angle=90, hjust = 1))+ 
##  facet_wrap(~ month, scales = "free_x")+
##  scale_fill_manual(values = c(
##    "Highest" = "gold",
##    "Lowest" = "blue",
##    "Normal" = "grey70"
##  ))+
##  labs(title = "for year 2021 average_marketcap and average_volume", x = "name of bitcoin", y = "average Market Cap", fill = "performance")+
##  theme_minimal()

# Identify top crypto per month
##top_caps <- df_sorted %>% group_by(month) %>% slice_max(order_by = as.numeric(avg_marketcap), n=1, with_ties = FALSE) %>% 
##  mutate(cap_type = "Highest") %>%
##  select(month, Name, cap_type)
#labeling avg_marketcap into highest and normal and adding a column to df
##df_2021_labeled <- df_sorted %>% left_join(top_caps, by = c("month", "Name")) %>%
##  mutate(cap_type = ifelse(is.na(cap_type),"Normal", cap_type))

#using forcats to sort the resulting bar graph
##df_2021_labeled <- df_2021_labeled %>% group_by(month)%>% 
##  mutate(Name = fct_reorder(Name, as.numeric(avg_marketcap))) %>% ungroup()
#adding labels on top of bars
#plotting: 
##baseplot=ggplot(df_2021_labeled, aes(x=Name, y=avg_marketcap, fill = cap_type)) +
##  geom_bar(stat = "identity")+ facet_wrap(~ month, scales = "free_x")+
##  scale_fill_manual(values = c("Highest" = "gold", "Normal" = "gray80"))+
##  labs(title = "TOp Performing Bitcoin by Average Market Cap (2021)",
##       x = "Crytocurrency Name", y = "Average Market Cap (USD)", fill = "Performance") +
##  theme_minimal() +
##  theme(axis.text.x = element_text(angle = 90, hjust =1, size = 8),
##        plot.title = element_text(hjust = 0.5, face = "bold"))+
##  geom_label(aes(label = paste0(round(as.numeric(avg_marketcap) /1e9, 1), "B"), vjust = -0.5, size = 0.5, fill = "white", color = "black"))
##ggplotly(baseplot, tooltip = "text")
#(This divides by 1e9 to show values in billions)
df_2021 <- df_sorted %>% filter(year == 2021) %>% 
  mutate(avg_marketcap = as.numeric(gsub(",", "", avg_marketcap)))
df_2021 <- df_2021 %>% group_by(month)%>% 
  mutate(cap_type = if_else(avg_marketcap == max(avg_marketcap, na.rm= TRUE), "Highest", "Normal")) %>%
ungroup()
#view(df_2021)
month_list <- unique(df_2021$month)
plot_list <- lapply(month_list, function(m){
  df_month <- df_2021 %>% filter(month == m)
  plot_ly(data = df_month, 
          x= ~Name, 
          y = ~avg_marketcap,
          type = 'bar',
          color = ~cap_type,
          colors = c("Normal" = "lightgray", "Highest"= "gold"),
          text = ~paste("Name:", Name,
                        "<br>Market Cap:", scales::comma(avg_marketcap)),
          hoverinfo = "text") %>%
    layout(title = paste("Month", m),
           xaxis = list(title = "Crypto", tickangle = -45),
           yaxis = list(title = "Market Cap"),
           showlegend = FALSE)
})
subplot(plot_list, nrows = 3, margin = 0.04, shareY = TRUE, titleY = TRUE)
