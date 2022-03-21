x <- 6source("data.R")
# Client Fallout Rate by Program-----
# clients$Charted <- is.na(clients$)
# clients %>% 
#   group_by(Referral) %>% 
#   #filter() %>% # Changed from FirstChartingDate
#   summarise(
#     Clients = n(),
#     Utilization = sum(Charted),
#     Fallout = n() - sum(Charted)
#   )# %>% write.csv("output//client fallout by program.csv")
# 
# sum(is.na(clients$FirstChartingDate))

# Clinicians at FreedomWorks-------
colnames(transactions)
transactions %>% group_by(Referral, Clinician) %>% filter(Referral == "Freedom Works") %>% count()





# Transactions distribution by day of week----
transactions %>% mutate(
  DayOfWeek = lubridate::wday(Day)
) %>% select(DayOfWeek, everything()) %>% ggplot() + geom_bar(aes(DayOfWeek))


# Client count by program----
clients %>% group_by(Referral) %>% count()

# BILL + NB Hours
transactionsBNB <- readRDS("transactions.RDS")
dim(transactions)
dim(transactionsBNB)

# Week over Week hours by Clinician----

transactions %>%
  filter(Clinician == "Rolando Ruiz") %>%
  group_by(Clinician, Week) %>%
  summarize(
    Clients = n_distinct(Client),
    Earnings = Earnings(sum(Units) / 4),
    Hours = sum(Units) / 4
  ) %>% 
  ungroup() %>%
  mutate(
    #`% Chng` = scales::percent((Hours - lead(Hours)) / lead(Hours)),
    `%Chng Avg` = 
      scales::percent((Hours - mean(Hours, na.rm=T))/mean(Hours,na.rm=T))
  ) %>% 
  arrange(desc(Week)) %>%
  select(-Clinician)


### CPRS Distinct Clients + Total Fee----

transactions %>% select(Clinician, Client, Fee, Units) %>% 
  mutate(
    Clinician = str_to_title(Clinician),
    Client = str_to_title(Client)
  ) %>%
  group_by(Clinician) %>% 
  summarize(`Unique Clients` = n_distinct(Client),
            `Total Earnings` = 25 * sum(Units)/4,
            `Total Hours` = sum(Units)/4,
            `Hours til Bonus` = 500 - sum(Units)/4,
            TotalFee = sum(Fee),
            Units = sum(Units)
  ) %>% kable

###

### Client by Clinician----
client_summary <- transactions %>% select(Clinician, Client, Fee) %>% 
  mutate(
    Clinician = str_to_title(Clinician),
    Client = str_to_title(Client)
  ) %>%
  group_by(Client) %>% 
  summarize(Clinician = n_distinct(Clinician),
            TotalFee = sum(Fee)
  ) %>% mutate(
    Clinician = as.ordered(Clinician)
  )

client_summary %>% 
  ggplot(aes(x=TotalFee, fill=Clinician)) +
  geom_histogram(binwidth=500, color="Black")

client_summary %>% 
  ggplot(aes(x=TotalFee, fill=Clinician)) +
  geom_density(position="stack")

###

### Revenue by CPRS Worker----
df <- transactions %>% select(Clinician, Day, Fee) %>% 
  group_by(Clinician, Day) %>% 
  summarize(TotalFee = sum(Fee)) 

df <- df %>% mutate(
  Clinician = str_to_title(Clinician),
  Month = as.Date(cut(Day, breaks = "month")),
  Week = as.Date(cut(Day, breaks = "week", start.on.monday = FALSE))
)
###

### Monthly / Weekly Revenue----
df <- transactions %>% select(Day, Fee) %>% 
  group_by(Day) %>% 
  summarize(TotalFee = sum(Fee)) 

df <- df %>% mutate(
  Month = as.Date(cut(Day, breaks = "month")),
  Week = as.Date(cut(Day, breaks = "week", start.on.monday = FALSE))
)

ggplot(data = df,
       aes(Month, TotalFee)) +
  stat_summary(fun = sum,
               geom = "bar") + 
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "1 month") + labs(title = "Total Revenue by Month")

ggplot(data = df,
       aes(Week, TotalFee)) +
  stat_summary(fun = sum,
               geom = "bar") + 
  scale_x_date(
    labels = date_format("%Y-%m-%d"),
    breaks = "1 month") + labs(title = "Weekly Total Revenue")
###

dt <- data.table(transactions)
dt <- dt[, .(Client, Clinician, Units, Fee)]
dt[, `All-time Fees` := sum(Fee), by = .(Clinician)]
dt[, .SD[.N], by = Clinician]


reactable(transactions, filterable = TRUE, minRows = 10)

gg_dt <-
  transactions %>% select(Clinician, Client, Fee, Units, Day) %>%
  mutate(
    Clinician = str_to_title(Clinician),
    Client = str_to_title(Client),
    Earnings = 25 * Units / 4,
    Month = as.Date(cut(Day, breaks = "month")),
    Week = as.Date(cut(Day, breaks = "week", start.on.monday = FALSE)) + 6
  ) %>%
  unclass() %>% as.data.frame(stringsAsFactors = T) # Convert all character vars to factor

gg_dt %>% group_by(Clinician, Month) %>% summarize(
  `Unique Clients` = n_distinct(Client),
  `Total Earnings` = 25 * sum(Units) /
    4,
  `Total Hours` = sum(Units) /
    4,
  `Hours til Bonus` = 500 - sum(Units) /
    4,
  #`All Time Fees` = sum(Fee),
  Units = sum(Units)
) %>%
  filter(`Total Earnings` > 500) %>%
  ggplot(aes(x = `Month`, y = `Total Earnings`)) +
  geom_point(aes(col = Clinician), size = 3) +
  geom_line(aes(col = Clinician)) + ylim(0, 10000) +
  scale_x_date(
    date_labels = "%m/%Y",
    date_breaks = "1 month",
    limits = c(as.Date("2021-06-01"), as.Date("2022-01-31"))
  )


#### Hours this week; Week over Week %Change; Hours until Bonus ####

two_wks_ago <-
  as.Date(cut(Sys.Date(), breaks = "week", start.on.monday = FALSE)) - 14 # Week Prior
last_wk <-
  as.Date(cut(Sys.Date(), breaks = "week", start.on.monday = FALSE)) - 7 # Week of

gg_dt %>% group_by(Clinician, Week) %>%
  summarize(`Earnings last week` = 25 * sum(Units) / 4,
            `Hours last week` = sum(Units) / 4) %>%
  filter(Week == as.Date("2022-01-23")) %>%
  left_join({
    gg_dt %>% group_by(Clinician, Week) %>%
      summarize(
        `Earnings this week` = 25 * sum(Units) / 4,
        `Hours this week` = sum(Units) / 4
      ) %>%
      filter(Week == as.Date("2022-01-30"))
  }, by = "Clinician") %>%
  left_join({
    gg_dt %>% group_by(Clinician) %>% summarize(`All-Time Earnings` = 25 * sum(Units) / 4,
                                                `Total Hours` = sum(Units) / 4,
                                                `Hours til Bonus` = max(0, 500 - sum(Units) / 4))
  }, by = "Clinician") %>%
  mutate(`Hours %Chg Wk/Wk` = 100 * (`Hours this week` - `Hours last week`) / `Hours last week`)

# ------------- Heat Maps
transactions %>% 
  filter(BillingType=="BILL" & Week >= "2021-11-28") %>% 
  ggplot(aes(Clinician, Client)) + 
  geom_bin_2d() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## Top Billed Clients
transactions %>%
  group_by(Client) %>%
  summarize(Units = sum(Units),
            Revenue = sum(Fee)) %>%
  arrange(desc(Units))

## CPRS Caseload Week over Week (non-cumulative) ------

transactions %>%
  select(Clinician, Week, Client) %>%
  group_by(Clinician, Week) %>%
  summarize(Caseload = n_distinct(Client)) %>%
  filter(!(Clinician %in% c("Tony Fondie"))) %>%
  ggplot(aes(Week, Caseload, col = Clinician)) +
  geom_point(size = 0.1) + geom_line(size = 1) + ylim(0, 20)

#--- RollSum

Roll.Caseload(transactions) %>%
  filter(!(Clinician %in% c("Tony Fondie", "Jacob Lusk"))) %>%
  ggplot(aes(Week, Caseload, col = Clinician)) +
  geom_point(size = 1) + geom_line(size = 1) +
  ylim(0, 20) +
  scale_x_date(
    labels = date_format("%Y-%m-%d"),
    breaks = "2 weeks",
    limits = c(as.Date("2021-11-07"), as.Date("2022-02-14"))
  )

### ------------ Client Week over Week Hours filtered by CPRS

# Client Number of Distinct CPRS Workers per week
transactions %>% group_by(Client, Week) %>% summarize(nClinicians = n_distinct(Clinician)) %>% View


weekly_hrs_client <-
  transactions %>% group_by(Client, Week) %>% summarize(Hours = sum(Units) /
                                                          4)
weekly_hrs_client %>% ggplot(aes(Week, Hours, col = Client)) + geom_point(size =
                                                                            1) + geom_step(size = 1)

