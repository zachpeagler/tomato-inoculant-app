# cleaning
library(tidyverse)
library(gtools)
##### TOMATO INOCULANT METHOD (2024) #####

## TIM EXPERIMENTAL CONSTANTS

tim_germ_date <- "2024-04-01"

tim_treatment_order <- c("Control",
                         "Liquid",
                         "CG",
                         "CBG")

tim_levels <- c("A1", "A2", "A3", "A4", "A5", "A6",
                "A7", "A8", "A9", "A10", "A11", "A12", "A13",
                "B1", "B2", "B3", "B4", "B5", "B6",
                "B7", "B8", "B9", "B10", "B11", "B12", "B13",
                "C1", "C2", "C3", "C4", "C5", "C6",
                "C7", "C8", "C9", "C10", "C11", "C12", "C13",
                "D1", "D2", "D3", "D4", "D5", "D6",
                "D7", "D8", "D9", "D10", "D11", "D12", "D13",
                "E1", "E2", "E3", "E4", "E5", "E6",
                "E7", "E8", "E9", "E10", "E11", "E12",
                "F1", "F2", "F3", "F4", "F5", "F6",
                "F7", "F8", "F9", "F10", "F11", "F12",
                "G1", "G2", "G3", "G4", "G5", "G6",
                "G7", "G8", "G9", "G10", "G11", "G12",
                "H1", "H2", "H3", "H4", "H5", "H6",
                "H7", "H8", "H9", "H10", "H11", "H12"
)

## FLUORO 

li_data_file <- "C:/Github/Thesis/data/TIM/TIM24_Fluoro.csv"
li_data <- read.csv(li_data_file)[,c(2,3,7,8,9,10,17,28,32,34,35,36,39,40)] %>%
  filter(leak_pct<10 & gsw > 0) %>%
  mutate(Date = parse_date_time(Date, orders = "mdy"),
         DaysFromGermination = as.numeric(round(difftime(Date, tim_germ_date, units = c("days")), 0)),
         Date = as.Date(Date),
         Time = parse_date_time(Time, orders = "T"),
         Time = as.factor(format(Time, "%H:%M:%S")),
         Treatment = factor(Treatment, 
                            levels = tim_treatment_order),
         Inoculation = case_when(
           Treatment=="Control"~FALSE,
           Treatment=="Liquid"~TRUE,
           Treatment=="CG"~FALSE,
           Treatment=="CBG"~TRUE
         ),
         Chitosan = case_when(
           Treatment=="Control"~FALSE,
           Treatment=="Liquid"~FALSE,
           Treatment=="CG"~TRUE,
           Treatment=="CBG"~TRUE
         ),
         Column = case_when(
           Column==1~"A",
           Column==2~"B",
           Column==3~"C",
           Column==4~"D",
           Column==5~"E",
           Column==6~"F",
           Column==7~"G",
           Column==8~"H",
         ),
         Plant = paste0(Column, Row),
         LogitPhiPS2 = logit(PhiPS2, FALSE),
         Column = as.factor(Column),
         Row = as.factor(Row),
         Plant = as.factor(Plant),
         Plant = factor(Plant, levels = tim_levels),
         P_atm = P_atm
  ) %>%
  rename(Pot=Column)

data_tim_fluoro <- li_data[,c(5,16,17,1,2,15,3,4,18,9,10,12,14,11,6,8,19)] %>%
  rename(AmbientHumidity = rh_s,
         AmbientTemperature = Tref,
         AmbientPressure = P_atm,
         AmbientLight = Qamb,
         LeafTemperature = Tleaf
  ) %>% mutate(
    AmbientLight = as.numeric(AmbientLight)
  )
save(data_tim_fluoro, file = "C:/Github/tomato-inoculant-app/app/data_tim_fluoro.RData")

## HEIGHT

h_data_file <- "C:/Github/Thesis/data/TIM/TIM24_Height.csv"
h_data <- read.csv(h_data_file) %>%
  mutate(Date = parse_date_time(Date, orders = "mdy"),
         DaysFromGermination = as.numeric(round(difftime(Date, tim_germ_date, units = c("days")), 0)),
         Date = as.Date(Date),
         Column = case_when(
           Column==1~"A",
           Column==2~"B",
           Column==3~"C",
           Column==4~"D",
           Column==5~"E",
           Column==6~"F",
           Column==7~"G",
           Column==8~"H",
         ),
         Inoculation = case_when(
           Treatment=="Control"~FALSE,
           Treatment=="Liquid"~TRUE,
           Treatment=="CG"~FALSE,
           Treatment=="CBG"~TRUE
         ),
         Chitosan = case_when(
           Treatment=="Control"~FALSE,
           Treatment=="Liquid"~FALSE,
           Treatment=="CG"~TRUE,
           Treatment=="CBG"~TRUE
         ),
         Treatment = factor(Treatment, levels = tim_treatment_order),
         Plant = as.factor(paste0(Column, Row)),
         Plant = factor(Plant, levels = tim_levels),
         Row = as.factor(Row),
         Column = as.factor(Column)
  ) %>%
  rename(Pot = Column)

data_tim_height <- h_data[,c(3,7,8,4,6,1,2,9,5)]

save(data_tim_height, file = "C:/Github/tomato-inoculant-app/app/data_tim_height.RData")

## DESTRUCTIVE SAMPLING

DS_data_file <- "C:/Github/Thesis/data/TIM/TIM24_DS.csv"
DS_data <- read.csv(DS_data_file) %>%
  mutate(
    Column = case_when(
      Column==1~"A",
      Column==2~"B",
      Column==3~"C",
      Column==4~"D",
      Column==5~"E",
      Column==6~"F",
      Column==7~"G",
      Column==8~"H",
    ),
    Inoculation = case_when(
      Treatment=="Control"~FALSE,
      Treatment=="Liquid"~TRUE,
      Treatment=="CG"~FALSE,
      Treatment=="CBG"~TRUE
    ),
    Chitosan = case_when(
      Treatment=="Control"~FALSE,
      Treatment=="Liquid"~FALSE,
      Treatment=="CG"~TRUE,
      Treatment=="CBG"~TRUE
    ),
    Treatment = factor(Treatment, levels = tim_treatment_order),
    Plant = as.factor(paste0(Column, Row)),
    Plant = factor(Plant, levels = tim_levels),
    Row = as.factor(Row),
    Column = as.factor(Column)
  ) %>%
  rename(Pot = Column)

data_tim_ds <- DS_data[,c(3,10,11,1,2,12,4,5,6,7,8,9)]

save(data_tim_ds, file = "C:/Github/tomato-inoculant-app/app/data_tim_ds.RData")

##### TOMATO INOCULANT LOCATION (2023) #####

## experimental constants
germdate23 <- "2023-04-14"

treatment_order23 <- c("Control",
                       "Soil",
                       "Foliar", 
                       "Soil+Foliar")

# data location
d23_fg_file <- "https://raw.githubusercontent.com/zachpeagler/Thesis/refs/heads/main/data/TIP/2023/TIP23_Fruit_Greenhouse.csv"
d23_fl_file <- "https://raw.githubusercontent.com/zachpeagler/Thesis/refs/heads/main/data/TIP/2023/TIP23_Fruit_Lab.csv"
d23_m_file <- "https://raw.githubusercontent.com/zachpeagler/Thesis/refs/heads/main/data/TIP/2023/TIP23_Multispeq.csv"
d23_li_file <- "https://raw.githubusercontent.com/zachpeagler/Thesis/refs/heads/main/data/TIP/2023/TIP23_LI600.csv"

d23_fg <- read.csv(d23_fg_file) %>%
  mutate(
    Row_num = case_when(
      Row=="A"~1,
      Row=="B"~2,
      Row=="C"~3,
      Row=="D"~4),
    Treatment = case_when(
      Row=="A"~"Control",
      Row=="B"~"Soil",
      Row=="C"~"Foliar",
      Row=="D"~"Soil+Foliar",
      TRUE ~ NA),
    Soil = case_when(
      Row=="A"~FALSE,
      Row=="B"~TRUE,
      Row=="C"~FALSE,
      Row=="D"~TRUE
    ),
    Foliar = case_when(
      Row=="A"~FALSE,
      Row=="B"~FALSE,
      Row=="C"~TRUE,
      Row=="D"~TRUE
    ),
    BER = case_when(
      BER==0~"FALSE",
      BER==1~"TRUE"),
    BER = as.logical(BER),
    Treatment = factor(Treatment, levels = treatment_order23),
    Fruit = 1,
    Plant = as.factor(paste0(Row, Pot)),
    Pot = as.factor(Pot),
    Row = as.factor(Row),
    Cluster = as.factor(Cluster),
    Date = as.Date(Date, "%m/%d/%Y")
  ) %>%
  rename(Mass = Weight)

d23_fg_summary <- d23_fg %>%
  group_by(Treatment, Plant) %>%
  summarise_at(vars(Fruit, BER, Mass),
               list(sum=sum, mean=mean)) %>%
  mutate(pBER = round(BER_sum/Fruit_sum, 4)) %>%
  ungroup() %>%
  mutate(BER_sum = as.numeric(BER_sum),
         Soil = case_when(
           Treatment=="Control"~FALSE,
           Treatment=="Soil"~TRUE,
           Treatment=="Foliar"~FALSE,
           Treatment=="Soil+Foliar"~TRUE
         ),
         Foliar = case_when(
           Treatment=="Control"~FALSE,
           Treatment=="Soil"~FALSE,
           Treatment=="Foliar"~TRUE,
           Treatment=="Soil+Foliar"~TRUE
         ),
         LogitpBER = logit(pBER + 0.0001))

data_til_fruit_summary <- d23_fg_summary[,c(1,10,11,2,3,4,5,8,9,12)]

d23_fl <- read.csv(d23_fl_file) %>%
  rename(pot = plant) %>%
  mutate(
    Treatment = case_when(row=="A"~"Control",
                          row=="B"~"Soil",
                          row=="C"~"Foliar",
                          row=="D"~"Soil+Foliar",
                          TRUE ~ NA),
    Soil = case_when(
      row=="A"~FALSE,
      row=="B"~TRUE,
      row=="C"~FALSE,
      row=="D"~TRUE
    ),
    Foliar = case_when(
      row=="A"~FALSE,
      row=="B"~FALSE,
      row=="C"~TRUE,
      row=="D"~TRUE
    ),
    Treatment = factor(Treatment, levels = treatment_order23),
    Ripeness = abs(1 - round(penetrometer/max(na.omit(penetrometer)), 2)),
    logit_sugar = logit(sugar_avg/100),
    Sugar_grams = (sugar_avg/100)*mass,
    row = as.factor(row),
    pot = as.factor(pot),
    cluster = as.factor(cluster),
    plant = as.factor(paste0(row, pot)),
    date = as.Date(date, "%m/%d/%Y"),
    DaysFromGermination = as.numeric(round(difftime(date, germdate23, units = c("days")), 0))
  )

data_til_fruit <- d23_fl[,c(10,11,12,1,2,16,9,17,4,5,13,8,15)] %>%
  rename(Row = row,
         Pot = pot,
         Plant = plant,
         Date = date,
         Mass = mass,
         Penetrometer = penetrometer,
         pSugar = sugar_avg,
         SugarGrams = Sugar_grams
         ) %>%
  mutate(pSugar = pSugar/100,
         LogitpSugar = logit(pSugar)
         )

data_til_fruit_sug <- na.omit(data_til_fruit) %>%
  group_by(Treatment, Soil, Foliar, Plant) %>%
  summarise_at(vars(pSugar),
               list(mean=mean)) %>%
  ungroup() %>%
  rename(pSugar_mean = mean) %>%
  mutate(LogitpSugar_mean = logit(pSugar_mean))

data_til_fruit_summary <- cbind(data_til_fruit_summary, data_til_fruit_sug[,c(5:6)])

save(data_til_fruit_summary, file = "C:/Github/tomato-inoculant-app/app/data_til_fruit_summary.RData")
save(data_til_fruit, file = "C:/Github/tomato-inoculant-app/app/data_til_fruit.RData")

d23_li <- read.csv(d23_li_file, stringsAsFactors = T) %>%
  mutate(Treatment = case_when(
    Row==1~"Control",
    Row==2~"Soil",
    Row==3~"Foliar",
    Row==4~"Soil+Foliar",
    TRUE~NA),
    Row = case_when(
      Row==1~"A",
      Row==2~"B",
      Row==3~"C",
      Row==4~"D"
    ),
    Soil = case_when(
      Row=="A"~FALSE,
      Row=="B"~TRUE,
      Row=="C"~FALSE,
      Row=="D"~TRUE
    ),
    Foliar = case_when(
      Row=="A"~FALSE,
      Row=="B"~FALSE,
      Row=="C"~TRUE,
      Row=="D"~TRUE
    )) %>%
  filter(leak_pct<10 & gsw > 0) %>%
  mutate(Date = parse_date_time(Date, orders = "mdy"),
         Date = as.Date(Date),
         Time = parse_date_time(Time, orders = "T"),
         DaysFromGermination = as.numeric(round(difftime(Date, germdate23, units = c("days")), 0)),
         Plant = as.factor(paste0(Row, Pot)),
         Treatment = factor(Treatment, 
                            levels = treatment_order23),
         LogitPhiPS2 = logit(PhiPS2, FALSE)
  ) %>%
  group_by(DaysFromGermination) %>%
  mutate(MinutesFromStart = round(difftime(Time, min(Time), units = "mins"), 2)) %>%
  ungroup() %>%
  mutate(
    Time = format(Time, "%H:%M:%S")
  )

## multispeq
d23_m <- read.csv(d23_m_file) %>%
  rename(Pot = Pot.ID) %>%
  mutate(Treatment = case_when(
    Row=="A"~"Control",
    Row=="B"~"Soil",
    Row=="C"~"Foliar",
    Row=="D"~"Soil+Foliar",
    TRUE~NA),
    Soil = case_when(
      Row=="A"~FALSE,
      Row=="B"~TRUE,
      Row=="C"~FALSE,
      Row=="D"~TRUE
    ),
    Foliar = case_when(
      Row=="A"~FALSE,
      Row=="B"~FALSE,
      Row=="C"~TRUE,
      Row=="D"~TRUE
    )) %>%
  mutate(Row_num = case_when(
    Row=="A"~1,
    Row=="B"~2,
    Row=="C"~3,
    Row=="D"~4),
    Plant = as.factor(paste0(Row, Pot)),
    Pot = as.factor(Pot),
    Row = as.factor(Row),
    Device.ID = as.factor(Device.ID),
    Date = as.Date(time, "%m/%d/%Y"),
    DaysFromGermination = as.numeric(round(difftime(Date, germdate23, units = c("days")), 0)),
    datetime = parse_date_time(time, "%m/%d/%Y %H:%M"),
    Time = format(datetime, "%H:%M:%S"),
    LogitPhiPS2 = logit(Phi2, FALSE),
  )

data_til_fluoro <- d23_li[,c(61,62,63,2,3,64,8,7,65,31,33,35,39,34,9,27,66)] %>%
  rename(AmbientHumidity = rh_s,
         AmbientTemperature = Tref,
         AmbientPressure = P_atm,
         AmbientLight = Qamb,
         LeafTemperature = Tleaf
         ) %>%
  mutate(
    Time = as.factor(Time),
    Row = as.factor(Row),
    Pot = as.factor(Pot),
    AmbientLight = as.numeric(AmbientLight),
    Device = "Li-600"
  )

temp_til_fluoro <- d23_m[,c(59,60,61,67,64,65,4,3,63,6,8,7,30,23,45,37,68)] %>%
  rename(
    AmbientHumidity = Ambient.Humidity,
    AmbientTemperature = Ambient.Temperature,
    AmbientPressure = Ambient.Pressure,
    AmbientLight = Light.Intensity..PAR.,
    LeafTemperature = Leaf.Temperature,
    gsw = pump,
    PhiPS2 = Phi2
  ) %>% mutate(
    Treatment = factor(Treatment, levels = treatment_order23),
    AmbientPressure = AmbientPressure / 10,
    Time = as.factor(Time),
    gsw = as.numeric("NA"),
    Device = "MultispeQ"
  )

data_til_fluoro <- rbind(data_til_fluoro, temp_til_fluoro) %>%
  mutate(Device = as.factor(Device))

save(data_til_fluoro, file = "C:/Github/tomato-inoculant-app/app/data_til_fluoro.RData")
# end up with one combined fluoro file, one summarized fruit file, and one fruit lab file

##### TOMATO INOCULANT TIMING (2024) #####

# experimental constants
germdate24 <- "2024-05-01"
treatment_order24 <- c("Control",
                       "Transplantation",
                       "Germination", 
                       "Germ+Trans")
plant_order24 <- c("A1", "A2", "A3", "A4", "A5", "A6",
                   "A7", "A8", "A9", "A10", "A11", "A12",
                   "B1", "B2", "B3", "B4", "B5", "B6",
                   "B7", "B8", "B9", "B10", "B11", "B12",
                   "C1", "C2", "C3", "C4", "C5", "C6",
                   "C7", "C8", "C9", "C10", "C11", "C12",
                   "D1", "D2", "D3", "D4", "D5", "D6",
                   "D7", "D8", "D9", "D10", "D11", "D12")

# data location
d24_f_file <- "https://raw.githubusercontent.com/zachpeagler/Thesis/refs/heads/main/data/TIP/2024/TIP24_Fruit.csv"
d24_m_file <- "https://raw.githubusercontent.com/zachpeagler/Thesis/refs/heads/main/data/TIP/2024/TIP24_Multispeq.csv"
d24_li_file <- "https://raw.githubusercontent.com/zachpeagler/Thesis/refs/heads/main/data/TIP/2024/TIP24_LI600.csv"


d24_f <- read.csv(d24_f_file) %>%
  filter(mass>0)%>%
  rename(pot = plant) %>%
  mutate(row = case_when(
    row==1~"A",
    row==2~"B",
    row==3~"C",
    row==4~"D"),
    treatment = case_when(
      row=="A"~"Control",
      row=="B"~"Transplantation",
      row=="C"~"Germination",
      row=="D"~"Germ+Trans",
      TRUE~NA),
    transplantation = case_when(
      row=="A"~FALSE,
      row=="B"~TRUE,
      row=="C"~FALSE,
      row=="D"~TRUE
    ),
    germination = case_when(
      row=="A"~FALSE,
      row=="B"~FALSE,
      row=="C"~TRUE,
      row=="D"~TRUE
    ),
    fruit = 1,
    date_analysis = parse_date_time(date_analysis, orders = "mdy"),
    date_harvest = parse_date_time(date_harvest, orders = "mdy"),
    date_analysis = as.Date(date_analysis),
    date_harvest = as.Date(date_harvest),
    daysfromharvesttoanalysis = as.numeric(round(difftime(date_analysis, date_harvest, units = c("days")), 0)),
    daysfromgermination = as.numeric(round(difftime(date_analysis, germdate24, units = c("days")), 0)),
    plant = as.factor(paste0(row, pot)),
    pot = as.factor(pot),
    treatment = factor(treatment, levels = treatment_order24),
    plant = factor(plant, levels = plant_order24),
    BER = as.logical(BER),
    fungus = as.logical(fungus),
    cracking = as.logical(cracking),
    ripeness = abs(1 - round(penetrometer/max(na.omit(penetrometer)), 2)),
    sugar_grams = (sugar_avg/100)*mass
  )

data_tit_fruit <- d24_f[,c(13,14,15,1,2,19,11,12,17,18,3,4,7,20,10,21)] %>%
  rename(Treatment = treatment,
         Transplantation = transplantation,
         Germination = germination,
         Row = row,
         Pot = pot,
         Plant = plant,
         DateHarvest = date_harvest,
         DateAnalysis = date_analysis,
         DaysFromHarvestToAnalysis = daysfromharvesttoanalysis,
         DaysFromGermination = daysfromgermination,
         Mass = mass,
         Penetrometer = penetrometer,
         Ripeness = ripeness,
         pSugar = sugar_avg,
         SugarGrams = sugar_grams
         ) %>%
  mutate(Row = as.factor(Row),
         pSugar = pSugar/100,
         LogitpSugar = logit(pSugar)
         )

save(data_tit_fruit, file = "C:/Github/tomato-inoculant-app/app/data_tit_fruit.RData")

data_tit_fruit_summary <- data_tit_fruit %>%
  mutate(Fruit = 1) %>%
  group_by(Treatment, Transplantation, Germination, Plant) %>%
  summarise_at(vars(Fruit, BER, Mass),
               list(sum=sum, mean=mean)) %>%
  mutate(pBER = round(BER_sum/Fruit_sum, 4),
         LogitpBER = logit(pBER + 0.0001)) %>%
  ungroup()
data_tit_fruit_summary <- data_tit_fruit_summary[,c(1,2,3,4,5,6,7,10,11,12)]

data_tit_fruit_sug <- na.omit(data_tit_fruit) %>%
  group_by(Treatment, Transplantation, Germination, Plant) %>%
  summarise_at(vars(pSugar),
               list(mean=mean)) %>%
  ungroup() %>%
  rename(pSugar_mean = mean) %>%
  mutate(LogitpSugar_mean = logit(pSugar_mean))

data_tit_fruit_summary <- cbind(data_tit_fruit_summary, data_tit_fruit_sug[,c(5:6)])

save(data_tit_fruit_summary, file = "C:/Github/tomato-inoculant-app/app/data_tit_fruit_summary.RData")

## Li-600
d24_li <- read.csv(d24_li_file, stringsAsFactors = F) %>%
  mutate(row_let = case_when(
    Row==1~"A",
    Row==2~"B",
    Row==3~"C",
    Row==4~"D"),
    Treatment = case_when(
      Row==1~"Control",
      Row==2~"Transplantation",
      Row==3~"Germination",
      Row==4~"Germ+Trans",
      TRUE~NA),
    transplantation = case_when(
      row_let=="A"~FALSE,
      row_let=="B"~TRUE,
      row_let=="C"~FALSE,
      row_let=="D"~TRUE
    ),
    germination = case_when(
      row_let=="A"~FALSE,
      row_let=="B"~FALSE,
      row_let=="C"~TRUE,
      row_let=="D"~TRUE
    )) %>%
  filter(leak_pct<10 & gsw > 0) %>%
  rename(Date_ref = Date, row_num = Row) %>%
  mutate(Date = parse_date_time(Date_ref, orders = "mdy"),
         Date = as.Date(Date),
         Time = parse_date_time(Time, orders = "T"),
         DaysFromGermination = as.numeric(round(difftime(Date, germdate24, units = c("days")), 0)),
         plant = as.factor(paste0(row_let, Pot)),
         plant = factor(plant, levels = plant_order24),
         Treatment = factor(Treatment, 
                            levels = treatment_order24),
         logitPS2 = logit(PhiPS2, FALSE)
  ) %>%
  group_by(DaysFromGermination) %>%
  mutate(MinutesFromStart = round(difftime(Time, min(Time), units = "mins"), 2)) %>%
  ungroup() %>%
  mutate(
    Time = format(Time, "%H:%M:%S")
  )
d24_li <- d24_li[,c(2,3,7,8,10,17,28,32,34,35,36,39,40,114,115,116,117,118,119,120,121,122)]

## multispeq
d24_m <- read.csv(d24_m_file) %>%
  mutate(Treatment = case_when(
    Row=="A"~"Control",
    Row=="B"~"Transplantation",
    Row=="C"~"Germination",
    Row=="D"~"Germ+Trans",
    TRUE~NA),
    Transplantation = case_when(
      Row=="A"~FALSE,
      Row=="B"~TRUE,
      Row=="C"~FALSE,
      Row=="D"~TRUE
    ),
    Germination = case_when(
      Row=="A"~FALSE,
      Row=="B"~FALSE,
      Row=="C"~TRUE,
      Row=="D"~TRUE
    )) %>%
  mutate(Row_num = case_when(
    Row=="A"~1,
    Row=="B"~2,
    Row=="C"~3,
    Row=="D"~4),
    Plant = as.factor(paste0(Row, Pot)),
    Plant = factor(Plant, levels = plant_order24),
    Pot = as.factor(Pot),
    Row = as.factor(Row),
    Device.ID = as.factor(Device.ID),
    Date = as.Date(time, "%m/%d/%Y"),
    DaysFromGermination = as.numeric(round(difftime(Date, germdate24, units = c("days")), 0)),
    time = parse_date_time(time, "%m/%d/%Y %H:%M"),
    Time = format(time, "%H:%M:%S"),
    logitPS2 = logit(Phi2, FALSE),
  )


data_tit_fluoro <- d24_li %>% mutate(
  Device = "Li600"
) %>% rename(AmbientLight = Qamb,
             AmbientPressure = P_atm,
             AmbientHumidity = rh_s,
             AmbientTemperature = Tref,
             LeafTemperature = Tleaf,
             Transplantation = transplantation,
             Germination = germination,
             Row = row_let,
             Plant = plant,
             LogitPhiPS2 = logitPS2
             ) %>%
  mutate(
         Time = as.factor(Time),
         AmbientLight = as.numeric(AmbientLight),
         Row = as.factor(Row),
         Pot = as.factor(Pot)
         )
data_tit_fluoro <- data_tit_fluoro[,c(15,16,17,1,18,19,14,4,20,8,9,11,13,10,5,7,21,23)]

temp_tit_fluoro <- d24_m[,c(63,64,65,70,68,69,8,7,67,10,12,11,34,27,49,41,71)] %>% mutate(
  Device = "MultispeQ") %>%
  rename(AmbientLight = Light.Intensity..PAR.,
         AmbientPressure = Ambient.Pressure,
         AmbientHumidity = Ambient.Humidity,
         AmbientTemperature = Ambient.Temperature,
         LeafTemperature = Leaf.Temperature,
         PhiPS2 = Phi2,
         LogitPhiPS2 = logitPS2,
         gsw = pump) %>%
  mutate(Time = as.factor(Time),
         AmbientPressure = AmbientPressure / 10,
         Treatment = factor(Treatment, levels = treatment_order24),
         gsw = as.numeric("NA")
         )

data_tit_fluoro <- rbind(data_tit_fluoro, temp_tit_fluoro) %>%
  mutate(Device = as.factor(Device))

save(data_tit_fluoro, file = "C:/Github/tomato-inoculant-app/app/data_tit_fluoro.RData")

