

# NOTES -------------------------------------------------------------------

## https://rpruim.github.io/fastR2/reference/wilson.ci.html
### The Wilson confidence intervals have better coverage rates for small samples.
### A. Agresti and B. A. Coull, Approximate is better then `exact' for interval 
### estimation of binomial proportions, American Statistician 52 (1998), 119--126.


rm(list = ls())

library(tidyverse)
library(lubridate)
library(ggplot2)
library(binom)


# Data  -------------------------------------------------------------------

# I. Idylla File: “Results_LODest_COVID19.csv”
Results_LODest_COVID19 = read.table('Results_LODest_COVID19_LM.csv', 
                                  header = TRUE, sep = ';') |>
  filter(Name == "SARS-CoV-2")

length(unique(Results_LODest_COVID19$CartridgeSerialNumber)) # 168
length(unique(Results_LODest_COVID19$SampleId)) # 8
#table(Results_LODest_COVID19$CartridgeSerialNumber, Results_LODest_COVID19$SampleId)
table(Results_LODest_COVID19$Name); 
table(Results_LODest_COVID19$Value)
table(Results_LODest_COVID19$Cycle) # 0's

# II. Disc File: “DiscInspectionTemplate_AIO_5561_02_biostat.xlsx”
## first three rows were hidden 
## unnamed 1st column has no number 72
## what should happens to remarked rows: error repeat run? Btn B&C small spot
### have been removed for this analysis, but in work circumstances, a thorough
### justification will be sort: cartridges removed are "47860723" and "47860227"
DiscInspectionTemplate_AIO_5561_02_biostat = 
  readxl::read_xlsx('DiscInspectionTemplate_AIO_5561_02_biostat_LM.xlsx') |>
  select(-1) |> slice(-(c(1:2, 171))) |> # filter(is.na(Remark)) |>
  mutate(`Run time` = openxlsx::convertToDate(`Run time`)) 

table(DiscInspectionTemplate_AIO_5561_02_biostat$`Sample ID`)
table(DiscInspectionTemplate_AIO_5561_02_biostat$`Sample Barcode`)
table(DiscInspectionTemplate_AIO_5561_02_biostat$`Disc ID`)
table(DiscInspectionTemplate_AIO_5561_02_biostat$`Instrument ID`)
table(DiscInspectionTemplate_AIO_5561_02_biostat$`Cartridge request`)

length(unique(DiscInspectionTemplate_AIO_5561_02_biostat$`Sample ID`)) # 7
length(unique(DiscInspectionTemplate_AIO_5561_02_biostat$`Sample Barcode`)) # 7
length(unique(DiscInspectionTemplate_AIO_5561_02_biostat$`Disc ID`)) # 166
length(unique(DiscInspectionTemplate_AIO_5561_02_biostat$`Instrument ID`)) #49
length(unique(DiscInspectionTemplate_AIO_5561_02_biostat$`Cartridge request`)) #2


# merged data'
Disc_Results_df = merge(
  DiscInspectionTemplate_AIO_5561_02_biostat |> 
    select(`Run time`, Operator, `Cartridge ID`, `Sample ID`),
  Results_LODest_COVID19 |> select(CartridgeSerialNumber, Name, Value),
  by.x = 'Cartridge ID', by.y = 'CartridgeSerialNumber')

CartridgesinResultsNotinDisc = as.character(Results_LODest_COVID19$CartridgeSerialNumber)[!(as.character(Results_LODest_COVID19$CartridgeSerialNumber) %in% 
  DiscInspectionTemplate_AIO_5561_02_biostat$`Cartridge ID`)]
CartridgesinResultsNotinDisc


# Questions ---------------------------------------------------------------

                                       
# 1. Make a table of cartridge detection rates (‘positive call rates’) at each input level.

InputLevelvsdetection = 
  table(Disc_Results_df$`Sample ID`, Disc_Results_df$Value) |> as.data.frame() |>
  rename('Input Level (copies/ml)' = `Var1`, "Value" = `Var2`) |> 
  mutate('Input Level (copies/ml)' = parse_number(as.character(`Input Level (copies/ml)`))) |>
  group_by(`Input Level (copies/ml)`) |> 
  mutate("Positive call rates" = Freq/sum(Freq))

InputLevelvsdetection |> 
  pivot_wider(names_from = Value, values_from = c(Freq, `Positive call rates`)) |>
  mutate("Detected/N (%)" = paste0(Freq_Detected, "/",Freq_Detected+Freq_Not_Detected, 
                                  "(", formatC(`Positive call rates_Detected`, digits=3, format = 'f'), ")")) |>
  select(`Input Level (copies/ml)`, `Detected/N (%)`)

# 2. Make a figure to visualize the relationship between the positive call rates and the input levels.

InputLevelvsdetectiondf = InputLevelvsdetection |> filter(Value == 'Detected') |> 
  ungroup() |> mutate(InputLevel = `Input Level (copies/ml)`) |>
  mutate(`Input Level (copies/ml)` = as.factor(`Input Level (copies/ml)`))

my_ticks <- c(unique(InputLevelvsdetectiondf$InputLevel), seq(1000, 4000, 250))
my_labels <- c(unique(InputLevelvsdetectiondf$InputLevel), rep("", length(seq(1000, 4000, 250))))

InputLevelvsdetectionPlot = 
ggplot(InputLevelvsdetectiondf,
       aes(x = `InputLevel`, y = `Positive call rates`)) +
  geom_point(shape = 16, size = 1.5, stat = "identity") + theme_bw() + 
  scale_x_continuous(breaks = my_ticks, minor_breaks = NULL, labels = my_labels) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(breaks = seq(0.7, 1.0, 0.05), limits = c(0.7, 1.0)) +
  ylab(bquote(paste('Positive call rates'))) + xlab("Input Level (copies/ml)") 


ggsave(file = "graphs/InputLevelvsdetectionPlot.png",
       plot = InputLevelvsdetectionPlot, width = 15, height = 12, units = "cm", 
       scale=1.5)


# 3. Use a statistical model to estimate the lowest input level (cps/ml) that 
# gives 95% positive call rates and the corresponding two-sided 95% Wilson 
# confidence interval of the input level.


Disc_Results_df = Disc_Results_df |> 
  mutate(ValueFactor = as.factor(ifelse(Value == 'Detected', 1, 0)),
         InputLevel = parse_number(as.character(`Sample ID`)))

model <- glm(ValueFactor ~ InputLevel, data = Disc_Results_df, family = binomial)

newdata = data.frame(InputLevel = seq(125, 4000, 1))
ypred = predict(model, newdata, type = 'response')

# lowest input value giving 95% positive rates
ypred_inputLevel = cbind(newdata, ypred)
Linputvalue = ypred_inputLevel[which(ypred_inputLevel[, "ypred"] >= 0.95)[1], "InputLevel"]

# Wilson confidence interval for the input value

n = 24 # there were 24 cartridges at each Input level
ci_wilson_rates = 
  binom.confint(x = ypred_inputLevel[which(ypred_inputLevel[, "ypred"] >= 0.95)[1], "ypred"]*n, 
                n = n, method = "wilson", conf.level = 0.95)

# ci_wilson_rates[, c("lower", "upper")] # 147, 423
LCL = ypred_inputLevel[which(ypred_inputLevel[, "ypred"] >= ci_wilson_rates[, c("lower")])[1], "InputLevel"]
UCL = ypred_inputLevel[tail(which(ypred_inputLevel[, "ypred"] <= ci_wilson_rates[, c("upper")]), 1), "InputLevel"]

cbind("lower" = LCL, "Input Value" = Linputvalue, "upper" = UCL)



# Extras ------------------------------------------------------------------

Operatorvsdetection = 
  table(Disc_Results_df$`Operator`, Disc_Results_df$Value) |> as.data.frame() |>
  rename('Operator' = `Var1`, "Value" = `Var2`) |> group_by(`Operator`) |> 
  mutate("Positive call rates" = Freq/sum(Freq))

Operatorvsdetection |> 
  pivot_wider(names_from = Value, values_from = c(Freq, `Positive call rates`)) |>
  mutate("Detected/N (%)" = paste0(Freq_Detected, "/",Freq_Detected+Freq_Not_Detected, 
                                   "(", formatC(`Positive call rates_Detected`, digits=3, format = 'f'), ")")) |>
  select(`Operator`, `Detected/N (%)`)

# 2.b Make a figure to visualize the relationship between the positive call rates and the operators.

Operatorvsdetectiondf = Operatorvsdetection |> filter(Value == 'Detected') |> 
  ungroup() 


OperatorvsdetectionPlot = 
  ggplot(Operatorvsdetectiondf,
         aes(x = `Operator`, y = `Positive call rates`)) +
  geom_point(shape = 16, size = 1.5, stat = "identity") + theme_bw() +
  scale_y_continuous(breaks = seq(0.9, 1.0, 0.05), limits = c(0.9, 1.0)) +
  ylab(bquote(paste('Positive call rates'))) + xlab("Operator") 


ggsave(file = "graphs/OperatorvsdetectionPlot.png",
       plot = OperatorvsdetectionPlot, width = 15, height = 12, units = "cm", 
       scale=1.5)


# Date vs positive rates
Datevsdetection = 
  table(Disc_Results_df$`Run time`, Disc_Results_df$Value) |> as.data.frame() |>
  rename('Date' = `Var1`, "Value" = `Var2`) |> mutate('Date' = as.Date(`Date`)) |>
  group_by(`Date`) |> mutate("Positive call rates" = Freq/sum(Freq))

Datevsdetection |> 
  pivot_wider(names_from = Value, values_from = c(Freq, `Positive call rates`)) |>
  mutate("Detected/N (%)" = paste0(Freq_Detected, "/",Freq_Detected+Freq_Not_Detected, 
                                   "(", formatC(`Positive call rates_Detected`, digits=3, format = 'f'), ")")) |>
  select(`Date`, `Detected/N (%)`)

# 2.c Make a figure to visualize the relationship between the positive call rates and date.

Datevsdetectiondf = Datevsdetection |> filter(Value == 'Detected') |> 
  ungroup()


DatevsdetectionPlot = 
  ggplot(Datevsdetectiondf,
         aes(x = `Date`, y = `Positive call rates`)) +
  geom_point(shape = 16, size = 1.5, stat = "identity") + theme_bw()  +
  scale_y_continuous(breaks = seq(0.9, 1.0, 0.05), limits = c(0.9, 1.0)) +
  ylab(bquote(paste('Positive call rates'))) + xlab("Date") 


ggsave(file = "graphs/DatevsdetectionPlot.png",
       plot = DatevsdetectionPlot, width = 15, height = 12, units = "cm", 
       scale=1.5)