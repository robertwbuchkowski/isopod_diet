# Load in Isopod and and complete basic statistics:

# Load libraries:
library(googledrive)
library(tidyverse)
library(readxl)

# Download data:

googledrive::drive_download(
  "https://docs.google.com/spreadsheets/d/19MYITSXVRA-OpUmAqh5Wdxxf9yP7bKX_0ZMHf7opSkI/edit?gid=2122048362#gid=2122048362",
  "Data/oniscus_asellus.xlsx",
  overwrite = T
)


# Load in the growth data sheets:

GR = read_xlsx("Data/oniscus_asellus.xlsx", sheet = "Growth rate", skip = 1)

# Clean up data:
GR = GR %>% filter(!is.na(`Weight of isopod`))

testthat::expect_true(all(GR$`Weight of isopod` > 0))

# Plot the data:
GR %>% ggplot(aes(x = Date, y = `Weight of isopod`, color = `Sample number`)) + geom_line(size = 2) + theme_classic()


# Load in the feeding data:

consumption = read_xlsx("Data/oniscus_asellus.xlsx", sheet = "Consumption rate vials", skip = 3,
                        col_names = c("Date",
                                      "Sample",
                                      "InitialVial",
                                      "InitialVialPellet",
                                      "LeftVialPellet",
                                      "EatenVial",
                                      "EatenVialPellet",
                                      "Notes"))

View(consumption)

consumption = consumption %>% 
  mutate(InitialPellet = InitialVialPellet - InitialVial,
         LeftPellet = LeftVialPellet - InitialVial,
         EatenPellet = EatenVialPellet - EatenVial) %>%
  mutate(FoodEaten = InitialPellet - LeftPellet + EatenPellet)

consumption %>% ggplot(aes(x = Date, y = EatenPellet, color = Sample)) + geom_line() + facet_wrap(~Sample, scales = "free_y")


consumption %>%
  group_by(Sample) %>%
  slice_min(Date) %>%
  ungroup() %>%
  mutate(InitalPellet = InitialVialPellet - InitialVial) %>%
  select(Sample, InitialPellet) %>%
  inner_join(
    consumption %>%
      group_by(Sample) %>%
      slice_max(Date) %>%
      mutate(LeftPellet = LeftVialPellet - InitialVial,
             EatenPellet = EatenVialPellet - EatenVial) %>%
      select(Sample, LeftPellet, EatenPellet),by = join_by(Sample)
  ) %>%
  mutate(Eaten = InitialPellet - LeftPellet + EatenPellet)


# Defecation:
defecation = read_xlsx("Data/oniscus_asellus.xlsx", sheet = "Defecation rate", skip = 1) %>%
  filter(!is.na(Feces))

testthat::expect_true(all(defecation$Feces > 0))

defecation %>%
  group_by(Sample) %>%
  mutate(CumFeces = cumsum(Feces)) %>%
  ggplot(aes(x = Date, y = CumFeces, color= Sample)) + geom_line(size = 2)

