################################################################################
#
# Test Taballierung
# 1) Daten aufbereiten
#
# Quelle: https://www-genesis.destatis.de/genesis/online
# Tabelle 12411-0013:
# Bevölkerung: Bundesländer, Stichtag, Geschlecht, Altersjahre
# Stichtag 31.12.2021, gefiltert auf Berlin
#
# Ulrike Niemann, Januar 2023
# 
#
################################################################################
#
# Environment vorsichtshalber leeren
rm(list = ls())
#
################################################################################
#
# benötite packages laden (und wenn nötig installieren)
#
if (!require("tidyverse")) install.packages("tidyverse"); library(tidyverse)
if (!require("readxl")) install.packages("readxl"); library(readxl)
#
################################################################################
#
# Daten einlesen
#
dat <- read_excel("./12411-0013-DLAND_$F_Berlin.xlsx", 
                  sheet = "12411-0013", 
                  range = "A5:D97")
#
################################################################################
#
# Daten aufbereiten
#
# erste Spalte umbenennen
dat <- dat %>% 
  rename(Alter = ...1)
#
# erste Zeile mit Datenstand löschen
dat <- dat %>% 
  filter(Alter != "31.12.2021")
#
# noch eine Spalte mit dem Alter als numeric
dat <- dat %>% 
  mutate(AlterJahr = parse_number(Alter))
#
# die unter 1-Jährigen auf 0 setzen
dat <- dat %>% 
  mutate(AlterJahr = if_else(Alter == "unter 1 Jahr", 0, AlterJahr))
#
# Variabel Alter als sortierten factor
dat <- dat %>% 
  mutate(Alter = factor(Alter) %>% fct_reorder(AlterJahr))
#
# Datei ins longformat bringen ... lässt sich besser mit arbeiten
dat <- dat %>% 
  pivot_longer(cols = 2:4, names_to = "Geschlecht", values_to = "Anzahl")
#
# Altersgruppen bilden
dat <- dat %>% 
  mutate(Altersgruppe = case_when(
    AlterJahr < 10 ~ 1,
    AlterJahr >= 10 & AlterJahr < 20 ~ 2,
    AlterJahr >= 20 & AlterJahr < 30 ~ 3,
    AlterJahr >= 30 & AlterJahr < 40 ~ 4,
    AlterJahr >= 40 & AlterJahr < 50 ~ 5,
    AlterJahr >= 50 & AlterJahr < 60 ~ 6,
    AlterJahr >= 60 & AlterJahr < 70 ~ 7,
    AlterJahr >= 70 & AlterJahr < 80 ~ 8,
    AlterJahr >= 80 ~ 9
  ))
#
# Altersgruppe als factor
dat <- dat %>% 
  mutate(Altersgruppe = factor(Altersgruppe, labels = c(
    "< 10 Jahre",
    "10 bis < 20 Jahre",
    "20 bis < 30 Jahre",
    "30 bis < 40 Jahre",
    "40 bis < 50 Jahre",
    "50 bis < 60 Jahre",
    "60 bis < 70 Jahre",
    "70 bis < 80 Jahre",
    ">= 80 Jahre")
  ))
#
# ------------------------------------------------------------------------------
# Daten als csv speichern 
write_csv2(dat, file = 'data.csv')
# Daten als RData speichern
save(dat, file = 'data.RData')
#
# ------------------------------------------------------------------------------
################################################################################
################################################################################

