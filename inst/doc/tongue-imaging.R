## ----setup, echo=FALSE, include=FALSE-----------------------------------------
knitr::opts_chunk$set(out.width = "300px", fig.align = "center", dpi = 300)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
theme_set(theme_bw())
library(rticulate)

## ----attach, eval=FALSE-------------------------------------------------------
# library(rticulate)

## ----dlc----------------------------------------------------------------------
# system.file() is needed here because the example files reside in the package.
# You can just include the file path directly in read_aaa, like 
# read_aaa("~/Desktop/splines.tsv", columns)
file_path <- system.file("extdata", "it01-dlc.tsv", package = "rticulate")

dlc <- read_aaa(file_path, format = "wide")
dlc

## ----dlc-2--------------------------------------------------------------------
dlc <- read_aaa(file_path)
dlc

## ----dlc-plot-----------------------------------------------------------------
dlc |> 
  ggplot(aes(X, Y)) +
  geom_point(alpha = 0.1, size = 0.5) +
  coord_fixed()

## ----dlc-plot-2---------------------------------------------------------------
dlc |> 
  filter(spline == "DLC_Tongue") |> 
  ggplot(aes(X, Y, group = frame_id)) +
  geom_path(alpha = 0.1, linewidth = 0.1) +
  coord_fixed()

## ----dlc-displ, fig.asp=2-----------------------------------------------------
dlc |> 
  filter(
    `Date Time of recording` == "29/11/2016 15:10:52",
    spline == "DLC_Tongue"
  ) |> 
  ggplot(aes(`Time of sample in annot`, Y, group = displ_id)) +
  geom_path() +
  facet_grid(rows = vars(knot), scales = "free_y")

## ----ema, eval=FALSE----------------------------------------------------------
# ema <- read_ag500_pos("0025.pos")

## ----ema-plot, eval=FALSE-----------------------------------------------------
# ema |>
#   filter(chn == 5) |>
#   ggplot(aes(x, z)) +
#   geom_point(size = 0.1)

## ----ema-plot-2, eval=FALSE---------------------------------------------------
# ema |>
#   filter(chn == 5) |>
#   ggplot(aes(time, z)) +
#   geom_point(size = 0.1)

## ----columns------------------------------------------------------------------
columns <- c(
    "speaker",
    "seconds",
    "rec_date",
    "prompt",
    "label",
    "TT_displacement",
    "TT_velocity",
    "TT_abs_velocity",
    "TD_displacement",
    "TD_velocity",
    "TD_abs_velocity"
)

## ----read-aaa-----------------------------------------------------------------
# system.file() is needed here because the example files reside in the package.
# You can just include the file path directly in read_aaa, like 
# read_aaa("~/Desktop/splines.tsv", columns)
file_path <- system.file("extdata", "it01.tsv", package = "rticulate")

tongue <- read_aaa(file_path, fan_lines = 42, column_names = columns)

## ----tibble-------------------------------------------------------------------
tongue

## ----join---------------------------------------------------------------------
stimuli <- read_csv(system.file("extdata", "stimuli.csv", package = "rticulate"))

tongue <- mutate(tongue, word = word(prompt, 2)) %>%
    left_join(y = stimuli) %>%
    mutate_if(is.character, as.factor)

## ----tibble-2-----------------------------------------------------------------
tongue

## ----read-multiple------------------------------------------------------------
tongue2 <- list.files(
    path = system.file("extdata", package = "rticulate"),
    pattern = "*\\d{2}.tsv",
    full.names = TRUE
    ) %>%
    read_aaa(., fan_lines = 42, column_names = columns)

