# ***********************************************************************************************
# Function  : InterUplift
# Objective : TODO
# Created by: Owner
# Created on: 2021/02/17
# URL       : https://www.rdocumentation.org/packages/tools4uplift/versions/1.0.0/topics/InterUplift
# ***********************************************************************************************


# ＜構文＞
# - 相互効果ありのアップリフトモデル


# ＜構文＞
# InterUplift(data, treat, outcome, predictors, input = "all", ...)


# ＜目次＞
# 0 準備
# 1 学習


# 0 準備 -------------------------------------------------------------------------

library(tidyverse)
library(tools4uplift)


# データ準備
data("SimUplift")

# データ確認
SimUplift %>% as_tibble()
SimUplift %>% glimpse()

# データ確認
# --- 介入群と統制群は1/0のフラグで区別する
# --- 1: treatment（介入群）
# --- 0: control（統制群）
SimUplift$treat %>% table()


# 1 学習 ----------------------------------------------------------------------------+

# 学習
# --- 相互効果ありのアップリフトモデル
fit <-
  SimUplift %>%
    InterUplift( "treat", "y", colnames(SimUplift[, 3:12]))
