# ***********************************************************************************************
# Function  : DualUplift
# Objective : TODO
# Created by: Owner
# Created on: 2021/02/17
# URL       : https://www.rdocumentation.org/packages/tools4uplift/versions/1.0.0/topics/DualUplift
# ***********************************************************************************************


# ＜構文＞
# - 介入群と統制群でそれぞれモデルを構築する


# ＜構文＞
# フォーミュラ方式
# DualUplift(formula, treat, data, ...)

# 変数方式
# DualUplift(data, treat, outcome, predictors, ...)


# ＜目次＞
# 0 準備
# 1 2つのモデルを学習


# 0 準備 -------------------------------------------------------------------------

library(tidyverse)
library(tools4uplift)
library(Hmisc)


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


# 1 2つのモデルを学習 ----------------------------------------------------------------

# 学習
# --- 介入群と統制群でそれぞれモデル構築＆学習
fit <-
  SimUplift %>%
    DualUplift(treat = "treat",
               outcome = "y",
               predictors = colnames(SimUplift[, 3:12]))

# データ構造
# --- control / treatmentで30個ずつのリストを形成する
fit %>% list.tree(2)
fit %>% glimpse()

# 確認
fit %>% print()

# サマリー
fit %>% summary()
