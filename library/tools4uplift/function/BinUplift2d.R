# ***********************************************************************************************
# Function  : BinUplift2d
# Objective : TODO
# Created by: Owner
# Created on: 2021/02/17
# URL       : https://www.rdocumentation.org/packages/tools4uplift/versions/1.0.0/topics/BinUplift2d
# ***********************************************************************************************


# ＜構文＞



# ＜構文＞
# BinUplift2d(data, var1, var2, treat, outcome, valid = NULL,
#            n.split = 3, n.min = 30, plotit = FALSE, nb.col = 20)


# ＜目次＞
# 0 準備
# 1 関数実行


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


# 1 関数実行 -----------------------------------------------------------------------

#
heatmap <-
  SimUplift %>%
    BinUplift2d(var1 = "X1", var2 = "X2", treat =  "treat", outcome = "y")

#
heatmap %>% glimpse()
