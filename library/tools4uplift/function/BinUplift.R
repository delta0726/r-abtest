# ***********************************************************************************************
# Function  : BinUplift
# Objective : TODO
# Created by: Owner
# Created on: 2021/02/17
# URL       : https://www.rdocumentation.org/packages/tools4uplift/versions/1.0.0/topics/BinUplift
# ***********************************************************************************************


# ＜構文＞



# ＜構文＞
# BinUplift(data, treat, outcome, x, n.split = 10, alpha = 0.05, n.min = 30)


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


# 1 関数実行 ---------------------------------------------------------------------------

binX1 <-
  SimUplift %>%
    BinUplift(treat = "treat", outcome = "y", x = "X1",
              n.split = 100, alpha = 0.01, n.min = 30)