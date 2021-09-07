#***************************************************************************************
# Title     : 効果検証入門
# Chapter   : 2章 介入効果を測るための回帰分析
# Objective : TODO
# Created by: Owner
# Created on: 2021/2/8
# Page      : P67 - P83
#***************************************************************************************


# ＜概要＞
# - 大量のパターンで回帰分析を行う際のmap()を用いた処理フローを学ぶ
# - 回帰分析における効果を可視化する方法として、回帰係数と標準誤差を用いたプロットを用いる
#   --- エラーバーを用いたプロットは効果検証においてよく用いられる


# ＜回帰分析に対する様々な議論＞  P83 - P89
# - 因果推論における回帰分析は他分野で重視される前提や性質が必ずしも必要ない
#


# ＜データの論点＞
# - コロンビアで行われた私立学校の学費の割引に関する実験から効果測定を行う
# - 教育に対して何らかの補助を行うことで質を改善する際に、学校側と生徒側のどちらに補助を行うべきか？
# - 応募者の中からランダムに私立学校の学費を半分程度政府が肩代わりするクーポンを配布
#   --- 効果1：私立学校の学費を下げる効果があるため、通う生徒が増えて教育の質が上がる
#   --- 効果2：落第するとクーポン効果が失われるので、継続的な学習インセンティブが与えられる


# ＜目次＞
# 0 準備
# 1 フォーミュラ定義
# 2 回帰分析の実行
# 3 通学率と割引券の利用についての分析
# 4 割引券と留年の傾向ついての分析
# 5 性別による効果の差
# 6 留年と通学年数への分析
# 7 割引券と労働時間ついての分析



# 0 準備 --------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(broom)
library(experimentdatar)

# データロード
data(vouchers)

# データ確認
# --- A tibble: 25,330 x 89
vouchers %>% print()
vouchers %>% glimpse()


# 1 フォーミュラ定義 ------------------------------------------------------------------------

# データ確認
vouchers %>% select(VOUCH0) %>% table()

# 変数定義
# --- 介入変数(Z)
formula_x_base <- "VOUCH0"

# 変数定義
# --- 共変量(X)
formula_x_covariate<-
  "SVY + HSVISIT + AGE + STRATA1 + STRATA2 + STRATA3 + STRATA4 + STRATA5 + STRATA6 +
  STRATAMS + D1993 + D1995 + D1997 + DMONTH1 + DMONTH2 + DMONTH3 + DMONTH4 + DMONTH5 +
  DMONTH6 + DMONTH7 + DMONTH8 + DMONTH9 + DMONTH10 + DMONTH11 + DMONTH12 + SEX2"

# 変数定義
# --- 被説明変数(y)
formula_y <-
  c("TOTSCYRS", "INSCHL", "PRSCH_C", "USNGSCH", "PRSCHA_1", "FINISH6", "FINISH7",
    "FINISH8", "REPT6", "REPT", "NREPT", "MARRIED", "HASCHILD", "HOURSUM", "WORKING3")

# フォーミュラ作成１
# --- 被説明変数(Y)の各要素と介入変数(Z)の回帰式を作成
base_reg_formula <-
  paste(formula_y, "~", formula_x_base) %>%
    set_names(paste(formula_y, "base", sep = "_"))

# フォーミュラ作成２
# --- 被説明変数(Y)の各要素と介入変数(Z)と共変量(X)のの回帰式を作成
covariate_reg_formula <-
  paste(formula_y, "~", formula_x_base, "+", formula_x_covariate) %>%
    set_names(paste(formula_y, "covariate", sep = "_"))

# フォーミュラ結合
# --- フォーミュラ1とフォーミュラ2
# --- ベクトルを作成
table3_fomula <- c(base_reg_formula, covariate_reg_formula)

# 確認
# --- 最終的なフォーミュラ数
table3_fomula %>% length()


# 2 回帰分析の実行 -------------------------------------------------------------------

# フォーミュラをデータフレーム変換
models <-
  table3_fomula %>%
    enframe(name = "model_index", value = "formula")

# モデル用データ
# --- bogota 1995のデータを抽出
regression_data <-
  vouchers %>%
    filter(TAB3SMPL == 1, BOG95SMP == 1)

# 回帰分析の実行
# --- Listed DataFrameによる計算
df_models <-
  models %>%
      mutate(model = map(.x = formula,
                         .f = lm,
                         data = regression_data)) %>%
      mutate(lm_result = map(.x = model, .f = tidy))

# 回帰結果を整形
df_results <-
  df_models %>%
    mutate(formula = as.character(formula)) %>%
    select(formula, model_index, lm_result) %>%
    unnest(cols = lm_result)

# 確認
# --- lmオブジェクトと回帰係数をフォーミュラごとに作成
# --- 主要な統計量のみをデータフレーム出力
df_models %>% print()
df_results %>% print()


# 3 通学率と割引券の利用についての分析 -------------------------------------------------

# ＜ポイント＞
# - 当選したグループで割引券がちゃんと使われたかを確認

# データ抽出
# --- VOUCH0の効果を取り出す
# --- PRSCHA_1：6年生の開始時に私立学校に在籍
# --- USNGSCH： 調査期間中に何らかの奨学金を使用
using_voucher_results <-
  df_results %>%
    filter(term == "VOUCH0",
           str_detect(model_index, "PRSCHA_1|USNGSCH")) %>%
    select(model_index, term, estimate, std.error, p.value) %>%
    arrange(model_index)

# データ確認
using_voucher_results %>% print()

# プロット作成
# --- 回帰係数の範囲をエラーバーを含めて表示
using_voucher_results %>%
  ggplot(aes(x = model_index, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymax = estimate + std.error * 1.96,
                    ymin = estimate - std.error * 1.96,
                    width = 0.1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.margin = margin(0.5,1,0.5,1, "cm"))



# 4 割引券と留年の傾向ついての分析 -------------------------------------------------

# データ抽出
# --- VOUCH0の効果を取り出す
going_private_results <-
  df_results %>%
    filter(term == "VOUCH0",
           str_detect(model_index, "PRSCH_C|INSCHL|FINISH|REPT")) %>%
    select(model_index, term, estimate, std.error, p.value) %>%
    arrange(model_index)

# データ確認
going_private_results %>% print()

# プロット作成
# --- 回帰係数の範囲をエラーバーを含めて表示
going_private_results %>%
  filter(str_detect(model_index, "covariate")) %>%
  ggplot(aes(x = model_index, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymax = estimate + std.error * 1.96,
                    ymin = estimate - std.error * 1.96,
                    width = 0.1)) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.margin = margin(0.5,1,0.5,1, "cm"))


# 5 性別による効果の差 -------------------------------------------------

# データ抽出
# ---  table4に使うデータを抜き出す
data_tbl4_bog95 <-
  vouchers %>%
    filter(BOG95SMP == 1, TAB3SMPL == 1,
           !is.na(SCYFNSH), !is.na(FINISH6), !is.na(PRSCHA_1),
           !is.na(REPT6), !is.na(NREPT), !is.na(INSCHL),
           !is.na(FINISH7),
           !is.na(PRSCH_C), !is.na(FINISH8), !is.na(PRSCHA_2),
           !is.na(TOTSCYRS), !is.na(REPT)
    ) %>%
    select(VOUCH0, SVY, HSVISIT, DJAMUNDI, PHONE, AGE,
           STRATA1:STRATA6, STRATAMS, DBOGOTA, D1993, D1995, D1997,
           DMONTH1:DMONTH12, SEX_MISS, FINISH6, FINISH7, FINISH8,
           REPT6, REPT, NREPT, SEX2, TOTSCYRS, MARRIED, HASCHILD,
           HOURSUM,WORKING3, INSCHL,PRSCH_C,USNGSCH,PRSCHA_1)


# 女子生徒の分析 ********************************************

# データ抽出
# --- 女子生徒のデータだけ取り出す
regression_data <-
  data_tbl4_bog95 %>%
    filter(SEX2 == 0)

# 回帰分析を実行
# --- 女子生徒のみ
df_models <-
  models %>%
    mutate(model = map(.x = formula, .f = lm, data = regression_data)) %>%
    mutate(lm_result = map(.x = model, .f = tidy))

# 結果整形
df_results_female <-
  df_models %>%
    mutate(formula = as.character(formula),
           gender = "female") %>%
    select(formula, model_index, lm_result, gender) %>%
    unnest(cols = lm_result)

# データ確認
df_results_female %>% print()


# 男子生徒の分析 ********************************************

# データ抽出
# --- 男子生徒のデータだけ取り出す
regression_data <-
  data_tbl4_bog95 %>%
    filter(SEX2 == 1)

# 回帰分析を実行
# --- 男子生徒のみ
df_models <-
  models %>%
    mutate(model = map(.x = formula, .f = lm, data = regression_data)) %>%
    mutate(lm_result = map(.x = model, .f = tidy))

# 結果整形
df_results_male <-
  df_models %>%
    mutate(formula = as.character(formula),
           gender = "male") %>%
    select(formula, model_index, lm_result, gender) %>%
    unnest(cols = lm_result)

# データ確認
df_results_male %>% print()


# 全体分析 ********************************************

# 回帰結果の抽出
# --- 男子生徒と女子生徒のデータを結合
# --- VOUCH0の効果を取り出す
using_voucher_results_gender <-
  df_results_male %>%
    bind_rows(df_results_female) %>%
    filter(term == "VOUCH0", str_detect(model_index, "PRSCHA_1|USNGSCH")) %>%
    select(gender, model_index, term, estimate, std.error, p.value) %>%
    arrange(gender, model_index) %>%
    filter(str_detect(model_index, "covariate"))

# プロット作成
# --- 回帰係数の範囲をエラーバーを含めて表示
# --- 男性と女性でファセットを作成
using_voucher_results_gender %>%
  filter(str_detect(model_index, "covariate")) %>%
  ggplot(aes(x = model_index, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymax = estimate + std.error * 1.96,
                    ymin = estimate - std.error * 1.96,
                    width = 0.1)) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.margin = margin(0.5,1,0.5,1, "cm")) +
  facet_grid(gender ~ .)


# 6 留年と通学年数への分析 -------------------------------------------------

# 回帰結果の抽出
# --- 留年と通学年数への分析結果の可視化(ch2_plot4.png)
# --- PRSCH_C,INSCHL,REPT,TOTSCYRS,FINISHに対する分析結果を抜き出す
going_private_results_gender <-
  df_results_male %>%
    bind_rows(df_results_female) %>%
    filter(term == "VOUCH0",
           str_detect(model_index, "PRSCH_C|INSCHL|REPT|TOTSCYRS|FINISH")) %>%
    select(gender, model_index, term, estimate, std.error, p.value) %>%
    arrange(model_index)

# プロット作成
# ---
going_private_results_gender %>%
  filter(str_detect(model_index, "covariate")) %>%
  ggplot(aes(y = estimate, x = model_index)) +
  geom_point() +
  geom_errorbar(aes(ymax = estimate + std.error*1.96,
                    ymin = estimate - std.error*1.96,
                    width = 0.1)) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.margin = margin(0.5,1,0.5,1, "cm")) +
  facet_grid(gender ~ .)


# 7 割引券と労働時間ついての分析 -------------------------------------------------

# 回帰結果の抽出
# --- 労働時間に対する分析結果の可視化(ch2_plot5.png)
# --- HOURに対する分析結果を抜き出す
working_hour_results_gender <-
  df_results_male %>%
    bind_rows(df_results_female) %>%
    filter(term == "VOUCH0", str_detect(model_index, "HOUR")) %>%
    select(gender, model_index, term, estimate, std.error, p.value) %>%
    arrange(gender, model_index)

# プロット作成
# ---
working_hour_results_gender %>%
  filter(str_detect(model_index, "covariate")) %>%
  ggplot(aes(y = estimate, x = model_index)) +
  geom_point() +
  geom_errorbar(aes(ymax = estimate + std.error*1.96,
                    ymin = estimate - std.error*1.96,
                    width = 0.1)) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.margin = margin(0.5,1,0.5,1, "cm")) +
  facet_grid(. ~ gender)
