#***************************************************************************************
# Title     : 効果検証入門
# Chapter   : 3章 傾向スコアを用いた分析
# Objective : TODO
# Created by: Owner
# Created on: 2021/2/9
# Page      : P118 - P130
#***************************************************************************************


# ＜目次＞
# 0 準備
# 1 バイアスのあるデータを作成
# 2 傾向スコアの推定
# 3 傾向スコアマッチング
# 4 逆確率重み付き推定（IPW）
# 5 統計モデルを用いたメールの配信のログを分析


# 0 準備 ------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(broom)
library(MatchIt)
library(WeightIt)
library(cobalt)
library(Matching)


# データ取り込み
email_data <- read_csv("http://www.minethatdata.com/Kevin_Hillstrom_MineThatData_E-MailAnalytics_DataMiningChallenge_2008.03.20.csv")

# データ確認
email_data %>% print()
email_data %>% glimpse()


# 1 バイアスのあるデータを作成 -----------------------------------------------------------

# ＜該当ページ＞
# - P30 - 31


# 基礎データ
# --- 女性向けメールが配信されたデータを削除
# --- 介入を表すtreatment変数を追加
male_df <-
  email_data %>%
    filter(segment != "Womens E-Mail") %>%
    mutate(treatment = ifelse(segment == "Mens E-Mail", 1, 0))

# シード固定
set.seed(1)

# パラメータ指定
# --- 条件に反応するサンプルの量を半分にする
obs_rate_c <- 0.5
obs_rate_t <- 0.5

## バイアスのあるデータを作成
biased_data <-
  male_df %>%
    mutate(obs_rate_c = ifelse( (history > 300) | (recency < 6) | (channel == "Multichannel"), obs_rate_c, 1),
           obs_rate_t = ifelse( (history > 300) | (recency < 6) | (channel == "Multichannel"), 1, obs_rate_t),
           random_number = runif(n = NROW(male_df))) %>%
    filter((treatment == 0 & random_number < obs_rate_c ) |
             (treatment == 1 & random_number < obs_rate_t))


# 2 傾向スコアの推定 -------------------------------------------------------------------

# ＜ポイント＞
# - 傾向スコアとは介入変数の割り当て確率のことをいう
#   --- 回帰モデルの情報が1次元に集約されている（マッチングするのに便利）
# - 傾向スコアはロジスティック回帰などのモデルを用いて推定することで観測することが可能
#   --- 傾向スコアはクラス確率として出力される
#   --- 回帰パラメータの解釈や多重共線性への配慮は不要


# 傾向スコアの推定
ps_model <-
  glm(treatment ~ recency + history + channel,
      data = biased_data, family = binomial)

# データ確認
# --- クラス確率(.fitted)が傾向スコア
ps_model %>% augment(type.predict = "response")


# 3 傾向スコアマッチング ---------------------------------------------------------------

# ＜ポイント＞
# - 傾向スコアマッチングは介入が行われているグループからサンプルを取り出し、
#   そのサンプルと近い傾向を持つサンプルを介入が行われていないグループからマッチングしてペアを作成する
#   --- マッチングさせることでセレクションバイアスを排除
#   --- マッチングしたペアは均等ウエイトで評価する(効果 = Y1 - Y0)
#   --- 傾向スコアをY1とY0で完全に一致させるには最適化が必要（マッチングによるペア作成で簡素化）

# 元データの確認
#     0     1
# 14665 17198
biased_data$treatment %>% table()

# 傾向スコアを利用したマッチング
m_near <-
  matchit(treatment ~ recency + history + channel,
          data = biased_data, method = "nearest", replace = TRUE)

# 結果確認
# --- 31863 (original), 24222 (matched)
#     0     1
#  7024 17198
m_near %>% print()
m_near %>% match.data() %>% use_series(treatment) %>% table()

# プロット作成
# --- Adjustは平均の差がゼロ付近になっている（X軸は平均の差）
m_near %>% love.plot(threshold = .1)

# データ抽出
# --- マッチング後のデータを作成
# --- 元データからマッチングしたレコードのみを抽出
matched_data <- m_near %>% match.data()
matched_data %>% glimpse()

# 介入効果の測定
# --- マッチング後のデータで効果の推定
PSM_result <-
   lm(spend ~ treatment, data = matched_data) %>%
     tidy()

# 確認
PSM_result %>% print()


# 4 逆確率重み付き推定（IPW） ----------------------------------------------------------

# 重みの推定
weighting <-
  weightit(treatment ~ recency + history + channel,
           data = biased_data,
           method = "ps",
           estimand = "ATE")

# プロット作成
# --- 重み付きデータでの共変量のバランス
weighting %>% love.plot(threshold = .1)

# 重み付きデータでの効果の推定
IPW_result <-
   lm(spend ~ treatment,
      data = biased_data, weights = weighting$weights) %>%
     tidy()


# 5 統計モデルを用いたメールの配信のログを分析 -----------------------------------------

# 乱数シードの設定
set.seed(1)

# 抽出レコードのサンプリング
train_flag <-
  male_df %>%
    nrow() %>%
    sample(nrow(male_df)/2, replace = FALSE)

# データ分割
male_df_train <-
  male_df[train_flag, ] %>%
    filter(treatment == 0)

male_df_test <-
  male_df[-train_flag, ]


# モデル構築
# --- 売上が発生する確率を予測するモデル
predict_model <-
  glm(conversion ~ recency + history_segment + channel + zip_code,
      data = male_df_train, family = binomial)

# 確率の予測
# --- 売上の発生確率からメールの配信確率を決める
pred_cv <-
  predict_model %>%
    predict(newdata = male_df_test,
            type = "response")

# 確率の変換
# --- パーセントランク
pred_cv_rank <-
  pred_cv %>%
    percent_rank()

# メール配信
# --- 配信確率を元にメールの配信を決める
mail_assign <-
  pred_cv_rank %>%
    sapply(rbinom, n = 1, size = 1)

# 配信ログの作成
ml_male_df <-
  male_df_test %>%
    mutate(mail_assign = mail_assign,
           ps = pred_cv_rank) %>%
    filter((treatment == 1 & mail_assign == 1) |
             (treatment == 0 & mail_assign == 0) )


## 実験をしていた場合の平均の差を確認
rct_male_lm <-
   lm(spend ~ treatment, data = male_df_test) %>%
     tidy()

## 平均の比較
ml_male_lm <-
   lm(spend ~ treatment, data = ml_male_df) %>%
     tidy()

# 傾向スコアマッチングの推定(TPS) -----------------------------------------------------

PSM_result <-
  Match(Y = ml_male_df$spend,
        Tr = ml_male_df$treatment,
        X = ml_male_df$ps,
        estimand = "ATT")

## 推定結果の表示
PSM_result %>% summary()

## IPWの推定
W.out <-
  weightit(treatment ~ recency + history_segment + channel + zip_code,
           data = ml_male_df,
           ps = ml_male_df$ps,
           method = "ps",
           estimand = "ATE")

# プロット作成
# --- 重み付けしたデータでの共変量のバランスを確認
W.out %>% love.plot(threshold = .1)

# 重みづけしたデータでの効果の分析
IPW_result <-
  ml_male_df %>%
    lm(spend ~ treatment, data = ., weights = W.out$weights) %>%
    tidy()
