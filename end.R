library(dplyr)
library(readr)
library(ggplot2)

# 載入三個 CSV 檔案
data_110 <- read_csv("110.csv")
data_111 <- read_csv("111.csv")
data_112 <- read_csv("112.csv")

# 為每個資料框添加年份欄位
data_110 <- data_110 %>% mutate(year = 110)
data_111 <- data_111 %>% mutate(year = 111)
data_112 <- data_112 %>% mutate(year = 112)

# 合併三個資料框
combined_data <- bind_rows(data_110, data_111, data_112)

# 計算每年男女的總出生數
# 將男女的出生數單獨處理並合併
total_births_gender <- combined_data %>%
  gather(gender, birth_count, `出生數男`, `出生數女`) %>%
  mutate(gender = ifelse(gender == "出生數男", "男", "女"))

# 計算每年總出生數（合併男女出生數）
total_births <- total_births_gender %>%
  group_by(year, gender) %>%
  summarise(total_births = sum(birth_count)) %>%
  ungroup()

# 繪製每年男女出生數的長條圖，區分顏色
ggplot(total_births, aes(x = factor(year), y = total_births, fill = gender)) +
  geom_bar(stat = "identity", position = "stack") +  # position = "stack" 會堆疊男女的出生數
  scale_fill_manual(values = c("男" = "blue", "女" = "pink")) +  # 使用兩個顏色來區分男生和女生
  labs(
    title = "每年台南市男女出生數",
    x = "年份",
    y = "出生數",
    fill = "性別"
  ) +
  theme_minimal()

