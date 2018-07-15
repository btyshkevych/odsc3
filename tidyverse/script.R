
# Важливі посилання
# Веб сторінка Tidyvers: https://www.tidyverse.org/
# Підказки по R та RStudio у зручному форматі: https://www.rstudio.com/resources/cheatsheets/


# КРОК 1: Інсталюємо пакунки. Це робиться всього один раз. 
# Якщо у вас все вже інстальовано пропустіть цей крок.
install.packages("tidyverse")
install.packages("ggmap")
install.packages("lubridate")

# КРОК 2: встановлюємо робочу директорію. Це можна зробити функціє або у графічному інтерфейсі. 
# Оскільки у мене всі файли знаходяться в папці "~/Desktop/rivers_data", я використовую наступний код:
setwd("~/Desktop/rivers_data")


# КРОК 3: За допомогою функції library() завантажуємо пакунки які потрібні нам для роботи.
library(tidyverse)
library(ggmap)
# lubridate пакунок для легкої роботи з датами.
library(lubridate)

# КРОК 4: ІМПОРТУЄМО ДАНІ У R
rivers <- read.csv("2008-18-18052018.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)


# КРОК 5: ДОСЛІДИМО ТА ВІДБЕРЕМО РІЗНІ ЧАСТИНИ ТАБЛИЦІ
# розглянемо структуру файлів та інші характеристики. Це все буде друкуватися у консоль.
colnames(rivers)
nrow(rivers)
str(rivers)
glimpse(rivers)
summary(rivers)
# оберемо колонку таблиці
rivers$ЗависліРечовиниМгДм3
# оберемо рядок таблиці
rivers[2, ]
# оберемо декілька рядків таблиці
rivers[2:4, ]
# оберемо клітинку: перше число це номер рядка, другий - стовпчика (як на шахівниці)
rivers[2,4]


# КРОК 6: ПЕРЕТВОРЮЄМО ТАБЛИЦЮ З ШИРОКОЇ У ДОВГУ ЗА ДОПОМОГОЮ ФУНКЦІЇ gather() З TIDYR
rivers_long <- gather(rivers, substance, concentration, -c(1:8))
# робимо glimpse(rivers_long) у консолі і дивимось на тип значень у полі concentration.
# Зараз вони вони всі рядки - "chr", а треба їх виправити на числові "dbl".
rivers_long$concentration <- as.numeric(rivers_long$concentration)
# також претворимодату як рядок у обєкт дату. Це допоможе нам їх обчислювати.
rivers_long$ДатаСпостережень <- dmy(rivers_long$ДатаСпостережень)

# РОЗПОЧИНАЄМО DPLYR АЛЕ СПОЧАТКУ ПРО PIPE
# Відступ про pipe - "%>%" і для чого він нам потрібний
# код без pipe: (1)фільтруємо сульфат-іон - (2)групуємо за постами - (3)зводимо за максимумом - (4)сортуємо за зростанням
# у вашому глобальному середовищі з'являється три проміжні таблиці і ви маєте памятати їх назви. Це зайве.
rivers_filtred <- filter(rivers_long, substance == "СульфатІониМгДм3") 
rivers_grouped <- group_by(rivers_filtred, НазваПС)
rivers_summarised <- summarise(rivers_grouped, max_value = max(concentration))
sufat_rivers <- arrange(rivers_summarised, max_value)

# код з pipe: (1)фільтруємо сульфат-іон - (2)групуємо за постами - (3)зводимо за максимумом - (4)сортуємо за зростанням
# зверніть увагу, як просто читається код і який він компактний.
sufat_rivers <- rivers_long %>%
  filter(substance == "СульфатІониМгДм3") %>%
  group_by(НазваПС) %>%
  summarise(max_value = max(concentration)) %>%
  arrange(max_value)

# КРОК 7. НАШЕ ЗАВДАННЯ РОЗРАЗРАХУВАТИ СЕРЕДНЄ ЗНАЧЕННЯ ПО КОНЦЕНТРАЦІЇ ЗАБРУДНЮЮЧИХ РЕЧОВИН (concentration)
# НА КОЖНОМУ ПОСТІ СПОСТЕРЕЖЕННЯ (НазваПС). ВИКОРИСТАЄМО DPLYR
# na.omit() - пропустити NA

substs_on_posts <- rivers_long %>%
  na.omit() %>%
  # відфільтруємо спостереження за 2018 рік
  # filter(rivers_long, ДатаСпостережень < as.Date("2018-01-01") & ДатаСпостережень >= as.Date("2017-12-20")) %>%
  # групувати треба за НазваПС, substance, але ми додамо ще lat та lon, щоб лишити їх у таблиці для візуалізації.
  group_by(НазваПС, substance, lat, lon) %>%
  summarize(mean = mean(concentration), median = median(concentration), min = min(concentration), max = max(concentration))
# запишимо таблицю
write.csv(gather(substs_on_posts, valueType, value, -c(1:4)), "riversStats.csv")

# КРОК 8. СТВОРЮЄМО КАРТУ У GGMAP
# визначимо вектор з центральними координатами для території України
ukraine <- c(lon = 31.2, lat = 49.0)
# закачаємо підкладку
ukraine_map <- get_map(ukraine, zoom = 5, scale = 3, 
                       maptype = "toner-lite", source = "stamen")

# ВІДМАЛЬОВУЄМО КАТКУ
# код без пояснень. Зверніть увагу я використав функцію log() на mean, щоб змешити великі значення у розподілі
ggmap(ukraine_map, 
      base_layer = ggplot(substs_on_posts, aes(lon, lat, col = mean))) + 
  geom_point(size = 0.5) + scale_colour_gradient(low = "#4286f4", high = "#373B44") + 
  facet_wrap(~ substance)


#анотований код
ggmap(ukraine_map,        # перший аргумент - це підкладка
      base_layer = ggplot(substs_on_posts, aes(lon, lat, col = mean))) +    # розміщуємо шар з точками base_layer. це по суті код для ggplot2
  geom_point(size = 0.5) +    # налаштування шару точок
  scale_colour_gradient(low = "#4286f4", high = "#373B44") + # налаштування кольрів
  facet_wrap(~substance) # розбиваємо на речовини

# проаналізуємо отримане зображення. Головна проблема з ним, що для всіх показників використана одна шкала,
# а, як ми бачимо всі показники мають різний розмах значень

# як вихід, спробуємо зробити багато карт. Щоб не писати багато коду використаємо цикл for для кожної речовини

for (i in unique(substs_on_posts$substance)) {
  # фільтруємо таблицю для кожної речовини
  data <- filter(substs_on_posts, substance == i)
  # відмальовуємо карту
  map <- ggmap(ukraine_map, 
               base_layer = ggplot(data, aes(lon, lat, col = mean))) + 
    geom_point(size = 0.75) + scale_colour_gradient(low = "#4286f4", high = "#373B44") + 
    labs(title = i) 
  # друкуємо карту, але можна написати функцію і для збереження
  print(map)
}
