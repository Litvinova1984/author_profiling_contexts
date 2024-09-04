# loading libraries 
library(text2map)
library(text2vec)
library(gutenbergr)
library(tidyverse)
library(textclean)
library(stringi)
library(openxlsx)
library(dplyr)
library(wordVectors)
library(quanteda)
library(factoextra)
library(FactoMineR)
library(writexl)
library(reshape2) 
library(radiant)
library(word2vec)

## setting working directory 
setwd("/Users/tatiana/Desktop/Публикации/2024/6. ICRES_текст_контексты/Признаки_big5_pic")

# choose words 
# remove FW from tokenmy 
# tokenmy1 =  tokens_select(tokenmy, selection="remove", pattern = c("*ADP", "_SYM", ".", "*AUX", "*DET", "*PRON", "*CONJ", "*PART", "*PROPN"), padding = F)
tokenmy1 =  tokens_select(tokenmy, selection="remove", pattern = c("*ADP", ".", "*AUX", "*DET", "*PRON", "*CONJ", "*PART", "*PROPN"), padding = F)
dtm_assoc = dfm(tokens_select(tokenmy1, selection="keep", pattern = "работа_NOUN", window = 3), tolower = F)
dtm_assoc

# build the semantic direction 1 (and so on till 46):
additions  <- c("смотреть_VERB", "видеть_VERB", "свет_NOUN",  "бордовый_ADJ",  "глядеть_VERB",  "бежевый_ADJ",  "бирюзовый_ADJ", "голубой_NOUN", "просмотр_NOUN",  "рыжий_ADJ", "сияние_NOUN", "светлый_ADJ", "лампа_NOUN", "солнечный_ADJ", "окно_NOUN", "сцена_NOUN", "лазурный_ADJ", "квадратный_ADJ", "васильковый_ADJ", "кудрявый_ADJ", "жест_NOUN", "серебряный_ADJ", "прочитать_VERB",  "огонь_NOUN", "безглазый_ADJ",  "расплывчатый_ADJ", "размытость_NOUN", "ослепительный_ADJ", "зажмурить_VERB", "вид_NOUN", "полумрак_NOUN", "ночник_NOUN", "длинноногий_ADJ",  "образ_NOUN", "краска_NOUN", "худой_ADJ", "красить_VERB", "рогатый_ADJ", "танец_NOUN", "жемчужный_ADJ", "облачный_ADJ", "курчавый_ADJ", "стеклянный_ADJ", "деревянный_ADJ", "зелень_NOUN", "сирень_NOUN", "безоблачный_ADJ", "рельефный_ADJ")
substracts <- c("мелодичный_ADJ", "унюхать_VERB", "дослушать_VERB",  "разговорный_ADJ", "хриплый_ADJ", "напутственный_ADJ", "внятный_ADJ", "исконный_ADJ", "обидный_ADJ", "горький_ADJ", "храпеть_VERB", "замолчать_VERB", "национализировать_VERB",  "приторный_ADJ", "щекотливый_ADJ", "будний_ADJ", "хрипеть_VERB",  "перетерпеть_VERB",  "интуитивный_ADJ", "исходный_ADJ", "лирический_ADJ", "шум_NOUN", "уклончивый_ADJ", "духовный_ADJ", "врожденный_ADJ", "сегодняшний_ADJ", "актуальный_ADJ", "завизжать_VERB", "мотив_NOUN", "топот_NOUN", "недоумевать_VERB", "поспорить_VERB", "утвердить_VERB",  "силиться_VERB", "тишина_NOUN", "добиваться_VERB", "поэтичный_ADJ", "упоительный_ADJ", "тактичный_ADJ", "дыхательный_ADJ", "удачный_ADJ", "безмолвный_ADJ",  "вкус_NOUN")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc1_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)
doc1_closeness[1:10,]
View(doc1_closeness)
# build the semantic direction 2
additions = c("белый_ADJ", "бледный_ADJ", "вид_NOUN", "видеть_VERB", "выглядеть_VERB", "глядеть_NOUN", "голубой_ADJ", "гореть_VERB", "заметить_VERB", "заметный_ADJ", "замечать_VERB", "золотой_ADJ", "картина_NOUN", "красный_ADJ", "кровавый_ADJ", "мельком_ADV", "наблюдать_VERB", "небесный_ADJ", "обнаружить_VERB", "показаться_VERB", "появиться_VERB", "рыжий_ADJ", "седа_NOUN", "серый_ADJ", "синий_ADJ", "слепой_ADJ", "смотреть_VERB", "увидеть_VERB", "цвет_NOUN", "цветный_ADJ", "яркий_ADJ") 
sc_death <- get_centroid(additions, WE)
doc2_closeness <- CMDist(dtm = dtm_assoc, cv = sc_death, wv = WE)
head(doc2_closeness)

# build the semantic direction 3
additions = c("светлый_ADJ", "яркий_ADJ")
substracts = c("темный_ADJ", "тусклый_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="paired")
doc3_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)
head(doc3_closeness)

# build the semantic direction 4
additions = c("цветный_ADJ", "голубой_ADJ", "красный_ADJ", "золотой_ADJ", "синий_ADJ", "желтый_ADJ", "зеленый_ADJ", "яркий_ADJ")
sc_death <- get_centroid(additions, WE)
doc4_closeness <- CMDist(dtm = dtm_assoc, cv = sc_death, wv = WE)

# build the semantic direction 5
additions = c("большой_ADJ", "огромный_ADJ")
substracts = c("малый_ADJ", "маленький_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc5_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 6
additions = c("быстрый_ADJ")
substracts = c("медленный_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove")
doc6_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 7
additions = c("красивый_ADJ", "миловидный_ADJ")
substracts = c("некрасивый_ADJ", "страшный_ADJ", "уродливый_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc7_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 8
additions = c("голова_NOUN", "грудь_NOUN", "живот_NOUN", "ладонь_NOUN", "лицо_NOUN", "лоб_NOUN", "нога_NOUN", "нос_NOUN", "палец_NOUN", "рот_NOUN", "рука_NOUN", "тело_NOUN", "шея_NOUN", "глаз_NOUN", "веко_NOUN", "бровь_NOUN", "ухо_NOUN", "макушка_NOUN")
sd_eval1 <- get_centroid(additions, WE)
doc8_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 9
additions = c("лимон_NOUN", "клубничный_ADJ", "соль_NOUN", "цитрусовый_ADJ", "вкус_NOUN", "яблочный_ADJ", "груша_NOUN", "ягодный_ADJ", "огурец_NOUN", "обед_NOUN", "горький_ADJ", "картофельный_ADJ", "приправа_NOUN", "облепиха_NOUN", "приторный_ADJ", "рассол_NOUN", "лук_NOUN", "быть_VERB", "откушать_VERB", "жевать_VERB", "пить_VERB", "аппетит_NOUN", "неспелый_ADJ", "столовая_NOUN", "зелень_NOUN", "застолье_NOUN", "кофеин_NOUN", "тошнотворный_ADJ", "жарить_VERB", "повар_NOUN", "варить_VERB", "холодильник_NOUN", "витаминный_ADJ", "хлебать_VERB", "глотать_VERB", "кусать_VERB", "грызть_VERB", "травянистый_ADJ", "приятный_ADJ", "духовка_NOUN", "кислота_NOUN", "бар_NOUN", "сосать_VERB", "празднество_NOUN", "цветочный_ADJ", "выкормить_VERB", "мясистый_ADJ", "сытый_ADJ", "жир_NOUN", "настой_NOUN")
substracts = c("выдвиженец_NOUN", "призыв_NOUN", "глушитель_NOUN", "бастовать_VERB", "суд_NOUN", "национализировать_VERB", "скандал_NOUN", "завал_NOUN", "конкурировать_VERB", "топот_NOUN", "ополченец_NOUN", "вызов_NOUN",  "щелчок_NOUN", "порыв_NOUN", "отчуждение_NOUN", "замедление_NOUN", "беглец_NOUN", "бег_NOUN", "движение_NOUN", "надгробие_NOUN", "полумрак_NOUN", "терроризировать_VERB", "шпионить_VERB", "светить_VERB",  "выслуживаться_VERB", "натяжение_NOUN", "дуэль_NOUN", "отыскать_VERB", "лужа_NOUN", "рост_NOUN", "ночник_NOUN", "мотив_NOUN", "утвердить_VERB", "фронтовик_NOUN", "пододеяльник_NOUN", "остановка_NOUN", "паутина_NOUN", "поворот_NOUN", "уязвимый_VERB", "неразлучный_ADJ", "лежачий_NOUN",  "стоять_VERB", "грозить_VERB", "подгузник_NOUN")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc9_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 10
additions = c("горький_ADJ", "соленый_ADJ")
substracts = c("сладкий_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc10_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 11
additions = c("шум_NOUN", "будильник_NOUN", "кричать_VERB", "музыкант_NOUN", "мелодичный_ADJ", "проигрыватель_NOUN", "тишина_NOUN", "завизжать_VERB", "свистеть_VERB", "храпеть_VERB", "топот_NOUN", "скандал_NOUN", "говорить_VERB", "щелчок_NOUN", "стучать_VERB", "звать_VERB", "хриплый_ADJ", "хлопать_VERB", "монотонный_ADJ", "фен_NOUN", "звонить_VERB", "поезд_NOUN", "вечеринка_NOUN", "шептать_VERB", "трещать_VERB", "хлопать_VERB", "безмолвный_ADJ", "слушать_VERB", "хрипеть_VERB", "кашлять_VERB", "разговорный_ADJ", "комар_NOUN",  "пистолет_NOUN", "щелкать_VERB", "застолье_NOUN", "глушитель_NOUN", "шаркать_VERB", "война_NOUN", "веселиться_VERB", "сцена_NOUN", "вздох_NOUN", "чайник_NOUN", "поэтичный_ADJ", "морской_ADJ", "танец_NOUN", "рынок_NOUN", "муха_NOUN", "внятный_ADJ", "вихрь_NOUN")
substracts = c("загорелый_ADJ", "жир_NOUN", "вид_NOUN", "размытость_NOUN", "хранить_VERB", "гниль_NOUN", "йод_NOUN", "квадратный_ADJ", "сберечь_VERB", "рассол_NOUN", "груша_NOUN", "сорняк_NOUN", "кофеин_NOUN", "глядеть_VERB", "клей_NOUN", "приправа_NOUN", "клубничный_ADJ", "копить_VERB", "форма_NOUN", "вкус_NOUN", "светить_VERB", "искривленный_VERB", "сидеть_VERB", "ржавчина_NOUN",  "волокнистый_ADJ", "ночник_NOUN", "медуница_NOUN", "зажмурить_VERB", "безглазый_ADJ", "высохнуть_VERB", "чернильный_ADJ", "паутина_NOUN", "длинноногий_ADJ", "бордовый_ADJ", "вес_NOUN", "мазь_NOUN", "гнойный_ADJ", "настой_NOUN", "зелень_NOUN", "пододеяльник_NOUN", "полнота_NOUN", "национализировать_VERB", "рост_NOUN", "сияние_NOUN", "спрятать_VERB", "курчавый_ADJ", "украсть_VERB", "облепиха_NOUN")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc11_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

#build the semantic direction 12
additions = c("бить_VERB", "глухой_ADJ", "громкий_ADJ", "звонить_VERB", "звучать_VERB", "крик_NOUN",  "кричать_VERB", "музыкальный_ADJ", "ответить_VERB", "петь_VERB", "раздаться_VERB", "слушать_VERB", "слышать_VERB", "тихий_ADJ", "тишина_NOUN", "шум_NOUN", "стук_NOUN", "звон_NOUN", "писк_NOUN", "хрип_NOUN", "звук_NOUN", "отзвук_NOUN", "гудок_NOUN", "слово_NOUN", "речь_NOUN", "кашель_NOUN")
sd_eval1 <- get_centroid(additions, WE)
doc12_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

#build the semantic direction 13
additions = c("громкий_ADJ")
substracts = c("тихий_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="paired")
doc13_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)


#build the semantic direction 14

additions = c("трогать_VERB", "пушистый_ADJ", "шершавый_ADJ", "мягкость_NOUN", "холодный_ADJ", "теплый_NOUN", "уколоть_VERB", "целовать_VERB", "жидкий_ADJ", "рельефный_ADJ", "скользкий_ADJ", "пожать_VERB", "ожог_NOUN", "деревянный_ADJ", "держать_VERB", "укол_NOUN", "схватить_VERB", "жесткий_ADJ", "поцарапать_VERB", "липучка_NOUN", "бумажный_ADJ", "плотный_ADJ", "песочный_ADJ", "мыть_VERB",   "боль_NOUN", "хлопать_VERB", "еж_NOUN", "стеклянный_ADJ", "брить_VERB", "брать_VERB", "нагрев_NOUN", "нож_NOUN", "каменистый_ADJ", "мыльный_ADJ", "вязать_VERB", "месить_VERB", "танец_NOUN", "охлаждение_NOUN", "волокнистый_ADJ", "пластмассовый_ADJ", "железный_ADJ", "огонь_NOUN", "замерзнуть_VERB", "резать_VERB", "упасть_VERB", "мазь_NOUN", "глинистый_ADJ", "плыть_VERB")
substracts = c("национализировать_VERB", "тон_NOUN",  "суждение_NOUN", "надменный_ADJ", "честный_ADJ", "будний_ADJ", "мотив_NOUN",  "пропагандировать_VERB", "выдвиженец_NOUN", "призывать_VERB", "разговорный_ADJ", "верить_VERB", "бойкотировать_VERB", "знать_VERB", "просмотр_NOUN", "исконный_ADJ", "размытость_NOUN", "смотреть_VERB", "выпрашивать_VERB", "завтрашний_ADJ",  "думать_VERB", "радиоактивный_ADJ", "недоумевать_VERB", "безмолвный_ADJ", "удачливый_ADJ", "актуальный_ADJ", "глумливый_ADJ", "безрассудный_ADJ", "шум_NOUN", "преуспевать_VERB", "выслуживаться_VERB", "гордый_ADJ", "праведный_ADJ", "напутственный_ADJ", "вызов_NOUN", "замолчать_VERB", "полярный_ADJ", "конкурировать_VERB", "гордиться_VERB", "реактивный_ADJ", "дослушать_VERB", "исходный_ADJ", "лукавый_ADJ", "обидчивый_ADJ", "обаятельный_ADJ", "внятный_ADJ", "ведать_VERB")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc14_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

#build the semantic direction 15
additions = c("влажный_ADJ", "гладкий_ADJ", "горячий_ADJ", "клейкий_ADJ", "колкий_ADJ", "колючий_ADJ", "леденить_VERB", "ледяной_ADJ", "липкий_ADJ", "мокрый_ADJ", "мягкий_ADJ", "сыр_NOUN", "холодный_ADJ", "шероховатый_ADJ", "шершавый_ADJ", "щетинистый_ADJ")
sd_eval1 <- get_centroid(additions, WE)
doc15_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 16
additions = c("теплый_ADJ", "горячий_ADJ") 
substracts = c("холодный_ADJ", "ледяной_ADJ", "прохладный_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc16_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 17
additions = "легкий_ADJ"
substracts = "тяжелый_ADJ"
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="paired")
doc17_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 18
additions = "гладкий_ADJ"
substracts = "жесткий_ADJ"
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="paired")
doc18_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 19
additions = c("болеть_VERB", "больной_NOUN", "боль_NOUN")
sd_eval1 <- get_centroid(additions, WE)
doc19_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 20

additions = c("нюхать_VERB", "сирень_NOUN", "хвойный_ADJ", "цветочный_ADJ", "цитрусовый_ADJ", "хлорка_NOUN", "яблочный_ADJ", "приправа_NOUN", "клубничный_ADJ", "курить_VERB", "скунс_NOUN", "ягодный_ADJ", "морской_ADJ", "столовая_NOUN", "обед_NOUN", "лук_NOUN", "тошнотворный_ADJ",   "жарить_VERB", "краска_NOUN", "груша_NOUN", "духовка_NOUN", "лимон_NOUN", "гниль_NOUN", "кофеин_NOUN", "туалет_NOUN", "огурец_NOUN", "мазь_NOUN", "варить_VERB", "облепиха_NOUN", "приторный_ADJ", "больница_NOUN", "красить_VERB", "зелень_NOUN",   "рынок_NOUN", "клей_NOUN", "травянистый_ADJ", "повар_NOUN", "рассол_NOUN", "застолье_NOUN", "холодильник_NOUN", "подгузник_NOUN", "приятный_ADJ", "токсичный_ADJ", "картофельный_ADJ", "уборная_NOUN", "земляной_ADJ", "дождливый_ADJ", "медуница_NOUN") 
substracts = c("бойкотировать_VERB", "давление_NOUN", "слушать_VERB", "выдвиженец_NOUN", "призыв_NOUN", "молить_VERB", "мотив_NOUN", "выслуживаться_VERB", "агитировать_VERB", "ведать_VERB", "ополченец_NOUN", "национализировать_VERB", "вызов_NOUN", "бежевый_ADJ", "помирить_VERB", "отчуждение_NOUN", "голосовать_VERB", "робеть_VERB", "рост_NOUN", "градусник_NOUN", "герой_NOUN", "размер_NOUN", "комар_NOUN", "огорчаться_VERB", "хлопать_VERB", "разговорный_ADJ", "соперничать_VERB", "поворот_NOUN", "звонить_VERB", "щелчок_NOUN", "худой_ADJ", "скользить_VERB", "стучать_VERB", "замедление_NOUN", "ночник_NOUN",   "жест_NOUN", "осмыслять_VERB", "звать_VERB", "бастовать_VERB", "визжать_VERB", "воровать_VERB", "будильник_NOUN", "ухабистый_ADJ", "паутина_NOUN", "решать_VERB", "шпионить_VERB", "добиваться_VERB")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc20_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)


# build the semantic direction 21
additions = c("улыбаться_VERB", "говорить_VERB", "щуриться_VERB", "моргать_VERB", "двигаться_VERB", "писать_VERB", "стучать_VERB", "бежать_VERB", "идти_VERB", "семенить_VERB", "бить_VERB", "кричать_VERB", "крик_NOUN", "спускаться_VERB", "поход_NOUN", "ходить_VERB", "ползать_VERB", "лететь_VERB", "шаг_NOUN", "бег_NOUN", "прыжок_NOUN", "прыгать_VERB", "бросок_NOUN")
sd_eval1 <- get_centroid(additions, WE)
doc21_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 22

additions = c("ложка_NOUN", "дрель_NOUN", "молоток_NOUN", "ручка_NOUN", "автомобиль_NOUN", "грабли_NOUN", "ластик_NOUN", "вилка_NOUN", "пинцет_NOUN", "нож_NOUN", "игла_NOUN", "велосипед_NOUN", "карандаш_NOUN", "машина_NOUN", "телефон_NOUN", "сковорода_NOUN", "фломастер_NOUN", "застежка_NOUN", "циркуль_NOUN", "лопата_NOUN", "клавиша_NOUN", "отвертка_NOUN", "тарелка_NOUN", "веник_NOUN", "стакан_NOUN", "наперсток_NOUN", "топор_NOUN", "напильник_NOUN",  "маркер_NOUN", "расческа_NOUN", "скальпель_NOUN", "копье_NOUN", "ножницы_NOUN", "дротик_NOUN", "пуговица_NOUN", "бумеранг_NOUN", "ключ_NOUN", "бисер_NOUN", "шнурок_NOUN", "мотоцикл_NOUN", "гайка_NOUN",  "кисть_NOUN", "мопед_NOUN", "пить_VERB", "гвоздь_NOUN", "штопор_NOUN", "самокат_NOUN", "запонка_NOUN")
substracts = c("ностальгия_NOUN", "гора_NOUN", "галлюцинация_NOUN", "стопа_NOUN", "презрение_NOUN", "яркость_NOUN", "зависть_NOUN", "скука_NOUN", "тошнота_NOUN", "акцент_NOUN", "мысль_NOUN", "отчаяние_NOUN", "лев_NOUN", "крокодил_NOUN", "инстинкт_NOUN", "фактор_NOUN", "стыд_NOUN", "понятие_NOUN", "допущение_NOUN", "небосвод_NOUN", "депрессия_NOUN", "цвет_NOUN", "тон_NOUN", "писк_NOUN", "тоска_NOUN", "воображение_NOUN", "луна_NOUN", "звезда_NOUN", "произношение_NOUN", "отговорка_NOUN", "сочность_NOUN", "вдох_NOUN", "молчание_NOUN", "факт_NOUN", "смысл_NOUN", "динозавр_NOUN", "равнодушие_NOUN", "рассвет_NOUN", "даль_NOUN", "обоняние_NOUN", "совет_NOUN", "солнце_NOUN", "сон_NOUN", "воспоминание_NOUN", "досада_NOUN", "облако_NOUN", "небо_NOUN", "отвращение_NOUN")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc22_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 23

additions = c("близкий_ADJ", "близко_ADV", "приближаться_VERB", "приходить_VERB")
substracts = c("далеко_ADV", "дальний_ADJ", "отдаляться_VERB", "уходить_VERB")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc23_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 24
additions = c("даль_ADV", "звезда_NOUN", "луна_NOUN", "метеорит_NOUN", "молния_NOUN", "небо_NOUN", "небосвод_NOUN", "небосклон_NOUN", "облако_NOUN", "потолок_NOUN", "пространство_NOUN", "радуга_NOUN", "рассвет_NOUN", "солнце_NOUN", "спутник_NOUN", "туча_NOUN", "фейерверк_NOUN", "чердак_NOUN", "штиль_NOUN")
substracts = c("асфальт_NOUN", "впадина_NOUN", "газон_NOUN", "гора_NOUN", "грязь_NOUN", "дно_NOUN", "колодец_NOUN", "люк_NOUN", "могила_NOUN", "паркет_NOUN", "плинтус_NOUN", "пол_NOUN", "порог_NOUN", "почва_NOUN", "прорубь_NOUN",  "пруд_NOUN", "сор_NOUN", "трава_NOUN", "тротуар_NOUN", "углубление_NOUN", "шоссе_NOUN", "шпала_NOUN", "яма_NOUN")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc24_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

#build the semantic direction 25
additions = c("один_NUM", "два_NUM", "три_NUM", "четыре_NUM", "пять_NUM", "шесть_NUM", "семь_NUM", "восемь_NUM", "девять_NUM", "десять_NUM", "сто_NUM","тысяча_NOUN")
sd_eval1 <- get_centroid(additions, WE)
doc25_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 26

additions = c("бесконечный_ADJ", "беспрерывно_ADV", "ближайший_ADJ", "будущий_ADJ", "бывший_ADJ", "век_NOUN", "временный_ADJ", "вчера_ADV", "далее_ADV", "долго_ADV", "древний_ADJ", "жизнь_NOUN", "завтра_NOUN", "закончить_VERB", "заново_ADV", "заранее_ADV", "история_NOUN", "кончить_VERB", "мгновение_NOUN", "миг_NOUN", "минута_NOUN", "момент_NOUN", "моментальный_ADJ", "навеки_ADV", "начало_NOUN", "начать_VERB", "неделя_NOUN", "немедленно_ADV", "ныне_ADV", "нынешний_ADJ", "нынче_ADV", "окончание_NOUN", "опять_VERB", "позднее_ADV", "поздний_ADJ", "позже_ADV", "постепенно_ADV", "предварительно_ADV", "прежде_ADV", "ранее_ADV", "ранний_ADJ", "рано_ADV", "сегодня_ADV", "секунда_NOUN", "современный_ADJ", "сразу_ADV", "срок_NOUN", "старый_ADJ", "успеть_VERB", "час_NOUN", "этап_NOUN", "эпоха_NOUN", "юный_ADJ")
sd_eval1 <- get_centroid(additions, WE)
doc26_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 27

additions = c("длительный_ADJ", "долгий_ADJ")
substracts = c("быстрый_ADJ", "короткий_ADJ", "мгновенный_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc27_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 28

additions = c("новый_ADJ")
substracts = c("старый_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove")
doc28_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)


#build the semantic direction 29

additions = c("последующий_ADJ", "причина_NOUN", "потому_ADV", "поэтому_ADV", "следовательно_ADV")
sd_eval1 <- get_centroid(additions, WE)
doc29_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 30

additions = c("благородный_ADJ", "добрый_ADJ", "искренний_ADJ", "ласковый_ADJ", "нежный_ADJ", "независимый_ADJ", "порядочный_ADJ", "разумный_ADJ", "решительный_ADJ", "талантливый_ADJ", "творческий_ADJ", "уверенный_ADJ", "умный_ADJ")
sd_eval1 <- get_centroid(additions, WE)
doc30_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

#build the semantic direction 31
additions = c("беседа_NOUN", "говорить_VERB", "договор_NOUN", "знакомый_ADJ", "коллега_NOUN", "конфликт_NOUN", "лидер_NOUN", "личность_NOUN", "любовь_NOUN", "бабушка_NOUN", "мальчик_NOUN", "мама_NOUN", "мать_NOUN", "муж_NOUN", "начальник_NOUN", "обсуждать_VERB", "общество_NOUN", "отец_NOUN", "папа_NOUN", "письмо_NOUN", "поддержка_NOUN", "подруга_NOUN", "помощь_NOUN", "понимание_NOUN", "понимать_VERB", "приятель_NOUN", "проговорить_VERB", "работа_NOUN", "работать_VERB", "разговор_NOUN", "разговаривать_VERB", "рассказывать_VERB", "речь_NOUN", "родитель_NOUN", "семейный_ADJ", "семья_NOUN", "сообщить_VERB", "сообщение_NOUN", "спор_NOUN", "товарищ_NOUN", "улыбаться_VERB", "улыбка_NOUN", "чуждый_ADJ", "шутка_NOUN", "смех_NOUN")
sd_eval1 <- get_centroid(additions, WE)
doc31_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 32
additions = c("мужчина_NOUN", "мужской_ADJ")
substracts = c("женщина_NOUN", "женский_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc32_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 33

additions = c("дух_NOUN", "душа_NOUN", "судьба_NOUN", "свобода_NOUN", "искусство_NOUN", "время_NOUN", "любовь_NOUN", "надежда_NOUN", "принятие_NOUN", "красота_NOUN", "счастье_NOUN", "мечта_NOUN", "ощущение_NOUN", "основной_ADJ", "сознание_NOUN", "изучение_NOUN", "мысль_NOUN", "потребность_NOUN", "творчество_NOUN", "воспоминание_NOUN", "открытие_NOUN", "эффективность_NOUN", "энергия_NOUN", "образ_NOUN", "необходимость_NOUN", "воздействие_NOUN", "будущий_ADJ", "чувство_NOUN", "развитие_NOUN", "сомнение_NOUN", "чудо_NOUN", "эффект_NOUN", "сожаление_NOUN", "реальность_NOUN", "стиль_NOUN", "добро_NOUN", "главный_ADJ", "взаимодействие_NOUN", "пространство_NOUN", "соединение_NOUN", "вера_NOUN", "увеличение_NOUN", "культура_NOUN", "разница_NOUN", "разрешение_NOUN", "выполнение_NOUN", "назначение_NOUN", "описание_NOUN", "перспектива_NOUN") 
substracts = c("стол_NOUN", "самолет_NOUN", "мальчик_NOUN", "квартира_NOUN", "больница_NOUN", "врач_NOUN", "водка_NOUN", "платье_NOUN", "кровать_NOUN", "девочка_NOUN", "телефон_NOUN", "пальто_NOUN", "фотография_NOUN", "рубль_NOUN", "поезд_NOUN", "птица_NOUN", "стакан_NOUN", "остров_NOUN", "сосед_NOUN", "мясо_NOUN", "бабушка_NOUN", "стекло_NOUN", "кресло_NOUN", "жена_NOUN", "стул_NOUN", "ухо_NOUN", "ладонь_NOUN", "этаж_NOUN", "автомобиль_NOUN", "книжка_NOUN", "лошадь_NOUN", "спина_NOUN", "телевизор_NOUN", "слеза_NOUN", "автобус_NOUN", "газета_NOUN", "бутылка_NOUN", "вино_NOUN", "шея_NOUN", "диван_NOUN", "пиво_NOUN", "танк_NOUN", "рука_NOUN", "нож_NOUN", "щека_NOUN", "мужчина_NOUN", "студент_NOUN", "хлеб_NOUN", "пистолет_NOUN", "нога_NOUN")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc33_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 34

additions = c("малина_NOUN", "морковь_NOUN", "собака_NOUN", "персик_NOUN", "лампочка_NOUN", "вилка_NOUN", "топор_NOUN", "кружок_NOUN", "яблоко_NOUN", "мяч_NOUN", "арбуз_NOUN", "сапог_NOUN", "фейерверк_NOUN", "глаз_NOUN", "таракан_NOUN", "медведь_NOUN", "апельсин_NOUN", "нитка_NOUN", "лимон_NOUN", "потолок_NOUN", "мухомор_NOUN", "черепаха_NOUN", "пить_VERB", "самолет_NOUN", "укроп_NOUN", "заяц_NOUN", "тигр_NOUN", "тыква_NOUN", "автомобиль_NOUN", "велосипед_NOUN", "тарелка_NOUN", "мопед_NOUN", "мандарин_NOUN", "грейпфрут_NOUN", "баклажан_NOUN", "ботинок_NOUN", "лягушка_NOUN", "акула_NOUN", "санки_NOUN", "утка_NOUN", "игла_NOUN", "расческа_NOUN", "трамвай_NOUN", "кошка_NOUN", "перец_NOUN", "ананас_NOUN", "бабочка_NOUN", "солнце_NOUN", "облако_NOUN", "отвертка_NOUN")
substracts = c("ударение_NOUN", "идея_NOUN", "понимание_NOUN", "уныние_NOUN", "досада_NOUN", "уход_NOUN", "расширение_NOUN", "стратегия_NOUN", "тяга_NOUN", "галлюцинация_NOUN", "отвращение_NOUN", "мысль_NOUN", "отчаяние_NOUN", "факт_NOUN", "замысел_NOUN", "любовь_NOUN", "вина_NOUN", "сочувствие_NOUN", "переживание_NOUN", "набор_NOUN", "произношение_NOUN", "отговорка_NOUN", "волнение_NOUN", "образ_NOUN", "хрип_NOUN", "стыд_NOUN", "тревога_NOUN", "воображение_NOUN", "выбор_NOUN", "стресс_NOUN", "свет_NOUN", "осязание_NOUN", "сужение_NOUN", "запах_NOUN", "зависть_NOUN", "стимул_NOUN", "восприятие_NOUN", "теория_NOUN", "навык_NOUN", "попытка_NOUN", "инстинкт_NOUN", "отбор_NOUN", "тон_NOUN", "фактор_NOUN", "сознание_NOUN", "значение_NOUN", "смысл_NOUN", "гипотеза_NOUN", "допущение_NOUN")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc34_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 35

additions = c("абсолютно_ADV", "безусловно_ADV", "более_ADV", "возможный_ADJ", "доказательство_NOUN", "думать_VERB", "задача_NOUN", "знать_VERB", "итог_NOUN", "конкретный_ADJ", "лишний_ADJ", "максимальный_ADJ", "мнение_NOUN", "наблюдение_NOUN", "наверняка_ADV", "намеренный_ADJ", "наоборот_ADV", "напоминать_VERB", "невероятный_ADJ", "недостаточно_ADV", "неизвестный_ADJ", "необходимый_ADJ", "непонятный_ADJ", "несомненно_ADV", "несмотря_ADV", "объяснение_NOUN", "обычно_ADV", "обычный_ADJ", "одинаково_ADV", "одинаковый_ADJ", "основной_ADJ", "отличие_NOUN", "очевидный_ADJ", "понятие_NOUN", "понять_VERB", "потребность_NOUN", "правило_VERB", "признак_NOUN", "причина_NOUN", "проблема_NOUN", "процесс_NOUN", "разница_NOUN", "реальность_NOUN", "результат_NOUN", "ситуация_NOUN", "смысл_NOUN", "событие_NOUN", "сознание_NOUN", "сравнение_NOUN", "теоретически_ADV", "удивительно_ADV", "ум_NOUN", "явление_NOUN", "явно_ADV", "явный_ADJ", "ясно_ADJ")
sd_eval1 <- get_centroid(additions, WE)
doc35_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)


# build the semantic direction 36
additions = c("война_NOUN", "смерть_NOUN", "убить_VERB", "мертвый_ADJ", "погибнуть_NOUN", "умереть_VERB", "несчастный_ADJ", "потерять_VERB", "уголовный_ADJ", "опасный_ADJ", "злой_NOUN", "пьяный_ADJ", "плохой_ADJ", "страшный_ADJ", "больной_ADJ", "удар_NOUN", "бояться_VERB", "очередь_NOUN", "грязный_ADJ", "глупый_ADJ", "слабый_ADJ", "вооруженный_ADJ", "бедный_ADJ", "судебный_ADJ", "бить_VERB", "бросить_VERB", "заставить_VERB", "судить_VERB", "плакать_VERB", "падать_VERB", "налоговый_ADJ", "оружие_NOUN", "упасть_VERB", "кровь_NOUN", "зависеть_VERB", "невозможный_ADJ", "лишний_ADJ", "мешать_VERB", "чужой_ADJ", "пустой_ADJ", "исчезнуть_VERB", "партийный_ADJ", "кончиться_VERB", "толстый_ADJ", "конец_NOUN", "дикий_ADJ", "кричать_VERB")
substracts = c("мама_NOUN", "счастье_NOUN", "улыбнуться_VERB", "здоровый_ADJ", "родной_ADJ", "успех_NOUN", "любимый_ADJ", "лучший_ADJ", "счастливый_ADJ", "любовь_NOUN", "любить_VERB", "солнце_NOUN", "довольный_ADJ", "хороший_ADJ", "море_NOUN", "красивый_ADJ", "солнечный_ADJ", "замечательный_ADJ", "друг_NOUN", "положительный_ADJ", "смеяться_VERB", "прекрасный_ADJ", "природа_NOUN", "милый_ADJ", "жить_VERB", "свобода_NOUN", "образование_NOUN", "небо_NOUN", "добрый_ADJ", "жизнь_NOUN", "семья_NOUN", "герой_NOUN", "светлый_ADJ", "культура_NOUN", "веселый_ADJ", "девушка_NOUN", "искусство_NOUN", "умный_ADJ", "живой_ADJ", "активный_ADJ", "летний_ADJ", "развитие_NOUN", "сын_NOUN", "мать_NOUN", "создавать_VERB", "отец_NOUN", "интересный_ADJ", "миллион_NOUN", "смешной_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc36_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 37
additions = c("полезный_ADJ")
substracts = c("вредный_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove")
doc37_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

#build the semantic direction 38
additions = c("приятный_ADJ")
substracts = c("неприятный_ADJ", "противный_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc38_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 39
additions = c("счастливый_ADJ", "радостный_ADJ")
substracts = c("грустный_ADJ", "несчастный_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc39_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

#build the semantic direction 40

additions = c("раздраженный_VERB", "разъяренный_VERB")
substracts = c("спокойный_ADJ", "умиротворенный_VERB")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc40_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 41  
additions = c("отвращение_NOUN", "неприязнь_NOUN")
substracts = c("влечение_NOUN", "любовь_NOUN", "любимый_ADJ", "любимый_VERB")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc41_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 42  
additions = c("опасный_ADJ", "страх_NOUN", "пугать_VERB")
substracts = c("безопасный_ADJ", "безобидный_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc42_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 43

additions = c("удивлять_VERB", "поражать_VERB")
substracts = c("скучать_VERB", "скучный_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc43_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 44
additions = c("вялый_ADJ", "апатичный_ADJ")
substracts = c("живой_ADJ", "буйный_ADJ", "бодрый_ADJ", "энергичный_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc44_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 45  
additions = c("необходимый_ADJ", "нужный_ADJ")
substracts = c("ненужный_ADJ", "лишний_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc45_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 46  
additions = c("взволнованный_VERB", "волновать_VERB", "волнительный_ADJ")
substracts = c("спокойный_ADJ", "безразличный_ADJ", "умиротворенный_VERB")
pairs1 <- cbind(additions, substracts) 
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc46_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

#final dataset construction

library(tidyverse)

#put all data frames into list

df_list <- list(doc1_closeness, doc2_closeness, doc3_closeness, doc4_closeness, doc5_closeness, doc6_closeness, doc7_closeness, doc8_closeness, doc9_closeness, doc10_closeness, doc11_closeness, doc12_closeness, doc13_closeness, doc14_closeness, doc15_closeness, doc16_closeness, doc17_closeness, doc18_closeness, doc19_closeness, doc20_closeness, doc21_closeness, doc22_closeness, doc23_closeness, doc24_closeness, doc25_closeness, doc26_closeness, doc27_closeness, doc28_closeness, doc29_closeness, doc30_closeness, doc31_closeness, doc32_closeness, doc33_closeness, doc34_closeness, doc35_closeness, doc36_closeness, doc37_closeness, doc38_closeness, doc39_closeness, doc40_closeness, doc41_closeness, doc42_closeness, doc43_closeness, doc44_closeness, doc45_closeness, doc46_closeness)      

# merge all data frames together

df_list %>% reduce(full_join, by='doc_id')
df_list = as.data.frame(df_list)
df_list = df_list[!duplicated(as.list(df_list))]
colnames(df_list) = names

# change name

colnames(df_list)[c(2:47)] <- paste("job", colnames(df_list)[c(2:47)], sep = "_")
job=df_list
str(me)
names = c("doc_id", "VisNorms", 	"VisLIWC", 	"VisIntens",	"VisColor",	"VisSize",	"VisMotion", 	"VisFace", "VisBody",	"GustNorms", 	"GustTaste",	"AudNorms",	"AudLIWC",	"AudIntens",	"SomatNorms",	"SomatLIWC",	"SomatSurface", "SomatProprioception",	 "SomatTexture",	"SomatNociception",	"OlfacNorms",	 "MotorBinder",	 "MotorPractice",	"SpatialProx", 	"SpatialUpDown",	"SpatialNumber",	"TemporalLIWC",	"TempDuration", "TempAge",	"Causal",	 "SocialSelf",	"SocialLIWC",	 "SocialGender",	 "CognitionAbstract",  "CognitionImage",	"CognitionLIWC",	"EmoSentiment",	"EmoBenefit",	"EmoPleasant",	"EmoHappy",	"EmoAngry",	"EmoDisgust",	"EmoFear",	"EmoSurprised",	"Drive",	"DriveNeeds",	"AttentionArousal")

# merging feature files
face = read.xlsx("/Users/tatiana/Desktop/Публикации/2024/6. ICRES_текст_контексты/Признаки_big5_pic/face.xlsx")

library(readxl)
myCols <- as.character(read_excel("/Users/tatiana/Desktop/Публикации/2024/6. ICRES_текст_контексты/Признаки_big5_pic/young.xlsx", n_max = 1, col_names = FALSE))
young <- read_excel("/Users/tatiana/Desktop/Публикации/2024/6. ICRES_текст_контексты/Признаки_big5_pic/young.xlsx", skip = 2, col_names = myCols)
View(young)

#put all data frames into list
df_list <- list(all, can, daughter, face, girl, granny, guy, human, life, man, me, mother, own, possible, see, side, sight, think, time, very, want, we, whole, woman, young)    

# merge all data frames together
df_list %>% reduce(full_join, by='doc_id')
View(df_list)
df_list = as.data.frame(df_list)
df_list = df_list[!duplicated(as.list(df_list))]
View(df_list)
write.xlsx(df_list, "combined.xlsx")

meta=read.xlsx("/Users/tatiana/Desktop/Публикации/2024/6. ICRES_текст_контексты/Корпусы/готовое/Большая_5_картина.xlsx", sheet="metadata")
head(meta)
finalpic= merge(df_list, meta, by="doc_id") 
View(finalpic)                   
write.xlsx(finalpic, "finalpic.xlsx")


#put all data frames into list letter
df_list1 <- list(man, me, own, very, think, life, whole, time, want, all, new, already, my, love, hope, longago, many, write, emoji, wait, your, friend, last, see, study, nice, soon, day, want2, learn, miss1, miss2, job)

# merge all data frames together letter
df_list1 %>% reduce(full_join, by='doc_id')
View(df_list1)
df_list1 = as.data.frame(df_list1)
df_list1 = df_list1[!duplicated(as.list(df_list1))]
head(df_list1)
write.xlsx(df_list, "combined.xlsx")

# Save resulting file
meta2=read.xlsx("/Users/tatiana/Desktop/Публикации/2024/6. ICRES_текст_контексты/Корпусы/готовое/Большая_5_письмо.xlsx", sheet=2)
finalletter= merge(df_list1, meta2, by="doc_id") 
write.xlsx(finalletter, "finalletter.xlsx")

# Exploratory analysis and classification

X <- finalletter[, -c(1, 1520:1527)]
X1 <- finalpic[, -c(1, 1152:1159)]

## Gender/ Extraversion prediction

Y <- as.factor(finalletter$Gender)
Y1 <- as.factor(finalpic$Gender)
Y2 <- Extraversionpic1 

## classification
plsda.srbct <- plsda(X1,Y2, ncomp = 5)

### tuning
perf.plsda.srbct <- perf(plsda.srbct, validation = 'Mfold', folds = 3, 
                         progressBar = T,  # Set to TRUE to track progress
                         nrepeat = 50)         # We suggest nrepeat = 50 
### plot results
plot(perf.plsda.srbct, sd = TRUE, legend.position = 'horizontal')
perf.plsda.srbct$error.rate.class

#### final classification
final.plsda.srbct <- plsda(X,Y, ncomp = 5)
plotLoadings(plsda.srbct, comp = 1, method = 'median', contrib = 'max')
cim(final.plsda.srbct, row.sideColors = color.mixo(Y))
auc.srbct <- auroc(plsda.srbct)
plsda.srbct$loadings

#### Visualization
plotIndiv(tune.pca.multi1,
          comp = c(1, 2),   # Specify components to plot
          ind.names = F, # Show row names of samples
          group = Gender,
          title = 'PCA comp 1 - 2',
          legend = TRUE, legend.title = 'Gender')

plotVar(tune.pca.multi1, comp = c(1, 2), 
        var.names = TRUE,
        cex = 3,   # To change the font size
        cutoff = 0.5,  # For further cutoff
        title = 'PCA comp 1 - 2')

# example of converting numeric values to factor 
Extraversionpic=cut(finalpic$Extraversion, breaks = c(0, 41, 51, 75),
                    labels = c("Low", "Medium", "High"))

library(forcats)

#### example of reducing the number of factor levels
Extraversionpic1 = fct_collapse(Extraversionpic, LM = c("Low", "Medium"), H = "High")