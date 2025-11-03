install.packages(c("tidyverse", "dplyr", "ggplot2", "readxl")) 
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl) 
#Neophodne biblioteke

#setwd("C:/Users/Djole/Desktop/UUNOP Seminarski")
setwd("E:/faks/letnji/uvod u nauku/Projekat/UUNOP Seminarski")
#Podesavanje potrebnog wd

df <- read_csv("UCI_Credit_Card.csv")

head(df)

dim(df)

summary(df)

#Iz summarya možemo već zaključiti neke neregularnosti Education ima vrednosti koje ne bi smele postojati, kao što su 0 ili 6,
#koje nisu navedene u listi dozvoljenih vrednosti, zatim u koloni mariage vidimo da postoji min vrednost 0, što takođe nije
#definisano spiskom podrazumevanih vrednosti, za PAY kolone postoje min vrednosti -2 koje takođe nisu definisane podrazumevanim
#vrednostima

#U iznosima mesečnih računa odnosno BILL_AMT postoje negativne vrednosti računa, što nije unutar opsega, ali bi potencijalno mogla
#bude preplata računa

table(df$SEX)
#vrednosti su korektne, na osnovu odnosa primećujemo da ima više žena nego muškaraca

table(df$EDUCATION)
#Vrednost 0 je nedefinisana, vrednosti 5 i 6 smatraju se nepoznatim vrednostima, tako da bi se to ili moglo spojiti u jednu grupu nepoznatih,
#Ili se potencijalno grupisati zajedno sa 0

table(df$MARRIAGE)

str(df)


df %>% 
  filter(EDUCATION == 0 & MARRIAGE == 0) %>% 
  select(ID, EDUCATION, MARRIAGE)

df

#Primećujemo da i za marriage i za education postoji po 54 vrednosti 0, koja ne pripada standardnim vrednostima, ali se na osnovu
#filtera može primetiti da se nigde ne poklapaju (nijedan red nema obe vrednosti 0)


#za tabelu marriage možemo uočiti da ima vrednost 0, koja je nedefinisana, iako samo 54 vrednosti imaju tu vrednost

#Podešavanje vrednosti 0 i 6 za EDUCTAION smestićemo u vrednosti 5, pošto su i vrednosti 5 i 6 unknown

df$EDUCATION[df$EDUCATION %in% c(0, 6)] <- 5

summary(df$AGE)

#Kao što možemo zaključiti na osnovu reda sa godinama svi su u godinama kada je brak validan, stoga su postojeće kategorije u redu.
#odnosno, ne treba kreirati dodatne kategorije kao što su unmarried i slično

df$MARRIAGE[df$MARRIAGE == 0] <- 3

summary(df)

#======================================================================================================================

#PROVERAVANJE NEODREĐENE -2 VREDNOSTI U DEMOGRAFSKIM FEATURIMA, GODINE, POL, EDUKACIJA, BRAK


#broj klijenata koji imaju vrednost -2 za sva plaćanja
sum(apply(df[, c("PAY_0","PAY_2","PAY_3","PAY_4","PAY_5","PAY_6")], 1, function(x) all(x == -2)))

#broj klijenata koji imaju vrednost -2 za bar neko plaćanje
sum(apply(df[, c("PAY_0","PAY_2","PAY_3","PAY_4","PAY_5","PAY_6")], 1, function(x) any(x == -2)))


#radimo proveru da li se na neki način klijenti koji za svoje vrednosti podmirenja imaju -2 vrednost izdvajaju
#u odnosu na sve ostale klijente

#prvo proveravamo njihov prosečni kreditni limit i godine

df %>%
  mutate(has_minus2 = apply(select(., PAY_0, PAY_2, PAY_3, PAY_4, PAY_5, PAY_6), 1, function(x) any(x == -2))) %>%
  group_by(has_minus2) %>%
  summarise(
    avg_limit = mean(LIMIT_BAL),
    avg_age = mean(AGE),
  )

#na osnovu tabele koju dobijamo primećujemo da imaju viši kreditni limit za oko 25 procenata u odnosu na one koji to nemaju,
#pa bi se moglo reći da im banka daje veće poverenje, odnosno da su pouzdaniji po pitanju podmirenja svojih dugova


#ovde proveravamo da li je edukacija ikako povezana sa vrednostima -2
df %>%
  mutate(has_minus2 = apply(select(., PAY_0, PAY_2, PAY_3, PAY_4, PAY_5, PAY_6), 1, function(x) any(x == -2))) %>%
  group_by(has_minus2, EDUCATION) %>%
  summarise(n = n()) %>%
  mutate(share = n / sum(n))

#jedino što na osnovu ovoga možemo zaključiti jeste da korisnici koji pripadaju ovoj grupi imaju donekle više nivoe
#obrazovanja, što u generalnom slučaju može takođe ukazati da potencijalno imaju stabilnije prihode i bolje razumevanje
#svojih obaveza

#zatim proveravamo da li pol ima ikakve veze

df %>%
  mutate(has_minus2 = apply(select(., PAY_0, PAY_2, PAY_3, PAY_4, PAY_5, PAY_6), 1, function(x) any(x == -2))) %>%
  group_by(has_minus2, SEX) %>%
  summarise(n = n()) %>%
  mutate(share = n / sum(n))
#jedino što zaključujemo je da je malo veći procenat žena u oba slučaja, što se poklapa sa generalnom slikom dataseta

#za kraj proveravamo da li činjenica da su u braku ima ikakve veze sa time

df %>%
  mutate(has_minus2 = apply(select(., PAY_0, PAY_2, PAY_3, PAY_4, PAY_5, PAY_6), 1, function(x) any(x == -2))) %>%
  group_by(has_minus2, MARRIAGE) %>%
  summarise(n = n()) %>%
  mutate(share = n / sum(n))

#kao što je očekivano, bračni status nije značajan faktor, ali govori da su oni u bračnoj zajednici u maloj meri imaju
#češće mesece bez duga


#NA OSNOVU SVEGA ŠTO JE UOČENO IZ PREGLEDA OVIH DEMOGRAFSKIH FEATUREA, JASNO JE DA SU KORISNICI KOD KOJIH POSTOJI ZA NEKI
#MESEČNI RAČUN SA OZNAKOM -2, EVIDENTNO POUZDANIJI KORISNICI, ODNOSNO DA NEMAJU ČESTE NEIZMIRENE RAČUNE

#kod -2 za plaćanja potencijalno može biti neki interni kod unutar banke tj. institucije na koju se odnosi, a mi ćemo,
#s obzirom na zaključke do kojih smo došli analizom, te vrednosti staviti kao -1, odnosno da je račun uredno plaćen bez kašnjenja


#dodaćemo i flag kolonu has_minus2 kako bismo zadržali info o tome koji korisnici su imali te vrednosti

df$had_minus2 <- apply(df[, c("PAY_0","PAY_2","PAY_3","PAY_4","PAY_5","PAY_6")],
                       1, function(x) any(x == -2))

#zamenjujemo vrednosti -2 sa -1

pay_cols <- c("PAY_0","PAY_2","PAY_3","PAY_4","PAY_5","PAY_6")

df[, pay_cols] <- lapply(df[, pay_cols], function(x) ifelse(x == -2, -1, x))

#proveravamo da li su se neke vrednosti -2 zadržale

sapply(df[, pay_cols], function(x) any(x == -2))

#pošto nisu uspešno smo zamenili sve
str(df)

#====================================================================================================================

# Histogram godina (AGE)
ggplot(df, aes(x = AGE)) +
  geom_histogram(binwidth = 5, fill = "#0073C2FF", color = "white") +
  labs(
    title = "Raspodela starosti klijenata",
    x = "Godine starosti",
    y = "Broj klijenata"
  )
# Većina klijenata je između 25 i 40 godina, što je i očekivano.
# Starijih ima vrlo malo, pa deluje da banka više radi sa mlađim korisnicima.
# Distribucija je asimetrična nadesno (više mlađih klijenata).

# Boxplot kreditnih limita (LIMIT_BAL)
ggplot(df, aes(y = LIMIT_BAL)) +
  geom_boxplot(fill = "#00AFBB") +
  labs(
    title = "Raspodela kreditnih limita",
    y = "Kreditni limit (u dolarima)"
  )
# Kod raspodele kreditnih limita vidi se da većina klijenata ima limit negde do oko 250.000 dolara.
# Iznad kutije ima dosta tačkica koje predstavljaju klijente sa većim limitima, oni su očigledni outlieri.
# Graf pokazuje da je raspodela asimetrična i da manji limiti preovlađuju.


# Raspodela pola klijenata
ggplot(df, aes(x = factor(SEX, labels = c("Muškarci", "Žene")))) +
  geom_bar(fill = "orange") +
  labs(
    title = "Raspodela pola klijenata",
    x = "Pol",
    y = "Broj klijenata"
  )
# Na grafu se vidi da u uzorku ima više žena nego muškaraca.
# Razlika nije ogromna, ali je primetna – žene su nešto zastupljenije kao korisnici kreditnih kartica.
# Moguće je da banka češće odobrava kartice ženama ili da su one aktivnije u korišćenju.

# Raspodela nivoa obrazovanja klijenata
ggplot(df, aes(x = factor(EDUCATION))) +
  geom_bar(fill = "lightblue") +
  labs(
    title = "Raspodela nivoa obrazovanja klijenata",
    x = "Nivo obrazovanja",
    y = "Broj klijenata"
  )
# Najviše klijenata ima univerzitetsko obrazovanje (oznaka 2), dok nešto manji broj ima postdiplomsko (1).
# Srednja škola (3) je dosta ređa, a vrednosti 4 i 5 se pojavljuju minimalno.
# Generalno, vidi se da većina korisnika ima viši stepen obrazovanja.

# Raspodela bračnog statusa klijenata
ggplot(df, aes(x = factor(MARRIAGE, labels = c("Married", "Single", "Others")))) +
  geom_bar(fill = "#009E73") +
  labs(
    title = "Raspodela bračnog statusa klijenata",
    x = "Bračni status",
    y = "Broj klijenata"
  )
# Na grafu se vidi da je najviše klijenata koji su single, dok je broj onih u braku nešto manji.
# Kategorija 'others' se pojavljuje vrlo retko, što uključuje razvedene i udovce.
# Ukupno gledano, većina klijenata nije u braku.

#====================================================================================================================
names(df)

# Preimenovanje ciljne kolone i pretvaranje u faktor
df <- df %>%
  rename(DEFAULT = `default.payment.next.month`) %>%
  mutate(DEFAULT = factor(DEFAULT, levels = c(0, 1), labels = c("No", "Yes")))
table(df$DEFAULT)
prop.table(table(df$DEFAULT)) * 100
# Većina klijenata (oko 78%) uredno izmiruje obaveze, dok oko 22% kasni sa plaćanjem.
# To znači da je dataset neuravnotežen, jer ima više onih bez duga nego sa dugom.
# Hajde sad to isto da prikažemo i na grafu da se bolje vidi razlika.

# Bar graf - raspodela DEFAULT vrednosti
ggplot(df, aes(x = DEFAULT, fill = DEFAULT)) +
  geom_bar() +
  scale_fill_manual(values = c("#00AFBB", "#E69F00")) +
  labs(
    title = "Raspodela klijenata po statusu plaćanja",
    x = "Status plaćanja",
    y = "Broj klijenata"
  )
# Na grafu se jasno vidi razlika stub "No" je znatno viši od stuba "Yes".
# To samo potvrđuje ono što smo videli u procentima – većina klijenata uredno plaća,
# dok manji deo kasni sa plaćanjem.

#====================================================================================================================
install.packages("ggcorrplot")

library(ggcorrplot)

# Izdvajanje samo numeričkih kolona (bez ID i faktorskih promenljivih)
numeric_df <- df %>%
  select_if(is.numeric) %>%
  select(-ID)  # ID nije koristan za korelaciju
summary(numeric_df)
# Računanje korelacione matrice
corr_matrix <- cor(numeric_df, use = "complete.obs")
head(corr_matrix[, 1:6])

# Korelaciona matrica (vizuelni prikaz)
ggcorrplot(corr_matrix,
           hc.order = TRUE,            
           type = "lower",             
           lab = FALSE,                
           colors = c("blue", "white", "red")) +
  labs(title = "Korelaciona matrica numeričkih promenljivih")

# Na grafu se vidi da su crvena polja najjača u delu BILL_AMT1 do BILL_AMT6,
# što znači da su mesečni iznosi računa međusobno veoma povezani.
# PAY_AMT kolone (uplate) su takođe crvenkaste i pokazuju jaku pozitivnu vezu.
# LIMIT_BAL ima umerenu crvenu nijansu prema BILL_AMT kolonama, što znači
# da klijenti sa većim limitom obično imaju i veće račune.
# Plava boja se vide između nekih PAY kolona i demografskih podataka (kao što su AGE i EDUCATION),
# što ukazuje na slabu ili negativnu vezu.
# Dakle najjače korelacije su kod mesečnih iznosa uplata i računa,
# dok su godine, pol i bračni status uglavnom nezavisni od ostalih promenljivih.

#=================================================================================================================
# Kreiranje prosečnih vrednosti za račune i uplate
df <- df %>%
  mutate(
    avg_bill_amt = rowMeans(select(., BILL_AMT1:BILL_AMT6), na.rm = TRUE),
    avg_pay_amt  = rowMeans(select(., PAY_AMT1:PAY_AMT6), na.rm = TRUE)
  )

# Pregled novih kolona
summary(df[, c("avg_bill_amt", "avg_pay_amt")])

# Odnos prosečne uplate i računa
df <- df %>%
  mutate(ratio_pay_bill = ifelse(avg_bill_amt > 0, avg_pay_amt / avg_bill_amt, 0))

# Odnos prosečnog računa i limita
df <- df %>%
  mutate(ratio_bill_limit = ifelse(LIMIT_BAL > 0, avg_bill_amt / LIMIT_BAL, 0))

# Pregled novih kolona
summary(df[, c("ratio_pay_bill", "ratio_bill_limit")])
summary(df[, c("LIMIT_BAL", "AGE", "avg_bill_amt", "avg_pay_amt", "ratio_pay_bill", 
          "ratio_bill_limit")])
# Iz ovoga zaključujemo da većina klijenata plaća samo manji deo računa,
# dok koristi oko 30% svog kreditnog limita.
# Ima i nekoliko ekstremnih vrednosti gde je uplata bila mnogo veća od računa,
# verovatno kad je neko odjednom isplatio sve dugove.
#=================================================================================================================
numeric_cols <- df %>%
  select(LIMIT_BAL, AGE, avg_bill_amt, avg_pay_amt, ratio_pay_bill, ratio_bill_limit)

scaled_data <- as.data.frame(scale(numeric_cols))
summary(scaled_data)
# Ovde smo skalirali numeričke kolone da sve budu u sličnom opsegu.
# Sad su vrednosti centrirane oko nule i nemaju ogromne razlike kao pre.
# Sve kolone normalizovane
#=================================================================================================================
install.packages("caret")

library(caret)
set.seed(123)
train_index <- createDataPartition(df$DEFAULT, p = 0.7, list = FALSE)

train_data <- df[train_index, ]
test_data  <- df[-train_index, ]

dim(train_data)
dim(test_data)
# Dataset je podeljen na trening (70%) i test (30%) deo.
# Ovako možemo da učimo model na većem delu podataka,
# pa da kasnije proverimo koliko dobro prepoznaje klijente koji kasne sa plaćanjem.

#=================================================================================================================
set.seed(123)

yes_data <- train_data[train_data$DEFAULT == "Yes", ]
no_data  <- train_data[train_data$DEFAULT == "No", ]
ggplot(df, aes(x = DEFAULT, fill = DEFAULT)) +
  geom_bar() +
  labs(title = "Raspodela klasa pre balansiranja", x = "DEFAULT", y = "Broj klijenata")
yes_oversampled <- yes_data[sample(1:nrow(yes_data), nrow(no_data), replace = TRUE), ]

# Kombinuj u novi balansirani skup
train_balanced <- rbind(no_data, yes_oversampled)

# Proveri raspodelu
table(train_balanced$DEFAULT)
prop.table(table(train_balanced$DEFAULT)) * 100
ggplot(train_balanced, aes(x = DEFAULT, fill = DEFAULT)) +
  geom_bar() +
  labs(title = "Raspodela klasa posle balansiranja", x = "DEFAULT", y = "Broj klijenata")
# Posle oversamplinga klase su izjednačene, sada imamo 50% “No” i 50% “Yes”.
# Dataset je balansiran i spreman za treniranje modela.
# Sad model ima jednake primere iz obe grupe i može realnije da uči razliku između onih koji plaćaju i onih koji kasne.

#=======================================================================================================================
# Prvo testiramo najjednostavniji model da vidimo da li uopšte radi

model_test1 <- glm(DEFAULT ~ LIMIT_BAL + AGE + avg_bill_amt,
                   data = train_balanced,
                   family = binomial)
summary(model_test1)

# Model radi, ali neke promenljive nisu značajne,
# pa pokušavamo sa još nekoliko koje su povezane sa plaćanjem.

# dodajem prosečne uplate i odnos uplate/računa


model_test2 <- glm(DEFAULT ~ LIMIT_BAL + AGE + avg_bill_amt + avg_pay_amt + ratio_pay_bill,
                   data = train_balanced,
                   family = binomial)
summary(model_test2)

# Rezultat je bolji, ali želimo da uključimo i odnos prosečnog računa i limita,
# jer to ima smisla prema korelacionoj matrici.

model_log <- glm(DEFAULT ~ LIMIT_BAL + AGE + avg_bill_amt + avg_pay_amt +
                   ratio_pay_bill + ratio_bill_limit,
                 data = train_balanced,
                 family = binomial)
summary(model_log)

# Probali smo više verzija modela da vidim koja kombinacija promenljivih daje najbolje rezultate.
# U prvom modelu LIMIT_BAL, AGE i avg_bill_amt ispadaju značajni,
# što pokazuje da klijenti sa većim limitom ređe kasne,
# dok stariji i oni sa većim računima imaju veći rizik kašnjenja.

# U drugom modelu smo dodali i prosečne uplate (avg_pay_amt) i odnos uplata/računa.
# Tu smo videli da su uplate (avg_pay_amt) vrlo značajne, ali da odnos (ratio_pay_bill) nije imao neki uticaj.

# Konačni model (model_log) daje najbolje rezultate.
# Promenljive ratio_pay_bill i intercept nisu značajne, pa se mogu izostaviti u sledećem koraku.

# Zakljucak: model lepo hvata veze između limita, uplata i iznosa računa, 
# što potvrđuje da finansijski faktori najviše uticu na rizik od kašnjenja.

#=======================================================================================================================

# Pravimo predikcije na test skupu
pred_probs <- predict(model_log, newdata = test_data, type = "response")
pred_classes <- ifelse(pred_probs > 0.5, "Yes", "No")
pred_classes <- factor(pred_classes, levels = c("No", "Yes"))

conf_matrix <- table(Predicted = pred_classes, Actual = test_data$DEFAULT)
conf_matrix

# Izračunavanje osnovnih metrika
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy

confusionMatrix(pred_classes, test_data$DEFAULT)

# ROC kriva 
library(pROC)
roc_curve <- roc(as.numeric(test_data$DEFAULT), as.numeric(pred_probs))
plot(roc_curve, col = "red", main = "ROC kriva modela")
auc(roc_curve)

# Model smo testirali na podacima koje ranije nije video.
# Tačnost iznosi oko 56%, što i nije savršeno, ali pokazuje da model ipak prepoznaje deo klijenata koji kasne sa plaćanjem.
# Vidi se da bolje prepoznaje one koji redovno plaćaju nego one koji kasne.
# ROC kriva ima AUC vrednost oko 0.63, što znači da model ima umerenu sposobnost razlikovanja između rizičnih i sigurnih klijenata.
# Ukratko, model radi solidno, ali bi se tačnost mogla poboljšati nekim naprednijim pristupima (npr. drugi algoritmi ili dodatne promenljive).


#=======================================================================================================================

# Uzimamo apsolutne vrednosti koeficijenata (bez intercepta)
importance <- abs(coef(model_log)[-1])

# Pretvaramo u tabelu i sortiramo po jačini uticaja
importance_df <- data.frame(
  Variable = names(importance),
  Importance = importance
) %>%
  arrange(desc(Importance))
importance_df

ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "#00AFBB") +
  coord_flip() +
  labs(
    title = "Važnost atributa u modelu",
    x = "Promenljiva",
    y = "Veličina uticaja (|koeficijent|)"
  )
# Kao što se vidi na grafu, najvažniji faktor u modelu je "ratio_bill_limit" – odnos prosečnog iznosa računa i odobrenog limita.
# Ova promenljiva najviše utiče na verovatnoću da klijent zakasni sa plaćanjem.
# Klijenti koji u proseku koriste veći deo svog limita češće kasne u plaćanju.
# Ostali faktori, poput starosti (AGE) i prosečnih uplata, imaju znatno manji uticaj.
# To pokazuje da model najviše zavisi od finansijskog ponašanja klijenata, a ne od demografskih osobina.

#=======================================================================================================================
thresholds <- c(0.3, 0.4, 0.5)

for (t in thresholds) {
  pred_classes <- ifelse(pred_probs > t, "Yes", "No")
  pred_classes <- factor(pred_classes, levels = c("No", "Yes"))
  cat("\nRezultati za prag =", t, "\n")
  print(confusionMatrix(pred_classes, test_data$DEFAULT)$overall["Accuracy"])
}

model_inter <- glm(DEFAULT ~ LIMIT_BAL * avg_pay_amt + AGE + avg_bill_amt + ratio_bill_limit,
                   data = train_balanced,
                   family = binomial)

summary(model_inter)
pred_probs_inter <- predict(model_inter, newdata = test_data, type = "response")
pred_classes_inter <- ifelse(pred_probs_inter > 0.5, "Yes", "No")
confusionMatrix(factor(pred_classes_inter, levels = c("No", "Yes")), test_data$DEFAULT)


roc_base <- roc(as.numeric(test_data$DEFAULT), as.numeric(pred_probs))
roc_inter <- roc(as.numeric(test_data$DEFAULT), as.numeric(pred_probs_inter))

plot(roc_base, col = "red", main = "Poređenje ROC kriva")
lines(roc_inter, col = "blue")
legend("bottomright", legend = c("Osnovni model", "Model sa interakcijom"),
       col = c("red", "blue"), lwd = 2)
auc(roc_base)
auc(roc_inter)

# U ovoj fazi testirali smo nekoliko mogućnosti za poboljšanje modela.
# Prvo smo promenili prag klasifikacije (0.3, 0.4, 0.5), ali je tačnost bila najviša
# kod podrazumevanog praga od 0.5, dok su niži pragovi smanjili preciznost

# Zatim smo napravili novi model sa interakcijom između limita (LIMIT_BAL)
# i prosečnih uplata (avg_pay_amt).Hteli smo da proverimo da li kombinacija
# ova dva faktora ima dodatni efekat na rizik kašnjenja
#
# Rezultati su pokazali da novi model ima nešto veću tačnost (57% a bilo je 56%)
# i veću AUC vrednost (0.6435 a bilo je 0.6381)
# Na ROC grafu (plava linija) vidi se da model sa interakcijom ima malu
# prednost u većini tačaka.
#
# Iako poboljšanje nije veliko, pokazuje da dodavanje interakcija između
# finansijskih promenljivih može pomoći modelu da bolje prepozna obrasce
# Zaključak je da logistička regresija dobro opisuje odnose u podacima,
# a i poboljšanja se mogu postići podešavanjem varijabli.

#=======================================================================================================================
# U ovom projektu analizirali smo podatke o korisnicima kreditnih kartica i pokušali da predvidimo
# da li će klijent kasniti sa plaćanjem (DEFAULT). 
# Tokom rada urađena je kompletna obrada podataka – od čišćenja i prilagođavanja vrednosti,
# preko kreiranja novih pokazatelja, do izgradnje i testiranja modela.

# Logistička regresija se pokazala kao dobar izbor jer jasno pokazuje koji faktori najviše utiču na rizik kašnjenja.
# Najvažnija promenljiva bila je odnos prosečnog iznosa računa i kreditnog limita (ratio_bill_limit).
# Klijenti koji koriste veći deo svog limita imaju veću verovatnoću da zakasne sa plaćanjem.

# Model je imao tačnost od oko 57% i AUC oko 0.64, što znači da umereno dobro razlikuje
# sigurne i rizične klijente. Testiranjem različitih pragova i dodavanjem interakcija
# dobili smo mala, ali realna poboljšanja. 

# Možemo zaključiti da finansijski faktori imaju mnogo veći uticaj od demografskih (kao što su pol ili bračni status).
# U budućim verzijama, model bi mogao da se unapredi primenom naprednijih algoritama 
# (Random Forest, XGBoost) ili dodatnim podacima o ponašanju korisnika (npr. istorija plaćanja po mesecima).

# Ukupno gledano, model je uspešno pokazao osnovne principe prediktivne analize
# i jasno povezao finansijsko ponašanje klijenata sa rizikom od neplaćanja.
