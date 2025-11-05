# Credit Card Default Prediction – UCI Credit Card Dataset

Ovaj projekat koristi **UCI Credit Card Dataset** za predviđanje verovatnoće da će klijent kasniti sa otplatom kredita (`default.payment.next.month`).  
Projekat je fokusiran na analizu finansijskih i demografskih faktora i na upoređivanje različitih modela mašinskog učenja kako bi se identifikovao onaj koji najtačnije predviđa rizik od kašnjenja.

---

## Dataset

**Izvor:** UCI Machine Learning Repository – Credit Card Default Dataset  
**Opis:**  
Skup podataka sadrži informacije o 30.000 klijenata kreditnih kartica u Tajvanu.  
Obuhvata demografske atribute (pol, godine, obrazovanje, bračni status) i finansijske karakteristike (limit kredita, istorija plaćanja, iznosi računa i uplata).

### Ključne promenljive

- **LIMIT_BAL** – Kreditni limit  
- **SEX, EDUCATION, MARRIAGE, AGE** – Demografski podaci  
- **PAY_0 – PAY_6** – Status plaćanja po mesecima  
- **BILL_AMT1 – BILL_AMT6** – Iznosi mesečnih računa  
- **PAY_AMT1 – PAY_AMT6** – Iznosi uplata  
- **DEFAULT** – Ciljna promenljiva (da li je klijent kasnio sa plaćanjem)

---

## Ciljevi projekta

- Analizirati koje karakteristike klijenata najviše utiču na rizik kašnjenja.  
- Izgraditi i uporediti više modela klasifikacije u **R-u**.  
- Odabrati model sa najboljom prediktivnom tačnošću.  
- Vizuelizovati rezultate i objasniti značaj atributa.

---

## Analitički koraci

### Korelaciona analiza
Korišćen je **ggcorrplot** za prikaz međusobnih odnosa numeričkih promenljivih.

### Feature Engineering
Kreirani su izvedeni atributi koji poboljšavaju performanse modela:
- `avg_bill_amt` – prosečan iznos mesečnih računa  
- `avg_pay_amt` – prosečna uplata  
- `ratio_pay_bill` – odnos uplata i računa  
- `ratio_bill_limit` – odnos računa i limita

### Balansiranje klasa
Primijenjen je **oversampling** manjinske klase (`Yes`) da bi model imao bolje performanse.

### Podela podataka
Podaci su podeljeni na **trening (70%)** i **test (30%)** skup.

---

## Modeli

### Logistička regresija
Osnovni model binarne klasifikacije koji objašnjava uticaj finansijskih i demografskih faktora.  
Tačnost: **56.4%**

### Decision Tree
Model zasnovan na pravilima i podelama po atributima.  
Tačnost: **58.6%**

### Random Forest
Ansambl model koji kombinuje više stabala odluke i daje najbolje rezultate.  
Tačnost: **73.3%**

---

## Zaključak modela

Najvažniji atributi su **finansijski pokazatelji**, posebno odnos prosečnog računa i limita (`ratio_bill_limit`), dok demografske karakteristike imaju manji uticaj.

---

## Vizualizacija

Kroz grafike su prikazane:
- Raspodele starosti, limita i pola klijenata  
- Odnosi obrazovanja i braka sa rizikom kašnjenja  
- Korelacije između računa, uplata i limita  
- ROC krive i uporedni prikaz tačnosti modela

---

## Zaključak

- Klijenti koji koriste veći procenat svog kreditnog limita imaju veću verovatnoću kašnjenja.  
- Finansijski indikatori imaju jaču prediktivnu moć od demografskih.  
- **Random Forest** daje najbolju tačnost i AUC vrednost, pa je izabran kao finalni model.  
- Model može pomoći bankama da unaprede procenu kreditnog rizika i donose sigurnije odluke.

---

## Tehnologije

**Jezik:** R  
**Biblioteke:** tidyverse, dplyr, ggplot2, ggcorrplot, caret, rpart, randomForest, pROC