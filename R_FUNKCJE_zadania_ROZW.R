###### ---- Zadania FUNKCJE I WYRAŻENIA WARUNKOWE ----

# 1. Stwórz funkcję o nazwie kostka, która będzie symulować n rzutów kostką.
# Wskazówka: Użyj funkcji sample() do losowania liczby oczek od 1 do 6.

kostka = function(n){
  sample(1:6, size = n, replace = TRUE)
}
#Na przykład:
kostka(4)
kostka(36)

# 2. Stwórz funkcję, która będzie tworzyć wektor o zadanej długości.
# Funkcja ma zwracać wektor liczb całkowitych od 1 do n:
#  długość wektora wynosi n, a wartości w wektorze to sekwencja liczb od 1 do n.

stworz_wektor = function(n){
  wektor = 1:n
  return(wektor)
}
#Na przykład:
stworz_wektor(7)
stworz_wektor(34)

# 3. Stwórz funkcję o nazwie pole_kola, która oblicza pole powierzchni koła dla danego promienia.

pole_kola = function(r){
  pole = 3.14 * r^2
  return(pole)
}
#Na przykład:
pole_kola(1)
pole_kola(2)

# 4. Stwórz funkcję, która oblicza długość przeciwprostokątnej w trójkącie prostokątnym.
dlugosc_przeciwprostokątnej = function(a, b){
  c = sqrt(a^2 + b^2)
  return(c)
}
#Na przykład:
dlugosc_przeciwprostokątnej(3, 4)
dlugosc_przeciwprostokątnej(10, 24)

# 5. Stwórz funkcję będącą najprostszą wersją kalkulatora 
# (dodawanie, odejmowanie, mnożenie albo dzielenie dwóch liczb).

kalkulator = function(znak, a, b){
  if (znak == "+"){
    wynik = a+b
  } else if (znak == "-"){
    wynik = a-b
  } else if (znak == "*"){
    wynik = a*b
  } else if (znak == "/" && b!=0){
    wynik = a/b
  } else if (znak == "/" && b==0){
    wynik = "Nie dzieli się przez zero!"
  }
  return(wynik)
}
#Na przykład:
kalkulator("+", 8, 2)
kalkulator("-", 8, 2)
kalkulator("*", 8, 2)
kalkulator("/", 8, 0)
kalkulator("/", 8, 2) 

# 6. Stwórz funkcję o nazwie przyznaj_nagrode()
# która symuluje rzut sześcienną kostką do gry i przyznaje nagrodę w zależności od wyniku rzutu. 
# Funkcja powinna działać według następujących zasad:
# - Jeśli wyrzucona liczba oczek to 6, funkcja powinna zwrócić komunikat: "Super bonus!"
# - Jeśli wyrzucona liczba oczek to 4 lub 5, funkcja powinna zwrócić komunikat: "Nagroda standardowa"
# - Jeśli wyrzucona liczba oczek to 1, 2 lub 3, funkcja powinna zwrócić komunikat: "Brak nagrody..."

przyznaj_nagrode = function(){
  wynik=sample(1:6, 1)
  if (wynik == 6){
    print("Super bonus!")
  } else if (wynik == 5 || wynik == 4){
    print("Nagroda standardowa")
  } else {
    print("Brak nagrody...")
  }
}
#Na przykład
przyznaj_nagrode()

# 7. Stwórz funkcję obliczającą podatek w zależności od dochodu. 
# Przyjmij następujące założenia:
# a) Jeżeli podatnik rozlicza się liniowo, wtedy niezależnie od kwoty płaci 19% podatku.
# b) Jeżeli podatnik rozlicza się na zasadach ogólnych, wtedy:
# - poniżej kwoty 85528zł płaci 18% podatku minus kwota zmniejszająca, czyli 556zł;
# - powyżej kwoty 85528zł płaci 14839zł + 32% nadwyżki powyżej 85528zł.

podatek = function(typ, dochod){
  if (typ == "liniowy"){
    podatek = dochod * 0.19
  } else if (typ == "ogolny"){
    if (dochod <= 85528){
      podatek = dochod * 0.18 - 556
    } else if (dochod > 85528){
      podatek = 14839 + 0.32 * (dochod - 85528)
    }
  }
  return(podatek)
}
#Na przykład:
podatek("liniowy", 90000)
podatek("ogolny", 90000)
podatek("ogolny", 70000)
