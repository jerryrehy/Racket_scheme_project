;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname rehak_uloha_28_taxi_verze_3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
; --------------------------------------------------------------------------------------------------------------------------------------------------
;| Úloha č.28                                                                                                                                       |
; --------------------------------------------------------------------------------------------------------------------------------------------------
;| Vytvořte funkci pro optimalizaci vozového parku taxislužby. Vstupem funkce je seznam zákaznických požadavků – každý požadavek je reprezentován   |
;| dvojicí (od, do), která udává časy, od kdy do kdy bude zákazník potřebovat vůz. Pro zjednodušení budeme časy v požadavcích zadávat celými čísly. |
;| Požadavky v seznamu nejsou žádným způsobem uspořádány. Funkce má vydat nejmenší počet vozů, se kterými bude možné obsloužit všechny požadavky    |
;| (tedy de facto největší počet paralelních požadavků).                                                                                            |
; --------------------------------------------------------------------------------------------------------------------------------------------------
;
;definování funkce počet aut (pocet_aut), vstup je seznam seznamů o minimální délce 1 neprázdný seznam. (viz volání funkce úplně dole)
;funkce počet aut volá funkci sečti auta (secti_auta), kde je vstupem 7 proměnných. (popsáno u definice funkce)
(define (pocet_aut Seznam) (secti_auta (cons (car Seznam) '()) (Vrat_Prvek4 Seznam) (cons (car Seznam) '()) '() (Vrat_Prvek5 Seznam) 0 1))
;
;7 vstupních proměnných:
;
;A = seznam seznamů
;B = seznam
;Prvky A a B se mezi sebou porovnávají
;Nekryjici = seznam seznamů, které se nekryjí a mažou se po dokončení cyklu
;Kryjici = seznam seznamů, obsahuje prvky, které se kryjí a budou znova použity v další iteraci
;Zbyle = seznam seznamů, které se postupně ubírají
;Taxiku = porměnná s počtem aut (Taxíků)
;Konec = proměnná kde se zaznamenává, má li nastat ukončení (0 = ukončit, 1 = pokračovat)
;
;funkce sečti auta obsahuje podmínku pro ukončení cyklu a volání funkce porovnej
(define (secti_auta A B Nekryjici Kryjici Zbyle Taxiku Konec)
  (cond
    ;Je-li předána 0 v proměnné Konec, vrátí výsledek v promenné Taxíků - počet aut
    [(= Konec 0) Taxiku]
    ;V případě nesplnění předchozí podmínky zavolá funkci porovnej
    [else (porovnej A B Nekryjici Kryjici Zbyle Taxiku)]
))
;
;funkce porovnávání s šesti vstupy, popsanými výše
(define (porovnej A B Nekryjici Kryjici Zbyle Taxiku)
  (cond
    ;podmínka je-li seznam Kryjici prázdný a zároveň je prázdný seznam B, vyvolá funkci secti_auta,
    ;odečítá se jedno auto a posílá se 0 pro ukončení programu
    [(and (empty? Kryjici) (empty? B)) (secti_auta (cons A '()) (Vrat_Prvek Kryjici) (cons A '()) '() (Vrat_Prvek Kryjici) (+ Taxiku 1) 0)]    
    ;podmínka je-li prázdný seznam B a zároveň prázdný seznam Zbyle, vyvolá se funkce secti_auta,
    ;přičítá se jedno auto a posílá se 1 pro pokračování programu
    [(and (empty? B) (empty? Zbyle)) (secti_auta (cons (car Kryjici) '()) (Vrat_Prvek4 Kryjici) (cons (car Kryjici) '()) '() (Vrat_Prvek5 Kryjici) (+ Taxiku 1) 1)]
    ;podmínka je-li prázdný seznam A, vyvolá se funkce porovnej,
    ;navíc se přidá seznam z B do seznamu Nekryjici
    [(empty? A) (porovnej (cons B Nekryjici) (Vrat_Prvek2 Zbyle) (cons B Nekryjici) Kryjici (Vrat_Prvek3 Zbyle) Taxiku)]
    ;podmínka časového křížení intervalů, je-li true, odebere se seznam z A a pokračuje se dále opětovným voláním funkce porovnej
    [(or 
        (and (>= (car(car A)) (car (cdr B))) (>= (car(cdr (car A))) (car(cdr B))))    
        (and (<= (car(car A)) (car B)) (<= (car (cdr (car A))) (car B))) 
     ) 
     (porovnej (cdr A) B Nekryjici Kryjici Zbyle Taxiku)]
    ;Jsou-li předchozí podmínky false použije se funkce porovnej, která předává hodnoty pomocí funkcí Vrat_Prvek.
    [else (porovnej Nekryjici (Vrat_Prvek2 Zbyle) Nekryjici (cons B Kryjici) (Vrat_Prvek3 Zbyle) Taxiku)]    
))
;následující funkce vracejí seznamy nebo prázdné seznamy dle vstupu
;funkce která při prázdém vstupním seznamu vrátí prázdný seznam
;pokud obsahuje jeden vstupní seznam, tak ho vrátí a pokud obsahuje více než jeden seznam, tak je vrátí všechny
(define (Vrat_Prvek A)
   (cond
     [(empty? A) '()]
     [(empty? (cdr A)) (car A)]
     [else (car (cdr A))]
))
;tato funkce při prázdném vstupním seznamu vrátí prázdný seznam
;jinak vrací první prvek vstupního seznamu
(define (Vrat_Prvek2 B)
   (cond
     [(empty? B) '()]
     [else (car B)]
))
;tato funkce při prázdném vstupním seznamu vrátí prázdný seznam
;jinak vrací ocas vstupního seznamu
(define (Vrat_Prvek3 C)
   (cond
     [(empty? C) '()]
     [else (cdr C)]
))
;tato funkce při prázdném vstupním seznamu vrátí prázdný seznam
;pokud vstupní seznam neobsahuje dva seznamy, vrátí prázdný seznam
;jinak vrací druhý seznam
(define (Vrat_Prvek4 D)
   (cond
     [(empty? D) '()]
     [(empty? (cdr D)) '()]
     [else (car (cdr D))]
))
;tato funkce při prázdném vstupním seznamu vrátí prázdný seznam
;pokud vstupní seznam obsahuje prázdný ocas, vrací prázdný seznam
;jinak vrací ocas ocasu vstupního seznamu
(define (Vrat_Prvek5 E)
   (cond
     [(empty? E) '()]
     [(empty? (cdr E)) '()]
     [else (cdr (cdr E))]
))
;volání hlavní funkce počet aut
(pocet_aut (list '(1 7) '(7 8) '(1 5) '(5 6) '(1 7) '(7 8) '(1 5) '(5 6)))
(pocet_aut (list '(8 10)))