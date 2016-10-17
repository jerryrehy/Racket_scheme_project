;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname rehak_uloha_28_taxi) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
; --------------------------------------------------------------------------------------------------------------------------------------------------
;| Úloha č.28                                                                                                                                       |
; --------------------------------------------------------------------------------------------------------------------------------------------------
;| Vytvořte funkci pro optimalizaci vozového parku taxislužby. Vstupem funkce je seznam zákaznických požadavků – každý požadavek je reprezentován   |
;| dvojicí (od, do), která udává časy, od kdy do kdy bude zákazník potřebovat vůz. Pro zjednodušení budeme časy v požadavcích zadávat celými čísly. |
;| Požadavky v seznamu nejsou žádným způsobem uspořádány. Funkce má vydat nejmenší počet vozů, se kterými bude možné obsloužit všechny požadavky    |
;| (tedy de facto největší počet paralelních požadavků).                                                                                            |
; --------------------------------------------------------------------------------------------------------------------------------------------------
;
;definování funkce počet aut (poc_aut), vstup je seznam seznamů o minimální délce 3 seznamy. (viz volání úplně dole)
;funkce počet aut volá funkci sečti auta (secti_auta), kde je vstupem 7 proměnných. (popsáno u definice funkce)
(define (poc_aut Seznam) (secti_auta (cons (car Seznam) '()) (car (cdr Seznam)) (cons (car Seznam) '()) '() (cdr (cdr Seznam)) 0 1))

;7 vstupních proměnných: 
;A = seznam seznamů
;B = seznam
;Good = seznam seznamů, které se nekryjí
;Bad = seznam seznamů, které se kryjí a budou znova použity
;Rest = seznam seznamů, které se postupně ubírají
;Num = porměnná s počtem aut
;End = proměnná kde se zaznamenává, má li nastat ukončení
(define (secti_auta A B Good Bad Rest Num End)
  (cond
    ;Je-li předán prázdný seznam Rest, vrátí výsledek Num - počet aut
    ;[(empty? Rest) Num]
    ;Je-li předána 0 v proměnné End, vrátí výsledek Num - počet aut
    [(= End 0) Num]
    ;V případě nesplnění předchozích podmínek vyvolá funkci porovnej
    [else (porovnej A B Good Bad Rest Num)]
))

;Funkce porovnávání s šesti vstupy, popsanými výše
(define (porovnej A B Good Bad Rest Num)
  (cond
    ;podmínka je li prázdný seznam Bad a zároveň prázdný seznam B vyvolá funkci secti_auta, odečítá se jedno auto a posílá se 0 pro ukončení programu
    [(and (empty? Bad) (empty? B)) (secti_auta (cons A '()) (tryit Bad) (cons A '()) '() (tryit Bad) (+ Num 1) 0)]    
    ;podmínka je li prázdný seznam B a zároveň prázdný seznam Rest vyvolá funkci secti_auta, odečítá se jedno auto a posílá se 1 pro pokračování programu
    [(and (empty? B) (empty? Rest)) (secti_auta (cons (car Bad) '()) (tryit4 Bad) (cons (car Bad) '()) '() (tryit5 Bad) (+ Num 1) 1)]
    ;podmínka je li prázdný seznam A vyvolá funkci porovnej, navíc se přidá seznam ze seznamu B do seznamu Good
    [(empty? A) (porovnej (cons B Good) (car Rest) (cons B Good) Bad (cdr Rest) Num)]
    ;podmínka křížení intervalů, jeli true, odebere se seznam z A a pokračuje se dále
    [(or 
        (and (>= (car(car A)) (car (cdr B))) (>= (car(cdr (car A))) (car(cdr B))))    
        (and (<= (car(car A)) (car B)) (<= (car (cdr (car A))) (car B))) 
     ) 
     (porovnej (cdr A) B Good Bad Rest Num)]
    ;je-li podmínka false použije se funkce porovnej s dalšími funkcemi vracejícími různé výsledky pro rozdílné vstupy
    [else (porovnej Good (tryit2 Rest) Good (cons B Bad) (tryit3 Rest) Num)]
))
;následující funkce vracejí seznamy nebo prázdné seznamy dle vstupu 
(define (tryit A)
   (cond
     [(empty? A) '()]
     [(empty? (cdr A)) (car A)]
     [else (car (cdr A))]
))
(define (tryit2 B)
   (cond
     [(empty? B) '()]
     [else (car B)]
))
(define (tryit3 C)
   (cond
     [(empty? C) '()]
     [else (cdr C)]
))
(define (tryit4 D)
   (cond
     [(empty? D) '()]
     [(empty? (cdr D)) '()]
     [else (car (cdr D))]
))
(define (tryit5 E)
   (cond
     [(empty? E) '()]
     [(empty? (cdr E)) '()]
     [else (cdr (cdr E))]
))
;volání hlavní funkce počet aut
(poc_aut (list '(4 7) '(1 8) '(4 7) '(1 2) '(5 6) '(1 2) '(1 2)))
(poc_aut (list '(1 2) '(5 6) '(1 2) '(1 2)))