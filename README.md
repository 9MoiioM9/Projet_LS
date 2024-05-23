# Projet Logiciels Sûrs

## 1 Logique des propositions

### Question 1 

* Il pleut et j'ai mon parapluie alors je ne suis pas mouillé.
* Il ne pleut pas alors je ne suis pas mouillé.
* Je n'ai pas mon parapluie et il pleut alors je suis mouillé.
* Aujourd'hui il pleut ou il ne pleut pas alors j'ai pris mon parapluie.
* J'ai mon parapluie alors je ne suis pas mouillé et je ne suis pas fatigué.

## 1.1 Syntaxe de la logique des propositions
## 1.1.1 Syntaxe concrète 

### Question 1 

Expression logique dans l'ordre correspondant : 
P = J'ai mon parapluie ; Q = il pleut ; R = je suis fatigué ; M = je suis mouillé.

* Q /\ P ==> ~M
* ~Q ==> ~M
* ~P /\ Q ==> M 
* Q \/ ~Q ==> P
* P ==> ~M /\ ~R

## 1.1.2 Syntaxe abstraite

### Question 2 

Voir le code sur le fichier Projet_LS.ml

### Question 3

Voir le code sur le fichier Projet_LS.ml

## 2 Vérification de la validité d'une formule propositionnelle 
## 2.1 Un interpréteur pour les formules propositionnelles

### Question 1 

Voir le code sur le fichier Projet_LS.ml

### Question 2 

Voir le code sur le fichier Projet_LS.ml

### Question 3 

### Question 4 

## 2.2 Sémantique via les tables de vérité

### Question 1 

* Q /\ P ==> ~M

    |   Q   |   P   |   M   |   Q /\ P  |    ~M   |   Q /\ P ==> ~M   |
    |-------|-------|-------|-----------|---------|-------------------|
    |   T   |   T   |   T   |     T     |    F    |         F         |
    |   T   |   T   |   F   |     T     |    T    |         T         |
    |   T   |   F   |   T   |     F     |    F    |         F         |
    |   T   |   F   |   F   |     F     |    T    |         T         |
    |   F   |   T   |   T   |     F     |    F    |         F         |
    |   F   |   T   |   F   |     F     |    T    |         T         |
    |   F   |   F   |   T   |     F     |    F    |         F         |
    |   F   |   F   |   F   |     F     |    T    |         T         |

* ~Q ==> ~M

    |   Q   |   M   |   ~Q   |    ~M    |   ~Q ==> ~M   |
    |-------|-------|--------|----------|---------------|
    |   T   |   T   |    F   |    F     |       F       |
    |   T   |   F   |    F   |    T     |       T       |
    |   F   |   T   |    T   |    F     |       F       |
    |   F   |   F   |    T   |    T     |       T       |

### Question 2 

### Question 3 

### Question 4 

### Question 5 

## 2.3 Tautologies 

### Question 1 
Une tautologie est une formule qui est toujours vraie, quellle que soit la valeur de vérité de ses variables.

* P ==> ~(~P)
* P \/ ~P
* Q ==> (P \/ ~P)

* P ==> ~(~P)

    |   P   |   ~P   |   P ==> ~(~P)   |
    |-------|--------|-----------------|
    |   T   |    F   |        T        |
    |   F   |    T   |        T        |


* P \/ ~P

    |   P   |   ~P   |   P \/ ~P   |
    |-------|--------|-------------|
    |   T   |    F   |      T      |
    |   F   |    T   |      T      |


* Q ==> (P \/ ~P)

    |   P   |   Q   |   ~P   |   P \/ ~P   |    Q ==> (P \/ ~P)    |
    |-------|-------|--------|-------------|-----------------------|
    |   T   |   T   |    F   |      T      |           T           |
    |   T   |   F   |    F   |      T      |           T           |
    |   F   |   T   |    T   |      T      |           T           |
    |   F   |   F   |    T   |      T      |           T           |

### Question 2 

### Question 3

## 3 Preuves de formules propositionnelles 
