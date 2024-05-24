# Projet Logiciels Sûrs
_Auteur_
__Lespagnol Killian et Tribouillois Julien__

_Pour la suite des exercices on concidère T pour True donc Vrai, F pour False donc Faux_

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

Voir le code sur le fichier Projet_LS.ml

### Question 4 

Voir le code sur le fichier Projet_LS.ml

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

Voir le code sur le fichier Projet_LS.ml

### Question 3 

Voir le code sur le fichier Projet_LS.ml

### Question 4 
Pour savoir combien de ligne on doit calculer on prend le nombre d'argument de la liste en entrée en puissance de 2.
Donc avec les trois arguments P, Q et R on aura 2^3 donc 8 lignes.

### Question 5 

Voir le code sur le fichier Projet_LS.ml

## 2.3 Tautologies 

### Question 1 
Une tautologie est une formule qui est toujours vraie, quellle que soit la valeur de vérité de ses variables.

* P ==> ~(~P)
* P \\/ ~P
* Q ==> (P \\/ ~P)

* P ==> ~(~P)

    |   P   |   ~P   |   P ==> ~(~P)   |
    |-------|--------|-----------------|
    |   T   |    F   |        T        |
    |   F   |    T   |        T        |


* P \\/ ~P

    |   P   |   ~P   |   P \\/ ~P  |
    |-------|--------|-------------|
    |   T   |    F   |      T      |
    |   F   |    T   |      T      |


* Q ==> (P \\/ ~P)

    |   P   |   Q   |   ~P   |   P \\/ ~P  |    Q ==> (P \\/ ~P)   |
    |-------|-------|--------|-------------|-----------------------|
    |   T   |   T   |    F   |      T      |           T           |
    |   T   |   F   |    F   |      T      |           T           |
    |   F   |   T   |    T   |      T      |           T           |
    |   F   |   F   |    T   |      T      |           T           |

### Question 2 

Fonction _tautology_ : 

    let tautology formule =
    let rec tautology_aux vars =
        match vars with
        | [] -> [[]]
        | v :: res ->
            let rest = tautology_aux res in
            List.concat [List.map (fun r -> (v, true) :: r) rest; List.map (fun r -> (v, false) :: r) rest]
    in
    let variables = List.sort_uniq compare (vars formule) in
    let valuations = tautology_aux variables in
    List.for_all (fun v -> eval formule v) valuations
    ;;


### Question 3
Concernant la fonction __tautology__ le principal inconvénient est la complexité exponentielle.
Avec un grand nombre de variables on obtient un très grand nombre de combinaisons, ce qui peu rendre la fonction inefficace pour des formules trop grande.
De plus on execute un double parcours de la liste des variables avec la fonction __List.sort_uniq__ mais aussi une double évaluation avec chaque appel de la fonction __eval__ à chaque valuations possibles.

## 3 Preuves de formules propositionnelles 

Preuve pour la proposition (P Q R : Prop) : (P \/ Q -> R) -> (P -> R) /\ (Q -> R) de l'énoncé : 

Lemma propProjet (P Q R : Prop) : (P \/ Q -> R) -> (P -> R) /\ (Q -> R).
Proof.
    intros.
    And_Intro.
    Impl_Intro.
    assume (P \/ Q).
    Impl_Elim in H and H1.
    exact H2.
    Or_Intro_1.
    exact H0.
    Impl_Intro.
    assume (P \/ Q).
    Impl_Elim in H and H1.
    exact H2.
    Or_Intro_2.
    exact H0.
Qed.


### Question 3.3 4 
Voici la preuve faite sur coq, nous nous sommes basé sur celle-ci afin de faire les étapes.
Nous avons bien sûr dû changer les étapes selon les résultats obtenu avec notre fonction __apply_tactic__

Lemma p (P Q R : Prop) : (P \/ Q -> R) -> (P -> R) /\ (Q -> R).
Proof.
    Impl_Intro.
    And_Intro.
    Impl_Intro.
    assume (P \/ Q).
    Impl_Elim in H and H1.
    exact H2.
    Or_Intro_1.
    exact H0.
    Impl_Intro.
    assume (P \/ Q).
    Impl_Elim in H and H1.
    exact H2.
    Or_Intro_2.
    exact H0.
Qed.




