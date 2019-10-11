## Références 

* Implémentation d'un langage de Programmation Logique d'Ordre Supérieur avec MALI
* Pascal Brisset

## Notes

Compilateur qui utilise la machine virtuelle MALI (depth-first search)

## Unification

Utilise l'unification d'ordre supérieur [Huet75] pour des λ-termes. Seulement semi-décidable 

λ-termes simplement typés classiques : variable, abstraction, application, avec types atomiques et ɑ -> β. Tous les termes sont clos 

Relations d'équivalences sur les termes : ɑ-conversion, β-reduction, η-expansion 

Pas d'unificateur minimal unique dans l'ordre supérieur. Algo d'unification par parcours d'arbre (de paires de termes):

### Algo (exécuté sur des termes en forme normale de tête η-expansée)

SIMPL: (term * term) list -> ((term * term) list * result)
* si entrée vide, OK
* TRIVIAL : unifier si la première paire est (var, expr)
* Si il n'y a pas de paire rigide-rigide, on retourne n et succès
* Si on a une paire rigide-rigide, on vérifie que les têtes sont les mêmes (ce sont forcément des atomes)
  Si c'est bien le cas, on doit maintenant unifier les membres des corps
  
MATCH: (term(flexible) * term(rigide)) -> substitution list
* On commence avec l'ensemble de substitutions vide
* Si la tete du terme rigide est une constante, on peut substituer à l'inconnue une abstraction ayant pour tête cette constante et pour corps des abstractions utilisant tous les arguments (autant d'arguments que la lg du corps)
* On peut également substituer une projection, en faisant attention au type **donc les termes doivent être annotés par leurs types**

Arbre d'unification pour e1, e2 :
* Racine : SIMPL(<e1, e2>)
* On prends une paire flexible-rigide dans un des noeuds, on MATCH. Si pas de résultat, le noeud est un échec
  Sinon, on applique chaque perm trouvée sur la paire, et on simplifie pour obtenir de nouveaux fils
* On travaille jusqu'a n'avoir que des succès, des échecs ou des paires flex-flex. Faire attention aux boucles dans l'arbre

## Utilisation de l'algo dans MALI

- typage (+ annotation des termes)
- η-expansion
- β-reduction (les expansions et réductions se font "par nécessite", et effectuées pendant le parcours des termes)
- production de code C : tetes de close transformées en structures, corps en procédures (via macros)

Depth-first search
Pour le backtracking : utilisation d'une pile de recherche. Avant chaque liaison d'une variable, on sauvegarde l'état et le choix fait.

Dans le cas de l'ordre supérieur, on obtient un ensemble de paires flexibles-rigides apres SIMPL. Comme MATCH est défini en Z dans MALI, on fait des appels à MATCH pour ces paires (et ensuite on fait le corps de la clause). Faire une vérification (unification) des types avant SIMPL

On peut aussi utiliser une procédure d'unification du premier ordre avant SIMPL, pour se débarrasser efficacement des termes du premier ordre.

### Quantification universelle
On choisit une constante pour la variable quantifiée, on unifie puis on vérifie que les variables libres du but n'ont pas été liées à un terme contenant la constante

## Plus sur MALI

Machine abstraite adaptée à la programmation logique avec GC.

### Représentation des données

* Designation : Nom -> Terme (environnement dynamique)
* Nom : Nature * Sorte
  avec Nature : Atome | Construit | Nuplet | Variable | Variable a Attribut | Niveau
  et Sorte le type (seulement pour atomes, construits, variables)

* NUplet utilisés pour représenter les abstractions et des applications.
* Représentation explicite de tous les types (nécessaire pour MATCH)

Utilisation des variables à attribut pour représenter les affectations : la variable est remplacée par son attribut
