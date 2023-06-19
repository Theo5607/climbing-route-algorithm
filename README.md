Projet TIPE Théo et Matthias

Simplifications:
-on se base sur un mur donné, dont on connaît déjà toutes les prises, les distances entre chacune d'elles, etc.<br>
-pour évaluer la difficulté d'un mouvement, on se limite dans l'approximation physique à la distance, à l'angle entre les deux prises et à leurs difficultés respectives (évaluées arbitrairement)
-on ne considère pas les mouvements des pieds<br>
-on considère dans un premier temps que le grimpeur n'a qu'une seule main, peut-être plus tard nous considérerons les deux mains<br><br>

Plan du projet:<br>
D'abord nous relions dans un graphe toutes les prises du mur entre elles afin de savoir lesquelles sont atteignables à partir d'une prise donnée. À partir de là, nous définissons une fonction H d'heuristique qui nous permet d'évaluer la difficulté d'un mouvement, puis que nous raffinerons au fur et à mesure. Cette fonction va nous permettre de pondérer chacune des arêtes de notre graphe et donc de déterminer le plus court chemin (d'un départ à une arrivée donnée) à l'aide de l'algorithme de Dijkstra.<br>
L'objectif secondaire est de créer prodéduralement des blocs et, à l'aide d'un algorithme génétique, de les améliorer pour les rendre plus réalistes.