import matplotlib.pyplot as plt
from collections import Counter
import numpy as np

# Fonction pour lire le fichier et extraire les points
def lire_fichier(file_path, precision=0):
    points = []
    with open(file_path, 'r') as file:
        for line in file:
            parts = line.split()
            x = float(parts[0]) * 0.46  # Multiplier la difficulté estimée par 0.46
            x = round(x, precision)  # Puis arrondir la difficulté estimée
            y = float(parts[1])  # Difficulté réelle
            # Ignorer les points dont l'abscisse (difficulté estimée) est 0
            if x != 0:
                points.append((x, y))
    return points

# Chemin du fichier texte
file_path = 'data.txt'

# Lire les points du fichier avec arrondi après multiplication
points = lire_fichier(file_path, precision=0)

# Compter les occurrences de chaque point
point_counts = Counter(points)

# Extraire les valeurs de x, y et les fréquences
x_all = np.array([point[0] for point in points])
y_all = np.array([point[1] for point in points])
x_unique = np.array([point[0] for point in point_counts.keys()])
y_unique = np.array([point[1] for point in point_counts.keys()])
sizes = [point_counts[point] * 50 for point in point_counts.keys()]  # Ajuster la taille des marqueurs

# Effectuer la régression linéaire sur l'ensemble des points
slope, intercept = np.polyfit(x_all, y_all, 1)
x_regression = np.linspace(min(x_all), max(x_all), 100)
y_regression = slope * x_regression + intercept

# Tracer le graphique sans relier les points et avec des tailles de marqueurs variables
plt.figure(figsize=(10, 6))
plt.scatter(x_unique, y_unique, s=sizes, color='b', label='Difficulté réelle')
plt.plot(x_regression, y_regression, color='r', label='Régression linéaire')
plt.xlabel('Difficulté estimée (après transformation)')
plt.ylabel('Difficulté réelle')
plt.title('Graphique de la difficulté réelle en fonction de la difficulté estimée')
plt.legend()
plt.grid(True)

# Afficher le graphique
plt.show()
