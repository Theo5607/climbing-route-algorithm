import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import numpy as np

# Fonction pour lire le fichier et extraire les points
def lire_fichier(file_path):
    x = []
    y = []
    z = []
    with open(file_path, 'r') as file:
        for line in file:
            parts = line.split()
            x.append(float(parts[0]))
            y.append(float(parts[1]))
            z.append(float(parts[2]))
    return np.array(x), np.array(y), np.array(z)

# Chemin du fichier texte
file_path = 'data3d.txt'

# Lire les points du fichier
x, y, z = lire_fichier(file_path)

# Tracer le graphique
fig = plt.figure(figsize=(12, 8))
ax = fig.add_subplot(111, projection='3d')
ax.scatter(x, y, z, c=z, cmap='viridis', marker='o')

# Ajouter des labels et un titre
ax.set_xlabel('X')
ax.set_ylabel('Y')
ax.set_zlabel('f(X, Y)')
ax.set_title('Nuage de points de la fonction f(X, Y)')

# Afficher le graphique
plt.show()
