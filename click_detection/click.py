import tkinter as tk

# Fonction pour mettre à jour les coordonnées de la souris
def update_mouse_coordinates(event):
    x, y = event.x, event.y
    mouse_coordinates_label.config(text=f"Coordonnées de la souris : ({x}, {y})")

# Fonction pour gérer les clics de souris
def handle_mouse_click(event):
    difficulty = current_difficulty.get()
    x, y = event.x, event.y
    positions.append((x, y, difficulty))
    color = get_color_for_difficulty(difficulty)
    canvas.create_oval(x - 5, y - 5, x + 5, y + 5, fill=color)  # Dessine un cercle à la couleur associée à la difficulté
    position_listbox.insert(tk.END, f"({x}, {400 - y}) - Difficulté {difficulty}")

# Fonction pour supprimer la position sélectionnée
def delete_selected_position():
    selected_index = position_listbox.curselection()
    if selected_index:
        index = int(selected_index[0])
        position_listbox.delete(index)
        del positions[index]
        redraw_canvas()


#Fonction pour effacer toutes les positions et les dessins
def clear_all_positions():
    global positions
    global aretes
    positions = []  
    aretes = []
    redraw_canvas()  # Efface tous les dessins sur la zone de dessin
    position_listbox.delete(0, tk.END)  # Efface tous les éléments de la liste


# Fonction pour redessiner tous les points sur la zone de dessin
def redraw_canvas():
    canvas.delete("all")  # Efface tous les dessins sur la zone de dessin
    for x, y, difficulty in positions:
        color = get_color_for_difficulty(difficulty)
        canvas.create_oval(x - 5, y - 5, x + 5, y + 5, fill=color)
    for p1, p2 in aretes:
        canvas.create_line(positions[p1][0], positions[p1][1], positions[p2][0], positions[p2][1], fill = "green", width = 5)



# Fonction pour obtenir la couleur en fonction de la difficulté
def get_color_for_difficulty(difficulty):
    if difficulty == 0 :
        return "black"
    elif difficulty == 1:
        return "purple"
    elif difficulty == 2:
        return "pink"
    elif difficulty == 3:
        return "green"
    elif difficulty == 4:
        return "blue"
    else:
        return "black"

# Fonction appelée lors du clic sur le bouton "Terminer"
def finish_button_click():
    # exporte la liste des positions / diff
    open('liste_prises.txt', 'w').close()
    file = open("liste_prises.txt", "a")
    file.write(str(len(positions)) + "\n")
    for (px, py, diff) in positions:     
        file.write(str(px/100) + " " + str((400-py)/100) + " " + str(diff) + " 0\n")

    file.close()

def affiche_graphe():
    print("import")
    global aretes 
    aretes = []
    file = open("liste_aretes.txt", 'r')
    for line in file:
        m, p1, p2 = line.split(" ")
        aretes.append((int(p1), int(p2)))
    file.close()
    redraw_canvas()

# Créer une fenêtre principale
root = tk.Tk()
root.title("Détection de clics de souris")

# Créer une zone de dessin
canvas = tk.Canvas(root, width=400, height=400, bg="white", relief="raised", bd=2)     #mur de 400x400cm
canvas.pack()

# Associer la fonction de gestion des clics à la zone de dessin
canvas.bind("<Button-1>", handle_mouse_click)

# Associer la fonction pour mettre à jour les coordonnées de la souris
canvas.bind("<Motion>", update_mouse_coordinates)

# Listes pour stocker les positions des clics et les aretes
positions = []

aretes = []

# Créer une liste déroulante pour choisir la difficulté
difficulty_label = tk.Label(root, text="Choisissez la difficulté :")
difficulty_label.pack()

difficulty_options = [0, 1, 2, 3, 4, 5] #0 correspond à la prise de départ et 5 a la prise d'arrivée
current_difficulty = tk.IntVar()
current_difficulty.set(difficulty_options[0])

difficulty_menu = tk.OptionMenu(root, current_difficulty, *difficulty_options)
difficulty_menu.pack()

# Créer une liste déroulante pour afficher les positions
position_listbox = tk.Listbox(root)
position_listbox.pack(side = "right")

# Créer un conteneur Frame pour les boutons sur le côté
button_frame = tk.Frame(root)
button_frame.pack(side="left", padx=10)  # Utilisez "left" pour placer le conteneur sur le côté gauche




# Bouton Exporter pour créer le fichier contenant les coordonnées des prises
finish_button = tk.Button(button_frame, text="Exporter", command=finish_button_click)
finish_button.pack()

#Bouton importer pour recuperer et afficher le meilleur chemin
affiche_button = tk.Button(button_frame, text="Importer", command=affiche_graphe)
affiche_button.pack()

# Bouton Supprimer pour supprimer la position sélectionnée
delete_button = tk.Button(button_frame, text="Supprimer", command=delete_selected_position)
delete_button.pack()

# Bouton Effacer tout pour effacer toutes les positions
clear_button = tk.Button(button_frame, text="Effacer tout", command=clear_all_positions)
clear_button.pack()




# Étiquette pour afficher les coordonnées de la souris
mouse_coordinates_label = tk.Label(root, text="")
mouse_coordinates_label.pack()

# Lancer la boucle principale
root.mainloop()
