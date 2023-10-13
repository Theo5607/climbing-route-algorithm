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

# Fonction pour supprimer un point en cliquant dessus
def delete_point_on_click(event):
    x, y = event.x, event.y
    for i, (px, py, _) in enumerate(positions):
        if abs(px - x) <= 5 and abs(py - y) <= 5:
            del positions[i]
            redraw_canvas()
            break

# Fonction pour redessiner tous les points sur la zone de dessin
def redraw_canvas():
    canvas.delete("all")  # Efface tous les dessins sur la zone de dessin
    for x, y, difficulty in positions:
        color = get_color_for_difficulty(difficulty)
        canvas.create_oval(x - 5, y - 5, x + 5, y + 5, fill=color)

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
    print("Terminer la saisie des positions")
    open('liste_prises.txt', 'w').close()
    f = open("liste_prises.txt", "a")
    for (px, py, diff) in positions:     
        f.write(str(px/100) + " " + str((400-py)/100) + " " + str(diff) + "\n")

    f.close()
    root.destroy()


# Créer une fenêtre principale
root = tk.Tk()
root.title("Détection de clics de souris")

# Créer une zone de dessin
canvas = tk.Canvas(root, width=400, height=400, bg="white", relief="raised", bd=2)     #mur de 400x400cm
canvas.pack()

# Associer la fonction de gestion des clics à la zone de dessin
canvas.bind("<Button-1>", handle_mouse_click)

# Associer la fonction pour supprimer un point en cliquant dessus
canvas.bind("<Button-3>", delete_point_on_click)  # Utilise le bouton droit de la souris

# Associer la fonction pour mettre à jour les coordonnées de la souris
canvas.bind("<Motion>", update_mouse_coordinates)

# Liste pour stocker les positions des clics
positions = []

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
position_listbox.pack()

# Bouton Supprimer pour supprimer la position sélectionnée
delete_button = tk.Button(root, text="Supprimer", command=delete_selected_position)
delete_button.pack()

# Bouton Terminer pour indiquer la fin de la saisie des positions
finish_button = tk.Button(root, text="Terminer", command=finish_button_click)
finish_button.pack()

# Étiquette pour afficher les coordonnées de la souris
mouse_coordinates_label = tk.Label(root, text="")
mouse_coordinates_label.pack()

# Lancer la boucle principale
root.mainloop()
