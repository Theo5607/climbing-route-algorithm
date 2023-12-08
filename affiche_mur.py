import tkinter as tk

positions = []

# Fonction pour redessiner tous les points sur la zone de dessin
def redraw_canvas():
    canvas.delete("all")  # Efface tous les dessins sur la zone de dessin
    i = 0
    for x, y, difficulty, angle in positions:
        color = get_color_for_difficulty(int(difficulty))
        print(difficulty, color)
        canvas.create_oval(x - 8, y - 8, x + 8, y + 8, fill=color)
        canvas.create_text(x,y,text= str(i))
        i += 1

def update_mouse_coordinates(event):
    x, y = event.x, event.y
    mouse_coordinates_label.config(text=f"Coordonnées de la souris : ({x}, {400-y})")


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


def affiche_graphe():
    file = open("mur.txt", 'r')
    n = int(file.readline())
    for i in range(0, n):
        line = file.readline()
        print(line)
        x, y, diff, angle = line.split(" ") 
        positions.append((100*float(x)+50, 400-100*float(y), diff, angle))
    file.close()
    redraw_canvas()

# Créer une fenêtre principale
root = tk.Tk()



# Créer une zone de dessin
canvas = tk.Canvas(root, width=400, height=400, bg="white", relief="raised", bd=2)     #mur de 400x400cm
canvas.pack()

canvas.bind("<Motion>", update_mouse_coordinates)


affiche_graphe()


# Étiquette pour afficher les coordonnées de la souris
mouse_coordinates_label = tk.Label(root, text="")
mouse_coordinates_label.pack()

# Lancer la boucle principale
root.mainloop()
