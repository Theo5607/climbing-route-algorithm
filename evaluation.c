#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdbool.h>
#define MAXV 100 /* nombre maximum de sommets */

struct prise {
    int x;
    int y;
    int diff;
    int i;
};
typedef struct prise prise;

struct edgenode {
    int y; // le voisin
    struct edgenode *next; // la suite de la liste
};
typedef struct edgenode edgenode;

struct graph {
    edgenode *edges[MAXV]; // tableau de listes d'adjacence
    int degree[MAXV]; // le degré de chaque sommet
    int nvertices;
    int nedges;
    bool directed; // indique si le graphe est orienté
};
typedef struct graph graph;

//fonctions de base sur les graphes

void initialize_graph(graph *g, bool directed)
{
    for(int i = 0; i < MAXV; i++)
    {
        g->degree[i] = 0;
        g->edges[i] = NULL;
    }
    g->nvertices = 0;
    g->nedges = 0;
    g->directed = directed;
}

void insert_edge(graph *g, int x, int y, bool directed)
{
    edgenode *edge = malloc(sizeof(edgenode));
    edge->y = y;
    edge->next = g->edges[x];
    g->edges[x] = edge;
    g->degree[x] = 1 + g->degree[x];
    if(!directed) {
        insert_edge(g, y, x, true);
    } else {
        g->nedges = 1 + g->nedges;
    }
}

//fonctions annexes

int carre(int x)
{
    return x*x;
}

float distance(prise p1, prise p2)
{
    return sqrt(carre(p2.x - p1.x) + carre(p2.y - p1.y));
}

graph* to_graph(prise* t, int n)
{
    graph *g;
    initialize_graph(g, true);
    for (int i = 0; i < n; i++)
    {
        for (int j = 0; i < n; i++)
        {
            prise p1 = t[i];
            prise p2 = t[j];
            if (distance(p1, p2) < 170 && p2.y > p1.y)
            {
                insert_edge(g, i, j, true);
            }
        }
    }
}

int main(){
    
}