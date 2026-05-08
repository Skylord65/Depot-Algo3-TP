#include <stdlib.h>
#include <assert.h>

#include "skiplist.h"
#include "rng.h"

typedef struct s_Node Node;

typedef struct s_Link
{
	Node* next;
	Node* prev;
} Link;

typedef struct s_Node
{
	int value;
	int nb_level;
	Link* level;
} Node;

struct s_SkipList
{
	Node* sentinel;
	unsigned int size;
	RNG rng;
};

SkipList* skiplist_create(int nblevels) {
	SkipList* list = malloc(sizeof(SkipList)+sizeof(Node));
	list->size = 0;
	list->sentinel->nb_level = nblevels;
	list->rng = rng_initialize(0, nblevels);
	list->sentinel->level = malloc(sizeof(Link)*nblevels);
	for (int i = 0; i<nblevels; i++) {
		list->sentinel->level[i].next = list->sentinel;
		list->sentinel->level[i].prev = list->sentinel;
	}
	return list;
}

void skiplist_delete(SkipList** d) {
	SkipList* l = *d;
	Node* p = l->sentinel; 
	while (p->level[0].next != l->sentinel) {
		Node* temp = p->level[0].next;
		free(p->level);
		free(p);
		p = temp;
		l->size--;
	}
	free(l->sentinel->level);
	free(l);
}

unsigned int skiplist_size(const SkipList* d){
	return d->size;
}

int skiplist_at(const SkipList* d, unsigned int i) {
	Node* p = d->sentinel->level[0].next;
	int j=0; 
	while (p != d->sentinel) {
		if(j==i) {
			return p->value;
		}
		i++;
		p = p->level[0].next;
	}
	return p->value;
}

SkipList* skiplist_insert(SkipList* d, int value) {
	(void)value;
	return d;
}
