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

struct s_Node
{
	int value;
	int nb_level;
	Link* level;
};

struct s_SkipList
{
	Node* sentinel;
	unsigned int size;
	RNG rng;
};

SkipList* skiplist_create(int nblevels) {
	SkipList* list = malloc(sizeof(SkipList)+sizeof(Node) + sizeof(Link)*nblevels);
	list->size = 0;
	list->sentinel = (Node*)list+1;
	list->sentinel->level = (Link*)list->sentinel+1;
	list->sentinel->nb_level = nblevels;
	list->rng = rng_initialize(0, nblevels);
	//list->sentinel->level = malloc();
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
		free(p);
		p = temp;
		l->size--;
	}
	free(l);
}

unsigned int skiplist_size(const SkipList* d){
	return d->size;
}

int skiplist_at(const SkipList* d, unsigned int i) {
	Node* p = d->sentinel->level[0].next;
	unsigned int j=0; 
	while (p != d->sentinel) {
		if(j==i) {
			return p->value;
		}
		j++;
		p = p->level[0].next;
	}
	return p->value;
}

void skiplist_map(const SkipList* d, ScanOperator f, void *user_data) {
	Node* p = d->sentinel->level[0].next;
	while (p!=d->sentinel)
	{
		f(p->value, user_data);
		p=p->level[0].next;
	}
}

SkipList* skiplist_insert(SkipList* d, int value) {
	int nblevels = rng_get_value(&d->rng+1);
	Node* n = malloc(sizeof(Node) + sizeof(Link)*nblevels);
	n->level = (Link*)n+1;
	n->nb_level = nblevels;
	n->value = value;
	Node* p = d->sentinel->level[d->sentinel->nb_level-1].next;
	int i_level = d->sentinel->nb_level-1;
	for(int i = 0; i<nblevels; i++) {
		n->level[i].next->level[i].prev = d->sentinel;
		n->level[i].prev->level[i].next = d->sentinel;
	}
	while ((p!=d->sentinel && i_level!=0) || (p!=d->sentinel && p->value<=value && i_level!=0)) { // TODO: need to be tested with only one `i_level==0` 
		if(p==d->sentinel || (p->value>value && i_level!=0)) {
			n->level[i_level].next = p;
			n->level[i_level].prev = p->level[i_level].prev;
			i_level--;
		} else if(p->value==value) {
			free(n);
			return d;
		}

		p = p->level[i_level].next;
	}

	for(int i = 0; i<nblevels; i++) {
		n->level[i].next->level[i].prev = n->level[i].next;
		n->level[i].prev->level[i].next = n->level[i].prev;
	}

	return d;
}
