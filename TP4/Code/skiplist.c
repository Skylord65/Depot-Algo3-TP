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
	SkipList* list = malloc(sizeof(SkipList)+sizeof(Node)+(sizeof(Link)*nblevels));
	list->size = 0;
	list->sentinel = (Node*)(list+1);
	list->sentinel->level = (Link*)(list->sentinel+1);
	list->sentinel->nb_level = nblevels;
	list->rng = rng_initialize(0, nblevels);
	for (int i = 0; i<nblevels; i++) {
		list->sentinel->level[i].next = list->sentinel;
		list->sentinel->level[i].prev = list->sentinel;
	}
	return list;
}

void skiplist_delete(SkipList** d) {
	SkipList* l = *d;
	Node* p = l->sentinel->level[0].next; 
	while (p != l->sentinel) {
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
	
	Node* p = d->sentinel->level[0].next;
	while (p!=d->sentinel) {
		if (p->value==value) {
			return d;
		}
		p=p->level[0].next;
	}
	
	int nblevels = rng_get_value(&d->rng)+1;
	Node* n = malloc(sizeof(Node) + sizeof(Link)*nblevels);
	n->level = (Link*)(n+1);
	n->nb_level = nblevels;
	n->value = value;

	p = d->sentinel;
	int i_level = d->sentinel->nb_level-1;
	
	for(int i = 0; i<nblevels; i++) {
		n->level[i].next = d->sentinel;
		n->level[i].prev = d->sentinel;
	}

	while (i_level>=0) {
		while (p->level[i_level].next!=d->sentinel && p->level[i_level].next->value<value) { 
			p = p->level[i_level].next;
		}

		if(i_level<nblevels) {
			n->level[i_level].prev = p;
			n->level[i_level].next = p->level[i_level].next;
		}
		i_level--;
	}

	for(int i = 0; i<nblevels; i++) {
		n->level[i].next->level[i].prev = n;
		n->level[i].prev->level[i].next = n;
	}
	d->size++;
	return d;
}

bool skiplist_search(const SkipList* d, int value, unsigned int *nb_operations) {
	int i_level = d->sentinel->nb_level-1;
	unsigned int operations = 0;
	Node* p = d->sentinel;
	operations++;
	while (i_level>=0) {
		while (p->level[i_level].next!=d->sentinel && p->level[i_level].next->value<value) { 
			p = p->level[i_level].next;
			operations++;
		}

		if(p->level[i_level].next!=d->sentinel && p->level[i_level].next->value==value) {
			*nb_operations = operations;
			return true;
		}

		i_level--;
	}
	*nb_operations = operations;
	return false;
}