/* A simple doubly-linked list, in the style of the Linux kernel and PipeWire */

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct list {
	struct list *prev;
	struct list *next;
};

struct node {
	struct list list;
	char *str;
};

#define NODE_PREV(n) ((struct node *)(n->list.prev))
#define NODE_NEXT(n) ((struct node *)(n->list.next))

struct node *node_new(const char *str)
{
	struct node *node = calloc(1, sizeof(struct node));

	node->str = strdup(str);

	return node;
}

void node_delete(struct node *node)
{
	free(node->str);
	free(node);
}

void list_insert(struct node *head, struct node *this)
{
	this->list.prev = &head->list;
	this->list.next = head->list.next;

	if (head->list.next)
		head->list.next->prev = &this->list;
	head->list.next = &this->list;
}

/* This is the only string-specific code, we could generalise this by sending in
 * a comparison function */
bool list_find(struct node *head, const char *str)
{
	while (head) {
		if (strcmp(head->str, str) == 0)
			break;

		head = NODE_NEXT(head);
	}

	return head;
}

struct node *list_delete(struct node *head)
{
	struct list *prev = head->list.prev;
	struct list *next = head->list.next;

	if (prev)
		prev->next = next;
	if (next)
		next->prev = prev;

	return (struct node *) (prev ? prev : next);
}

void list_dump(struct node *head)
{
	if (!head)
		printf("(empty)\n");

	while (head) {
		printf("%s", head->str);

		head = NODE_NEXT(head);

		if (head)
			printf(" -> ");
		else
			printf("\n");
	}
}

int main(void)
{
	struct node *head = node_new("1"), *del;

	list_dump(head);

	list_insert(head, node_new("2"));

	list_dump(head);

	list_insert(head, node_new("3"));

	list_dump(head);

	del = head;
	head = list_delete(head);
	node_delete(del);

	list_dump(head);

	del = NODE_NEXT(head);
	head = list_delete(del);
	node_delete(del);

	list_dump(head);

	del = head;
	head = list_delete(del);
	node_delete(del);

	list_dump(head);

	return 0;
}
