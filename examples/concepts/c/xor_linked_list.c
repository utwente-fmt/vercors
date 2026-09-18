#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

struct Node {
    /*@ unique<1> */int data;
    /*@ unique<2> */uintptr_t link;
};

struct List {
    struct Node *head;
    struct Node *tail;
    /*@ unique<3> */size_t length;
};

/*@ adt Trigger {
    pure _Bool injection_a(int i);
    axiom (\forall int i; {:injection_a(i):});

    pure _Bool valid_link_trigger(struct Node *i, struct Node *j, struct Node *k);
    axiom (\forall struct Node *i, struct Node *j, struct Node *k; {:valid_link_trigger(i, j, k):});
}*/


/*@ inline resource list_basic(int length, struct Node *head, struct Node *tail, seq<struct Node*> nodes) = length >= 0 **
                    |nodes| == length + 2 ** nodes[0] == head ** nodes[length + 1] == tail **
                    (\forall int i = 0 .. length+2, int j = 0 .. length+2; i != j && {:Trigger.injection_a(i):} && {:Trigger.injection_a(j):} ==> nodes[i] != nodes[j]) **
                    (\forall int i = 0 .. length+2; {:nodes[i]:} != NULL) **
                    (\forall int i = 0 .. length+2; {:\pointer_block_offset(nodes[i]):} == 0) **
                    (\forall int i = 0 .. length+2; {:\pointer_block_length(nodes[i]):} == 1) **
                    (\forall* int i = 0 .. length+2; Trigger.injection_a(i) ==> Perm(*{:nodes[i]:}, write)) **
                    (\forall* int i = 0 .. length+2; 0 <= {:nodes[i]->link:} && {:nodes[i]->link:} <= 18446744073709551615);
*/
/*@ inline resource valid_link(seq<struct Node*> nodes, int i, int j, int link) = (link == ((uintptr_t)nodes[i] ^  (uintptr_t)nodes[j])); */



/*@ requires 1 <= index && index < length && |nodes| == length + 2;
    requires 0 <= link && link <= 18446744073709551615;
    requires nodes[index+1] != NULL;
    requires Value(*nodes[index+1]);
    requires Trigger.valid_link_trigger(nodes[index + 1], nodes[index], nodes[index + 2]) ==> valid_link(nodes, index, index + 2, nodes[index+1]->link);
    requires prev == (uintptr_t)nodes[index+2];
    requires next == ((uintptr_t)link ^ (uintptr_t)prev);
    requires link == nodes[index+1]->link;
    ensures (struct Node *)next == nodes[index];
    ensures \result;
pure _Bool node_lemma_backward(seq<struct Node*> nodes, int link, int index, int length, int prev, int next);*/

/*@ requires 0 <= index && index < length && |nodes| == length + 2;
    requires 0 <= link && link <= 18446744073709551615;
    requires nodes[index+1] != NULL;
    requires Value(*nodes[index+1]);
    requires Trigger.valid_link_trigger(nodes[index + 1], nodes[index], nodes[index + 2]) ==> valid_link(nodes, index, index+2, nodes[index+1]->link);
    requires prev == (uintptr_t)nodes[index];
    requires next == ((uintptr_t)link ^ (uintptr_t)prev);
    requires link == nodes[index+1]->link;
    ensures (struct Node *)next == nodes[index+2];
    ensures \result;
pure _Bool node_lemma_forward(seq<struct Node*> nodes, int link, int index, int length, int prev, int next);*/

//@ given seq<struct Node*> nodes;
//@ context_everywhere list != NULL ** Perm(*list, 1\2);
//@ context_everywhere [1\2]list_basic(list->length, list->head, list->tail, nodes);
//@ context_everywhere (\forall* int i = 1 .. list->length + 1, int j, int k; j == i - 1 && k == i + 1; {:Trigger.valid_link_trigger(nodes[i], nodes[j], nodes[k]):} ==> valid_link(nodes, j, k, nodes[i]->link));
//@ requires 0 <= index && index <= list->length;
//@ requires list->length > 0;
//@ ensures (\forall int i = 0 .. list->length + 2; \old(nodes[i]->data) == {:nodes[i]->data:});
//@ ensures \result == nodes[index + 1];
struct Node *find_node(struct List *list, size_t index) {
    struct Node *node;

    if (index == list->length) {
        return list->tail;
    }

    if (index > list->length / 2) {
        uintptr_t prev = (uintptr_t)list->tail;
        node = (struct Node *)(list->tail->link ^ (uintptr_t)list->head);
        //@ assume node == nodes[list->length];
        //@ loop_invariant 0 <= index && index < list->length;
        //@ loop_invariant index <= i && i < list->length;
        //@ loop_invariant list->length > 0;
        //@ loop_invariant prev == (uintptr_t)nodes[i+2];
        //@ loop_invariant node == nodes[i+1];
        //@ loop_invariant (\forall int i = 0 .. list->length + 2; \old(nodes[i]->data) == {:nodes[i]->data:});
        for (size_t i = list->length - 1; i != index; i--) {
            uintptr_t next = (node->link ^ prev);
            //@ assert node_lemma_backward(nodes, node->link, i, list->length, prev, next);
            prev = (uintptr_t)node;
            node = (struct Node *)next;
        }
    } else {
        uintptr_t prev = (uintptr_t)list->head;
        node = (struct Node *)(list->head->link ^ (uintptr_t)list->tail);
        //@ assume node == nodes[1];
        //@ loop_invariant 0 <= index && index < list->length;
        //@ loop_invariant 0 <= i && i <= index;
        //@ loop_invariant list->length > 0;
        //@ loop_invariant prev == (uintptr_t)nodes[i];
        //@ loop_invariant node == nodes[i+1];
        //@ loop_invariant (\forall int i = 0 .. list->length + 2; \old(nodes[i]->data) == {:nodes[i]->data:});
        for (size_t i = 0; i != index; i++) {
            uintptr_t next = (node->link ^ prev);
            //@ assert node_lemma_forward(nodes, node->link, i, list->length, prev, next);
            prev = (uintptr_t)node;
            node = (struct Node *)next;
        }
    }
    return node;
}

//@ yields seq<struct Node*> nodes;
//@ ensures \result != NULL ** Perm(*\result, write);
//@ ensures list_basic(\result->length, \result->head, \result->tail, nodes);
//@ ensures \result->length == 0;
struct List *new() {
    struct Node *sentinel_front = (struct Node *)malloc(sizeof(struct Node));
    //@ assume sentinel_front != NULL;
    sentinel_front->data = -1;
    struct Node *sentinel_back = (struct Node *)malloc(sizeof(struct Node));
    //@ assume sentinel_back != NULL;
    sentinel_back->data = -1;
    sentinel_front->link = 0;
    sentinel_back->link = 0;
    struct List *list = (struct List *)malloc(sizeof(struct List));
    //@ assume list != NULL;
    list->head = sentinel_front;
    list->tail = sentinel_back;
    list->length = 0;

    //@ ghost nodes = [list->head, list->tail];
    return list;
}


//@ given seq<struct Node*> nodes;
//@ yields seq<struct Node*> outNodes;
//@ context list != NULL ** Perm(*list, write);
//@ requires list_basic(list->length, list->head, list->tail, nodes);
//@ ensures list_basic(list->length, list->head, list->tail, outNodes);
//@ requires (\forall* int i = 1 .. list->length + 1, int j, int k; j == i - 1 && k == i + 1; {:Trigger.valid_link_trigger(nodes[i], nodes[j], nodes[k]):} ==> valid_link(nodes, j, k, nodes[i]->link));
//@ ensures (\forall* int i = 1 .. list->length + 1, int j, int k; j == i - 1 && k == i + 1; {:Trigger.valid_link_trigger(outNodes[i], outNodes[j], outNodes[k]):} ==> valid_link(outNodes, j, k, outNodes[i]->link));
//@ requires 0 <= index && index <= list->length;
//@ ensures list->length == \old(list->length) + 1;
//@ ensures (\forall int i = 0 .. index + 1; {:nodes[i]:} == {:outNodes[i]:});
//@ ensures (\forall int i = 1 .. index + 1; \old({:nodes[i]->data:}) == {:outNodes[i]->data:});
//@ ensures (\forall int i = index + 2 .. list->length + 1; nodes[i - 1] == {:outNodes[i]:});
//@ ensures (\forall int i = index + 2 .. list->length + 1; \old(nodes[i - 1]->data) == {:outNodes[i]->data:});
//@ ensures outNodes[index+1]->data == data;
void insert(struct List *list, size_t index, int data) {
    struct Node *node = (struct Node *)malloc(sizeof(struct Node));
    //@ assume node != NULL;
    node->data = data;
    //@ assume (\forall int i = 0 .. list->length + 2; {:nodes[i]:} != node);

    if (list->length == 0) {
        list->head->link = (uintptr_t)node ^ (uintptr_t)list->tail;
        list->tail->link = (uintptr_t)node ^ (uintptr_t)list->head;
        node->link = ((uintptr_t)list->head ^ (uintptr_t)list->tail);
        //@ ghost outNodes = nodes[0 .. (index + 1)] + [node] + nodes[(index + 1) .. ];
        list->length++;
        //@ assert Trigger.valid_link_trigger(outNodes[index + 1], outNodes[index], outNodes[index + 2]) ==> valid_link(outNodes, index, index + 2, node->link);
    } else {
        struct Node *prev;
        if (index == 0) {
            prev = list->head;
        } else {
            prev = find_node(list, index - 1) /*@ given {nodes = nodes} */;
        }
        struct Node *next = find_node(list, index) /*@ given {nodes = nodes} */;
        node->link = (uintptr_t)prev ^ (uintptr_t)next;

        prev->link = (prev->link ^ (uintptr_t)next) ^ (uintptr_t)node;
        //@ ghost if (index > 0) { assume prev->link == ((uintptr_t)nodes[index-1] ^ (uintptr_t)node); }
        next->link = (next->link ^ (uintptr_t)prev) ^ (uintptr_t)node;
        //@ ghost if (index < list->length) { assume next->link == ((uintptr_t)node ^ (uintptr_t)nodes[index+2]); }
        list->length++;
        //@ ghost outNodes = nodes[0 .. (index + 1)] + [node] + nodes[(index + 1) .. ];
        //@ assert (\forall int i = 1 .. |outNodes| - 2; {:outNodes[i]:} != NULL);
        //@ ghost if (index > 0) { assert Trigger.valid_link_trigger(outNodes[index], outNodes[index - 1], outNodes[index + 1]) ==> valid_link(outNodes, index - 1, index + 1, prev->link); }
        //@ assert Trigger.valid_link_trigger(outNodes[index + 1], outNodes[index], outNodes[index + 2]) ==> valid_link(outNodes, index, index + 2, node->link);
        //@ ghost if (index < list->length - 1) { assert Trigger.valid_link_trigger(outNodes[index + 2], outNodes[index + 1], outNodes[index + 3]) ==> valid_link(outNodes, index + 1, index + 3, next->link); }
    }
}

//@ given seq<struct Node*> nodes;
//@ context list != NULL ** Perm(*list, 1\2);
//@ context [1\2]list_basic(list->length, list->head, list->tail, nodes);
//@ context (\forall* int i = 1 .. list->length + 1, int j, int k; j == i - 1 && k == i + 1; {:Trigger.valid_link_trigger(nodes[i], nodes[j], nodes[k]):} ==> valid_link(nodes, j, k, nodes[i]->link));
//@ requires 0 <= index && index < list->length;
//@ requires list->length > 0;
//@ ensures \result == nodes[index + 1]->data;
int get(struct List *list, size_t index) {
    struct Node *node = find_node(list, index) /*@ given {nodes = nodes} */;

    return node->data;
}


//@ given seq<struct Node*> nodes;
//@ yields seq<struct Node*> outNodes;
//@ context list != NULL ** Perm(*list, write);
//@ requires list_basic(list->length, list->head, list->tail, nodes);
//@ ensures list_basic(list->length, list->head, list->tail, outNodes);
//@ requires (\forall* int i = 1 .. list->length + 1, int j, int k; j == i - 1 && k == i + 1; {:Trigger.valid_link_trigger(nodes[i], nodes[j], nodes[k]):} ==> valid_link(nodes, j, k, nodes[i]->link));
//@ ensures (\forall* int i = 1 .. list->length + 1, int j, int k; j == i - 1 && k == i + 1; {:Trigger.valid_link_trigger(outNodes[i], outNodes[j], outNodes[k]):} ==> valid_link(outNodes, j, k, outNodes[i]->link));
//@ requires 0 <= index && index < list->length;
//@ requires list->length > 0;
//@ ensures list->length == \old(list->length) - 1;
//@ ensures (\forall int i = 0 .. index + 1; {:nodes[i]:} == {:outNodes[i]:});
//@ ensures (\forall int i = 1 .. index + 1; \old({:nodes[i]->data:}) == {:outNodes[i]->data:});
//@ ensures (\forall int i = index + 1 .. list->length + 1; nodes[i + 1] == {:outNodes[i]:});
//@ ensures (\forall int i = index + 1 .. list->length + 1; \old(nodes[i + 1]->data) == {:outNodes[i]->data:});
//@ ensures \result == \old(nodes[index + 1]->data);
int delete(struct List *list, size_t index) {
    struct Node *prev;
    if (index == 0) {
        prev = list->head;
    } else {
        prev = find_node(list, index - 1) /*@ given {nodes = nodes} */;
    }
    struct Node *node = find_node(list, index) /*@ given {nodes = nodes} */;
    struct Node *next = find_node(list, index + 1) /*@ given {nodes = nodes} */;

    prev->link = prev->link ^ (uintptr_t)node ^ (uintptr_t)next;
    //@ ghost if (index > 0) { assume prev->link == ((uintptr_t)nodes[index-1] ^ (uintptr_t)next); }
    next->link = next->link ^ (uintptr_t)node ^ (uintptr_t)prev;
    //@ ghost if (index < list->length - 1) { assume next->link == ((uintptr_t)prev ^ (uintptr_t)nodes[index+3]); }

    int data = node->data;
    free(node);

    list->length--;
    //@ ghost outNodes = nodes[0 .. (index + 1)] + nodes[(index + 2) .. ];
    //@ ghost if (index > 0) { assert Trigger.valid_link_trigger(outNodes[index], outNodes[index - 1], outNodes[index + 1]) ==> valid_link(outNodes, index - 1, index + 1, prev->link); }
    //@ ghost if (index < list->length) { assert Trigger.valid_link_trigger(outNodes[index + 1], outNodes[index], outNodes[index + 2]) ==> valid_link(outNodes, index, index + 2, next->link); }

    return data;
}


int main(void) {
    //@ ghost seq<struct Node *> nodes;
    struct List *list = new() /*@ yields {nodes=nodes} */;
    insert(list, 0, 1) /*@ given {nodes = nodes} */ /*@ yields {nodes = outNodes} */;
    //@ assert 1 == get(list, 0) /*@ given {nodes = nodes} */;
    //@ ghost seq<struct Node *> oldNodes = nodes;
    insert(list, 1, 2) /*@ given {nodes = nodes} */ /*@ yields {nodes = outNodes} */;
    //@ assert 1 == get(list, 0) /*@ given {nodes = nodes} */;
    //@ assert 2 == get(list, 1) /*@ given {nodes = nodes} */;
    insert(list, 0, 3) /*@ given {nodes = nodes} */ /*@ yields {nodes = outNodes}*/;
    //@ assert 3 == get(list, 0) /*@ given {nodes = nodes} */;
    //@ assert 1 == get(list, 1) /*@ given {nodes = nodes} */;
    //@ assert 2 == get(list, 2) /*@ given {nodes = nodes} */;
    insert(list, 1, 4) /*@ given {nodes = nodes} */ /*@ yields {nodes = outNodes} */;
    //@ assert 3 == get(list, 0) /*@ given {nodes = nodes} */;
    //@ assert 4 == get(list, 1) /*@ given {nodes = nodes} */;
    //@ assert 1 == get(list, 2) /*@ given {nodes = nodes} */;
    //@ assert 2 == get(list, 3) /*@ given {nodes = nodes} */;
    insert(list, 3, 5) /*@ given {nodes = nodes} */ /*@ yields {nodes = outNodes}*/;
    //@ assert 3 == get(list, 0) /*@ given {nodes = nodes} */;
    //@ assert 4 == get(list, 1) /*@ given {nodes = nodes} */;
    //@ assert 1 == get(list, 2) /*@ given {nodes = nodes} */;
    //@ assert 5 == get(list, 3) /*@ given {nodes = nodes} */;
    //@ assert 2 == get(list, 4) /*@ given {nodes = nodes} */;
    //@ assert 5 == delete(list, 3) /*@ given {nodes = nodes} */ /*@ yields {nodes = outNodes}*/;
    //@ assert 3 == get(list, 0) /*@ given {nodes = nodes} */;
    //@ assert 4 == get(list, 1) /*@ given {nodes = nodes} */;
    //@ assert 1 == get(list, 2) /*@ given {nodes = nodes} */;
    //@ assert 2 == get(list, 3) /*@ given {nodes = nodes} */;
    //@ assert 1 == delete(list, 2) /*@ given {nodes = nodes} */ /*@ yields {nodes = outNodes}*/;
    //@ assert 3 == get(list, 0) /*@ given {nodes = nodes} */;
    //@ assert 4 == get(list, 1) /*@ given {nodes = nodes} */;
    //@ assert 2 == get(list, 2) /*@ given {nodes = nodes} */;
    //@ assert 3 == delete(list, 0) /*@ given {nodes = nodes} */ /*@ yields {nodes = outNodes}*/;
    //@ assert 4 == get(list, 0) /*@ given {nodes = nodes} */;
    //@ assert 2 == get(list, 1) /*@ given {nodes = nodes} */;
    //@ assert 2 == delete(list, 1) /*@ given {nodes = nodes} */ /*@ yields {nodes = outNodes}*/;
    //@ assert 4 == get(list, 0) /*@ given {nodes = nodes} */;
    //@ assert 4 == delete(list, 0) /*@ given {nodes = nodes} */ /*@ yields {nodes = outNodes}*/;
    //@ assert 0 == list->length;
    return 0;
}


