// Example of a linked list to test that structs containing pointers to themselfes work.
#include <stdlib.h>

typedef struct List_t {
    int v;
    struct List_t *next;
} List;

/*@
declare DEF_RESULT_(List*, lPtr);
@*/

// Mock-function for allocating a list-node
/*@
assumed contract for external ir malloc_list;
args;
returns List *;
ensures _result(lPtr) != NULL;
ensures _Perm(_result(lPtr), _write);
ensures _result(lPtr)->next == NULL;
@*/
List *malloc_list() {
    return (List *) malloc(sizeof(List));
}

/*@
predicate list_write(List *from) := 
    _sep( from != NULL, 
    _sep(_Perm(from, _write),  
         _imply(from->next != NULL , list_write(from->next))
        )); 
@*/

/*@
requires _imply(list != NULL , list_write(list));
ensures  _result(lPtr) != NULL;
ensures  list_write(_result(lPtr));
@*/
List *prepend(int elem, List *list) {
    List *new_head = malloc_list();
    new_head->v = elem;
    new_head->next = list;
    /*@
    fold list_write(new_head);
    @*/
    return new_head;
}

/*@
requires _imply(l != NULL , list_write(l));
ensures  _result(lPtr) != NULL;
ensures  list_write(_result(lPtr));
@*/
List *append(List *l, int elem) {
    if (l == NULL) {
        List *new_node = malloc_list();
        new_node->v = elem;
        /*@
        fold list_write(new_node);
        @*/
        return new_node;
    }
    
    /*@
    unfold list_write(l);
    @*/
    l->next = append(l->next, elem);
    /*@
    fold list_write(l);
    @*/
    return l;
}

