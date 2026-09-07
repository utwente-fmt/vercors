#include <stdint.h>

struct Node {
    struct Node *next;
};

typedef struct Node Node_t;

//@ ghost typedef seq<Node_t*> Nodes;

struct List {
    struct Node *head;
    Nodes ns;
};

typedef struct List List_t;

/*@
inline resource state(List_t* list) =
  list != NULL ** Perm(list->head, 1) ** Perm(list->ns, 1) **
  (list->head == NULL
    ? |list->ns| == 0
    : (
      |list->ns| > 0 ** list->ns[0] == list->head **
      (\forall* int i = 0 .. |list->ns|; {: list->ns[i] :} != NULL) **
      (\forall int i = 0 .. |list->ns|, int j = 0 .. |list->ns|; i != j; {: list->ns[i] :} != {: list->ns[j] :}) **
      (\forall* int i = 0 .. |list->ns|; Perm({: list->ns[i]->next :}, 1)) **
      // This next quantifier caused slowness/instability.
      (\forall* int i = 0 .. |list->ns| - 1, int j; j == i + 1; {: list->ns[i]->next :} == {: list->ns[j] :}) **
      list->ns[|list->ns| - 1]->next == NULL
    )
  );
*/

/*@
yields int k;
context state(l);
requires toremove != NULL;
requires toremove \in l->ns;
ensures |l->ns| == |\old(l->ns)| - 1;
ensures k \in {0 .. |\old(l->ns)|};
ensures toremove == \old(l->ns[k]);
ensures l->ns[0 .. k] == \old(l->ns[0 .. k]);
ensures (\forall int i = k .. |\old(l->ns)| - 1, int j; j == i + 1; {: l->ns[i] :} == \old({: l->ns[j] :}));
*/
void removeBetter(List_t *l, Node_t *toremove) {
    struct Node **p = &l->head;
    //@ ghost k = 0;

    //@ assert l->head != NULL;

    /*@ loop_invariant true
        ** Perm(state(l), 1\2)
        ** k \in {0 .. |l->ns|}
        ** p != NULL
        ** (k == 0 ==> p == &(l->head))
        ** (k != 0 ==> p == &(l->ns[k - 1]->next))
        ** (*p) != NULL
        ** toremove \in l->ns[k..]
        ;
    */
    while (*p != toremove) {
        //@ assume \pointer_block(*p) == \pointer_block(toremove);
        p = &(*p)->next;
        k += 1;
        /*@ ghost {
            assert (p == &(l->ns[k - 1]->next)); // Guess we need to trigger something unexpected here?
            if ((*p) == NULL) {
                if (k != |l->ns| - 1) {
                    assert p == &(l->ns[k - 1]->next);
                    assert k - 1 < |l->ns| - 1;
                    assert l->ns[k]->next == l->ns[k+1]; // It started verifying when I added this one
                    assert (*p) != NULL;
                    assert false;
                }
                assert k + 1 == |l->ns| - 1;
                assert false;
            }
        } @*/
    }
    //@ assume \pointer_block(*p) == \pointer_block(toremove);

    //@ label pre;
    struct Node **oldP = p;
    //@ ghost Nodes oldNs = l->ns;
    *p = toremove->next;
    //@ ghost l->ns = l->ns[0 .. k] + l->ns[k + 1 ..];

    /*@ ghost
        if (l->head == NULL) {
            assert oldP == &(l->head);
            assert \old[pre](toremove->next == NULL);
            assert \old[pre](toremove->next == NULL);
            if (|oldNs| > 1) {
                assert oldNs[1] == toremove->next;
            }
            assert |oldNs| == 1;
            assert |l->ns| == 0;
        }
    @*/

    /*@
    assert state(l);
    assert |l->ns| == |\old(l->ns)| - 1;
    assert k \in {0 .. |\old(l->ns)|};
    assert toremove == \old(l->ns[k]);
    assert l->ns[0 .. k] == \old(l->ns[0 .. k]);
    assert (\forall int i = k .. |\old(l->ns)| - 1, int j; j == i + 1; {: l->ns[i] :} == \old({: l->ns[j] :}));
    */
}

