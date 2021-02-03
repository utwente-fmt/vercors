/***********************************************************************************
Created by Mohsen Safari.
************************************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <cuda.h>

//@ pure int inf() = -1; //100000000; any negative integer

/*@ 
pure bool Graph(int V, int A, seq<int> start, seq<int> end, seq<int> weight) =
	0 < V &&
	0 < A && |start| == A && |end| == A && |weight| == A &&
	(\forall int i; 0 <= i && i < A; 
		0 <= start[i] && start[i] < V && 
		0 <= end[i] && end[i] < V && 
		start[i] != end[i] && 
		(\forall int j; 0 <= j && j < A && i != j && start[i] == start[j]; end[i] != end[j]) &&
     weight[i] > 0); 
@*/

/*@ 
requires Graph(V, A, start, end, weight);
ensures (\forall int i; i >= 0 && i < A; Path(V, A, start, end, weight, start[i], end[i], seq<int> { i }));
pure bool Path(int V, int A, seq<int> start, seq<int> end, seq<int> weight, int x, int y, seq<int> P) =
	0 <= x && x < V &&
	0 <= y && y < V &&
	(\forall int i; 0 <= i && i < |P|; 0 <= P[i] && P[i] < A) &&
	(0 == |P| ==> x == y) &&
	(0 < |P| ==> start[P[0]] == x && end[P[|P| - 1]] == y) &&
	(\forall int i; 0 <= i && i < |P| - 1; end[P[i]] == start[P[i + 1]]);
@*/

/*@ 
requires Graph(V, A, start, end, weight);
ensures (\forall int i; i >= 0 && i < A; SimplePath(V, A, start, end, weight, start[i], end[i], seq<int> { i }));
pure bool SimplePath(int V, int A, seq<int> start, seq<int> end, seq<int> weight, int x, int y, seq<int> P) =
  Path(V, A, start, end, weight, x, y, P) &&
  (\forall int i; 0 <= i && i < |P| - 1; (\forall int j; i < j && j < |P|; start[P[i]] != end[P[j]])); 
@*/

/*@ 
requires Graph(V, A, start, end, weight);
pure bool ExPath(int V, int A, seq<int> start, seq<int> end, seq<int> weight, int x, int y, int len) =
	(\exists seq<int> P; Path(V, A, start, end, weight, x, y, P); |P| <= len);
@*/

/*@   
requires Graph(V, A, start, end, weight);
ensures (\forall int i; i >= 0 && i < A; ExPathEqual(V, A, start, end, weight, start[i], end[i], weight[i]));
ensures ExPathEqual(V, A, start, end, weight, x, x, 0);
pure bool ExPathEqual(int V, int A, seq<int> start, seq<int> end, seq<int> weight, int x, int y, int len) =
	(\exists seq<int> P; Path(V, A, start, end, weight, x, y, P); cost(V, A, start, end, weight, P) == len);
@*/

/*@
requires Graph(V, A, start, end, weight);
requires (\forall int i; 0 <= i && i < |P|; 0 <= P[i] && P[i] < A);
ensures 0 <= \result;
ensures 0 == |P| ==> \result == 0;
ensures (P != seq<int>{}) ==> \result > 0;
ensures (\result == 0) ==> (P == seq<int>{});
ensures (\result != 0) ==> (P != seq<int>{});
ensures 1 == |P| ==> \result == weight[P[0]];
pure int cost(int V, int A, seq<int> start, seq<int> end, seq<int> weight, seq<int> P) =  
  0 < |P| ? weight[P[0]] + cost(V, A, start, end, weight, tail(P)) : 0;
@*/  

/*@
ensures (xs + ys) + zs == xs + (ys + zs);
pure bool iseq_assoc(seq<int> xs, seq<int> ys, seq<int> zs) = true;
@*/

/*@
requires Graph(V, A, start, end, weight);
requires Path(V, A, start, end, weight, x, y, P);
requires 0 <= a && a < A;
requires end[a] == x;
ensures \result && Path(V, A, start, end, weight, start[a], y, seq<int> { a } + P);
pure bool lemma_path_append(int V, int A, seq<int> start, seq<int> end, seq<int> weight, int x, int y, seq<int> P, int a);
@*/

/*@
requires Graph(V, A, start, end, weight);
requires Path(V, A, start, end, weight, s, t, P);
requires Path(V, A, start, end, weight, t, u, Q);
ensures \result && Path(V, A, start, end, weight, s, u, P + Q);
pure bool lemma_path_trans(int V, int A, seq<int> start, seq<int> end, seq<int> weight, int s, int t, int u, seq<int> P, seq<int> Q) =
	0 < |P| ==> (
		lemma_path_trans(V, A, start, end, weight, end[P[0]], t, u, tail(P), Q) &&
		lemma_path_append(V, A, start, end, weight, end[P[0]], u, tail(P) + Q, P[0]) &&
		iseq_assoc(seq<int> { P[0] }, tail(P), Q) &&
		seq<int> { P[0] } + tail(P) == P
	); 
@*/

/*@
requires 0 < |xs|;
ensures (xs + ys)[0] == xs[0];
ensures tail(xs + ys) == tail(xs) + ys;
ensures \result;  
pure bool lemma_app_nonempty(seq<int> xs, seq<int> ys) = true; 
@*/

/*@
requires 0 < |ys|;
ensures \result && ((xs + seq<int>{ys[0]}) + tail(ys)) == xs + ys;
pure bool lemma_app_append_right(seq<int> xs, seq<int> ys) =
  iseq_assoc(xs, seq<int>{ys[0]}, tail(ys));
@*/

/*@
requires Graph(V, A, start, end, weight);
requires (\forall int i; 0 <= i && i < |P|; 0 <= P[i] && P[i] < A);
requires (\forall int i; 0 <= i && i < |Q|; 0 <= Q[i] && Q[i] < A);
ensures \result && cost(V, A, start, end, weight, P + Q) == cost(V, A, start, end, weight, P) + cost(V, A, start, end, weight, Q);
pure bool lemma_cost_app(int V, int A, seq<int> start, seq<int> end, seq<int> weight, seq<int> P, seq<int> Q) =  
  0 < |P| ==> lemma_app_nonempty(P, Q) && lemma_cost_app(V, A, start, end, weight, tail(P), Q);
@*/

/*@
requires Graph(V, A, start, end, weight);
requires Path(V, A, start, end, weight, s, t, P);
requires Path(V, A, start, end, weight, t, u, Q);
ensures \result && Path(V, A, start, end, weight, s, u, P + Q);
ensures \result && cost(V, A, start, end, weight, P + Q) == cost(V, A, start, end, weight, P) + cost(V, A, start, end, weight, Q);
pure bool lemma_cost_path(int V, int A, seq<int> start, seq<int> end, seq<int> weight, int s, int t, int u, seq<int> P, seq<int> Q) =
  lemma_path_trans(V, A, start, end, weight, s, t, u, P, Q) &&
  lemma_cost_app(V, A, start, end, weight, P, Q);
@*/

/*@
requires b;
ensures \result == xs;
pure seq<int> assertSeq(seq<int> xs, bool b) = xs;
@*/

/*@
requires Graph(V, A, start, end, weight);
ensures \result && (\forall int i; i >= 0 && i < A; Path(V, A, start, end, weight, start[i], end[i], seq<int>{i}));
ensures	\result && (\forall int i; i >= 0 && i < A; ExPathEqual(V, A, start, end, weight, start[i], end[i], weight[i]));	
pure bool lemma_one_path(int V, int A, seq<int> start, seq<int> end, seq<int> weight) = true;
@*/


/* ** Invariant 10 */
/*@
requires Graph(V, A, start, end, weight);
requires ExPathEqual(V, A, start, end, weight, s, t, len1); 
requires ExPathEqual(V, A, start, end, weight, t, u, len2); 
ensures \result && ExPathEqual(V, A, start, end, weight, s, u, len1+len2);
pure bool lemma_expath_trans(int V, int A, seq<int> start, seq<int> end, seq<int> weight, int s, int t, int u, int len1, int len2) =
	(\forall seq<int> P; Path(V, A, start, end, weight, s, t, P);
		(\forall seq<int> Q; Path(V, A, start, end, weight, t, u, Q);
			lemma_cost_path(V, A, start, end, weight, s, t, u, P, Q)));
@*/


/*@
  requires i >= 0 && i <= |xs|-1;
  ensures |\result| == |xs| - 1 - i;
  ensures (\forall int k; k >= 0 && k < |\result|; \result[k] == xs[i+k]);
  ensures i == 0 ==> (\forall int k; k >= 0 && k < |xs|-1; \result[k] == xs[k]);
  pure seq<int> subseq(seq<int> xs, int i) = (i != |xs|-1) ? seq<int>{xs[i]} + subseq(xs, i+1) : seq<int>{}; 
@*/

/* ** Invariant 12 */
/*@
requires Graph(V, A, start, end, weight);
requires |oldcost| == V && |cost| == V;
requires (\forall int k; 0 <= k && k < V; cost[k] == inf() ==> oldcost[k] == cost[k]);
requires (\forall int k; 0 <= k && k < V; oldcost[k] == inf() ==> oldcost[k] <= cost[k]);
requires (\forall int k; 0 <= k && k < V; oldcost[k] != inf() ==> cost[k] <= oldcost[k]);
requires (\forall int k; 0 <= k && k < V && k != src; cost[k] == inf() || cost[k] > 0);
requires 0 <= i && i < V-1;
requires 0 <= src && src < V;
requires 0 <= vertex && vertex < V;
requires Path(V, A, start, end, weight, src, vertex, P);
requires |P| <= i + 1;
requires oldcost[src] == 0 && cost[src] == 0 && cost[vertex] == inf();
requires (\forall int v; 0 <= v && v < V && ExPath(V, A, start, end, weight, src, v, i); oldcost[v] != inf());
requires (\forall int a; 0 <= a && a < A && oldcost[start[a]] != inf(); cost[end[a]] != inf());
ensures false;
pure bool lemma_inv12_helper1(int V, int A, seq<int> start, seq<int> end, seq<int> weight, seq<int> oldcost, seq<int> cost, int src, int i, int vertex, seq<int> P) =
  vertex != src && 0 < |P| && Path(V, A, start, end, weight, src, start[P[|P| - 1]], subseq(P, 0));
@*/

/*@
requires Graph(V, A, start, end, weight);
requires |oldcost| == V && |cost| == V;
requires (\forall int k; 0 <= k && k < V; cost[k] == inf() ==> oldcost[k] == cost[k]);
requires (\forall int k; 0 <= k && k < V; oldcost[k] == inf() ==> oldcost[k] <= cost[k]);
requires (\forall int k; 0 <= k && k < V; oldcost[k] != inf() ==> cost[k] <= oldcost[k]);
requires (\forall int k; 0 <= k && k < V && k != src; cost[k] == inf() || cost[k] > 0);
requires 0 <= i && i < V-1;
requires 0 <= src && src < V;
requires 0 <= vertex && vertex < V;
requires oldcost[src] == 0 && cost[src] == 0 && cost[vertex] == inf();
requires (\forall int v; 0 <= v && v < V && ExPath(V, A, start, end, weight, src, v, i); oldcost[v] != inf());
requires (\forall int a; 0 <= a && a < A && oldcost[start[a]] != inf(); cost[end[a]] != inf());
ensures (\forall seq<int> P; Path(V, A, start, end, weight, src, vertex, P) && |P| <= i + 1; false);
pure bool lemma_inv12_helper2(int V, int A, seq<int> start, seq<int> end, seq<int> weight, seq<int> oldcost, seq<int> cost, int src, int i, int vertex) =
  (\forall seq<int> P; Path(V, A, start, end, weight, src, vertex, P) && |P| <= i + 1; lemma_inv12_helper1(V, A, start, end, weight, oldcost, cost, src, i, vertex, P));
@*/

/*@
requires Graph(V, A, start, end, weight);
requires |oldcost| == V && |cost| == V;
requires (\forall int k; 0 <= k && k < V; cost[k] == inf() ==> oldcost[k] == cost[k]);
requires (\forall int k; 0 <= k && k < V; oldcost[k] == inf() ==> oldcost[k] <= cost[k]);
requires (\forall int k; 0 <= k && k < V; oldcost[k] != inf() ==> cost[k] <= oldcost[k]);
requires (\forall int k; 0 <= k && k < V && k != src; cost[k] == inf() || cost[k] > 0);
requires 0 <= i && i < V-1;
requires 0 <= src && src < V;
requires oldcost[src] == 0 && cost[src] == 0;
requires (\forall int v; 0 <= v && v < V && ExPath(V, A, start, end, weight, src, v, i); oldcost[v] != inf());
requires (\forall int a; 0 <= a && a < A && oldcost[start[a]] != inf(); cost[end[a]] != inf());
ensures (\forall int vertex; 0 <= vertex && vertex < V; (\forall seq<int> P; Path(V, A, start, end, weight, src, vertex, P) && |P| <= i + 1; vertex != inf()));
pure bool lemma_inv12_helper3(int V, int A, seq<int> start, seq<int> end, seq<int> weight, seq<int> oldcost, seq<int> cost, int src, int i) =
  (\forall int vertex; 0 <= vertex && vertex < V && cost[vertex] == inf(); lemma_inv12_helper2(V, A, start, end, weight, oldcost, cost, src, i, vertex));
@*/

/*@
requires Graph(V, A, start, end, weight);
requires |oldcost| == V && |cost| == V;
requires (\forall int k; 0 <= k && k < V; cost[k] == inf() ==> oldcost[k] == cost[k]);
requires (\forall int k; 0 <= k && k < V; oldcost[k] == inf() ==> oldcost[k] <= cost[k]);
requires (\forall int k; 0 <= k && k < V; oldcost[k] != inf() ==> cost[k] <= oldcost[k]);
requires (\forall int k; 0 <= k && k < V && k != src; cost[k] == inf() || cost[k] > 0);
requires 0 <= i && i < V-1;
requires 0 <= src && src < V;
requires oldcost[src] == 0 && cost[src] == 0;
requires (\forall int v; 0 <= v && v < V && ExPath(V, A, start, end, weight, src, v, i); oldcost[v] != inf());
requires (\forall int a; 0 <= a && a < A && oldcost[start[a]] != inf(); cost[end[a]] != inf());
ensures (\forall int v; 0 <= v && v < V && ExPath(V, A, start, end, weight, src, v, i + 1); cost[v] != inf());
pure bool lemma_inv12_preserved(int V, int A, seq<int> start, seq<int> end, seq<int> weight, seq<int> oldcost, seq<int> cost, int src, int i) =
   lemma_inv12_helper3(V, A, start, end, weight, oldcost, cost, src, i);
@*/

/* ** Invariant 11 */
/*@
requires Graph(V, A, start, end, weight);
requires |oldcost| == V && |cost| == V;
requires (\forall int k; 0 <= k && k < V; cost[k] == inf() ==> oldcost[k] == cost[k]);
requires (\forall int k; 0 <= k && k < V; oldcost[k] == inf() ==> oldcost[k] <= cost[k]);
requires (\forall int k; 0 <= k && k < V; oldcost[k] != inf() ==> cost[k] <= oldcost[k]);
requires (\forall int k; 0 <= k && k < V && k != src; cost[k] == inf() || cost[k] > 0);
requires 0 <= i && i < V-1;
requires 0 <= src && src < V;
requires 0 <= vertex && vertex < V;
requires Path(V, A, start, end, weight, src, vertex, P);
requires |P| <= i + 1;
requires oldcost[src] == 0 && cost[src] == 0 && cost[vertex] != inf();
requires cost(V, A, start, end, weight, P) < cost[vertex];
requires (\forall int v; 0 <= v && v < V && oldcost[v] != inf(); 
          (\forall seq<int> t; Path(V, A, start, end, weight, src, v, t) && 
           |t| <= i; oldcost[v] <= cost(V, A, start, end, weight, t))); // inv 11
requires (\forall int v; 0 <= v && v < V && ExPath(V, A, start, end, weight, src, v, i); oldcost[v] != inf()); // inv 12
requires (\forall int v; 0 <= v && v < V && oldcost[v] != inf(); ExPathEqual(V, A, start, end, weight, src, v, oldcost[v])); // inv 10
requires (\forall int a; 0 <= a && a < A && (oldcost[start[a]] != inf()) && 
           (oldcost[end[a]] == inf() || oldcost[start[a]] + weight[a] <= oldcost[end[a]]); cost[end[a]] <= oldcost[start[a]] + weight[a]);
ensures false;
pure bool lemma_inv11_helper1(int V, int A, seq<int> start, seq<int> end, seq<int> weight, seq<int> oldcost, seq<int> cost, int src, int i, int vertex, seq<int> P) =
     |P| > 0 &&   
     (P == subseq(P, 0) + seq<int>{P[|P| - 1]}) &&
     lemma_cost_path(V, A, start, end, weight, src, start[P[|P| - 1]], vertex, subseq(P, 0), seq<int>{P[|P| - 1]});
@*/

/*@
requires Graph(V, A, start, end, weight);
requires |oldcost| == V && |cost| == V;
requires (\forall int k; 0 <= k && k < V; cost[k] == inf() ==> oldcost[k] == cost[k]);
requires (\forall int k; 0 <= k && k < V; oldcost[k] == inf() ==> oldcost[k] <= cost[k]);
requires (\forall int k; 0 <= k && k < V; oldcost[k] != inf() ==> cost[k] <= oldcost[k]);
requires (\forall int k; 0 <= k && k < V && k != src; cost[k] == inf() || cost[k] > 0);
requires 0 <= i && i < V-1;
requires 0 <= src && src < V;
requires 0 <= vertex && vertex < V;
requires oldcost[src] == 0 && cost[src] == 0 && cost[vertex] != inf();
requires (\forall int v; 0 <= v && v < V && oldcost[v] != inf(); 
          (\forall seq<int> t; Path(V, A, start, end, weight, src, v, t) && 
           |t| <= i; oldcost[v] <= cost(V, A, start, end, weight, t))); // inv 11
requires (\forall int v; 0 <= v && v < V && ExPath(V, A, start, end, weight, src, v, i); oldcost[v] != inf()); // inv 12
requires (\forall int v; 0 <= v && v < V && oldcost[v] != inf(); ExPathEqual(V, A, start, end, weight, src, v, oldcost[v])); // inv 10
requires (\forall int a; 0 <= a && a < A && (oldcost[start[a]] != inf()) && 
           (oldcost[end[a]] == inf() || oldcost[start[a]] + weight[a] <= oldcost[end[a]]); cost[end[a]] <= oldcost[start[a]] + weight[a]);
ensures (\forall seq<int> P; Path(V, A, start, end, weight, src, vertex, P) && |P| <= i + 1 && cost(V, A, start, end, weight, P) < cost[vertex]; false);
pure bool lemma_inv11_helper2(int V, int A, seq<int> start, seq<int> end, seq<int> weight, seq<int> oldcost, seq<int> cost, int src, int i, int vertex) =
  (\forall seq<int> P; Path(V, A, start, end, weight, src, vertex, P) && |P| <= i + 1 && cost(V, A, start, end, weight, P) < cost[vertex];
    lemma_inv11_helper1(V, A, start, end, weight, oldcost, cost, src, i, vertex, P));
@*/

/*@
requires Graph(V, A, start, end, weight);
requires |oldcost| == V && |cost| == V;
requires (\forall int k; 0 <= k && k < V; cost[k] == inf() ==> oldcost[k] == cost[k]);
requires (\forall int k; 0 <= k && k < V; oldcost[k] == inf() ==> oldcost[k] <= cost[k]);
requires (\forall int k; 0 <= k && k < V; oldcost[k] != inf() ==> cost[k] <= oldcost[k]);
requires (\forall int k; 0 <= k && k < V && k != src; cost[k] == inf() || cost[k] > 0);
requires 0 <= i && i < V-1;
requires 0 <= src && src < V;
requires oldcost[src] == 0 && cost[src] == 0;
requires (\forall int v; 0 <= v && v < V && oldcost[v] != inf(); 
          (\forall seq<int> t; Path(V, A, start, end, weight, src, v, t) && 
           |t| <= i; oldcost[v] <= cost(V, A, start, end, weight, t))); // inv 11
requires (\forall int v; 0 <= v && v < V && ExPath(V, A, start, end, weight, src, v, i); oldcost[v] != inf()); // inv 12
requires (\forall int v; 0 <= v && v < V && oldcost[v] != inf(); ExPathEqual(V, A, start, end, weight, src, v, oldcost[v])); // inv 10
requires (\forall int a; 0 <= a && a < A && (oldcost[start[a]] != inf()) && 
           (oldcost[end[a]] == inf() || oldcost[start[a]] + weight[a] <= oldcost[end[a]]); cost[end[a]] <= oldcost[start[a]] + weight[a]);
ensures (\forall int vertex; 0 <= vertex && vertex < V && cost[vertex] != inf();
            (\forall seq<int> P; Path(V, A, start, end, weight, src, vertex, P) && |P| <= i + 1 && cost(V, A, start, end, weight, P) < cost[vertex]; vertex != inf()));
pure bool lemma_inv11_helper3(int V, int A, seq<int> start, seq<int> end, seq<int> weight, seq<int> oldcost, seq<int> cost, int src, int i) =
  (\forall int vertex; 0 <= vertex && vertex < V && cost[vertex] != inf(); lemma_inv11_helper2(V, A, start, end, weight, oldcost, cost, src, i, vertex));
@*/

/*@
requires Graph(V, A, start, end, weight);
requires |oldcost| == V && |cost| == V;
requires (\forall int k; 0 <= k && k < V; cost[k] == inf() ==> oldcost[k] == cost[k]);
requires (\forall int k; 0 <= k && k < V; oldcost[k] == inf() ==> oldcost[k] <= cost[k]);
requires (\forall int k; 0 <= k && k < V; oldcost[k] != inf() ==> cost[k] <= oldcost[k]);
requires (\forall int k; 0 <= k && k < V && k != src; cost[k] == inf() || cost[k] > 0);
requires 0 <= i && i < V-1;
requires 0 <= src && src < V;
requires oldcost[src] == 0 && cost[src] == 0;
requires (\forall int v; 0 <= v && v < V && oldcost[v] != inf(); 
          (\forall seq<int> t; Path(V, A, start, end, weight, src, v, t) && 
           |t| <= i; oldcost[v] <= cost(V, A, start, end, weight, t))); // inv 11
requires (\forall int v; 0 <= v && v < V && ExPath(V, A, start, end, weight, src, v, i); oldcost[v] != inf()); // inv 12
requires (\forall int v; 0 <= v && v < V && oldcost[v] != inf(); ExPathEqual(V, A, start, end, weight, src, v, oldcost[v])); // inv 10
requires (\forall int a; 0 <= a && a < A && (oldcost[start[a]] != inf()) && 
           (oldcost[end[a]] == inf() || oldcost[start[a]] + weight[a] <= oldcost[end[a]]); cost[end[a]] <= oldcost[start[a]] + weight[a]);
ensures (\forall int vertex; 0 <= vertex && vertex < V && cost[vertex] != inf();
            (\forall seq<int> P; Path(V, A, start, end, weight, src, vertex, P) && |P| <= i + 1; cost(V, A, start, end, weight, P) >= cost[vertex]));
pure bool lemma_inv11_preserved(int V, int A, seq<int> start, seq<int> end, seq<int> weight, seq<int> oldcost, seq<int> cost, int src, int i) = 
  lemma_inv11_helper3(V, A, start, end, weight, oldcost, cost, src, i);
@*/

//////////////////////////////////////////////////////////////////////////////////Lemma1_simple_path
/*@
requires bound >= 0;
pure bool inbounds(seq<int> xs, int bound) = (\forall int i; i >= 0 && i < |xs|; xs[i] < bound);
@*/

/*@
pure bool nodupl(seq<int> xs) = 
(\forall int i; i >= 0 && i < |xs|; (\forall int j; j > i && j < |xs|; xs[i] != xs[j]));
@*/

/**
 * Gives the set containing all elements ranging from 0 up to (and including) `max`
 */
/*@
requires max >= 0;
ensures (\forall int v; v >= 0 && v <= max; (v \in \result));
ensures (\forall int v; (v \in \result); v >= 0 && v <= max);
ensures |\result| == max + 1;
pure set<int> rangeset(int max) = max > 0 ? set<int>{max} + rangeset(max-1) : set<int>{0};
@*/

/**
 * Converts `xs` to a set.
 */ 
/*@
ensures (\forall int v; (v \in \result); (v \in xs));
ensures (\forall int v; (v \in xs); (v \in \result));
pure set<int> seq2set(seq<int> xs) = |xs| > 0 ? set<int>{xs[0]} + seq2set(tail(xs)) : set<int>{};
@*/

/**
 * If all elements in `xs` are unique, then `xs` has the same size as `seq2set(xs)`.
 */
/*@
requires nodupl(xs);
ensures \result && |xs| == |seq2set(xs)|;
pure bool seq2set_nodupl(seq<int> xs) = |xs| > 0 ==> seq2set_nodupl(tail(xs));
@*/

/**
 * If all elements in `xs` are unique and within 0..`bound`,
 * then `xs` contains at most `bound` elements.
 */
/*@
requires 0 < bound;
requires inbounds(xs, bound);
requires nodupl(xs);
ensures \result;
pure bool seq_bounded_size(seq<int> xs, int bound) = 
  |seq2set(xs) - rangeset(bound - 1)| == |seq2set(xs)| - |seq2set(xs) * rangeset(bound - 1)| && seq2set_nodupl(xs);
@*/

/**
 * Gives the list of all vertices that are on `P`.
 * Since `P` is simple, all these vertices must be unique.
 */
/*@
requires Graph(V, A, start, end, weight);
requires SimplePath(V, A, start, end, weight, x, y, P);
ensures |\result| == |P| + 1;
ensures \result[0] == x;
ensures (\forall int i; 0 < i && i < |\result|; \result[i] == end[P[i - 1]]);
ensures inbounds(\result, V);
ensures nodupl(\result);
pure seq<int> collect(int V, int A, seq<int> start, seq<int> end, seq<int> weight, int x, int y, seq<int> P) =
  seq<int>{x} + collect_helper(V, A, start, end, weight, x, y, P);
@*/

/**
 * Auxiliary helper function for `collect`.
 */
/*@
requires Graph(V, A, start, end, weight);
requires SimplePath(V, A, start, end, weight, x, y, P);
ensures |\result| == |P|;
ensures (\forall int i; 0 <= i && i < |\result|; \result[i] == end[P[i]]);
ensures inbounds(\result, V);
ensures nodupl(\result);
pure seq<int> collect_helper(int V, int A, seq<int> start, seq<int> end, seq<int> weight, int x, int y, seq<int> P) =
  0 < |P| ? seq<int>{end[P[0]]} + collect_helper(V, A, start, end, weight, end[P[0]], y, tail(P)) : seq<int>{};
@*/

/*@
requires Graph(V, A, start, end, weight);
requires SimplePath(V, A, start, end, weight, x, y, P);
ensures \result && |P| < V;  
pure bool simple_path_lemma1_helper(int V, int A, seq<int> start, seq<int> end, seq<int> weight, int x, int y, seq<int> P) =
  seq_bounded_size(collect(V, A, start, end, weight, x, y, P), V);
@*/

/*@
requires Graph(V, A, start, end, weight);
ensures (\forall seq<int> P; SimplePath(V, A, start, end, weight, x, y, P); |P| < V);
pure bool simple_path_lemma1(int V, int A, seq<int> start, seq<int> end, seq<int> weight, int x, int y) =  
  (\forall seq<int> P; SimplePath(V, A, start, end, weight, x, y, P); simple_path_lemma1_helper(V, A, start, end, weight, x, y, P));
@*/

/*@
requires Graph(V, A, start, end, weight);
ensures \result && (\forall int v; v >= 0 && v < V; (\forall seq<int> P; SimplePath(V, A, start, end, weight, x, v, P); |P| < V));
pure bool simple_path_lemma1_preserved(int V, int A, seq<int> start, seq<int> end, seq<int> weight, int x) = 
  (\forall int v; v >= 0 && v < V; (\forall seq<int> P; SimplePath(V, A, start, end, weight, x, v, P); simple_path_lemma1(V, A, start, end, weight, x, v)) ); 
@*/

//////////////////////////////////////////////////////////////////////////////////Lemma2&3&4_simple_path
/**
  * Given any simple (x,y)-path `P` and simple (y,z)-path `Q`
  * for which it holds that `P[1..] ++ Q` is also a simple path (see fourth requires clause),
  * this function constructs and returns a simple (x,z)-path.
  */
/*@
requires Graph(V, A, start, end, weight);
requires SimplePath(V, A, start, end, weight, x, y, P);
requires SimplePath(V, A, start, end, weight, y, z, Q);
requires (\forall int i; i > 0 && i < |P|; 
          (\forall int j; j >= 0 && j < |Q|; start[P[i]] != end[Q[j]]));
requires (\forall int i; i >= 0 && i < |P|; end[P[i]] != x);
ensures SimplePath(V, A, start, end, weight, x, z, \result);
ensures |\result| <= |P| + |Q|;
ensures cost(V, A, start, end, weight, \result) <= cost(V, A, start, end, weight, P) + cost(V, A, start, end, weight, Q);
pure seq<int> trim(int V, int A, seq<int> start, seq<int> end, seq<int> weight, int x, int y, int z, seq<int> P, seq<int> Q) =
    0 < |Q| ? (end[Q[0]] == x ?
               trim(V, A, start, end, weight, x, x, z, seq<int>{}, tail(Q)) :
               assertSeq(trim(V, A, start, end, weight, x, end[Q[0]], z, P+seq<int>{Q[0]}, tail(Q)), 
               (lemma_cost_app(V, A, start, end, weight, P+seq<int>{Q[0]}, tail(Q)) &&
               lemma_app_append_right(P, Q) && 
               lemma_cost_app(V, A, start, end, weight, P, Q)))) 
            : P;
@*/  

/**
 * Given any simple (y,z)-path `P` and (x,y)-arc `a`,
 * this function constructs and returns a simple (x,z)-path.
 */
/*@
requires Graph(V, A, start, end, weight);
requires SimplePath(V, A, start, end, weight, y, z, P);
requires 0 <= a && a < A && start[a] == x && end[a] == y;
ensures SimplePath(V, A, start, end, weight, x, z, \result);
ensures |\result| <= |P| + 1; 
ensures cost(V, A, start, end, weight, \result) <= weight[a] + cost(V, A, start, end, weight, P);
pure  seq<int> extend(int V, int A, seq<int> start, seq<int> end, seq<int> weight, int x, int y, int z, int a, seq<int> P) =
   trim(V, A, start, end, weight, x, y, z, seq<int>{a}, P);
@*/

/**
 * Converts any given (x,y)-path `P` to a simple (x,y)-path.
 * The other Half part of lemma 2
 * And lemma 3 And lemma 4
 */
/*@  
requires Graph(V, A, start, end, weight);
requires Path(V, A, start, end, weight, x, y, P);
ensures SimplePath(V, A, start, end, weight, x, y, \result);
ensures |\result| <= |P|;
ensures cost(V, A, start, end, weight, \result) <= cost(V, A, start, end, weight, P);
pure seq<int> convert(int V, int A, seq<int> start, seq<int> end, seq<int> weight, int x, int y, seq<int> P) =
  |P| > 0 ? extend(V, A, start, end, weight, x, end[P[0]], y, P[0], convert(V, A, start, end, weight, end[P[0]], y, tail(P)))
  : P;
@*/

/*@ 
requires Graph(V, A, start, end, weight);
requires SimplePath(V, A, start, end, weight, x, y, P);
ensures \result && Path(V, A, start, end, weight, x, y, P);    
pure bool simple_path_Part_1_lemma2_helper(int V, int A, seq<int> start, seq<int> end, seq<int> weight, int x, int y, seq<int> P) = true;
@*/

/*@
requires Graph(V, A, start, end, weight);
ensures (\forall seq<int> P; SimplePath(V, A, start, end, weight, x, y, P); Path(V, A, start, end, weight, x, y, P));
pure bool simple_path_Part_1_lemma2(int V, int A, seq<int> start, seq<int> end, seq<int> weight, int x, int y) =
(\forall seq<int> P; SimplePath(V, A, start, end, weight, x, y, P); simple_path_Part_1_lemma2_helper(V, A, start, end, weight, x, y, P));
@*/


/**
 * Every simple path is a path (trivial).
 */
/*@
requires Graph(V, A, start, end, weight);
ensures \result && (\forall int v; v >= 0 && v < V; 
         (\forall seq<int> P; SimplePath(V, A, start, end, weight, x, v, P); Path(V, A, start, end, weight, x, v, P)));
pure bool simple_path_Part_1_lemma2_preserved(int V, int A, seq<int> start, seq<int> end, seq<int> weight, int x) =
   (\forall int v; v >= 0 && v < V; simple_path_Part_1_lemma2(V, A, start, end, weight, x, v));
@*/

/**
 * For every path convert(path) is a path.
 * For every path convert(path) is a simple path.
 */
/*@
requires Graph(V, A, start, end, weight);
ensures (\forall int v; v >= 0 && v < V; 
         (\forall seq<int> P; Path(V, A, start, end, weight, x, v, P); Path(V, A, start, end, weight, x, v, convert(V, A, start, end, weight, x, v, P))));
ensures \result && (\forall int v; v >= 0 && v < V; 
         (\forall seq<int> P; Path(V, A, start, end, weight, x, v, P); SimplePath(V, A, start, end, weight, x, v, convert(V, A, start, end, weight, x, v, P))));
pure bool simple_path_Part_2_lemma2_preserved(int V, int A, seq<int> start, seq<int> end, seq<int> weight, int x) = true;
@*/

/**
 * For every path |convert(path)| <= |path|.
 */
/*@
requires Graph(V, A, start, end, weight);
ensures \result && (\forall int v; v >= 0 && v < V; 
         (\forall seq<int> P; Path(V, A, start, end, weight, x, v, P); 
           |convert(V, A, start, end, weight, x, v, P)| <= |P|));
pure bool simple_path_lemma3_preserved(int V, int A, seq<int> start, seq<int> end, seq<int> weight, int x) = true;
@*/

/**
 * For every path cost(convert(path)) <= cost(path).
 */
/*@
requires Graph(V, A, start, end, weight);
ensures \result && (\forall int v; v >= 0 && v < V; 
         (\forall seq<int> P; Path(V, A, start, end, weight, x, v, P); 
            cost(V, A, start, end, weight, convert(V, A, start, end, weight, x, v, P)) <= cost(V, A, start, end, weight, P)));
pure bool simple_path_cost_lemma4(int V, int A, seq<int> start, seq<int> end, seq<int> weight, int x) = true;
@*/


////////////////////////////////////////////////////////////////////////////////
//compare
////////////////////////////////////////////////////////////////////////////////  
  /*@
  requires start_cost == inf() || start_cost >= 0;
  requires end_cost == inf() || end_cost >= 0;
  requires weight > 0;
  ensures (start_cost != inf()) && (end_cost == inf() || start_cost + weight <= end_cost) ==> true;
  ensures true ==> (start_cost != inf()) && (end_cost == inf() || start_cost + weight <= end_cost);
  ensures (start_cost == inf()) || (end_cost != inf() && start_cost + weight > end_cost) ==> false;
  ensures false ==> (start_cost == inf()) || (end_cost != inf() && start_cost + weight > end_cost);
  ensures (true && end_cost != inf()) ==> start_cost + weight <= end_cost;
  pure bool compare(int start_cost, int weight, int end_cost) = 
    (start_cost != inf() && (end_cost == inf() || start_cost+weight <= end_cost)) ? true : false;
  @*/
////////////////////////////////////////////////////////////////////////////////
//Kernel
////////////////////////////////////////////////////////////////////////////////
/*@
  yields seq<int> contrib;
  given seq<int> cost_seq;
  given seq<int> oldcost_seq;
  given seq<int> start_seq;
  given seq<int> end_seq;
  given seq<int> weight_seq; 

  context_everywhere V == 1024 && A == 10 * V;
  context_everywhere opencl_gsize == V;
  context_everywhere 10 == opencl_gcount;
  context_everywhere source >= 0 && source < V;
  context_everywhere counter >= 0 && counter < V-1;
  context_everywhere g_start.length == A && g_end.length == A && g_weight.length == A && g_cost.length == V;
  context_everywhere |start_seq| == A && |end_seq| == A && |weight_seq| == A && |cost_seq| == V && |contrib| == A;

  kernel_invariant (\forall* int i; 0 <= i && i < A; Perm(g_start[i], 1\4));
  kernel_invariant (\forall* int i; 0 <= i && i < A; Perm(g_end[i], 1\4));
  kernel_invariant (\forall int i; 0 <= i && i < A; g_start[i] >= 0 && g_start[i] < V && g_end[i] >= 0 && g_end[i] < V);
  kernel_invariant (\forall int i; 0 <= i && i < A; g_start[i] != g_end[i]);
  kernel_invariant (\forall int i; 0 <= i && i < A; (\forall int j; 0 <= j && j < A && i != j; g_start[i] == g_start[j] ==> g_end[i] != g_end[j]));
  
  kernel_invariant (\forall* int i; 0 <= i && i < A; Perm(g_weight[i], 1\4));
  kernel_invariant (\forall int i; 0 <= i && i < A; weight[i] > 0);
  
  kernel_invariant (\forall* int i; 0 <= i && i < V; Perm(g_cost[i], write));

  kernel_invariant (\forall int i; 0 <= i && i < A; start[i] == start_seq[i]);
  kernel_invariant (\forall int i; 0 <= i && i < A; end[i] == end_seq[i]);
  kernel_invariant (\forall int i; 0 <= i && i < A; weight[i] == weight_seq[i]); 
  kernel_invariant (\forall int i; 0 <= i && i < A; contrib1[i] == 0) ==> (\forall int i; 0 <= i && i < V; oldcost_seq[i] == cost_seq[i]);
  kernel_invariant (\forall int i; 0 <= i && i < V; cost_seq[i] == inf() ==> oldcost_seq[i] == cost_seq[i]);
  kernel_invariant (\forall int i; 0 <= i && i < V; oldcost_seq[i] == inf() ==> oldcost_seq[i] <= cost_seq[i]);
  kernel_invariant (\forall int i; 0 <= i && i < V; oldcost_seq[i] != inf() ==> cost_seq[i] <= oldcost_seq[i]); 
  kernel_invariant (\forall int i; 0 <= i && i < V && i != source; oldcost_seq[i] == inf() || oldcost_seq[i] > 0); 
  kernel_invariant oldcost_seq[source] == 0;
  kernel_invariant (\forall int i; 0 <= i && i < V && i != source; cost_seq[i] == inf() || cost_seq[i] > 0);
  kernel_invariant cost_seq[source] == 0;
  kernel_invariant (\forall int i; 0 <= i && i < V; cost[i] == cost_seq[i]);
  kernel_invariant (\forall int i; 0 <= i && i < A && contrib[i] == 1 && oldcost_seq[start_seq[i]] != inf(); cost_seq[end_seq[i]] != inf());
  kernel_invariant (\forall int a; 0 <= a && a < A && contrib1[a] == 1 && (oldcost_seq[start[a]] != inf()) && 
                    (oldcost_seq[end[a]] == inf() || oldcost_seq[start[a]] + weight_seq[a] <= oldcost_seq[end[a]]); cost[end[a]] <= oldcost_seq[start[a]] + weight_seq[a]);
  kernel_invariant (\forall int i; 0 <= i && i < A && cost_seq[start[i]] != inf(); ExPathEqual(V, A, start_seq, end_seq, weight_seq, source, start[i], cost_seq[start[i]])); // inv 10
  kernel_invariant (\forall int i; 0 <= i && i < A && cost_seq[end[i]] != inf(); ExPathEqual(V, A, start_seq, end_seq, weight_seq, source, end[i], cost_seq[end[i]])); // inv 10 
  kernel_invariant (\forall int i; 0 <= i && i < V && cost_seq[i] != inf(); ExPathEqual(V, A, start_seq, end_seq, weight_seq, source, i, cost_seq[i])); // inv 10
  kernel_invariant (\forall int v; 0 <= v && v < V && oldcost_seq[v] != inf(); ExPathEqual(V, A, start_seq, end_seq, weight_seq, source, v, oldcost_seq[v])); // inv 10
  kernel_invariant (\forall int vertex; 0 <= vertex && vertex < V && oldcost_seq[vertex] != inf(); 
                    (\forall seq<int> P; Path(V, A, start_seq, end_seq, weight_seq, source, vertex, P) && |P| <= counter[0]; cost(V, A, start_seq, end_seq, weight_seq, P) >= oldcost_seq[vertex])); // inv 11
  kernel_invariant (\forall int v; 0 <= v && v < V && ExPath(V, A, start_seq, end_seq, weight_seq, source, v, counter[0]); oldcost_seq[v] != inf()); // inv 12
  

  requires \pointer_index(g_start, \gtid, 1\4);
  requires \pointer_index(g_end, \gtid, 1\4);
  requires \pointer_index(g_weight, \gtid, 1\4);
  ensures contrib[\gtid] == 1;
@*/
__global__ void CUDAKernel(int* g_start, int* g_end, int* g_weight, int* g_cost, int V, int A, int counter, int source)
{
  int tid = blockIdx.x * V + threadIdx.x;
  //@ assert tid == \gtid;

  atomicRelax(g_cost+g_end[tid], g_weight[tid], g_cost[g_start[tid]]) 
    /*@ then {
        cost_seq[end[tid]] = (compare(oldcost_seq[start_seq[tid]], weight_seq[tid], oldcost_seq[end_seq[tid]]) ?  (cost[start[tid]] + weight[tid]) : cost_seq[end[tid]]);
                compare(oldcost_seq[start_seq[tid]], weight_seq[tid], oldcost_seq[end_seq[tid]]) ? lemma_one_path(V, A, start_seq, end_seq, weight_seq) : true;
                compare(oldcost_seq[start_seq[tid]], weight_seq[tid], oldcost_seq[end_seq[tid]]) ? ExPathEqual(V, A, start_seq, end_seq, weight_seq, start[tid], end[tid], weight[tid]) : true;
                compare(oldcost_seq[start_seq[tid]], weight_seq[tid], oldcost_seq[end_seq[tid]]) ? lemma_expath_trans(V, A, start_seq, end_seq, weight_seq, source, start[tid], end[tid], cost_seq[start[tid]], weight[tid]) : true;
                compare(oldcost_seq[start_seq[tid]], weight_seq[tid], oldcost_seq[end_seq[tid]]) ? (cost[end[tid]] == cost[start[tid]] + weight[tid]) : true;
                compare(oldcost_seq[start_seq[tid]], weight_seq[tid], oldcost_seq[end_seq[tid]]) ? ExPathEqual(V, A, start_seq, end_seq, weight_seq, source, end[tid], cost_seq[start[tid]]+weight[tid]) : true;
                contrib[tid] = 1;
    } @*/;
   
    /* I would like to have such an atomic (block) which I encoded it as "atomicRelax"
  Atomic{
  
    if(atomicCompare(g_cost[g_end[tid]], g_weight[tid], g_cost[g_start[tid]])){
	
	    g_cost[g_end[tid]] = g_cost[g_start[tid]] + g_weight[tid];

	    //@ ghost cost_seq = cost_seq[end[tid] -> (cost[start[tid]] + weight[tid])];
      //@ ghost lemma_one_path(V, A, start_seq, end_seq, weight_seq);
      //@ assert ExPathEqual(V, A, start_seq, end_seq, weight_seq, start[tid], end[tid], weight[tid]); 
      //@ assert lemma_expath_trans(V, A, start_seq, end_seq, weight_seq, source, start[tid], end[tid], cost_seq[start[tid]], weight[tid]);
      //@ assert cost[end[tid]] == cost[start[tid]] + weight[tid];
      //@ assert ExPathEqual(V, A, start_seq, end_seq, weight_seq, source, end[tid], cost_seq[start[tid]]+weight[tid]);  
 
        
    }

   }//@ then {contrib[tid] = 1;} @//;	
    
   requires start_cost == inf() || start_cost >= 0;
   requires end_cost == inf() || end_cost >= 0;
   requires weight > 0;
   ensures (start_cost != inf()) && (end_cost == inf() || start_cost + weight <= end_cost) ==> true;
   ensures true ==> (start_cost != inf()) && (end_cost == inf() || start_cost + weight <= end_cost);
   ensures (start_cost == inf()) || (end_cost != inf() && start_cost + weight > end_cost) ==> false;
   ensures false ==> (start_cost == inf()) || (end_cost != inf() && start_cost + weight > end_cost);
   ensures (true && end_cost != inf()) ==> start_cost + weight <= end_cost;
   boolean atomicCompare(int start_cost, int weight, int end_cost) =
    (start_cost != inf()) && (end_cost == inf() || start_cost + weight <= end_cost) ?
          true
    :
          false;
    */  


}

////////////////////////////////////////////////////////////////////////////////
//@ ensures \pointer(\result, N, write);
int *vercorsMallocInt(int N);
void vercorsFreeInt(int *ar);
//@ ensures \pointer(\result, N, write);
int *vercorsCudaMallocInt(int N);
void vercorsCudaFreeInt(int *addr);
void vercorsCudaMemcpyInt(int *tgt, int *src, int N, int direction); 

////////////////////////////////////////////////////////////////////////////////
// Main Program
////////////////////////////////////////////////////////////////////////////////
int main(int argc, char** argv) 
{
	
	int V = 1024; // no. of vertices
	int A = 10 * V; // no. of arcs
    	
	// allocate host memory
  int* host_start = vercorsMallocInt(A);
  int* host_end = vercorsMallocInt(A);
  int* host_weight = vercorsMallocInt(A);
  int* host_cost = vercorsMallocInt(V);
  
  int source = V/2;
  
  // these assumptions come from the input graph file which we change it into assertions 
  //@ assume (\forall int i; 0 <= i && i < A; host_start[i] >= 0 && host_start[i] < V);
  //@ assume (\forall int i; 0 <= i && i < A; host_end[i] >= 0 && host_end[i] < V);
  //@ assume (\forall int i; 0 <= i && i < A; host_start[i] != host_end[i]); // No self-loop
  
  //@ assume (\forall int i; 0 <= i && i < A; host_weight[i] > 0);
  
  //@ assume (\forall int i; 0 <= i && i < A; (\forall int j; 0 <= j && j < A && i != j; host_start[i] == host_start[j] ==> host_end[i] != host_end[j])); 
     
  for(unsigned int i = 0; i < V; i++) 
  {
    host_cost[i] = -1;
  }
  host_cost[source] = 0;
  
  //@ assert (\forall int i; 0 <= i && i < V && i != source; host_cost[i] == inf());
  
  //@ assert host_cost[source] == 0;

  //Copy the arrays to device memory
  int* device_start;
  device_start = vercorsCudaMallocInt(A);
  vercorsCudaMemcpyInt( device_start, host_start, A, cudaMemcpyHostToDevice) ;

  //Copy the arrays to device memory
  int* device_end;
  device_end = vercorsCudaMallocInt(A);
  vercorsCudaMemcpyInt( device_end, host_end, A, cudaMemcpyHostToDevice) ;
  
  //Copy the arrays to device memory
  int* device_weight;
  device_weight = vercorsCudaMallocInt(A);
  vercorsCudaMemcpyInt( device_weight, host_weight, A, cudaMemcpyHostToDevice) ;


  //Copy the arrays to device memory
  int* device_cost;
  device_cost = vercorsCudaMallocInt(V);
  vercorsCudaMemcpyInt( device_cost, host_cost, V, cudaMemcpyHostToDevice) ;
  
    
  //setup execution parameters
	int num_of_blocks = 10;
	int num_of_threads_per_block = V;
  
  //dim3  grid( num_of_blocks, 1, 1); //grid has three parameters to indicate the dimensions. Here we have one dimensional grid (of blocks). It can be one, two or three dimensions.
  //dim3  threads( num_of_threads_per_block, 1, 1); //threads indicates the dimensions of one block. Here each block has one dimension (of threads). It can be one, two or three dimensions.
  
  int counter = 0;

  //@ ghost seq<int> contrib;
  //@ assume (\forall int i; i >= 0 && i < A; contrib[i] == 0);
  
  //@ ghost seq<int> cost_seq ;
  //@ assume |cost_seq| == V && (\forall int i; i >= 0 && i < V; cost_seq[i] == host_cost[i]);
  
  //@ ghost seq<int> oldcost_seq ; 
  //@ assume |oldcost_seq| == V && (\forall int i; i >= 0 && i < V; oldcost_seq[i] == host_cost[i]);
  
  //@ ghost seq<int> start_seq ; 
  //@ assume |start_seq| == A && (\forall int i; i >= 0 && i < A; start_seq[i] == host_start[i]);
  
  //@ ghost seq<int> end_seq ;
  //@ assume |end_seq| == A && (\forall int i; i >= 0 && i < A; end_seq[i] == host_end[i]);
  
  //@ ghost seq<int> weight_seq  ;
  //@ assume |weight_seq| == A && (\forall int i; i >= 0 && i < A; weight_seq[i] == host_weight[i]);

  



  //@ loop_invariant V > 0 && A > 0;
  //@ loop_invariant source >= 0 && source < V;
  //@ loop_invariant counter.length == 1;
  //@ loop_invariant Perm(counter[0], write);
  //@ loop_invariant counter[0] >= 0 && counter[0] <= V - 1;
  //@ loop_invariant start != null;
  //@ loop_invariant weight != null;
  //@ loop_invariant end != null;
  //@ loop_invariant cost != null;
  //@ loop_invariant start.length == A;
  //@ loop_invariant weight.length == A;
  //@ loop_invariant end.length == A;
  //@ loop_invariant cost.length == V;
  //@ loop_invariant Perm(start_seq, read);
  //@ loop_invariant |start_seq| == A;
  //@ loop_invariant Perm(weight_seq, read);
  //@ loop_invariant |weight_seq| == A;
  //@ loop_invariant Perm(end_seq, read);
  //@ loop_invariant |end_seq| == A;
  //@ loop_invariant Perm(cost_seq, 1);
  //@ loop_invariant Perm(oldcost_seq, 1);
  //@ loop_invariant |cost_seq| == V;
  //@ loop_invariant |oldcost_seq| == V;
  //@ loop_invariant (\forall int i; 0 <= i && i < V; oldcost_seq[i] == cost_seq[i]);
  //@ loop_invariant contrib1 != null;
  //@ loop_invariant contrib1.length == A;
  //@ loop_invariant (\forall* int i; 0 <= i && i < A; Perm(contrib1[i], write));
  //@ loop_invariant (\forall int i; 0 <= i && i < A; contrib1[i] == 0);
  //@ loop_invariant (\forall* int i; 0 <= i && i < A; Perm(start[i], read));
  //@ loop_invariant (\forall int i; 0 <= i && i < A; start[i] >= 0 && start[i] < V);
  //@ loop_invariant (\forall* int i; 0 <= i && i < A; Perm(end[i], read));
  //@ loop_invariant (\forall int i; 0 <= i && i < A; end[i] >= 0 && end[i] < V);
  //@ loop_invariant (\forall int i; 0 <= i && i < A; start[i] != end[i]);
  //@ loop_invariant (\forall int i; 0 <= i && i < A; (\forall int j; 0 <= j && j < A && i != j; start[i] == start[j] ==> end[i] != end[j]));
  //@ loop_invariant (\forall int i; 0 <= i && i < A; start[i] == start_seq[i]);
  //@ loop_invariant (\forall int i; 0 <= i && i < A; end[i] == end_seq[i]);
  //@ loop_invariant (\forall* int i; 0 <= i && i < A; Perm(weight[i], read));
  //@ loop_invariant (\forall int i; 0 <= i && i < A; weight[i] == weight_seq[i]);
  //@ loop_invariant (\forall int i; 0 <= i && i < A; weight[i] > 0);
  //@ loop_invariant (\forall* int i; 0 <= i && i < V; Perm(cost[i], write));
  //@ loop_invariant (\forall int i; 0 <= i && i < V && i != source; cost_seq[i] == inf() || cost_seq[i] > 0);
  //@ loop_invariant cost[source] == 0;
  //@ loop_invariant (\forall int i; 0 <= i && i < V; cost[i] == cost_seq[i]);
  //@ loop_invariant (\forall int i; 0 <= i && i < V && cost_seq[i] != inf(); ExPathEqual(V, A, start_seq, end_seq, weight_seq, source, i, cost_seq[i]));
  //@ loop_invariant (\forall int vertex; 0 <= vertex && vertex < V && cost_seq[vertex] != inf(); (\forall seq<int> P; Path(V, A, start_seq, end_seq, weight_seq, source, vertex, P) && |P| <= counter[0]; cost_seq[vertex] <= cost(V, A, start_seq, end_seq, weight_seq, P)));
  //@ loop_invariant (\forall int v; 0 <= v && v < V && ExPath(V, A, start_seq, end_seq, weight_seq, source, v, counter[0]); cost_seq[v] != inf());
  while(counter < V-1)
  {  
	  //Kernel launch
    CUDAKernel<<< /*grid*/num_of_blocks, /*threads*/num_of_threads_per_block/*, 0*/ >>>(device_start, device_end, device_weight, device_cost, V, A, counter, source)
        /*@ with { contrib=contrib; cost_seq=cost_seq; oldcost_seq=oldcost_seq; start_seq=start_seq; end_seq=end_seq; weight_seq=weight_seq; } @*/;
    
    //@ ghost contrib[A] = 0 ;
    //@ assume (\forall int i; i >= 0 && i < A; contrib[i] == 0);
    
    //@ assert lemma_inv12_preserved(V, A, start_seq, end_seq, weight_seq, oldcost_seq, cost_seq, source, counter);
    
    //@ assert lemma_inv11_preserved(V, A, start_seq, end_seq, weight_seq, oldcost_seq, cost_seq, source, counter);
    
    //@ ghost oldcost_seq = cost_seq ;
    //@ assume (\forall int i; i >= 0 && i < V; oldcost_seq[i] == cost_seq[i]);

    counter = counter + 1;
  }
  
  //Lemma 1
  //@ assert simple_path_lemma1_preserved(V, A, start_seq, end_seq, weight_seq, source);
  //@ assert (\forall int v; v >= 0 && v < V; (\forall seq<int> P; SimplePath(V, A, start_seq, end_seq, weight_seq, source, v, P); |P| < V));
  
  //Lemma 2
  //@ assert simple_path_Part_1_lemma2_preserved(V, A, start_seq, end_seq, weight_seq, source);
  //@ assert (\forall int v; v >= 0 && v < V; (\forall seq<int> P; SimplePath(V, A, start_seq, end_seq, weight_seq, source, v, P); Path(V, A, start_seq, end_seq, weight_seq, source, v, P)));
  //@ assert simple_path_Part_2_lemma2_preserved(V, A, start_seq, end_seq, weight_seq, source);
  //@assert (\forall int v; v >= 0 && v < V; (\forall seq<int> P; Path(V, A, start_seq, end_seq, weight_seq, source, v, P); Path(V, A, start_seq, end_seq, weight_seq, source, v, convert(V, A, start_seq, end_seq, weight_seq, source, v, P))));
  //@ assert (\forall int v; v >= 0 && v < V; (\forall seq<int> P; Path(V, A, start_seq, end_seq, weight_seq, source, v, P); SimplePath(V, A, start_seq, end_seq, weight_seq, source, v, convert(V, A, start_seq, end_seq, weight_seq, source, v, P))));
  
  //Lemma 3
  //@ assert simple_path_lemma3_preserved(V, A, start_seq, end_seq, weight_seq, source);
  //@ assert (\forall int v; v >= 0 && v < V;(\forall seq<int> P; Path(V, A, start_seq, end_seq, weight_seq, source, v, P); |convert(V, A, start_seq, end_seq, weight_seq, source, v, P)| <= |P|));
                     
  //Lemma 4
  //@ assert simple_path_cost_lemma4(V, A, start_seq, end_seq, weight_seq, source);
  //@ assert (\forall int v; v >= 0 && v < V; (\forall seq<int> P; Path(V, A, start_seq, end_seq, weight_seq, source, v, P); cost(V, A, start_seq, end_seq, weight_seq, convert(V, A, start_seq, end_seq, weight_seq, source, v, P)) <= cost(V, A, start_seq, end_seq, weight_seq, P)));
           
                    
  
  //@ assert (\forall int vertex; 0 <= vertex && vertex < V && cost_seq[vertex] != inf(); (\forall seq<int> P; Path(V, A, start_seq, end_seq, weight_seq, source, vertex, P); cost_seq[vertex] <= cost(V, A, start_seq, end_seq, weight_seq, P))); // prop. 8
  
  //@ assert (\forall int v; 0 <= v && v < V && (\exists seq<int> P; Path(V, A, start_seq, end_seq, weight_seq, source, v, P); true); cost_seq[v] != inf()); // prop. 9
  
  
  // copy result from device to host
  vercorsCudaMemcpyInt(host_cost, device_cost, V, cudaMemcpyDeviceToHost);	
	 
  
  // cleanup memory
  vercorsFreeInt(host_start);
  vercorsFreeInt(host_end);
  vercorsFreeInt(host_weight);
  vercorsFreeInt(host_cost);
  vercorsCudaFreeInt(device_start);
  vercorsCudaFreeInt(device_end);
  vercorsCudaFreeInt(device_weight);
  vercorsCudaFreeInt(device_cost);

	return 0;
       
}

