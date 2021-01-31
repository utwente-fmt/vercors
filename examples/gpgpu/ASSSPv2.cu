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







////////////////////////////////////////////////////////////////////////////////
//compare
////////////////////////////////////////////////////////////////////////////////  
  /*@
  requires start_cost == inf() || start_cost >= 0;
  requires end_cost == inf() || end_cost >= 0;
  requires weight > 0;
  ensures (start_cost != inf()) && (end_cost == inf() || start_cost + weight <= end_cost) ==> \result;
  ensures \result ==> (start_cost != inf()) && (end_cost == inf() || start_cost + weight <= end_cost);
  ensures (start_cost == inf()) || (end_cost != inf() && start_cost + weight > end_cost) ==> !\result;
  ensures !\result ==> (start_cost == inf()) || (end_cost != inf() && start_cost + weight > end_cost);
  ensures (\result && end_cost != inf()) ==> start_cost + weight <= end_cost;
  pure bool compare(int start_cost, int weight, int end_cost) = 
    (start_cost != inf() && (end_cost == inf() || start_cost+weight <= end_cost)) ? true : false;
  @*/


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
  
  //@ loop_invariant i >= 0 && i <= V;
  //@ loop_invariant (\forall* int k; 0 <= k && k < V; Perm(host_cost[k], write));
  //@ loop_invariant (\forall int k; 0 <= k && k < i; host_cost[k] == -1);   
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
  //@ assume (\forall int i; i >= 0 && i < A; host_start[i] == device_start[i]);

  //Copy the arrays to device memory
  int* device_end;
  device_end = vercorsCudaMallocInt(A);
  vercorsCudaMemcpyInt( device_end, host_end, A, cudaMemcpyHostToDevice) ;
  //@ assume (\forall int i; i >= 0 && i < A; host_end[i] == device_end[i]);
  
  //Copy the arrays to device memory
  int* device_weight;
  device_weight = vercorsCudaMallocInt(A);
  vercorsCudaMemcpyInt( device_weight, host_weight, A, cudaMemcpyHostToDevice) ;
  //@ assume (\forall int i; i >= 0 && i < A; host_weight[i] == device_weight[i]);


  //Copy the arrays to device memory
  int* device_cost;
  device_cost = vercorsCudaMallocInt(V);
  vercorsCudaMemcpyInt( device_cost, host_cost, V, cudaMemcpyHostToDevice) ;
  //@ assume (\forall int i; i >= 0 && i < V; host_cost[i] == device_cost[i]);
  
    
  //setup execution parameters
	int num_of_blocks = 10;
	int num_of_threads_per_block = V;
  
  //dim3  grid( num_of_blocks, 1, 1); //grid has three parameters to indicate the dimensions. Here we have one dimensional grid (of blocks). It can be one, two or three dimensions.
  //dim3  threads( num_of_threads_per_block, 1, 1); //threads indicates the dimensions of one block. Here each block has one dimension (of threads). It can be one, two or three dimensions.
  
  int counter = 0;

  //@ ghost seq<int> contrib; 
  //@ assume |contrib| == A && (\forall int i; i >= 0 && i < A; contrib[i] == 0);
  
  //@ ghost seq<int> contrib2; 
  //@ assume |contrib2| == A && (\forall int i; i >= 0 && i < A; contrib2[i] == 0);
  
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

  

  

  //@ loop_invariant V == 1024 && A == 10 * V;
  //@ loop_invariant source >= 0 && source < V;
  //@ loop_invariant counter >= 0 && counter <= V - 1;
  //@ loop_invariant |start_seq| == A;
  //@ loop_invariant |weight_seq| == A;
  //@ loop_invariant |end_seq| == A;
  //@ loop_invariant |cost_seq| == V;
  //@ loop_invariant |oldcost_seq| == V;
  //@ loop_invariant |contrib| == A;
  //@ loop_invariant |contrib2| == A;
  //@ loop_invariant (\forall int i; 0 <= i && i < V; oldcost_seq[i] == cost_seq[i]);
  //@ loop_invariant (\forall int i; 0 <= i && i < A; contrib[i] == 0);
  //@ loop_invariant (\forall int i; 0 <= i && i < A; contrib2[i] == 0);
  //@ loop_invariant (\forall* int i; 0 <= i && i < A; Perm(host_start[i], read)); 
  //@ loop_invariant (\forall int i; 0 <= i && i < A; host_start[i] >= 0 && host_start[i] < V);
  //@ loop_invariant (\forall* int i; 0 <= i && i < A; Perm(host_end[i], read)); 
  //@ loop_invariant (\forall int i; 0 <= i && i < A; host_end[i] >= 0 && host_end[i] < V);
  //@ loop_invariant (\forall int i; 0 <= i && i < A; host_start[i] != host_end[i]); 
  //@ loop_invariant (\forall int i; 0 <= i && i < A; (\forall int j; 0 <= j && j < A && i != j; host_start[i] == host_start[j] ==> host_end[i] != host_end[j]));		
  //@ loop_invariant (\forall int i; 0 <= i && i < A; host_start[i] == start_seq[i]);
  //@ loop_invariant (\forall int i; 0 <= i && i < A; host_end[i] == end_seq[i]);	
  //@ loop_invariant (\forall* int i; 0 <= i && i < A; Perm(host_weight[i], read));
  //@ loop_invariant (\forall int i; 0 <= i && i < A; host_weight[i] == weight_seq[i]); 
  //@ loop_invariant (\forall int i; 0 <= i && i < A; host_weight[i] > 0);							
  //@ loop_invariant (\forall* int i; 0 <= i && i < V; Perm(host_cost[i], write));
  //@ loop_invariant (\forall int i; 0 <= i && i < V && i != source; cost_seq[i] == inf() || cost_seq[i] > 0);
  //@ loop_invariant host_cost[source] == 0;  								
  //@ loop_invariant (\forall int i; 0 <= i && i < V; host_cost[i] == cost_seq[i]);
  //@ loop_invariant (\forall int i; 0 <= i && i < V && cost_seq[i] != inf(); ExPathEqual(V, A, start_seq, end_seq, weight_seq, source, i, cost_seq[i])); 
  while(counter < V-1)
  {  
    
    
    
    //@ ghost contrib = contrib2 ;
    //@ assert (\forall int i; i >= 0 && i < A; contrib[i] == 0);
    
    //@ ghost oldcost_seq = cost_seq ;
    //@ assert (\forall int i; i >= 0 && i < V; oldcost_seq[i] == cost_seq[i]);

    counter = counter + 1;
  }
  
  
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

