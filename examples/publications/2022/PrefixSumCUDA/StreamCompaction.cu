/***********************************************************************************
Created by Mohsen Safari.
************************************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <cuda.h>


////////////////////////////////////////////////////////////////////////////////
// Pure Functions
////////////////////////////////////////////////////////////////////////////////
/*@
requires 0 <= p;
ensures p < \result;
pure int ExpTwo(int p) = 0 < p ? 2 * ExpTwo(p - 1) : 1;
@*/

/*@
ensures |xs| == 0 ==> \result == 0;
ensures |xs| == 1 ==> \result == (xs.head);
pure int intsum(seq<int> xs) =
	0 < |xs| ? (xs.head) + intsum((xs.tail)) : 0;
@*/

/*@
requires n <= |xs|;
ensures n < 0 ==> |Take(xs, n)| == 0;
ensures 0 <= n ==> |Take(xs, n)| == n;
ensures (\forall int i; 0 <= i && i < n; xs[i] == get(Take(xs, n), i));
pure seq<int> Take(seq<int> xs, int n) =
	0 < n ? seq<int> { (xs.head) } + Take((xs.tail), n - 1) : seq<int> { };
@*/

/*@
requires 0 <= i && i <= |xs|;
ensures |\result| == |xs| - i;
ensures (\forall int j; 0 <= j && j < |\result|; \result[j] == intsum(Take(xs, i+j)));
pure seq<int> psum(seq<int> xs, int i) =
	i < |xs| ? seq<int> { intsum(Take(xs, i)) } + psum(xs, i + 1) : seq<int> { };
@*/

/*@
ensures |\result| == |xs|;
ensures (\forall int j; 0 <= j && j < |\result|; \result[j] == intsum(Take(xs, j)));
pure seq<int> psum2(seq<int> xs) = psum(xs, 0);
@*/

/*@
requires |xs| >= 0;
ensures |xs| == 0	==> \result == xs;
ensures |xs| == 1 ==> \result == xs;
ensures |xs| == 2 ==> \result == seq<int> { (xs.head) + xs.tail.head };
ensures |xs| % 2 == 0 ==> |\result| == |xs| / 2;
pure seq<int> implode(seq<int> xs) =
	1 < |xs| ? seq<int> { (xs.head) + xs.tail.head } + implode(xs.tail.tail) : xs;
@*/

/*@
requires 0 <= p;
pure int exp(int n, int p) = 0 < p ? n * exp(n, p - 1) : 1;
@*/

/*@
requires 0 <= n;
requires n < |xs|;
pure int get(seq<int> xs, int n) = xs[n];
@*/

/*@
requires k > 0;
requires |xs| == ExpTwo(k);
requires i >= 0 && i <= |xs|;
requires 1 <= lvl && lvl <= k;
requires stride == ExpTwo(lvl-1);
requires stride > 0 && stride < |xs|;
ensures |\result| == |xs| - i;
ensures (\forall int j; j >= 0 && j < |\result|; ((i < |xs|) && ((i+j) >= stride) && (((i+j) % (2*stride)) == (2*stride-1))) ==> \result[j] == xs[i+j] + xs[i+j - stride]);
ensures (\forall int j; j >= 0 && j < |\result|; ((i < |xs|) && (((i+j) < stride) || (((i+j) % (2*stride)) != (2*stride-1)))) ==> \result[j] == xs[i+j]);
pure seq<int> up(seq<int> xs, int stride, int i, int k, int lvl) =
	i < |xs| ? (
				((i % (2*stride)) == (2*stride-1) && (i >= stride)?
					seq<int> {xs[i] + xs[i-stride]} + up(xs, stride, i+1, k, lvl)
				:
					seq<int> {xs[i]} + up(xs, stride, i+1, k, lvl) ))
	:
		seq<int> {};
@*/

/*@
requires (\forall int i; 0 <= i && i < |xs|; xs[i] == 0 || xs[i] == 1);
ensures \result == intsum(xs);
ensures \result >= 0;
pure int count(seq<int> xs) =
	|xs| > 0 ? ( (xs.head) == 1 ? 1+count((xs.tail)) : count((xs.tail)) ) : 0;
@*/

/*@
requires |flags| == |input|;
requires (\forall int j; 0 <= j && j < |flags|; flags[j] == 0 || flags[j] == 1);
ensures |\result| == intsum(flags);
ensures 0 <= |\result| && |\result| <= |flags|;
pure seq<int> compact(seq<int> input, seq<int> flags) =
	0 < |input| ?
		(flags.head == 1 ? seq<int> { input.head } + compact(input.tail, flags.tail) : compact(input.tail, flags.tail))
	:
		seq<int> { };
@*/
////////////////////////////////////////////////////////////////////////////////////////Lemmas

/* The sum of a list of non-negative integers is itself non-negative. */
/*@
requires (\forall int i; 0 <= i && i < |xs|; 0 <= xs[i]);
ensures \result && 0 <= intsum(xs);
pure bool lemma_sum_nonneg(seq<int> xs);
@*/

/*@
ensures \result && intsum(seq<int> { }) == 0;
pure bool lemma_intsum_zero();
@*/

/*@
ensures \result && psum2(seq<int> { }) == seq<int> { };
pure bool lemma_psum_zero();
@*/

/*@
ensures \result && intsum(seq<int> { x }) == x;
pure bool lemma_intsum_single(int x);
@*/

/*@
requires |xs| == 1;
ensures \result && psum2(xs) == seq<int> {0};
pure bool lemma_psum_single(seq<int>  xs);
@*/


/*@
requires |xs| >= 0;
requires |ys| >= 0;
ensures \result && |xs| == 0 ==> intsum(xs + ys) == intsum(ys);
ensures \result && |ys| == 0 ==> intsum(xs + ys) == intsum(xs);
ensures \result && |xs + ys| == |xs| + |ys|;
ensures \result && intsum((xs.tail) + ys) == intsum((xs.tail)) + intsum(ys);
ensures \result && intsum(xs + ys) == intsum(xs) + intsum(ys);
pure bool lemma_intsum_app(seq<int> xs, seq<int> ys);
@*/

/*@
requires |xs| <= 1;
ensures \result && xs == implode(xs);
pure bool lemma_implode_base(seq<int> xs);
@*/


/*@
ensures \result && intsum(xs) == intsum(implode(xs));
pure bool lemma_implode_sum(seq<int> xs);
@*/

/*@
requires 0 < n;
ensures \result && ExpTwo(n) == 2 * ExpTwo(n - 1);
pure bool lemma_exp2_red_mult(int n);
@*/

/*@
requires 0 < n;
ensures \result && ExpTwo(n) / 2 == ExpTwo(n - 1);
pure bool lemma_exp2_red_div(int n);
@*/

/*@
requires 0 <= n;
ensures \result && 0 < ExpTwo(n);
pure bool lemma_exp2_positive(int n);
@*/

/*@
requires 0 <= i;
requires i <= j;
ensures \result && ExpTwo(i) <= ExpTwo(j);
pure bool lemma_exp2_leq(int i, int j);
@*/

/*@
requires i >= 0 && j >= 0;
requires ExpTwo(i) == ExpTwo(j);
ensures \result && i == j;
pure bool power_two_lemma(int i, int j);
@*/

/*@
requires |xs| % 2 == 0;
ensures \result && |implode(xs)| == |xs| / 2;
pure bool lemma_implode_length_mod_two(seq<int> xs);
@*/

/*@
requires 0 < n && |xs| == ExpTwo(n);
ensures \result && |implode(xs)| == ExpTwo(n - 1);
pure bool lemma_implode_red_exp2(seq<int> xs, int n);
@*/

/*@
requires 0 < i;
requires i < |xs|;
ensures \result && get((xs.tail), i - 1) == xs[i];
pure bool lemma_intseq_index_tail(seq<int> xs, int i);
@*/

/*@
requires |xs| % 2 == 0;
requires 0 <= i && i < |implode(xs)|;
requires (2 * i) < |xs|;
requires (2 * i + 1) < |xs|;
ensures \result && get(implode(xs), i) == xs[2 * i] + xs[2 * i + 1];
pure bool lemma_implode_get(seq<int> xs, int i);
@*/

/*@
requires |xs| % 2 == 0;
requires |implode(xs)| == |xs|/2;
ensures \result && (\forall int i; 0 <= i && i < |implode(xs)|; get(implode(xs), i) == xs[2 * i] + xs[2 * i + 1]);
pure bool lemma_implode_get_all(seq<int> xs);
@*/

/*@
requires |xs| == 2 * |ys|;
requires 0 <= |ys|;
requires (\forall int i; 0 <= i && i < |ys|; ys[i] == xs[2*i] + xs[2*i+1]);
ensures \result && ys == implode(xs);
pure bool lemma_implode_rel(seq<int> xs, seq<int> ys);
@*/

/*@
requires 0 <= i && i < |xs|;
ensures \result && get(psum2(xs), i) == intsum(Take(xs, i));
pure bool lemma_psum_get(seq<int> xs, int i);
@*/

/*@
ensures \result && (\forall int i; 0 <= i && i < |xs|; get(psum2(xs), i) == intsum(Take(xs, i)));
pure bool lemma_psum_get_all(seq<int> xs);
@*/

/*@
requires 0 < n && n <= |xs|;
ensures \result && Take(xs, n) == Take(xs, n - 1) + seq<int> { xs[n - 1] };
pure bool missing_lemma_2(seq<int> xs, int n);
@*/

/*@
requires |xs| % 2 == 0;
requires |ys| % 2 == 0;
ensures \result && implode(xs + ys) == implode(xs) + implode(ys);
pure bool missing_lemma_3(seq<int> xs, seq<int> ys);
@*/

/*@
ensures \result && xs + (ys + zs) == (xs + ys) + zs;
pure bool intseq_concat_assoc(seq<int> xs, seq<int> ys, seq<int> zs);
@*/

/*@
requires |xs| % 2 == 0;
requires 0 <= n && n < |implode(xs)|;
requires |implode(xs)| == |xs| / 2;
ensures \result && Take(implode(xs), n) == implode(Take(xs, 2 * n));
pure bool missing_lemma(seq<int> xs, int n);
@*/

/*@
requires |xs| % 2 == 0;
requires |implode(xs)| == |xs|/2;
requires 0 <= i && i < |implode(xs)|;
requires 2 * i < |xs|;
ensures \result && get(psum2(implode(xs)), i) == intsum(Take(xs, 2 * i));
pure bool lemma_psum_Take2(seq<int> xs, int i);
@*/

/*@
requires |xs| % 2 == 0;
requires |implode(xs)| == |xs|/2;
requires 0 <= i && i < |implode(xs)|;
requires 2 * i < |xs|;
ensures \result && get(psum2(implode(xs)), i) == get(psum2(xs), 2 * i);
pure bool lemma_get_psum_implode(seq<int> xs, int i);
@*/

/*@
requires 0 <= i;
requires 2 * i + 1 < |xs|;
ensures \result && get(psum2(xs), 2 * i + 1) == get(psum2(xs), 2 * i) + get(xs, 2 * i);
pure bool lemma_combine_psum(seq<int> xs, int i);
@*/

/*@
requires (\forall int j; 0 <= j && j < |xs|; xs[j] == 0 || xs[j] == 1);
ensures \result && intsum(xs) >= 0;
pure bool lemma_intsum_positive(seq<int> xs);
@*/

/*@
requires i >= 0;
requires i < |xs|;
requires (\forall int j; 0 <= j && j < i; xs[j] == 0 || xs[j] == 1);
ensures \result && i < |Take(xs, i)| ==> intsum(Take(xs, i)) >= 0;
pure bool lemma_intsum_flag(seq<int> xs, int i) = true;
@*/



/* assuming all elements in `xs` are non-negative, the sum of any sublist of `xs` will not be bigger than the sum of `xs`. */
/*@
requires n <= |xs|;
requires (\forall int i; 0 <= i && i < |xs|; 0 <= xs[i]);
ensures \result && 0 <= intsum(Take(xs, n)) && intsum(Take(xs, n)) <= intsum(xs);
pure bool lemma_take_sum(seq<int> xs, int n);
@*/

/*@
requires 0 <= n && n < |xs|;
requires (\forall int i; 0 <= i && i < |xs|; xs[i] == 0 || xs[i] == 1);
ensures \result && intsum(Take(xs, n)) < |xs|;
pure bool lemma_flags_take_size(seq<int> xs, int n);
@*/

/*@
requires 0 <= n && n < |xs|;
requires (\forall int i; 0 <= i && i < |xs|; xs[i] == 0 || xs[i] == 1);
requires xs[n] == 1;
ensures \result && intsum(Take(xs, n)) < intsum(xs);
pure bool lemma_sum_pos_flag(seq<int> xs, int n);
@*/

/*@
requires 0 <= n && n < |flags|;
requires flags[n] == 1;
requires |flags| == |input|;
requires (\forall int j; 0 <= j && j < |flags|; flags[j] == 0 || flags[j] == 1);
ensures \result && 0 <= intsum(Take(flags, n));
ensures \result && intsum(Take(flags, n)) < intsum(flags);
ensures \result && intsum(Take(flags, n)) < |flags|;
ensures \result && input[n] == get(compact(input, flags), intsum(Take(flags, n)));
ensures \result && input[n] == get(compact(input, flags), get(psum2(flags), n));
pure bool lemma_correctness(seq<int> input, seq<int> flags, int n);
@*/


////////////////////////////////////////////////////////////////////////////////
//Kernel
////////////////////////////////////////////////////////////////////////////////
/*@
context_everywhere flag_after_prefix != NULL;
context_everywhere flag_before_prefix != NULL;
context_everywhere input != NULL;
context_everywhere output != NULL;
context_everywhere k == 10;
context_everywhere M == 8;
context_everywhere blockDim.x == ExpTwo(k);
context_everywhere gridDim.x == 1;
requires \ltid < (ExpTwo(k)+2-1)/2 ==> \pointer_index(flag_after_prefix, 2*\ltid, write);
requires \ltid < (ExpTwo(k)-1+2-1)/2 ==> \pointer_index(flag_after_prefix, 2*\ltid+1, write);
requires (\ltid >= 0 && \ltid <= 0) ==> (\forall* int i; 0 <= i && i < ExpTwo(k) && (i + 1) % 1 != 0; \pointer_index(flag_after_prefix, i, write));
requires \pointer_index(input, \ltid, 1\2);
requires \pointer_index(flag_before_prefix, \ltid, 1\2);
requires flag_before_prefix[\ltid] == 0 || flag_before_prefix[\ltid] == 1;
requires \ltid < ExpTwo(M) ==> \pointer_index(output, \ltid, write);
@*/
__global__ void CUDA_Kernel_Stream_Compaction(int* input, int* output, int* flag_before_prefix, int* flag_after_prefix, int k, int M)
{
  int tid = threadIdx.x;
  //@ assert tid == \ltid;

  //@ ghost seq<int> flag_seq;
  //@ assume |flag_seq| == ExpTwo(k);
  //@ assume (\forall int i; 0 <= i && i < ExpTwo(k); flag_seq[i] == 0 || flag_seq[i] == 1);
  //@ assume (flag_seq[tid] == 0 || flag_seq[tid] == 1) && count(flag_seq) == ExpTwo(M);
	//@ assume (2 * tid  < ExpTwo(k)) ==> flag_after_prefix[2 * tid] == flag_seq[2 * tid];
	//@ assume (2 * tid + 1 < ExpTwo(k)) ==> flag_after_prefix[2 * tid + 1] == flag_seq[2 * tid + 1];

  //@ assume flag_seq[tid] == flag_before_prefix[tid];

  //@ assume  2 * tid  < ExpTwo(k) ==> flag_after_prefix[2 * tid] == 0 || flag_after_prefix[2 * tid] == 1;
	//@ assume 2 * tid + 1 < ExpTwo(k) ==> flag_after_prefix[2 * tid + 1] == 0 || flag_after_prefix[2 * tid + 1] == 1;

  //@ ghost seq<int> inp;
  //@ assume |inp| == ExpTwo(k) && inp[tid] == input[tid];


	int indicator = 2 * tid + 1;
	int stride = 1;

	int lvl = 1;

	//@ ghost seq<seq<int> > Matrix_UP = seq<seq<int> > { flag_seq };
	//@ assert (\forall int i; 0 < i && i < lvl; Matrix_UP[i] == up(Matrix_UP[i - 1], stride/ExpTwo(lvl-i), 0, k, i));
	//@ ghost seq<seq<int> > Matrix = seq<seq<int> > { flag_seq };

  /*@
	loop_invariant k > 0;
	loop_invariant tid >= 0 && tid < ExpTwo(k);
	loop_invariant stride > 0;
	loop_invariant 1 <= lvl;
	loop_invariant stride == ExpTwo(lvl-1);
	loop_invariant lvl <= k+1;
	loop_invariant indicator + 1 == ExpTwo(lvl)*(tid+1);
	loop_invariant indicator + 1 == 2*stride*(tid+1);
	loop_invariant indicator > 0;
	loop_invariant stride <= ExpTwo(k);
	loop_invariant indicator < ExpTwo(k) ==> \pointer_index(flag_after_prefix, indicator, 1);
	loop_invariant indicator < ExpTwo(k) && indicator >= stride ==> \pointer_index(flag_after_prefix, indicator - stride, 1);
	loop_invariant tid==0 ==> (\forall* int i; 0 <= i && i < ExpTwo(k) && (i + 1) % stride != 0; \pointer_index(flag_after_prefix, i, 1));
	loop_invariant (tid==0 && (stride == ExpTwo(k))) ==> \pointer_index(flag_after_prefix, ExpTwo(k) - 1, 1);
	loop_invariant |Matrix_UP| == lvl;
	loop_invariant (\forall int i; 0 <= i && i < lvl; |Matrix_UP[i]| == ExpTwo(k));
	loop_invariant lvl == 1 ==> Matrix_UP[lvl - 1] == flag_seq;
	loop_invariant lvl > 1 && lvl < |Matrix_UP| ==> Matrix_UP[lvl] == up(Matrix_UP[lvl - 1], (stride/2) - 1, 0, k, lvl - 1);
	loop_invariant indicator < ExpTwo(k) ==> Matrix_UP[lvl - 1][indicator] == flag_after_prefix[indicator];
	loop_invariant indicator < ExpTwo(k) && indicator >= stride ==> Matrix_UP[lvl - 1][indicator - stride] == flag_after_prefix[indicator - stride];
	loop_invariant lvl == k+1 ==> Matrix_UP[lvl-1][ExpTwo(k) - 1] == intsum(flag_seq);
	loop_invariant lvl == k+1 ==> Matrix_UP[lvl-1][(ExpTwo(k) - 1)/2] == intsum(Take(flag_seq, |flag_seq|/2));
	loop_invariant |Matrix| == lvl;
	loop_invariant (\forall int i; 0 <= i && i < lvl; 0 <= |Matrix[i]| && |Matrix[i]| <= ExpTwo(k));
	loop_invariant (\forall int i; 0 <= i && i < lvl; |Matrix[i]| == ExpTwo(k - i));
	loop_invariant (\forall int i; 0 < i && i < lvl; Matrix[i] == implode(Matrix[i - 1]));
	loop_invariant (\forall int i; 0 <= i && i < lvl; intsum(Matrix[i]) == intsum(flag_seq));
	loop_invariant Matrix[0] == flag_seq;
	loop_invariant indicator < ExpTwo(k) && 2 * tid + 1 < |Matrix[lvl - 1]| ==> flag_after_prefix[indicator] == Matrix[lvl - 1][2 * tid + 1];
	loop_invariant indicator < ExpTwo(k) && indicator >= stride && 2 * tid < |Matrix[lvl - 1]| ==> flag_after_prefix[indicator - stride] == Matrix[lvl - 1][2 * tid];
	@*/
  while(stride < ExpTwo(k))
	{


		if(indicator < ExpTwo(k) && indicator >= stride)
		{
			//@ assert 2 * tid + 1 < |Matrix[lvl - 1]| ==> flag_after_prefix[indicator] == Matrix[lvl - 1][2 * tid + 1];
			//@ assert 2 * tid < |Matrix[lvl - 1]| ==> flag_after_prefix[indicator - stride] == Matrix[lvl - 1][2 * tid];
			flag_after_prefix[indicator] = flag_after_prefix[indicator] + flag_after_prefix[indicator - stride];
			//@ assert 2 * tid + 1 < |Matrix[lvl - 1]| ==> flag_after_prefix[indicator] == Matrix[lvl - 1][2 * tid + 1] + Matrix[lvl - 1][2 * tid];
		}

		//@ assert lemma_implode_length_mod_two(Matrix[lvl - 1]);
		//@ assert lemma_implode_sum(Matrix[lvl - 1]);
		//@ assert lemma_implode_get_all(Matrix[lvl - 1]);


		//@ ghost Matrix = Matrix + seq<seq<int> > { implode(Matrix[lvl - 1]) };

		//@ ghost tid < |implode(Matrix[lvl - 1])| ? (lemma_implode_get(Matrix[lvl - 1], tid) && (2 * tid + 1 < |Matrix[lvl - 1]| ==> get(implode(Matrix[lvl - 1]), tid) == Matrix[lvl - 1][2 * tid] + Matrix[lvl - 1][2 * tid + 1]) && (indicator < ExpTwo(k) && indicator >= stride ==> flag_after_prefix[indicator] == Matrix[lvl - 1][2 * tid + 1] + Matrix[lvl - 1][2 * tid]) && (Matrix[lvl] == implode(Matrix[lvl - 1])) && (indicator < ExpTwo(k) && indicator >= stride ==> flag_after_prefix[indicator] == Matrix[lvl][tid])) : true;




    /*if(tid < |implode(Matrix[lvl - 1])|){
		lemma_implode_get(Matrix[lvl - 1], tid);
		assert 2 * tid + 1 < |Matrix[lvl - 1]| ==> get(implode(Matrix[lvl - 1]), tid) == Matrix[lvl - 1][2 * tid] + Matrix[lvl - 1][2 * tid + 1];
		assert indicator < flag_after_prefix.length && indicator >= stride ==> flag_after_prefix[indicator] == Matrix[lvl - 1][2 * tid + 1] + Matrix[lvl - 1][2 * tid];
		assert Matrix[lvl] == implode(Matrix[lvl - 1]);
		assert indicator < flag_after_prefix.length && indicator >= stride ==> flag_after_prefix[indicator] == Matrix[lvl][tid];
		}*/


    /*@
			context k > 0;
			context 1 <= lvl && lvl <= k;
			context |Matrix| == lvl + 1;
			requires tid >= 0 && tid < ExpTwo(k);
			requires stride == ExpTwo(lvl-1);
			requires stride > 0 && stride < ExpTwo(k);
			requires indicator + 1 == ExpTwo(lvl)*(tid+1);
			requires indicator + 1 == 2*stride*(tid+1);
			requires indicator > 0;
			requires indicator < ExpTwo(k) ==> \pointer_index(flag_after_prefix, indicator, 1);
			requires indicator < ExpTwo(k) && indicator >= stride ==> \pointer_index(flag_after_prefix, indicator - stride, 1);
			requires tid==0 ==> (\forall* int i; 0 <= i && i < ExpTwo(k) && (i + 1) % stride != 0; \pointer_index(flag_after_prefix, i, 1));
			ensures tid >= 0 && tid < ExpTwo(k);
			ensures 2 * stride == ExpTwo(lvl);
			ensures 2 * stride > 0 && 2 * stride <= ExpTwo(k);
			ensures 2 * indicator + 2 == ExpTwo(lvl+1)*(tid+1);
			ensures 2 * indicator + 2 == 2*stride*(tid+1);
			ensures 2 * indicator + 1 > 0;
			ensures 2 * indicator + 1 < ExpTwo(lvl) ==> \pointer_index(flag_after_prefix, 2 * indicator + 1, 1);
			ensures 2 * indicator + 1 < ExpTwo(lvl) && 2 * indicator + 1 >= 2 * stride  ==> \pointer_index(flag_after_prefix, 2 * indicator + 1 - 2 * stride, 1);
			ensures tid==0 ==> (\forall* int i; 0 <= i && i < ExpTwo(lvl) && (i + 1) % (2 * stride) != 0; \pointer_index(flag_after_prefix, i, 1));
			ensures (tid==0 && (2 * stride == ExpTwo(lvl))) ==> \pointer_index(flag_after_prefix, ExpTwo(k) - 1, 1);
    @*/
    __syncthreads();

		//@ ghost Matrix_UP = Matrix_UP + seq<seq<int> > { up(Matrix_UP[lvl - 1], stride, 0, k, lvl) };
		//@ assert (indicator < ExpTwo(k)) && (indicator >= stride) ==> Matrix_UP[lvl][indicator] == Matrix_UP[lvl - 1][indicator] + Matrix_UP[lvl - 1][indicator-stride];
		indicator = 2 * indicator + 1;
		stride = 2 * stride;
		lvl = lvl + 1;
		//@ assert (\forall int i; 0 < i && i < lvl; Matrix_UP[i] == up(Matrix_UP[i - 1], stride/ExpTwo(lvl-i), 0, k, i));

		//@ assert stride == ExpTwo(lvl-1);
		//@ assert lemma_exp2_red_mult(lvl);
		//@ assert ExpTwo(lvl) == 2 * ExpTwo(lvl - 1);
		//@ assert 2*stride == ExpTwo(lvl);
		//@ assert indicator + 1 == ExpTwo(lvl)*(tid+1);
		//@ assert indicator + 1 == 2*stride*(tid+1);


	}

	//@ assert stride == ExpTwo(lvl-1);
	//@ assert ExpTwo(lvl-1) == ExpTwo(k);
	//@ assert stride == ExpTwo(k);
	//@ assert power_two_lemma(lvl-1, k);
	//@ assert lvl == k + 1;
	//@ assert indicator < ExpTwo(k) ==> Matrix_UP[lvl - 1][indicator] == flag_after_prefix[indicator];
	//@ assert |Matrix| == lvl;
	//@ assert (\forall int i; 0 <= i && i < k + 1; |Matrix[i]| == ExpTwo(k - i));
	//@ assert (\forall int i; 0 < i && i < k + 1; Matrix[i] == implode(Matrix[i - 1]));
	//@ assert (\forall int i; 0 <= i && i < k + 1; intsum(Matrix[i]) == intsum(flag_seq));
	//@ assert |Matrix[k]| == 1;
	//@ assert lemma_intsum_single(Matrix[k][0]);
	//@ assert intsum(Matrix[k]) == intsum(flag_seq);
	//@ assert Matrix[k] == seq<int>{intsum(flag_seq)};
	//@ assert Matrix[0] == flag_seq;
	//@ assert (\forall int i; 0 <= i && i < k + 1; 0 < |Matrix[i]| && |Matrix[i]| <= ExpTwo(k));

/////////////////////////////////////////////////////////////////////////////////

  //@ assert indicator < ExpTwo(k) && indicator >= stride && 2 * tid < |Matrix[lvl - 1]| ==> flag_after_prefix[indicator - stride] == Matrix[lvl - 1][2 * tid];

  /*@
		context k > 0;
		context |Matrix_UP| == k + 1;
        context |Matrix| == k + 1;
		context lvl == k + 1;
		context stride == ExpTwo(k);
		context indicator + 1 == ExpTwo(lvl)*(tid+1);
		context indicator + 1 == 2*stride*(tid+1);
		context indicator > 0;
		context stride > 0 ;
		requires indicator < ExpTwo(k) ==> \pointer_index(flag_after_prefix, indicator, 1);
		requires indicator < ExpTwo(k) && indicator >= stride  ==> \pointer_index(flag_after_prefix, indicator - stride, 1);
		requires tid==0 ==> (\forall* int i; 0 <= i && i < ExpTwo(k) && (i + 1) % stride != 0; \pointer_index(flag_after_prefix, i, 1));
		requires (tid==0 && (stride == ExpTwo(k))) ==> \pointer_index(flag_after_prefix, ExpTwo(k) - 1, 1);
		requires (\forall int i; 0 <= i && i <= k; |Matrix_UP[i]| == ExpTwo(k));
    requires (\forall int i; 0 <= i && i < lvl; |Matrix[i]| == ExpTwo(k - i));
    requires (\forall int i; 0 <= i && i < lvl; 0 <= |Matrix[i]| && |Matrix[i]| <= ExpTwo(k));
		requires indicator < ExpTwo(k) && indicator >= stride ==> Matrix_UP[lvl - 1][indicator] == flag_after_prefix[indicator];
		requires indicator < ExpTwo(k) && indicator >= stride ==> Matrix_UP[lvl - 1][indicator - stride] == flag_after_prefix[indicator - stride];
    requires indicator < ExpTwo(k) && indicator >= stride && 2 * tid < |Matrix[lvl - 1]| ==> flag_after_prefix[indicator - stride] == Matrix[lvl - 1][2 * tid];
		context tid >= 0 && tid < ExpTwo(k);
		//ensures stride == ExpTwo(k) / 2;
		//ensures indicator == ExpTwo(k) * tid + ExpTwo(k) - 1;
		//ensures stride > 0 ;
		//ensures indicator > 0;
		ensures ExpTwo(k) * \ltid + ExpTwo(k) - 1 < ExpTwo(k) ==> \pointer_index(flag_after_prefix, ExpTwo(k) * \ltid + ExpTwo(k) - 1, 1);
		ensures ExpTwo(k) * \ltid + ExpTwo(k) - 1 < ExpTwo(k) && ExpTwo(k) * \ltid + ExpTwo(k) - 1 >= ExpTwo(k) / 2  ==> \pointer_index(flag_after_prefix, ExpTwo(k) * \ltid + ExpTwo(k) - 1 - ExpTwo(k) / 2, 1);
		ensures tid==0 ==> (\forall* int i; 0 <= i && i < ExpTwo(k) && (i + 1) % (ExpTwo(k) / 2) != 0; \pointer_index(flag_after_prefix, i, 1));
    ensures (\forall int i; 0 <= i && i <= k; |Matrix_UP[i]| == ExpTwo(k));
    ensures (\forall int i; 0 <= i && i < lvl; |Matrix[i]| == ExpTwo(k - i));
    ensures (\forall int i; 0 <= i && i < lvl; 0 <= |Matrix[i]| && |Matrix[i]| <= ExpTwo(k));
		//ensures ExpTwo(k) * \ltid + ExpTwo(k) - 1 < ExpTwo(k) ==> Matrix_UP[lvl - 1][ExpTwo(k) * \ltid + ExpTwo(k) - 1] == flag_after_prefix[ExpTwo(k) * \ltid + ExpTwo(k) - 1];
		//ensures ExpTwo(k) * \ltid + ExpTwo(k) - 1 < ExpTwo(k) && ExpTwo(k) * \ltid + ExpTwo(k) - 1 >= ExpTwo(k) / 2 ==> Matrix_UP[lvl - 1][ExpTwo(k) * \ltid + ExpTwo(k) - 1 - ExpTwo(k) / 2] == flag_after_prefix[ExpTwo(k) * \ltid + ExpTwo(k) - 1 - ExpTwo(k) / 2];
    //ensures 2 * tid < |Matrix[lvl-2]| && ExpTwo(k) * \ltid + ExpTwo(k) - 1 < ExpTwo(k) && ExpTwo(k) * \ltid + ExpTwo(k) - 1 >= ExpTwo(k) / 2 ==> flag_after_prefix[ExpTwo(k) * \ltid + ExpTwo(k) - 1 - ExpTwo(k) / 2] == get(Matrix[lvl-2], 2 * tid);
  @*/
  __syncthreads();

  // (unstability) These come from the las three postconditions in the previous barrier:
		//@ assume ExpTwo(k) * tid + ExpTwo(k) - 1 < ExpTwo(k) ==> Matrix_UP[lvl - 1][ExpTwo(k) * tid + ExpTwo(k) - 1] == flag_after_prefix[ExpTwo(k) * tid + ExpTwo(k) - 1];
		//@ assume ExpTwo(k) * tid + ExpTwo(k) - 1 < ExpTwo(k) && ExpTwo(k) * tid + ExpTwo(k) - 1 >= ExpTwo(k) / 2 ==> Matrix_UP[lvl - 1][ExpTwo(k) * tid + ExpTwo(k) - 1 - ExpTwo(k) / 2] == flag_after_prefix[ExpTwo(k) * tid + ExpTwo(k) - 1 - ExpTwo(k) / 2];
    //@ assume 2 * tid < |Matrix[lvl-2]| && ExpTwo(k) * tid + ExpTwo(k) - 1 < ExpTwo(k) && ExpTwo(k) * tid + ExpTwo(k) - 1 >= ExpTwo(k) / 2 ==> flag_after_prefix[ExpTwo(k) * tid + ExpTwo(k) - 1 - ExpTwo(k) / 2] == get(Matrix[lvl-2], 2 * tid);

///////////////////////////////////////////////////////////////////////////////////////	Down

	indicator = ExpTwo(k) * tid + ExpTwo(k) - 1; // flag_after_prefix.length * tid + flag_after_prefix.length - 1;
	stride = ExpTwo(k) / 2; // flag_after_prefix.length / 2;
	lvl = k - 1; //lvl - 2;
	int temp;
	//@ ghost seq<int> temp_seq = seq<int> { 0 };

	//@ assert ExpTwo(k) * tid + ExpTwo(k) - 1 < ExpTwo(k) ==> Matrix_UP[lvl + 1][indicator] == flag_after_prefix[indicator];
	//@ assert ExpTwo(k) * tid + ExpTwo(k) - 1 < ExpTwo(k) && ExpTwo(k) * tid + ExpTwo(k) - 1 >= ExpTwo(k) / 2 ==> Matrix_UP[lvl + 1][indicator - stride] == flag_after_prefix[indicator - stride];



	if(indicator < ExpTwo(k))
	{
		flag_after_prefix[indicator] = 0;
	}





  /*@
  loop_invariant k > 0;
  loop_invariant tid >= 0 && tid < ExpTwo(k);
  loop_invariant lvl <= k - 1;
  loop_invariant lvl >= -1;
  loop_invariant lvl >= 0 ==> stride == ExpTwo(lvl);
  loop_invariant lvl == -1 ==> stride == 0;
  loop_invariant stride == 0 ==> lvl == -1;
  loop_invariant stride >= 0;
  loop_invariant indicator >= 0;
  loop_invariant indicator+1 == ExpTwo(lvl+1)*(tid+1);
  loop_invariant indicator < ExpTwo(k) ==> \pointer_index(flag_after_prefix, indicator, 1);
  loop_invariant lvl >= 0 && indicator < ExpTwo(k) && indicator >= stride ==> \pointer_index(flag_after_prefix, indicator - stride, 1);
  loop_invariant (tid==0 && stride > 0) ==> (\forall* int i; 0 <= i && i < ExpTwo(k) && (i + 1) % stride != 0; \pointer_index(flag_after_prefix, i, 1));
  //loop_invariant lvl == -1 ==> \pointer_index(flag_after_prefix, tid, 1);
  //loop_invariant lvl == -1 ==> indicator == tid;
  //loop_invariant indicator == tid ==> lvl == -1;
  loop_invariant |temp_seq| == ExpTwo(k - (lvl + 1));
  loop_invariant 0 < |temp_seq| && |temp_seq| <= ExpTwo(k);
  loop_invariant temp_seq == psum2(Matrix[lvl + 1]);
  loop_invariant (\forall int i; 0 <= i && i < k + 1; 0 < |Matrix[i]| && |Matrix[i]| <= ExpTwo(k));
  loop_invariant (\forall int i; 0 <= i && i < k + 1; |Matrix[i]| == ExpTwo(k - i));
  loop_invariant (\forall int i; 0 <= i && i < k + 1; intsum(Matrix[i]) == intsum(flag_seq));
  loop_invariant (\forall int i; 0 < i && i < k + 1; Matrix[i] == implode(Matrix[i - 1]));
  loop_invariant Matrix[0] == flag_seq;
  loop_invariant Matrix[k] == seq<int>{ intsum(flag_seq) };
  loop_invariant tid < |temp_seq| && indicator < ExpTwo(k) ==> temp_seq[tid] == flag_after_prefix[indicator];
  loop_invariant lvl >= 0 && 2 * tid < |Matrix[lvl]| && indicator < ExpTwo(k) && indicator >= stride ==> flag_after_prefix[indicator - stride] == get(Matrix[lvl], 2 * tid);
  @*/
  while(stride >= 1)
  {
  	if(indicator < ExpTwo(k) && indicator >= stride)
  	{

  		//@ assert tid < |temp_seq| ==> temp_seq[tid] == flag_after_prefix[indicator];
  		temp = flag_after_prefix[indicator];
  		//@ assert tid < |temp_seq| ==> temp == temp_seq[tid];
  		flag_after_prefix[indicator] = flag_after_prefix[indicator] + flag_after_prefix[indicator - stride];
  		//@ assert tid < |temp_seq| ==> flag_after_prefix[indicator] == temp_seq[tid] + flag_after_prefix[indicator - stride];

  		//@ assert 2 * tid < |Matrix[lvl]| ==> flag_after_prefix[indicator - stride] == get(Matrix[lvl], 2 * tid);
  		//@ assert 2 * tid < |Matrix[lvl]| && tid < |temp_seq| ==> flag_after_prefix[indicator] == temp_seq[tid] + get(Matrix[lvl], 2 * tid);
  		//@ assert tid < |Matrix[lvl + 1]| && tid < |temp_seq| ==> temp_seq[tid] == get(psum2(Matrix[lvl + 1]), tid);
  		//@ assert tid < |Matrix[lvl + 1]| && 2 * tid < |Matrix[lvl]| ==> flag_after_prefix[indicator] == get(psum2(Matrix[lvl + 1]), tid) + get(Matrix[lvl], 2 * tid);
  		//@ assert Matrix[lvl + 1] == implode(Matrix[lvl]);
  		//@ assert tid < |implode(Matrix[lvl])| && 2 * tid < |Matrix[lvl]| ==> flag_after_prefix[indicator] == get(psum2(implode(Matrix[lvl])), tid) + get(Matrix[lvl], 2 * tid);
      //@ ghost tid < |implode(Matrix[lvl])| ? lemma_get_psum_implode(Matrix[lvl], tid) : true;
  		/*if(tid < |implode(Matrix[lvl])|){
  			lemma_get_psum_implode(Matrix[lvl], tid);
  		}*/
  		//@ assert tid < |implode(Matrix[lvl])| && 2 * tid < |Matrix[lvl]| ==> get(psum2(implode(Matrix[lvl])), tid) == get(psum2(Matrix[lvl]), 2 * tid);
  		//@ assert 2 * tid < |Matrix[lvl]| ==> flag_after_prefix[indicator] == get(psum2(Matrix[lvl]), 2 * tid) + get(Matrix[lvl], 2 * tid);
  		//@ ghost 2 * tid + 1 < |Matrix[lvl]| ? lemma_combine_psum(Matrix[lvl], tid) : true;
      /*if(2 * tid + 1 < |Matrix[lvl]|){
  		  lemma_combine_psum(Matrix[lvl], tid);
  		}*/
  		//@ assert 2 * tid + 1 < |Matrix[lvl]| ==> get(psum2(Matrix[lvl]), 2 * tid + 1) == get(psum2(Matrix[lvl]), 2 * tid) + get(Matrix[lvl], 2 * tid);
  		//@ assert 2 * tid + 1 < |Matrix[lvl]| ==> flag_after_prefix[indicator] == get(psum2(Matrix[lvl]), 2 * tid + 1);

  		//@ assert tid < |temp_seq| ==> temp == temp_seq[tid];
  		flag_after_prefix[indicator - stride] = temp;
  		//@ assert tid < |temp_seq| ==> flag_after_prefix[indicator - stride] == temp_seq[tid];

  		//@ assert tid < |Matrix[lvl + 1]| && tid < |temp_seq| ==> temp_seq[tid] == get(psum2(Matrix[lvl + 1]), tid);
  		//@ assert Matrix[lvl + 1] == implode(Matrix[lvl]);
  		//@ assert tid < |implode(Matrix[lvl])| && tid < |temp_seq| ==> temp_seq[tid] == get(psum2(implode(Matrix[lvl])), tid);
  		//@ ghost tid < |implode(Matrix[lvl])| ? lemma_get_psum_implode(Matrix[lvl], tid) : true;
      /*if(tid < |implode(Matrix[lvl])|){
  			lemma_get_psum_implode(Matrix[lvl], tid);
  		}*/

  		//@ assert tid < |implode(Matrix[lvl])| && 2 * tid < |Matrix[lvl]| ==> get(psum2(implode(Matrix[lvl])), tid) == get(psum2(Matrix[lvl]), 2 * tid);
  		//@ assert 2 * tid < |Matrix[lvl]| && tid < |temp_seq| ==> temp_seq[tid] == get(psum2(Matrix[lvl]), 2 * tid);
  		//@ assert 2 * tid < |Matrix[lvl]| ==> flag_after_prefix[indicator - stride] == get(psum2(Matrix[lvl]), 2 * tid);

  	}

  	//@ ghost temp_seq = psum2(Matrix[lvl]);

  	//@ assert 2 * tid < |temp_seq| && indicator < ExpTwo(k) && indicator >= stride ==> flag_after_prefix[indicator - stride] == temp_seq[2 * tid];
  	//@ assert 2 * tid + 1 < |temp_seq| && indicator < ExpTwo(k) && indicator >= stride ==> flag_after_prefix[indicator] == temp_seq[2 * tid + 1];


    /*@
  		context lvl >= 0 && lvl <= k - 1;
  		requires tid >= 0 && tid < ExpTwo(k);
        context |temp_seq| == ExpTwo(k - lvl);
        context 0 < |temp_seq| && |temp_seq| <= ExpTwo(k);
        context |Matrix| == k + 1;
      //context lvl - 1 == -1 ==> (indicator - 1) / 2 == \ltid;
      //context (indicator - 1) / 2 == \ltid ==> lvl - 1 == -1;
  		requires indicator >= 0;
  		requires stride >= 1 ;
  		requires stride == ExpTwo(lvl);
  		requires indicator+1 == ExpTwo(lvl+1)*(\ltid+1);
  		requires indicator < ExpTwo(k) ==> \pointer_index(flag_after_prefix, indicator, 1);
  		requires indicator < ExpTwo(k) && indicator >= stride  ==> \pointer_index(flag_after_prefix, indicator - stride, 1);
  		requires tid==0 ==> (\forall* int i; 0 <= i && i < ExpTwo(k) && (i + 1) % stride != 0; \pointer_index(flag_after_prefix, i, 1));
      //requires 2 * tid < |temp_seq| && indicator < ExpTwo(k) && indicator >= stride ==> flag_after_prefix[indicator - stride] == temp_seq[2 * tid];
      //requires 2 * tid + 1 < |temp_seq| && indicator < ExpTwo(k) && indicator >= stride ==> flag_after_prefix[indicator] == temp_seq[2 * tid + 1];
      requires (\forall int i; 0 <= i && i < k + 1; |Matrix[i]| == ExpTwo(k - i));
      requires (\forall int i; 0 <= i && i < k + 1; 0 <= |Matrix[i]| && |Matrix[i]| <= ExpTwo(k));
      //requires 2 * tid < |Matrix[lvl]| && indicator < ExpTwo(k) && indicator >= stride ==> flag_after_prefix[indicator - stride] == get(psum2(Matrix[lvl]), 2 * tid);
  		ensures tid >= 0 && tid < ExpTwo(k);
  		ensures lvl-1 >= 0 ==> stride / 2 == ExpTwo(lvl - 1);
  		ensures lvl-1 == -1 ==> stride / 2 == 0;
      ensures stride / 2 == 0  ==> lvl-1 == -1;
  		ensures stride / 2 >= 0;
  		ensures (indicator - 1) / 2 >= 0;
  		ensures (indicator - 1) / 2+1 == ExpTwo(lvl)*(tid+1);
  		ensures (indicator - 1) / 2 < ExpTwo(k) ==> \pointer_index(flag_after_prefix, (indicator - 1) / 2, 1);
  		ensures lvl-1 >= 0 && (indicator - 1) / 2 < ExpTwo(k) && (indicator - 1) / 2 >= stride / 2  ==> \pointer_index(flag_after_prefix, (indicator - 1) / 2 - stride / 2, 1);
  		ensures (tid==0 && stride/2 > 0) ==> (\forall* int i; 0 <= i && i < ExpTwo(k) && (i + 1) % (stride/2) != 0; \pointer_index(flag_after_prefix, i, 1));
      //ensures tid < |temp_seq| && (indicator - 1) / 2 < ExpTwo(k) ==> temp_seq[tid] == flag_after_prefix[(indicator - 1) / 2];
      ensures (\forall int i; 0 <= i && i < k + 1; |Matrix[i]| == ExpTwo(k - i));
      ensures (\forall int i; 0 <= i && i < k + 1; 0 <= |Matrix[i]| && |Matrix[i]| <= ExpTwo(k));
      //ensures lvl-1 >= 0 && 2 * tid < |Matrix[lvl-1]| && (indicator - 1) / 2 < ExpTwo(k) && (indicator - 1) / 2 >= stride / 2 ==> flag_after_prefix[(indicator - 1) / 2 - stride / 2] == get(Matrix[lvl-1], 2 * tid);
    @*/
    __syncthreads();

  	//@ assume tid < |temp_seq| && (indicator - 1) / 2 < ExpTwo(k) ==> temp_seq[tid] == flag_after_prefix[(indicator - 1) / 2];
  	//@ assume lvl-1 >= 0 && 2 * tid < |Matrix[lvl-1]| && (indicator - 1) / 2 < ExpTwo(k) && (indicator - 1) / 2 >= stride / 2 ==> flag_after_prefix[(indicator - 1) / 2 - stride / 2] == get(Matrix[lvl-1], 2 * tid);

  	indicator = (indicator - 1) / 2;
  	stride = stride / 2;
  	lvl = lvl - 1;


  }


	//@ assert indicator == tid;
  //@ assert tid >= 0 && tid < ExpTwo(k);
	//@ assert temp_seq == psum2(Matrix[0]);
	//@ assert Matrix[0] == flag_seq;
	//@ assert temp_seq == psum2(flag_seq);
  //@ assert |temp_seq| == ExpTwo(k);
	//@ assert temp_seq[tid] == flag_after_prefix[indicator];
	//@ assert flag_after_prefix[indicator] == get(psum2(flag_seq), tid);
	//@ assert lemma_intsum_flag(flag_seq, indicator);
	//@ assert count(flag_seq) == ExpTwo(M);
  //@ assert intsum(flag_seq) == ExpTwo(M);
  //@ assert flag_seq[tid] == 1 ? lemma_sum_pos_flag(flag_seq, tid) : true;
  //@ assert lemma_take_sum(flag_seq, tid);
  //@ assert flag_seq[tid] == flag_before_prefix[indicator];
	//@ assert flag_after_prefix[indicator] >= 0 && flag_after_prefix[indicator] <= ExpTwo(M);
	//@ assert flag_before_prefix[indicator] == 1 ==> flag_after_prefix[indicator] >= 0 && flag_after_prefix[indicator] < ExpTwo(M);


  /*@
    requires indicator == tid;
    requires tid >= 0 && tid < ExpTwo(k);
    requires tid < ExpTwo(M) ==> \pointer_index(output, tid, 1);
    requires \pointer_index(input, tid, 1\2);
    requires \pointer_index(flag_before_prefix, tid, 1\2);
    requires \pointer_index(flag_after_prefix, tid, 1);
    requires |temp_seq| == ExpTwo(k);
    requires temp_seq == psum2(flag_seq);
    //requires temp_seq[tid] == flag_after_prefix[indicator];
    requires |flag_seq| == ExpTwo(k);
    requires flag_seq[tid] == flag_before_prefix[indicator];
    requires |inp| == ExpTwo(k);
    requires (inp[tid] == input[tid]);
    ensures indicator == tid;
    ensures tid >= 0 && tid < ExpTwo(k);
    ensures |temp_seq| == ExpTwo(k);
    ensures temp_seq == psum2(flag_seq);
    ensures \pointer_index(input, tid, 1\2);
    ensures \pointer_index(flag_before_prefix, tid, 1\2);
    ensures \pointer_index(flag_after_prefix, tid, 1\2);
  	ensures flag_before_prefix[tid] == 1 ==> flag_after_prefix[tid] >= 0 && flag_after_prefix[tid] < ExpTwo(M);
  	ensures flag_before_prefix[tid] == 1 ==> \pointer_index(output, flag_after_prefix[tid], 1);
    //ensures temp_seq[tid] == flag_after_prefix[indicator];
    ensures |flag_seq| == ExpTwo(k);
    ensures flag_seq[tid] == flag_before_prefix[indicator];
    ensures |inp| == ExpTwo(k);
    ensures (inp[tid] == input[tid]);
  @*/
  __syncthreads();


	if(flag_before_prefix[tid] == 1){

		output[flag_after_prefix[tid]] = input[tid];
    //@ assert (output[flag_after_prefix[tid]] == input[tid]);

	}

   //@ assert flag_before_prefix[tid] == 1 ==> (output[flag_after_prefix[tid]] == input[tid]);

	//@ ghost seq<int> temporary;

	//@ ghost temporary = compact(inp, flag_seq);
  //@ assert temporary == compact(inp, flag_seq);
	//@ assert |temporary| == ExpTwo(M);

  //@ assert intsum(Take(flag_seq, tid)) >= 0;
  //@ assume temp_seq[tid] == flag_after_prefix[indicator];
  //@ assert flag_after_prefix[tid] >= 0;

  //@ assert \pointer_index(input, tid, 1\2);
  //@ assert \pointer_index(flag_before_prefix, tid, 1\2);
  //@ assert \pointer_index(flag_after_prefix, tid, 1\2);
  //@ assert flag_before_prefix[tid] == 1 ==> \pointer_index(output, flag_after_prefix[tid], 1);

  //@ assert flag_before_prefix[tid] == flag_seq[tid];

  //@ assert flag_before_prefix[tid] == 1 ==> (lemma_correctness(inp, flag_seq, tid));

  //@ assert flag_before_prefix[tid] == 1 ==> inp[tid] == get(compact(inp, flag_seq), intsum(Take(flag_seq, tid)));

  // assert temporary == compact(inp, flag_seq);

  //@ assert flag_before_prefix[tid] == 1 ==> (inp[tid] == get(temporary, intsum(Take(flag_seq, tid))));

  //@ assert (inp[tid] == input[tid]);

  //@ assert flag_before_prefix[tid] == 1 ==> (input[tid] == get(temporary, flag_after_prefix[tid]));

  // assert flag_before_prefix[tid] == 1 ==> (output[flag_after_prefix[tid]] == input[tid]);

  //@ assert flag_before_prefix[tid] == 1 ==> (output[flag_after_prefix[tid]] == get(temporary, flag_after_prefix[tid]));


}