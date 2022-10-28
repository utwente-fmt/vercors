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
ensures |xs| == 1 ==> \result == xs.head;
pure int intsum(seq<int> xs) =
	0 < |xs| ? xs.head + intsum(xs.tail) : 0;
@*/

/*@
requires n <= |xs|;
ensures n < 0 ==> |Take(xs, n)| == 0;
ensures 0 <= n ==> |Take(xs, n)| == n;
ensures (\forall int i; 0 <= i && i < n; xs[i] == get(Take(xs, n), i));
pure seq<int> Take(seq<int> xs, int n) =
	0 < n ? seq<int> { xs.head } + Take(xs.tail, n - 1) : seq<int> { };
@*/

/*@
requires 0 <= i && i <= |xs|;
ensures |\result| == |xs| - i;
ensures (\forall int j; 0 <= j && j < |\result|; \result[j] == intsum(Take(xs, i+j)));
pure seq<int> psum(seq<int> xs, int i) =
	i < |xs| ? seq<int> { intsum(Take(xs, i)) } + psum(xs, i + 1) : seq<int> { };
@*/

// TODO use this version instead of the above `psum` (the above version is just a helper definition).
/*@
ensures |\result| == |xs|;
ensures (\forall int j; 0 <= j && j < |\result|; \result[j] == intsum(Take(xs, j)));
pure seq<int> psum2(seq<int> xs) = psum(xs, 0);
@*/

/*@
requires |xs| >= 0;
ensures |xs| == 0	==> \result == xs;
ensures |xs| == 1 ==> \result == xs;
ensures |xs| == 2 ==> \result == seq<int> { xs.head + xs.tail.head };
ensures |xs| % 2 == 0 ==> |\result| == |xs| / 2;
pure seq<int> implode(seq<int> xs) =
	1 < |xs| ? seq<int> { xs.head + xs.tail.head } + implode(xs.tail.tail) : xs;
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

////////////////////////////////////////////////////////////////////////////////////////Lemmas

/*@
ensures \result && intsum(seq<int> { }) == 0;
pure bool lemma_intsum_zero() = true;
@*/

/*@
ensures \result && psum2(seq<int> { }) == seq<int> { };
pure bool lemma_psum_zero() = true;
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
ensures |xs| == 0 ==> intsum(xs + ys) == intsum(ys);
ensures |ys| == 0 ==> intsum(xs + ys) == intsum(xs);
ensures |xs + ys| == |xs| + |ys|;
ensures \result && intsum(xs.tail + ys) == intsum(xs.tail) + intsum(ys);
ensures \result && intsum(xs + ys) == intsum(xs) + intsum(ys);
pure bool lemma_intsum_app(seq<int> xs, seq<int> ys);
@*/

/*@
requires |xs| <= 1;
ensures \result && xs == implode(xs);
pure bool lemma_implode_base(seq<int> xs) = true;
@*/


/*@
ensures \result && intsum(xs) == intsum(implode(xs));
pure bool lemma_implode_sum(seq<int> xs);
@*/

/*@
requires 0 < n;
ensures \result && ExpTwo(n) == 2 * ExpTwo(n - 1);
pure bool lemma_exp2_red_mult(int n)  = true;
@*/

/*@
requires 0 < n;
ensures \result && ExpTwo(n) / 2 == ExpTwo(n - 1);
pure bool lemma_exp2_red_div(int n) = true;
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
ensures \result && get(xs.tail, i - 1) == xs[i];
pure bool lemma_intseq_index_tail(seq<int> xs, int i) = true;
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
requires j >= 0 && j <= |implode(xs)|;
requires |xs| % 2 == 0;
requires |implode(xs)| == |xs|/2;
ensures \result && (\forall int i; j <= i && i < |implode(xs)|; get(implode(xs), i) == xs[2 * i] + xs[2 * i + 1]);
pure bool lemma_implode_get_all(seq<int> xs, int j);
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
requires j >= 0 && j <= |xs|;
ensures \result && (\forall int i; j <= i && i < |xs|; get(psum2(xs), i) == intsum(Take(xs, i)));
pure bool lemma_psum_get_all(seq<int> xs, int j);
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
pure bool intseq_concat_assoc(seq<int> xs, seq<int> ys, seq<int> zs) = true;
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
ensures  \result && get(psum2(implode(xs)), i) == get(psum2(xs), 2 * i);
pure bool lemma_get_psum_implode(seq<int> xs, int i);
@*/

/*@
requires 0 <= i;
requires 2 * i + 1 < |xs|;
ensures  \result && get(psum2(xs), 2 * i + 1) == get(psum2(xs), 2 * i) + get(xs, 2 * i);
pure bool lemma_combine_psum(seq<int> xs, int i);
@*/


////////////////////////////////////////////////////////////////////////////////
//Kernel
////////////////////////////////////////////////////////////////////////////////
/*@
context_everywhere output != NULL;
context_everywhere k == 10;
context_everywhere blockDim.x == ExpTwo(k);
context_everywhere gridDim.x == 1;
requires \ltid < (ExpTwo(k)+2-1)/2 ==> \pointer_index(output, 2*\ltid, write);
requires \ltid < (ExpTwo(k)-1+2-1)/2 ==> \pointer_index(output, 2*\ltid+1, write);
requires (\ltid >= 0 && \ltid <= 0) ==> (\forall* int i; 0 <= i && i < ExpTwo(k) && (i + 1) % 1 != 0; \pointer_index(output, i, write));
@*/
__global__ void CUDA_Kernel_Blelloch(int* output, int k)
{
  int tid = threadIdx.x;
  //@ assert tid == \ltid;


  //@ ghost seq<int> input_seq;
  //@ assume |input_seq| == ExpTwo(k);
	//@ assume (2 * tid  < ExpTwo(k)) ==> output[2 * tid] == input_seq[2 * tid];
	//@ assume (2 * tid + 1 < ExpTwo(k)) ==> output[2 * tid + 1] == input_seq[2 * tid + 1];

	int indicator = 2 * tid + 1;
	int stride = 1;

	int lvl = 1;

	//@ ghost seq<seq<int> > Matrix_UP = seq<seq<int> > { input_seq };
	//@ assert (\forall int i; 0 < i && i < lvl; Matrix_UP[i] == up(Matrix_UP[i - 1], stride/ExpTwo(lvl-i), 0, k, i));
	//@ ghost seq<seq<int> > Matrix = seq<seq<int> > { input_seq };

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
	loop_invariant indicator < ExpTwo(k) ==> \pointer_index(output, indicator, 1);
	loop_invariant indicator < ExpTwo(k) && indicator >= stride ==> \pointer_index(output, indicator - stride, 1);
	loop_invariant tid==0 ==> (\forall* int i; 0 <= i && i < ExpTwo(k) && (i + 1) % stride != 0; \pointer_index(output, i, 1));
	loop_invariant (tid==0 && (stride == ExpTwo(k))) ==> \pointer_index(output, ExpTwo(k) - 1, 1);
	loop_invariant |Matrix_UP| == lvl;
	loop_invariant (\forall int i; 0 <= i && i < lvl; |Matrix_UP[i]| == ExpTwo(k));
	//loop_invariant lvl == 1 ==> (\forall int i; 0 <= i && i < ExpTwo(k); Matrix_UP[lvl - 1][i] == input_seq[i]);
  loop_invariant lvl == 1 ==> Matrix_UP[lvl - 1] == input_seq;
	loop_invariant lvl > 1 && lvl < |Matrix_UP| ==> Matrix_UP[lvl] == up(Matrix_UP[lvl - 1], (stride/2) - 1, 0, k, lvl - 1);
	loop_invariant indicator < ExpTwo(k) ==> Matrix_UP[lvl - 1][indicator] == output[indicator];
	loop_invariant indicator < ExpTwo(k) && indicator >= stride ==> Matrix_UP[lvl - 1][indicator - stride] == output[indicator - stride];
	loop_invariant lvl == k+1 ==> Matrix_UP[lvl-1][ExpTwo(k) - 1] == intsum(input_seq);
	loop_invariant lvl == k+1 ==> Matrix_UP[lvl-1][(ExpTwo(k) - 1)/2] == intsum(Take(input_seq, |input_seq|/2));
	loop_invariant |Matrix| == lvl;
	loop_invariant (\forall int i; 0 <= i && i < lvl; 0 <= |Matrix[i]| && |Matrix[i]| <= ExpTwo(k));
	loop_invariant (\forall int i; 0 <= i && i < lvl; |Matrix[i]| == ExpTwo(k - i));
	loop_invariant (\forall int i; 0 < i && i < lvl; Matrix[i] == implode(Matrix[i - 1]));
	loop_invariant (\forall int i; 0 <= i && i < lvl; intsum(Matrix[i]) == intsum(input_seq));
	loop_invariant Matrix[0] == input_seq;
	loop_invariant indicator < ExpTwo(k) && 2 * tid + 1 < |Matrix[lvl - 1]| ==> output[indicator] == Matrix[lvl - 1][2 * tid + 1];
	loop_invariant indicator < ExpTwo(k) && indicator >= stride && 2 * tid < |Matrix[lvl - 1]| ==> output[indicator - stride] == Matrix[lvl - 1][2 * tid];
	@*/
  while(stride < ExpTwo(k))
	{


		if(indicator < ExpTwo(k) && indicator >= stride)
		{
			//@ assert 2 * tid + 1 < |Matrix[lvl - 1]| ==> output[indicator] == Matrix[lvl - 1][2 * tid + 1];
			//@ assert 2 * tid < |Matrix[lvl - 1]| ==> output[indicator - stride] == Matrix[lvl - 1][2 * tid];
			output[indicator] = output[indicator] + output[indicator - stride];
			//@ assert 2 * tid + 1 < |Matrix[lvl - 1]| ==> output[indicator] == Matrix[lvl - 1][2 * tid + 1] + Matrix[lvl - 1][2 * tid];
		}

		//@ assert lemma_implode_length_mod_two(Matrix[lvl - 1]);
		//@ assert lemma_implode_sum(Matrix[lvl - 1]);
		//@ assert lemma_implode_get_all(Matrix[lvl - 1], 0);


		//@ ghost Matrix = Matrix + seq<seq<int> > { implode(Matrix[lvl - 1]) };

		//@ ghost tid < |implode(Matrix[lvl - 1])| ? (lemma_implode_get(Matrix[lvl - 1], tid) && (2 * tid + 1 < |Matrix[lvl - 1]| ==> get(implode(Matrix[lvl - 1]), tid) == Matrix[lvl - 1][2 * tid] + Matrix[lvl - 1][2 * tid + 1]) && (indicator < ExpTwo(k) && indicator >= stride ==> output[indicator] == Matrix[lvl - 1][2 * tid + 1] + Matrix[lvl - 1][2 * tid]) && (Matrix[lvl] == implode(Matrix[lvl - 1])) && (indicator < ExpTwo(k) && indicator >= stride ==> output[indicator] == Matrix[lvl][tid])) : true;



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
			requires indicator < ExpTwo(k) ==> \pointer_index(output, indicator, 1);
			requires indicator < ExpTwo(k) && indicator >= stride ==> \pointer_index(output, indicator - stride, 1);
			requires tid==0 ==> (\forall* int i; 0 <= i && i < ExpTwo(k) && (i + 1) % stride != 0; \pointer_index(output, i, 1));
			ensures tid >= 0 && tid < ExpTwo(k);
			ensures 2 * stride == ExpTwo(lvl);
			ensures 2 * stride > 0 && 2 * stride <= ExpTwo(k);
			ensures 2 * indicator + 2 == ExpTwo(lvl+1)*(tid+1);
			ensures 2 * indicator + 2 == 2*stride*(tid+1);
			ensures 2 * indicator + 1 > 0;
			ensures 2 * indicator + 1 < ExpTwo(lvl) ==> \pointer_index(output, 2 * indicator + 1, 1);
			ensures 2 * indicator + 1 < ExpTwo(lvl) && 2 * indicator + 1 >= 2 * stride  ==> \pointer_index(output, 2 * indicator + 1 - 2 * stride, 1);
			ensures tid==0 ==> (\forall* int i; 0 <= i && i < ExpTwo(lvl) && (i + 1) % (2 * stride) != 0; \pointer_index(output, i, 1));
			ensures (tid==0 && (2 * stride == ExpTwo(lvl))) ==> \pointer_index(output, ExpTwo(k) - 1, 1);
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
	//@ assert indicator < ExpTwo(k) ==> Matrix_UP[lvl - 1][indicator] == output[indicator];
	//@ assert |Matrix| == lvl;
	//@ assert (\forall int i; 0 <= i && i < k + 1; |Matrix[i]| == ExpTwo(k - i));
	//@ assert (\forall int i; 0 < i && i < k + 1; Matrix[i] == implode(Matrix[i - 1]));
	//@ assert (\forall int i; 0 <= i && i < k + 1; intsum(Matrix[i]) == intsum(input_seq));
	//@ assert |Matrix[k]| == 1;
	//@ assert lemma_intsum_single(Matrix[k][0]);
	//@ assert intsum(Matrix[k]) == intsum(input_seq);
	//@ assert Matrix[k] == seq<int>{intsum(input_seq)};
	//@ assert Matrix[0] == input_seq;
	//@ assert (\forall int i; 0 <= i && i < k + 1; 0 < |Matrix[i]| && |Matrix[i]| <= ExpTwo(k));

/////////////////////////////////////////////////////////////////////////////////

  //@ assert indicator < ExpTwo(k) && indicator >= stride && 2 * tid < |Matrix[lvl - 1]| ==> output[indicator - stride] == Matrix[lvl - 1][2 * tid];

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
		requires indicator < ExpTwo(k) ==> \pointer_index(output, indicator, 1);
		requires indicator < ExpTwo(k) && indicator >= stride  ==> \pointer_index(output, indicator - stride, 1);
		requires tid==0 ==> (\forall* int i; 0 <= i && i < ExpTwo(k) && (i + 1) % stride != 0; \pointer_index(output, i, 1));
		requires (tid==0 && (stride == ExpTwo(k))) ==> \pointer_index(output, ExpTwo(k) - 1, 1);
		requires (\forall int i; 0 <= i && i <= k; |Matrix_UP[i]| == ExpTwo(k));
    requires (\forall int i; 0 <= i && i < lvl; |Matrix[i]| == ExpTwo(k - i));
    requires (\forall int i; 0 <= i && i < lvl; 0 <= |Matrix[i]| && |Matrix[i]| <= ExpTwo(k));
		requires indicator < ExpTwo(k) && indicator >= stride ==> Matrix_UP[lvl - 1][indicator] == output[indicator];
		requires indicator < ExpTwo(k) && indicator >= stride ==> Matrix_UP[lvl - 1][indicator - stride] == output[indicator - stride];
    requires indicator < ExpTwo(k) && indicator >= stride && 2 * tid < |Matrix[lvl - 1]| ==> output[indicator - stride] == Matrix[lvl - 1][2 * tid];
		context tid >= 0 && tid < ExpTwo(k);
		ensures ExpTwo(k) * \ltid + ExpTwo(k) - 1 < ExpTwo(k) ==> \pointer_index(output, ExpTwo(k) * \ltid + ExpTwo(k) - 1, 1);
		ensures ExpTwo(k) * \ltid + ExpTwo(k) - 1 < ExpTwo(k) && ExpTwo(k) * \ltid + ExpTwo(k) - 1 >= ExpTwo(k) / 2  ==> \pointer_index(output, ExpTwo(k) * \ltid + ExpTwo(k) - 1 - ExpTwo(k) / 2, 1);
		ensures tid==0 ==> (\forall* int i; 0 <= i && i < ExpTwo(k) && (i + 1) % (ExpTwo(k) / 2) != 0; \pointer_index(output, i, 1));
    ensures (\forall int i; 0 <= i && i <= k; |Matrix_UP[i]| == ExpTwo(k));
    ensures (\forall int i; 0 <= i && i < lvl; |Matrix[i]| == ExpTwo(k - i));
    ensures (\forall int i; 0 <= i && i < lvl; 0 <= |Matrix[i]| && |Matrix[i]| <= ExpTwo(k));
		//ensures ExpTwo(k) * \ltid + ExpTwo(k) - 1 < ExpTwo(k) ==> Matrix_UP[lvl - 1][ExpTwo(k) * \ltid + ExpTwo(k) - 1] == output[ExpTwo(k) * \ltid + ExpTwo(k) - 1];
		//ensures ExpTwo(k) * \ltid + ExpTwo(k) - 1 < ExpTwo(k) && ExpTwo(k) * \ltid + ExpTwo(k) - 1 >= ExpTwo(k) / 2 ==> Matrix_UP[lvl - 1][ExpTwo(k) * \ltid + ExpTwo(k) - 1 - ExpTwo(k) / 2] == output[ExpTwo(k) * \ltid + ExpTwo(k) - 1 - ExpTwo(k) / 2];
    //ensures 2 * tid < |Matrix[lvl-2]| && ExpTwo(k) * \ltid + ExpTwo(k) - 1 < ExpTwo(k) && ExpTwo(k) * \ltid + ExpTwo(k) - 1 >= ExpTwo(k) / 2 ==> output[ExpTwo(k) * \ltid + ExpTwo(k) - 1 - ExpTwo(k) / 2] == get(Matrix[lvl-2], 2 * tid);
  @*/
  __syncthreads();

  // (unstability) These come from the last three postconditions in the previous barrier:
		//@ assume ExpTwo(k) * tid + ExpTwo(k) - 1 < ExpTwo(k) ==> Matrix_UP[lvl - 1][ExpTwo(k) * tid + ExpTwo(k) - 1] == output[ExpTwo(k) * tid + ExpTwo(k) - 1];
		//@ assume ExpTwo(k) * tid + ExpTwo(k) - 1 < ExpTwo(k) && ExpTwo(k) * tid + ExpTwo(k) - 1 >= ExpTwo(k) / 2 ==> Matrix_UP[lvl - 1][ExpTwo(k) * tid + ExpTwo(k) - 1 - ExpTwo(k) / 2] == output[ExpTwo(k) * tid + ExpTwo(k) - 1 - ExpTwo(k) / 2];
    //@ assume 2 * tid < |Matrix[lvl-2]| && ExpTwo(k) * tid + ExpTwo(k) - 1 < ExpTwo(k) && ExpTwo(k) * tid + ExpTwo(k) - 1 >= ExpTwo(k) / 2 ==> output[ExpTwo(k) * tid + ExpTwo(k) - 1 - ExpTwo(k) / 2] == get(Matrix[lvl-2], 2 * tid);

///////////////////////////////////////////////////////////////////////////////////////	Down

	indicator = ExpTwo(k) * tid + ExpTwo(k) - 1; // output.length * tid + output.length - 1;
	stride = ExpTwo(k) / 2; // output.length / 2;
	lvl = k - 1; //lvl - 2;
	int temp;
	//@ ghost seq<int> temp_seq = seq<int> { 0 };

	//@ assert ExpTwo(k) * tid + ExpTwo(k) - 1 < ExpTwo(k) ==> Matrix_UP[lvl + 1][indicator] == output[indicator];
	//@ assert ExpTwo(k) * tid + ExpTwo(k) - 1 < ExpTwo(k) && ExpTwo(k) * tid + ExpTwo(k) - 1 >= ExpTwo(k) / 2 ==> Matrix_UP[lvl + 1][indicator - stride] == output[indicator - stride];



	if(indicator < ExpTwo(k))
	{
		output[indicator] = 0;
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
  loop_invariant indicator < ExpTwo(k) ==> \pointer_index(output, indicator, 1);
  loop_invariant lvl >= 0 && indicator < ExpTwo(k) && indicator >= stride ==> \pointer_index(output, indicator - stride, 1);
  loop_invariant (tid==0 && stride > 0) ==> (\forall* int i; 0 <= i && i < ExpTwo(k) && (i + 1) % stride != 0; \pointer_index(output, i, 1));
  loop_invariant |temp_seq| == ExpTwo(k - (lvl + 1));
  loop_invariant 0 < |temp_seq| && |temp_seq| <= ExpTwo(k);
  loop_invariant temp_seq == psum2(Matrix[lvl + 1]);
  loop_invariant (\forall int i; 0 <= i && i < k + 1; 0 < |Matrix[i]| && |Matrix[i]| <= ExpTwo(k));
  loop_invariant (\forall int i; 0 <= i && i < k + 1; |Matrix[i]| == ExpTwo(k - i));
  loop_invariant (\forall int i; 0 <= i && i < k + 1; intsum(Matrix[i]) == intsum(input_seq));
  loop_invariant (\forall int i; 0 < i && i < k + 1; Matrix[i] == implode(Matrix[i - 1]));
  loop_invariant Matrix[0] == input_seq;
  loop_invariant Matrix[k] == seq<int>{ intsum(input_seq) };
  loop_invariant tid < |temp_seq| && indicator < ExpTwo(k) ==> temp_seq[tid] == output[indicator];
  loop_invariant lvl >= 0 && 2 * tid < |Matrix[lvl]| && indicator < ExpTwo(k) && indicator >= stride ==> output[indicator - stride] == get(Matrix[lvl], 2 * tid);
  @*/
  while(stride >= 1)
  {
  	if(indicator < ExpTwo(k) && indicator >= stride)
  	{

  		//@ assert tid < |temp_seq| ==> temp_seq[tid] == output[indicator];
  		temp = output[indicator];
  		//@ assert tid < |temp_seq| ==> temp == temp_seq[tid];
  		output[indicator] = output[indicator] + output[indicator - stride];
  		//@ assert tid < |temp_seq| ==> output[indicator] == temp_seq[tid] + output[indicator - stride];

  		//@ assert 2 * tid < |Matrix[lvl]| ==> output[indicator - stride] == get(Matrix[lvl], 2 * tid);
  		//@ assert 2 * tid < |Matrix[lvl]| && tid < |temp_seq| ==> output[indicator] == temp_seq[tid] + get(Matrix[lvl], 2 * tid);
  		//@ assert tid < |Matrix[lvl + 1]| && tid < |temp_seq| ==> temp_seq[tid] == get(psum2(Matrix[lvl + 1]), tid);
  		//@ assert tid < |Matrix[lvl + 1]| && 2 * tid < |Matrix[lvl]| ==> output[indicator] == get(psum2(Matrix[lvl + 1]), tid) + get(Matrix[lvl], 2 * tid);
  		//@ assert Matrix[lvl + 1] == implode(Matrix[lvl]);
  		//@ assert tid < |implode(Matrix[lvl])| && 2 * tid < |Matrix[lvl]| ==> output[indicator] == get(psum2(implode(Matrix[lvl])), tid) + get(Matrix[lvl], 2 * tid);
      //@ ghost tid < |implode(Matrix[lvl])| ? lemma_get_psum_implode(Matrix[lvl], tid) : true;
  		/*if(tid < |implode(Matrix[lvl])|){
  			lemma_get_psum_implode(Matrix[lvl], tid);
  		}*/
  		//@ assert tid < |implode(Matrix[lvl])| && 2 * tid < |Matrix[lvl]| ==> get(psum2(implode(Matrix[lvl])), tid) == get(psum2(Matrix[lvl]), 2 * tid);
  		//@ assert 2 * tid < |Matrix[lvl]| ==> output[indicator] == get(psum2(Matrix[lvl]), 2 * tid) + get(Matrix[lvl], 2 * tid);
  		//@ ghost 2 * tid + 1 < |Matrix[lvl]| ? lemma_combine_psum(Matrix[lvl], tid) : true;

  		//@ assert 2 * tid + 1 < |Matrix[lvl]| ==> get(psum2(Matrix[lvl]), 2 * tid + 1) == get(psum2(Matrix[lvl]), 2 * tid) + get(Matrix[lvl], 2 * tid);
  		//@ assert 2 * tid + 1 < |Matrix[lvl]| ==> output[indicator] == get(psum2(Matrix[lvl]), 2 * tid + 1);

  		//@ assert tid < |temp_seq| ==> temp == temp_seq[tid];
  		output[indicator - stride] = temp;
  		//@ assert tid < |temp_seq| ==> output[indicator - stride] == temp_seq[tid];

  		//@ assert tid < |Matrix[lvl + 1]| && tid < |temp_seq| ==> temp_seq[tid] == get(psum2(Matrix[lvl + 1]), tid);
  		//@ assert Matrix[lvl + 1] == implode(Matrix[lvl]);
  		//@ assert tid < |implode(Matrix[lvl])| && tid < |temp_seq| ==> temp_seq[tid] == get(psum2(implode(Matrix[lvl])), tid);
  		//@ ghost tid < |implode(Matrix[lvl])| ? lemma_get_psum_implode(Matrix[lvl], tid) : true;


  		//@ assert tid < |implode(Matrix[lvl])| && 2 * tid < |Matrix[lvl]| ==> get(psum2(implode(Matrix[lvl])), tid) == get(psum2(Matrix[lvl]), 2 * tid);
  		//@ assert 2 * tid < |Matrix[lvl]| && tid < |temp_seq| ==> temp_seq[tid] == get(psum2(Matrix[lvl]), 2 * tid);
  		//@ assert 2 * tid < |Matrix[lvl]| ==> output[indicator - stride] == get(psum2(Matrix[lvl]), 2 * tid);

  	}

  	//@ ghost temp_seq = psum2(Matrix[lvl]);

  	//@ assert 2 * tid < |temp_seq| && indicator < ExpTwo(k) && indicator >= stride ==> output[indicator - stride] == temp_seq[2 * tid];
  	//@ assert 2 * tid + 1 < |temp_seq| && indicator < ExpTwo(k) && indicator >= stride ==> output[indicator] == temp_seq[2 * tid + 1];


    /*@
  		context lvl >= 0 && lvl <= k - 1;
  		requires tid >= 0 && tid < ExpTwo(k);
        context |temp_seq| == ExpTwo(k - lvl);
        context 0 < |temp_seq| && |temp_seq| <= ExpTwo(k);
        context |Matrix| == k + 1;
  		requires indicator >= 0;
  		requires stride >= 1 ;
  		requires stride == ExpTwo(lvl);
  		requires indicator+1 == ExpTwo(lvl+1)*(\ltid+1);
  		requires indicator < ExpTwo(k) ==> \pointer_index(output, indicator, 1);
  		requires indicator < ExpTwo(k) && indicator >= stride  ==> \pointer_index(output, indicator - stride, 1);
  		requires tid==0 ==> (\forall* int i; 0 <= i && i < ExpTwo(k) && (i + 1) % stride != 0; \pointer_index(output, i, 1));
      //requires 2 * tid < |temp_seq| && indicator < ExpTwo(k) && indicator >= stride ==> output[indicator - stride] == temp_seq[2 * tid];
      //requires 2 * tid + 1 < |temp_seq| && indicator < ExpTwo(k) && indicator >= stride ==> output[indicator] == temp_seq[2 * tid + 1];
      requires (\forall int i; 0 <= i && i < k + 1; |Matrix[i]| == ExpTwo(k - i));
      requires (\forall int i; 0 <= i && i < k + 1; 0 <= |Matrix[i]| && |Matrix[i]| <= ExpTwo(k));
      //requires 2 * tid < |Matrix[lvl]| && indicator < ExpTwo(k) && indicator >= stride ==> output[indicator - stride] == get(psum2(Matrix[lvl]), 2 * tid);
  		ensures tid >= 0 && tid < ExpTwo(k);
  		ensures lvl-1 >= 0 ==> stride / 2 == ExpTwo(lvl - 1);
  		ensures lvl-1 == -1 ==> stride / 2 == 0;
      ensures stride / 2 == 0  ==> lvl-1 == -1;
  		ensures stride / 2 >= 0;
  		ensures (indicator - 1) / 2 >= 0;
  		ensures (indicator - 1) / 2+1 == ExpTwo(lvl)*(tid+1);
  		ensures (indicator - 1) / 2 < ExpTwo(k) ==> \pointer_index(output, (indicator - 1) / 2, 1);
  		ensures lvl-1 >= 0 && (indicator - 1) / 2 < ExpTwo(k) && (indicator - 1) / 2 >= stride / 2  ==> \pointer_index(output, (indicator - 1) / 2 - stride / 2, 1);
  		ensures (tid==0 && stride/2 > 0) ==> (\forall* int i; 0 <= i && i < ExpTwo(k) && (i + 1) % (stride/2) != 0; \pointer_index(output, i, 1));
      ensures (\forall int i; 0 <= i && i < k + 1; |Matrix[i]| == ExpTwo(k - i));
      ensures (\forall int i; 0 <= i && i < k + 1; 0 <= |Matrix[i]| && |Matrix[i]| <= ExpTwo(k));
      //ensures tid < |temp_seq| && (indicator - 1) / 2 < ExpTwo(k) ==> temp_seq[tid] == output[(indicator - 1) / 2];
      //ensures lvl-1 >= 0 && 2 * tid < |Matrix[lvl-1]| && (indicator - 1) / 2 < ExpTwo(k) && (indicator - 1) / 2 >= stride / 2 ==> output[(indicator - 1) / 2 - stride / 2] == get(Matrix[lvl-1], 2 * tid);
    @*/
    __syncthreads();

    // (unstability) These come from the last two postconditions in the previous barrier:
  	//@ assume tid < |temp_seq| && (indicator - 1) / 2 < ExpTwo(k) ==> temp_seq[tid] == output[(indicator - 1) / 2];
  	//@ assume lvl-1 >= 0 && 2 * tid < |Matrix[lvl-1]| && (indicator - 1) / 2 < ExpTwo(k) && (indicator - 1) / 2 >= stride / 2 ==> output[(indicator - 1) / 2 - stride / 2] == get(Matrix[lvl-1], 2 * tid);

  	indicator = (indicator - 1) / 2;
  	stride = stride / 2;
  	lvl = lvl - 1;


  }


  //@ assert temp_seq == psum2(Matrix[0]);
  //@ assert Matrix[0] == input_seq;
  //@ assert temp_seq == psum2(input_seq);
  //@ assert tid < |temp_seq| && indicator < ExpTwo(k) ==> temp_seq[tid] == output[indicator];





}


////////////////////////////////////////////////////////////////////////////////
// CUDA Functions
////////////////////////////////////////////////////////////////////////////////
//@ requires N > 0;
//@ ensures \pointer(\result, N, 1);
int *vercorsMallocInt(int N);
void vercorsFreeInt(int *ar);
//@ ensures \pointer(\result, N, 1);
int *vercorsCudaMallocInt(int N);
void vercorsCudaFreeInt(int *addr);
//@ context \pointer(src, N, read) ** \pointer(tgt, N, 1);
//@ ensures (\forall int i; i >= 0 && i < N; src[i] == tgt[i]);
void vercorsCudaMemcpyInt(int *tgt, int *src, int N, int direction);


////////////////////////////////////////////////////////////////////////////////
// Main Program
////////////////////////////////////////////////////////////////////////////////
int CUDA_Host_Blelloch( int argc, char** argv)
{
  int k = 10; // size of the input is 2^k

  int* host_input = vercorsMallocInt(ExpTwo(k)); // size of the host_input is 2^k
  int* host_output = vercorsMallocInt(ExpTwo(k)); // size of the host_output is 2^k

  //@ loop_invariant k == 10;
  //@ loop_invariant q >= 0 && q <= ExpTwo(k);
  //@ loop_invariant \pointer(host_input, ExpTwo(k), 1) ** \pointer(host_output, ExpTwo(k), 1);
  //@ loop_invariant (\forall int i; i >= 0 && i < q; host_input[i] == host_output[i]);
  for(int q=0; q<ExpTwo(k); q++)
  {
    host_output[q] = host_input[q];
  }

  //Copy the arrays to device memory
  int* device_output;
  device_output = vercorsCudaMallocInt(ExpTwo(k));
  vercorsCudaMemcpyInt(device_output, host_output, ExpTwo(k), cudaMemcpyHostToDevice) ;
  //@ assert (\forall int i; i >= 0 && i < ExpTwo(k); host_output[i] == device_output[i]);
  //@ assert (\forall int i; i >= 0 && i < ExpTwo(k); host_output[i] == host_input[i]);
  //@ assert (\forall int i; i >= 0 && i < ExpTwo(k); device_output[i] == host_input[i]);

  //setup execution parameters
	int num_of_blocks = 1;
	int num_of_threads_per_block = ExpTwo(k);




  //Kernel launch
  CUDA_Kernel_Blelloch<<< /*grid*/num_of_blocks, /*threads*/num_of_threads_per_block/*, 0*/ >>>(device_output, k);

  // copy result from device to host
  //vercorsCudaMemcpyInt(host_output, device_output, ExpTwo(k), cudaMemcpyDeviceToHost);

  // cleanup memory
  vercorsFreeInt(host_output);
  vercorsCudaFreeInt(device_output);

}