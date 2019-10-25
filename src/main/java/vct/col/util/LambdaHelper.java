package vct.col.util;

import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Predicate;
import scala.Function1;
import scala.Function2;
import scala.runtime.AbstractFunction1;
import scala.runtime.AbstractFunction2;
import scala.runtime.BoxedUnit;

/**
 * Collection of methods to convert Java lambdas to Scala lambdas. 
 * @author whmoortwijn
 */
public class LambdaHelper {
	/**
	 * Converts a unary Java lambda procedure (a consumer) `f` to an equivalent
	 * lambda procedure for Scala, returning `Unit` (i.e. `void`).
	 * @param f The lambda procedure to convert
	 * @return The converted procedure
	 */
	public static <T> Function1<T,BoxedUnit> fun(Consumer<T> f) {
		return new AbstractFunction1<T,BoxedUnit>() {
			@Override
	    	public BoxedUnit apply(T arg) {
				f.accept(arg);
	    		return BoxedUnit.UNIT;
	    	}
		};
	}
	
	/**
	 * Converts a binary Java lambda procedure (a bi-consumer) `f` to an equivalent
	 * binary lambda procedure for Scala, returning `Unit` (i.e. `void`).
	 * @param f The lambda procedure to convert
	 * @return The converted procedure
	 */
	public static <T,U> Function2<T,U,BoxedUnit> fun(BiConsumer<T,U> f) {
		return new AbstractFunction2<T,U,BoxedUnit>() {
			@Override
	    	public BoxedUnit apply(T arg1, U arg2) {
				f.accept(arg1, arg2);
	    		return BoxedUnit.UNIT;
	    	}
		};
	}
}
