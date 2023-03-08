/*
 * Copyright 2012-2016 École polytechnique fédérale de Lausanne (EPFL), Switzerland
 * Copyright 2012-2016 Crossing-Tech SA, Switzerland
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Author: Simon Bliudze, Anastasia Mavridou, Larisa Safina, Radoslaw Szymanek and Alina Zolotukhina
 * Date: 23.02.22
 */

package org.javabip.annotations;

// import java.lang.annotation.Retention;
// import java.lang.annotation.RetentionPolicy;

/**
 * A state predicate annotation
 */
// Retention(RetentionPolicy.RUNTIME)
public /*@ bip_annotation @*/ @interface StatePredicate {

	/**
	 * state is a string containing the name of the state
	 * @return the name of the state.
	 */
	String state();

	/**
	 * expr is a string containing plain Java expressions
	 * it must hold when the state is entered.
	 * when the component moves to a new state, expr may be assumed to hold at the start of the transition
	 * @return expr.
	 */
	String expr();
}
