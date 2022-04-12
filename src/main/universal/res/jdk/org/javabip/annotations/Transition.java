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
 * Author: Simon Bliudze, Anastasia Mavridou, Radoslaw Szymanek and Alina Zolotukhina
 * Date: 15.10.12
 */

package org.javabip.annotations;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * It annotates function with information about transition it is performing.
 * 
 * @author Alina Zolotukhina
 */
// Retention(RetentionPolicy.RUNTIME)
public @interface Transition {

	/**
	 * It returns the name of the transition.
	 * 
	 * @return the name of the transition.
	 */
	String name();

	/**
	 * It returns the source state of the transition.
	 * 
	 * @return the source state of the transition.
	 */
	String source();

	/**
	 * It returns the target state of the transition.
	 * 
	 * @return the target state of the transition.
	 */
	String target();

	/**
	 * It returns the guard expression associated with this transition.
	 * 
	 * @return the guard expression of the transition.
	 */
	String guard() default "";

}
