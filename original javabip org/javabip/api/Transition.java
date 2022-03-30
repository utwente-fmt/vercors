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
 */

package org.javabip.api;

import java.util.Collection;

/**
 * It specifies the functionality of the BIP component transition.
 */
public interface Transition {

	/**
	 * It gets the string containing the guard expression of the transition.
	 * 
	 * @return the guard expression.
	 */
	public String guard();

	/**
	 * It returns the name of transition.
	 * 
	 * @return the name of transition.
	 */
	public String name();

	/**
	 * It returns the source place of transition.
	 * 
	 * @return the source place of transition.
	 */
	public String source();

	/**
	 * It returns the target place of transition.
	 * 
	 * @return the target place of transition.
	 */
	public String target();

	/**
	 * It checks if transition has a guard.
	 * 
	 * @return true, if transition has a guard, false otherwise.
	 */
	public boolean hasGuard();

	/**
	 * It gets a collection of guard objects corresponding to the guards used in the guard expression of the transition.
	 * 
	 * @return the collection of transition guards.
	 */
	public Collection<Guard> transitionGuards();
}
