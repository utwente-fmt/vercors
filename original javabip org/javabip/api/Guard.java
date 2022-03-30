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

package org.javabip.api;

import java.util.Collection;

/**
 * It specifies the interface for transition guards.
 */
public interface Guard {

	/**
	 * It returns the name of the guard.
	 * 
	 * @return the name of the guard.
	 */
	public String name();

	/**
	 * It returns the collection of specifications of data that is required by this guard in order to be evaluated.
	 * 
	 * @return the collection of data required for guard evaluation.
	 */
	public Collection<Data<?>> dataRequired();

	/**
	 * It returns true if guard requires data for its evaluation.
	 * 
	 * @return true if guard requires data, false otherwise.
	 */
	public Boolean hasData();

	/**
	 * Given the data arguments, evaluates the guard.
	 * 
	 * @param args
	 *            the component is passed as the first argument, then the data valuations required by the guard.
	 * @return the result of the guard evaluation.
	 */

	public boolean evaluateGuard(Object... args);
}