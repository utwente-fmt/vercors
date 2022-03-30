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
 * Date: 24.01.14
 */

package org.javabip.api;

import java.util.Set;

/**
 * It specifies the interface for data being provided by BIP component.
 * 
 * @param <T>
 *            the generic type
 */
public interface DataOut<T> extends Data<T> {

	/**
	 * It specifies the ports for which the specification of output data applies, i.e. which are allowed to export the
	 * data.
	 * 
	 * @return the ports if type requires a list of ports.
	 */
	public Set<Port> allowedPorts();

	/**
	 * It specifies the type of data accessibility.
	 * 
	 * @return the type
	 */
	// public AccessType portAccessType();
}

