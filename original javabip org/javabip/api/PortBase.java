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

/**
 * It specifies the functionality of the BIP component port.
 */
public interface PortBase {

	/**
	 * It gets the id of the port.
	 * 
	 * @return the port id.
	 */
	public String getId();

	/**
	 * It returns the spec type to which this port belongs to. Often it is fully qualified name of class specifying the
	 * BIP specification.
	 * 
	 * @return the spec type.
	 */
	public String getSpecType();

}
