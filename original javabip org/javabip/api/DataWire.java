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
 * Date: 15.10.13
 */

package org.javabip.api;

/**
 * Datawire is part of BIP glue to specify data transfers occurring between BIP components.
 */
public interface DataWire {

	/**
	 * Gets the port from which the data is obtained.
	 * 
	 * @return the port from which the data is obtained.
	 */
	public PortBase getFrom();

	/**
	 * Gets the port to which the data is being sent to.
	 * 
	 * @return the port to which the data is being sent to.
	 */
	public PortBase getTo();

	/**
	 * Checks if the given input data of the given component type is at the incoming end of the wire. The function is
	 * used by the BIP Engine.
	 * 
	 * @param inDataItem
	 *            the name of the input data.
	 * @param componentType
	 *            the component type.
	 * @return true, if the specified data name of the component type is the input data.
	 */
	public boolean isIncoming(String inDataItem, String componentType);
}
