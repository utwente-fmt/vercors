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

import java.util.Map;

/**
 * Specifies the actor functionality of the BIP component. This functionality focuses on asynchronous/spontaneous events. 
 *
 */
public interface BIPActor {
	/**
	 * It informs BIP component that a given spontaneous event associated with a port id has occurred.
	 * 
	 * @param portID
	 *            the port id specifying a spontaneous event that has occurred.
	 */
	void inform(String portID);

	/**
	 * It informs BIP component that a given spontaneous event associated with a port id has occurred.
	 * 
	 * @param portID
	 *            the port id specifying a spontaneous event that has occurred.
	 * @param data
	 *            the data provided with a spontaneous event.
	 */
	void inform(String portID, Map<String, Object> data);

}
