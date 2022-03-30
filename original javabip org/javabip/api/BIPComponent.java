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

import java.util.List;
import java.util.Map;

/**
 * It specifies the functionality of BIP component in BIP framework.
 */
public interface BIPComponent extends Identifiable, BIPActor {

	/**
	 * It enforces the execution of a given enforceable transition associated to a given port id.
	 * 
	 * TODO DESIGN BUG currently if portID is null then component is being asked to do nothing (nop). A separate
	 * function should be added to specify that no transition is enforced. For example, doNothing().
	 * 
	 * @param portID
	 *            the port id of the transition being enforced.
	 */
	void execute(String portID);

	/**
	 * It provides data specified by the name parameter casted to the type provided by clazz parameter.
	 * 
	 * @param <T>
	 *            the generic type
	 * @param name
	 *            the name of the data being requested.
	 * @param clazz
	 *            clazz specifies the type of the data being returned.
	 * @return the data that was requested as specified by name and clazz parameter.
	 */
	public <T> T getData(String name, Class<T> clazz);

	/**
	 * It makes it possible to query BIP Component if a given port is enabled for given data valuations.
	 * 
	 * @param port
	 *            the port of the associated transition being checked for enabledness.
	 * @param data
	 *            a list of data valuations, where each map is a complete set of data needed by the transition.
	 * @return the list of booleans specifying for each map if the transition is enabled or not.
	 */
	public List<Boolean> checkEnabledness(PortBase port, List<Map<String, Object>> data);

	/**
	 * Provides data to a BIP component for a given data dependent transition that will be soon enforced.
	 * 
	 * @param dataName
	 *            the name of the data being specified.
	 * @param value
	 *            the value of the data identified by dataName parameter.
	 */
	public void setData(String dataName, Object value);

}
