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
import java.util.Set;

/**
 * It specifies the behaviour of any BIP component. Instances of those class are meant to be immutable even if using
 * data structures which provide a possibility for mutation. Do not mutate.
 */
public interface Behaviour {

	/**
	 * Gets the mapping between each state and the ports that can be enabled in this state.
	 * 
	 * @return the map of states to the set of ports.
	 */
	public Map<String, Set<Port>> getStateToPorts();

	/**
	 * Gets the name of the component type for this behavior.
	 * 
	 * @return the component type described by this behavior.
	 */
	public String getComponentType();

	/**
	 * Gets the list of the enforceable ports within this behavior.
	 * 
	 * @return the list of enforceable ports.
	 */
	public List<Port> getEnforceablePorts();

	/**
	 * Returns true if the port is spontaneous.
	 * 
	 * @param port
	 *            the name of the port, can not be null or empty.
	 * @return true if the port is spontaneous, otherwise false.
	 */
	public boolean isSpontaneousPort(String port);

	/**
	 * Gets all the possible states of the component described by this behaviour.
	 * 
	 * @return the set of all states.
	 */
	public Set<String> getStates();

	/**
	 * Port to data in for guard.
	 * 
	 * @return the map
	 */
	public Map<Port, Set<Data<?>>> portToDataInForGuard();

	// TODO DESIGN BUG what if a given port is associated with different transitions within behavior and these
	// transitions have different guards associated to them? This comment applies to both functions of
	// portToDataInForGuard and portToDataInForTransition.
	/**
	 * Gets the set of Data required for the guard execution of a transition corresponding to the specified port.
	 * 
	 * @param port
	 *            the port for whose guard required data is returned.
	 * @return the sets of data required by the guard to the given transition associated with a given port.
	 */
	public Set<Data<?>> portToDataInForGuard(Port port);
	//It is used as a set by DataCoordinator

	/**
	 * Gets the set of Data required for the execution of a transition corresponding to the specified port.
	 * 
	 * @param port
	 *            the port corresponding to some transition requiring data
	 * @return the iterable of data required for the execution of a transition associated with a given port.
	 */
	public Iterable<Data<?>> portToDataInForTransition(Port port);

	/**
	 * Given the name of a data, returns the list of ports of this component which need the data for a guard or for a
	 * transition.
	 * 
	 * @param dataName
	 *            the name of the input data variable
	 * @return the list of ports that need data for guard computation or transition execution.
	 */
	public List<Port> portsNeedingData(String dataName);

	/**
	 * Given the name of a data, returns the set of ports of this component which can provide this data.
	 * 
	 * @param dataName
	 *            the name of the output data variable.
	 * @return set of ports which can provide the data.
	 */
	public Set<Port> getDataProvidingPorts(String dataName);

	/**
	 * Gets all the transitions of the component specified by this behaviour.
	 * 
	 * @return the list of all transitions.
	 */
	public List<Transition> getAllTransitions();

	/**
	 * Gets the current state of the component specified by this behaviour.
	 * 
	 * @return the current state of the component.
	 */
	public String getCurrentState();

	/**
	 * Gets all the ports of the component specified by this behaviour.
	 * 
	 * @return an iterable of all ports.
	 */
	public Iterable<Port> getAllPorts();

}