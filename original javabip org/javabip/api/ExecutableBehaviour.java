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
 * Date: 27.01.14
 */

package org.javabip.api;

import java.lang.invoke.MethodHandle;
import java.lang.reflect.InvocationTargetException;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.javabip.exceptions.BIPException;

/**
 * It provides an executable behavior that makes it possible to execute transitions within the behavior.
 */
public interface ExecutableBehaviour extends Behaviour {
	// It is put in the API and made public in order for developers to be able to use it while doing their own
	// executors.
	/**
	 * Gets the current state.
	 * 
	 * @return the current state.
	 */
	public String getCurrentState();

	/**
	 * Execute the enforceable transition or spontaneous event based transition as specified by the port.
	 * 
	 * @param portId
	 *            the port id.
	 */
	public void executePort(String portId);

	/**
	 * Execute the enforceable or spontaneous event based transition which needs data that is provided as map parameter.
	 * 
	 * @param portID
	 *            the port id.
	 * @param data
	 *            the data valuations for the transition.
	 */
	public void execute(String portID, Map<String, ?> data);

	/**
	 * It attempts to execute an internal transition if any is enabled.
	 * 
	 * @param guardToValue
	 *            the map of guard valuations.
	 */
	public void executeInternal(Map<String, Boolean> guardToValue);

	/**
	 * Checks for transitions from current state. Enforceable transition with guards with data is treated as enabled, as
	 * the guards value is not known and depends on the value provided.
	 * 
	 * @param portID
	 *            the port id.
	 * @return true, if there is an enabled transition from the current state.
	 */
	public boolean hasEnabledTransitionFromCurrentState(String portID, Map<String, Boolean> guardToValue);

	/**
	 * Gets the mapping of the output data name to java MethodHandle corresponding to a function computing this data.
	 * 
	 * @return the map between the name of the output data and the MethodHandle computing its value.
	 */
	public Map<String, MethodHandle> getDataOutMapping();

	/**
	 * Returns for the specified port the evalutation of its guard on the given data valuations.
	 * 
	 * @param port
	 *            the port to evaluate its guard.
	 * @param data
	 *            the list of data valuations required for the guard of the port.
	 * @return the list of booleans indicating which valuations are accepted.
	 * @throws IllegalAccessException
	 *             the illegal access exception
	 * @throws IllegalArgumentException
	 *             the illegal argument exception
	 * @throws InvocationTargetException
	 *             the invocation target exception
	 */
	// TODO TEST Discussion point, what if guard expression (e.g. !f && g ) requires some data, will it work? Add test
	// for this case.
	public List<Boolean> checkEnabledness(String port, List<Map<String, Object>> data) throws IllegalAccessException,
			IllegalArgumentException, InvocationTargetException;

	/**
	 * Gets the globally disabled ports within the current state the behaviour is in.
	 * 
	 * @param guardToValue
	 *            the map of guard valuations (without data).
	 * @return the globally disabled ports.
	 */
	public Set<Port> getGloballyDisabledEnforceablePortsWithoutDataTransfer(Map<String, Boolean> guardToValue);

	/**
	 * It returns true if there is an enforceable transition that does not need data.
	 * 
	 * @param guardToValue
	 *            the map of guard valuations (without data).
	 * @return true if there is an enforceable transition that does not need data, otherwise false.
	 * @throws BIPException
	 */
	public boolean existInCurrentStateAndEnabledEnforceableWithoutData(Map<String, Boolean> guardToValue)
			throws BIPException;

	/**
	 * Return true if a transition corresponding to the particular port in the current state has data on guards but does
	 * not require data for execution.
	 * 
	 * @param port
	 *            name of the port.
	 * @return true if transition has data on guard but does not require data for execution, otherwise false.
	 * @throws BIPException
	 */
	public boolean transitionNoDataGuardData(String port) throws BIPException;

	/**
	 * It returns true if there is an enabled spontaneous transition.
	 * 
	 * @param guardToValue
	 *            the map of guard valuations.
	 * @return true if there is an enabled spontaneous transition, false otherwise.
	 */
	public boolean existInCurrentStateAndEnabledSpontaneous(Map<String, Boolean> guardToValue);

	/**
	 * It returns true if there is an enabled internal transition.
	 * 
	 * @param guardToValue
	 *            the map of guard valuations.
	 * @return true if there is an enabled internal transition, false otherwise.
	 * @throws BIPException
	 *             when there are two enabled internal transitions from the same state.
	 */
	public boolean existEnabledInternal(Map<String, Boolean> guardToValue) throws BIPException;

	/**
	 * It returns true if there is an enforceable transition requiring data for guard evaluation.
	 * 
	 * @return true if there is an enforceable transition requiring data for guard evaluation, otherwise false.
	 */
	public boolean existInCurrentStateAndEnforceableWithData();

	/**
	 * Compute guards which do not require data.
	 * 
	 * @param currentState
	 *            the current state of the component.
	 * 
	 * @return the map between guard names and their boolean values.
	 */
	public Map<String, Boolean> computeGuardsWithoutData(String currentState);
}
