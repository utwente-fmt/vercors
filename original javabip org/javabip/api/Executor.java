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
 * It specifies the interface for the executor that governs the execution of the BIP component.
 */
public interface Executor extends BIPComponent {

	/**
	 * It tells the executor to register the component for which the executor was created in the specified BIP engine.
	 * 
	 * @param bipEngine
	 *            the BIP engine where the component has been registered.
	 */
	void register(BIPEngine bipEngine);

	/**
	 * It tells the executor that it is no longer collaborating with a previously registered BIP engine.
	 */
	void deregister();

	/**
	 * It returns the BIP engine that Executor uses.
	 */
	BIPEngine engine();

}
