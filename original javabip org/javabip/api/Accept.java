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
 * It specifies the functionality of the accept macro of the BIP Glue.
 */
public interface Accept {

	/**
	 * It returns the effect port of the accept.
	 * 
	 * @return the effect port of the given accept.
	 */
	public PortBase getEffect();

	/**
	 * It returns a collection of causes for a given effect of the accept macro of the glue.
	 * 
	 * @return the cause ports of the given accept.
	 */
	public Collection<PortBase> getCauses();

}
