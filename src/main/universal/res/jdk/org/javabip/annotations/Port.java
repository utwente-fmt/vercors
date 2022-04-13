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

package jdk.org.javabip.annotations;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

import org.javabip.api.PortType;

/**
 * It specifies the name and the type of the port.
 * 
 * @author Alina Zolotukhina
 */
// Retention(RetentionPolicy.RUNTIME)
public @interface Port {

	/**
	 * It returns the name of the port.
	 * 
	 * @return the name of the port.
	 */
	String name();

	/**
	 * It specifies the type of the port. The type can be either spontaneous or enforceable.
	 * 
	 * @return the type of the port.
	 */
	// PortType type(); // TODO (RR): When enums are back, turn this on again

}
