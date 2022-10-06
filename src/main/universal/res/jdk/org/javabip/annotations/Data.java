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

package org.javabip.annotations;

// import java.lang.annotation.Retention;
// import java.lang.annotation.RetentionPolicy;

/**
 * It is used to annotated data sent between BIP components. It can be used as annotation on the code directly or
 * created programmatically and passed to BehaviourBuilder API.
 * 
 * Data can be specified as annotations within BIP Specification class. Even if behavior spec is specified
 * programmatically it is still possible to use data annotation parser. If no data information is provided then the name
 * is assumed to be equal to Method.name() + SEPARATOR + parameter.no
 * 
 * @author Alina Zolotukhina
 */
// Retention(RetentionPolicy.RUNTIME)
public /*@ bip_annotation @*/ @interface Data {

	/**
	 * It returns the name of the data.
	 * 
	 * @return the name of the data.
	 */
	String name();

	/**
	 * It returns the type of access to the data via specific ports. The default value is 'any' which means that data
	 * can be obtained within any port (transition).
	 * 
	 * @return the access type for the data
	 */
	// TODO TEST create a test for Specifications with unallowed ports
	// TODO TEST create a test for Specifications with witness
	// AccessType accessTypePort() default AccessType.any; // any, witness, allowed, unallowed

	/**
	 * It returns the ports (if required) for a specific access type being used.
	 * 
	 * @return the array of ports.
	 */
	String[] ports(); // default {};

}
