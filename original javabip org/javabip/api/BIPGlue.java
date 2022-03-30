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

import java.io.OutputStream;
import java.util.List;

/**
 * It specifies functionality required from BIP Glue based on accepts/requires formalism.
 */
public interface BIPGlue {

	/**
	 * Gets the accept constraints.
	 * 
	 * @return the accept constraints
	 */
	public List<Accept> getAcceptConstraints();

	/**
	 * Gets the requires constraints.
	 * 
	 * @return the requires constraints
	 */
	public List<Require> getRequiresConstraints();

	/**
	 * Gets the data wires.
	 * 
	 * @return the data wires
	 */
	public List<DataWire> getDataWires();

	/**
	 * It writes xml representation of BIP glue to provided output stream.
	 * 
	 * @param outputStream
	 *            the output stream to which BIP glue xml representation is written to.
	 */
	public void toXML(OutputStream outputStream);

}
