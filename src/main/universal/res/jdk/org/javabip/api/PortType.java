package org.javabip.api;

/**
 * The enum specifying different port types.
 *
 */
public enum PortType {

    /** This port is associated with an enforceable transition. */
    enforceable,
    /** This port is associated with a spontaneous transition. */
    spontaneous,
    /** This port is associated with an internal transition that does not require any external trigger. */
    internal

}