package viper.api;

import hre.util.Triple;

import java.util.List;

public interface ProgramFactory<O, T, E, S, DFunc, DAxiom, P> {

    /**
     * Create an empty program.
     *
     * @return program
     */
    public P program();

    /**
     * Add a method to a program.
     *
     * @param p     Program
     * @param o     Origin
     * @param name  Method name
     * @param pres  List of pre conditions
     * @param posts List of post conditions
     * @param in    List of input argument declarations
     * @param out   List of output argument declarations
     * @param local List of local variables
     * @param body  Body statement
     */
    public void add_method(P p, O o, String name,
                           List<E> pres,
                           List<E> posts,
                           List<Triple<O, String, T>> in,
                           List<Triple<O, String, T>> out,
                           List<Triple<O, String, T>> local,
                           List<String> labels,
                           S body);

    public void add_field(P p, O o, String name, T t);

    public void add_predicate(P p, O o, String name, List<Triple<O, String, T>> args, E body);

    public void add_function(P p, O o, String name, List<Triple<O, String, T>> args, T t, List<E> pres, List<E> posts, E body);

    public DAxiom daxiom(O o, String name, E expr, String domain);

    public DFunc dfunc(O o, String name, List<Triple<O, String, T>> args, T t, String domain, boolean unique);

    public void add_adt(P p, O o, String name, List<DFunc> funs, List<DAxiom> axioms, List<String> pars);

    public P parse_program(String file);

    public <T2, E2, S2, DFunc2, DAxiom2, P2>
    P2 convert(ViperAPI<O, T2, E2, S2, DFunc2, DAxiom2, P2> other, P program);

}
