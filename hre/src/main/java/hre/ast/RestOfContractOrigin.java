package hre.ast;

/**
 * Sometimes, there can be an error in the contract. When this error goes off, the error supposed
 * to be cause by the synthetic "assert false" is not triggered. Therefore, it looks like it is absent because
 * the backend managed to prove it. But instead, it just didn't come up at all. When that happens, we should
 * be able to find an error with origin RestOfContractOrigin. If this is the case, we shouldn't expect
 * an AssertOrigin to show up, since this probably hid it.
 */
public class RestOfContractOrigin extends BranchOrigin {
    public RestOfContractOrigin(String branch, Origin base) {
        super(branch, base);
    }
}
