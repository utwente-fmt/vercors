package hre.ast;



public class AssertOrigin extends BranchOrigin {
    public RestOfContractOrigin restOfContractOrigin;

    public AssertOrigin(String branch, Origin base, RestOfContractOrigin restOfContractOrigin){
        super(branch, base);
        this.restOfContractOrigin = restOfContractOrigin;
    }
}
