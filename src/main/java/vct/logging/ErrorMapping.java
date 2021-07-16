package vct.logging;

import hre.ast.BranchOrigin;
import vct.logging.VerCorsError.ErrorCode;

import java.util.HashMap;

public class ErrorMapping extends PassAddVisitor {

    private HashMap<String, HashMap<ErrorCode, ErrorCode>> map
            = new HashMap<String, HashMap<ErrorCode, ErrorCode>>();

    public ErrorMapping(PassReport report) {
        super(report);
    }

    @Override
    public void visit(VerCorsError error) {
        if (error.main instanceof BranchOrigin) {
            BranchOrigin origin = (BranchOrigin) error.main;
            HashMap<ErrorCode, ErrorCode> codemap = map.get(origin.branch);
            if (codemap != null) {
                ErrorCode val = codemap.get(error.code);
                if (val != null) {
                    error = new VerCorsError(val, error.sub, origin.base, error.aux);
                }
            }
        }
        super.visit(error);
    }

    public void add(String branch, ErrorCode key, ErrorCode val) {
        HashMap<ErrorCode, ErrorCode> codemap = map.get(branch);
        if (codemap == null) {
            codemap = new HashMap<ErrorCode, ErrorCode>();
            map.put(branch, codemap);
        }
        codemap.put(key, val);
    }

}
