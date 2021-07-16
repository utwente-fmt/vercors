package vct.experiments.hoare_logic_checker;

import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ASTClass;

import java.util.ArrayList;
import java.util.List;

@SuppressWarnings("all")
public class Brain {
    private boolean keepChecking;
    private String sourceTriple;
    private int currentSet = 0;
    private String[] smtTriple;
    private String[] variables;
    private String counterExample;//array?
    private Translator trans;
    private SMTinter smtinter;
    private SMTresult results;

    public Brain(ASTClass programme) {
        smtinter = new SMTinter();
        trans = new Translator(programme, this);
    }

    public static void main(ASTClass program) {
        Brain b = new Brain(program);
    }

    public boolean checkWithZ3(List<String> assertions, List<String> variablesZ3, ASTNode node) {
        boolean ans = false;
        variables = new String[variablesZ3.size()];
        for (int i = 0; variablesZ3.size() > i; i++) {
            variables[i] = variablesZ3.get(i);
        }
        smtTriple = new String[assertions.size()];
        for (int i = 0; assertions.size() > i; i++) {
            smtTriple[i] = "(assert";
            smtTriple[i] = smtTriple[i].concat(assertions.get(i));
            smtTriple[i] = smtTriple[i].concat(")");
            smtTriple[i] = smtTriple[i].replaceAll("\\\\", "");
        }
        results = smtinter.checkTripleZ3(smtTriple, variables, "");
        if (results.getSatisfying() && results.gotAnswer()) {
            List<String> assist = new ArrayList<String>();
            for (int i = 0; results.getExample().size() > i; i++) {
                String temp = results.getExample().get(i);
                if (!temp.endsWith("sat") && !temp.contains("exit 0")) {
                    temp = temp.replaceAll("stdout: ", "");
                    temp = temp.replaceAll("\\(", "");
                    temp = temp.replaceAll("\\)", "");
                    temp = temp.replaceAll("- ", "-");
                    temp = temp.replaceAll(" ", " = ");
                    assist.add(temp);
                }
            }
            node.getOrigin().report("error", assist);
            ans = true;
        }
        currentSet = assertions.size();
        return ans;
    }

    public void newProblem() {
        currentSet = 0;
    }
}
