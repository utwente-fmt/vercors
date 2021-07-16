package vct.parsers;

import vct.col.ast.generic.ASTNode;
import vct.col.ast.generic.ASTSequence;

import java.util.ArrayList;
import java.util.Iterator;

public class TempSequence implements ASTSequence<TempSequence> {

    private ArrayList<ASTNode> list = new ArrayList<ASTNode>();

    @Override
    public Iterator<ASTNode> iterator() {
        return list.iterator();
    }

    @Override
    public TempSequence add(ASTNode item) {
        list.add(item);
        return this;
    }

    @Override
    public int size() {
        return list.size();
    }

    @Override
    public ASTNode get(int i) {
        return list.get(i);
    }

}
