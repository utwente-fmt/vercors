package vct.antlr4.parser;

import static hre.lang.System.Failure;

import java.io.IOException;
import java.io.InputStream;

import vct.col.ast.generic.ASTNode;
import vct.col.ast.generic.ASTSequence;
import vct.col.ast.stmt.decl.Contract;
import vct.col.ast.stmt.decl.Method;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;

/**
 * Common setup for parsing specification comments.
 */ 
public abstract class CommentParser<Parser extends org.antlr.v4.runtime.Parser,Lexer extends org.antlr.v4.runtime.Lexer> {
  
  protected Parser parser;
  protected Lexer lexer;
  protected CommonTokenStream tokens;
  protected ErrorCounter ec;
  
  public CommentParser(ErrorCounter ec, Parser parser, Lexer lexer) {
    this.parser=parser;
    this.lexer=lexer;
    this.ec=ec;
    parser.removeErrorListeners();
    parser.addErrorListener(ec);
    lexer.removeErrorListeners();
    lexer.addErrorListener(ec);
  }

  public Contract contract(ASTSequence<?> seq,InputStream fifo){
    ANTLRInputStream input;
    try {
      input = new ANTLRInputStream(fifo);
    } catch (IOException e) {
      throw Failure("I/O error");
    }
    lexer.reset();
    lexer.setInputStream(input);
    tokens = new CommonTokenStream(lexer);
    parser.reset();
    parser.setTokenStream(tokens);
    TempSequence cu=parse_contract(seq);
    Contract contract=null;
    for(ASTNode n:cu){
      if (n instanceof Contract){
        contract=(Contract)n;
      } else {
        if (contract!=null && n instanceof Method){
          Method m=(Method)n;
          if (m.getContract()!=null){
            throw Failure("double contract");
          }
          m.setContract(contract);
        }
        seq.add(n);
        contract=null;
      }
    }
    return contract;
  }
  
  public abstract TempSequence parse_contract(ASTSequence<?> seq);

  public void annotations(ASTNode node, InputStream fifo){
    ANTLRInputStream input;
    try {
      input = new ANTLRInputStream(fifo);
    } catch (IOException e) {
      throw Failure("I/O error");
    }
    lexer.reset();
    lexer.setInputStream(input);
    tokens = new CommonTokenStream(lexer);
    parser.reset();
    parser.setTokenStream(tokens);
    TempSequence cu=parse_annotations();
    for(ASTNode n:cu){
      node.attach(n);
    }
  }

  public abstract TempSequence parse_annotations();
  
}
