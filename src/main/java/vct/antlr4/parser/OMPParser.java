package vct.antlr4.parser;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;

import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.RuleNode;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;

import vct.antlr4.parser.OMPoption.Kind;
import vct.antlr4.parser.OMPoption.Schedule;
import vct.antlr4.generated.ompLexer;
import vct.antlr4.generated.ompParser;
import vct.antlr4.generated.ompParser.Omp_optionContext;
import vct.antlr4.generated.ompParser.Omp_scheduleContext;
import vct.antlr4.generated.ompParser.Omp_sectionContext;
import vct.antlr4.generated.ompParser.Omp_sectionsContext;
import vct.antlr4.generated.ompParser.Omp_simdContext;
import vct.antlr4.generated.ompParser.Omp_simdlenContext;
import vct.antlr4.generated.ompParser.Omp_simdoptContext;
import vct.antlr4.generated.ompParser.Omp_singleContext;
import vct.antlr4.generated.ompParser.*;
import vct.antlr4.generated.ompVisitor;

public class OMPParser {
  
  public static OMPpragma parse(String pragma){
    InputStream stream=new ByteArrayInputStream(pragma.getBytes());
    ANTLRInputStream input;
    try {
      input = new ANTLRInputStream(stream);
    } catch (IOException e) {
      throw new Error(e.getMessage());
    }
    ompLexer lexer = new ompLexer(input);
    CommonTokenStream tokens = new CommonTokenStream(lexer);
    ErrorCounter ec=new ErrorCounter(pragma);
    ompParser parser = new ompParser(tokens);
    lexer.removeErrorListeners();
    lexer.addErrorListener(ec);
    parser.removeErrorListeners();
    parser.addErrorListener(ec);
    ParseTree tree = parser.omp_pragma();
    ConversionVisitor visitor=new ConversionVisitor();
    return (OMPpragma) tree.accept(visitor);
  }

}

class ConversionVisitor implements ompVisitor<OMPelement> {

  @Override
  public OMPelement visit(ParseTree arg0) {
    throw new Error("Cannot visit parse tree");
  }

  @Override
  public OMPelement visitChildren(RuleNode arg0) {
    throw new Error("Cannot visit children");
  }

  @Override
  public OMPelement visitErrorNode(ErrorNode arg0) {
    throw new Error("parse error "+ arg0 +"detected");
  }

  @Override
  public OMPelement visitTerminal(TerminalNode arg0) {
    throw new Error("missing case "+arg0.getText());
  }

  @Override
  public OMPelement visitOmp_pragma(Omp_pragmaContext ctx) {
    return ctx.getChild(0).accept(this);
   }

  @Override
  public OMPelement visitId_list(Id_listContext ctx) {
    throw new Error("id_list cannot be converted to element");
  }

  @Override
  public OMPelement visitOmp_parallel_for(Omp_parallel_forContext ctx) {
    return new OMPpragma(OMPpragma.Kind.ParallelFor,map(ctx,2));
  }
  
  private OMPoption[] map(ParserRuleContext ctx,int ofs){
    int N=ctx.getChildCount();
    OMPoption[] res=new OMPoption[N-ofs];
    for(int i=ofs;i<N;i++) {
      res[i-ofs]=(OMPoption)ctx.getChild(i).accept(this);
    }
    return res;
  }
  
  @Override
  public OMPelement visitOmp_for(Omp_forContext ctx) {
    return new OMPpragma(OMPpragma.Kind.For,map(ctx,1));
  }

  @Override
  public OMPelement visitOmp_parallel(Omp_parallelContext ctx) {
    return new OMPpragma(OMPpragma.Kind.Parallel,map(ctx,1));
  }

  @Override
  public OMPelement visitOmp_private(Omp_privateContext ctx) {
    String vars[]=new String[ctx.getChildCount()];
    for(int i=0;i<vars.length;i++){
      vars[i]=ctx.getChild(i).getText();
    }
    return new OMPprivate(vars);
  }

  @Override
  public OMPelement visitOmp_nowait(Omp_nowaitContext ctx) {
    return new OMPnowait();
  }

  @Override
  public OMPelement visitOmp_option(Omp_optionContext ctx) {
    return ctx.getChild(0).accept(this);
  }

  @Override
  public OMPelement visitOmp_schedule(Omp_scheduleContext ctx) {
    return new OMPoption(Kind.Schedule,Schedule.Static);
  }

  @Override
  public OMPelement visitOmp_single(Omp_singleContext ctx) {
    throw new Error("missing case");
  }

  @Override
  public OMPelement visitOmp_sections(Omp_sectionsContext ctx) {
     return new OMPpragma(OMPpragma.Kind.Sections,map(ctx,1));//Saeed 
    //throw new Error("missing case");
  }

  @Override
  public OMPelement visitOmp_section(Omp_sectionContext ctx) {
    return new OMPpragma(OMPpragma.Kind.Section,map(ctx,1));//Saeed 
    //throw new Error("missing case");
  }

  @Override
  public OMPelement visitOmp_simd(Omp_simdContext ctx) {
    return new OMPpragma(OMPpragma.Kind.Simd,map(ctx,1));
  }

  @Override
  public OMPelement visitOmp_simdlen(Omp_simdlenContext ctx) {
    int N=Integer.parseInt(ctx.getChild(2).getText());
    return new OMPoption(Kind.SimdLen,N);
  }

  @Override
  public OMPelement visitOmp_simdopt(Omp_simdoptContext ctx) {
    return new OMPoption(Kind.Simd);
  }

}

