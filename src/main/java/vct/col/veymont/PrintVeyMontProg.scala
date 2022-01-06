package vct.col.veymont

import hre.lang.System.{Debug, Fail}
import vct.col.ast.stmt.decl.ProgramUnit
import vct.col.ast.syntax.{JavaDialect, JavaSyntax, PVLSyntax}

import java.io.{File, FileOutputStream, IOException, PrintWriter}

object PrintVeyMontProg {

  def print(prog : ProgramUnit, destFileName : String) : ProgramUnit = {
    try {
      val f = new File(destFileName);
      val b = f.createNewFile();
      if(!b) {
      Debug("File %s already exists and is now overwritten", destFileName);
    }
      val out = new PrintWriter(new FileOutputStream(f));
      if(destFileName.endsWith(".pvl"))
      PVLSyntax.get().print(out,prog)
      else if(destFileName.endsWith(".java")) {
      out.println("import java.util.concurrent.*;")
      out.println("import java.util.List;")
      out.println("import java.util.Map;")
      JavaSyntax.getJava(JavaDialect.JavaVerCors).print(out, new JavaForkJoin(prog).rewriteAll())
    }
      else Fail("VeyMont Fail: VeyMont cannot write output to file %s",destFileName)
      out.close();
    } catch {
      case e: IOException => Debug(e.getMessage);
    }
    prog
  }
}
