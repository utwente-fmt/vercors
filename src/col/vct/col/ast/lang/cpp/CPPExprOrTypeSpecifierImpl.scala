package vct.col.ast.lang.cpp

import vct.col.ast.CPPExprOrTypeSpecifier
import vct.col.ast.ops.{CPPExprOrTypeSpecifierOps, CPPExprOrTypeSpecifierFamilyOps}

trait CPPExprOrTypeSpecifierImpl[G] extends CPPExprOrTypeSpecifierOps[G] with CPPExprOrTypeSpecifierFamilyOps[G] { this: CPPExprOrTypeSpecifier[G] =>

}