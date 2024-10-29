package util

import me.pieterbos.mill.cpp.{CppModule => BaseCppModule, CppExecutableModule => BaseCppExecutableModule, _}
import mill.T

trait CppModule extends BaseCppModule {
  override def standard: T[options.CppStandard] = T[options.CppStandard] { options.CppStandard.Cpp20 }
}

trait CppExecutableModule extends BaseCppExecutableModule with CppModule