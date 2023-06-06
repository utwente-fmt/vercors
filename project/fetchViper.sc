/*
This is a separate file, because targets are generally invalidated if the build definition changes. The fetching target
being in a different file reduces that chance.
*/

import $file.git

import mill._
import git.GitModule

object silverGit extends GitModule {
  def url = T { "https://github.com/viperproject/silver.git" }
  def commitish = T { "ae4a12399cd0b42bedabf01be2cda93700244bd6" }
}

object siliconGit extends GitModule {
  def url = T { "https://github.com/viperproject/silicon.git" }
  def commitish = T { "a60324dd46923b861bae7b4a40f807227d693fc3" }
}

object carbonGit extends GitModule {
  def url = T { "https://github.com/viperproject/carbon.git" }
  def commitish = T { "ba130077713a427213a331a3dc1d92898b4bdf9e" }
}