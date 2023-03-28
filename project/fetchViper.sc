/*
This is a separate file, because targets are generally invalidated if the build definition changes. The fetching target
being in a different file reduces that chance.
*/

import $file.git

import mill._
import git.GitModule

object silverGit extends
  GitModule {
  def url = T { "https://github.com/viperproject/silver.git" }
  def commitish = T { "11bde93e486e983141c01ac7df270e9f06e8ab06" }
}

object siliconGit extends GitModule {
  def url = T { "https://github.com/viperproject/silicon.git" }
  def commitish = T { "f844927fe6f54c3dbc5adbccfa011034c8036640" }
}

object carbonGit extends GitModule {
  def url = T { "https://github.com/viperproject/carbon.git" }
  def commitish = T { "44f9225dcde2374c3b8051b6d56ac88c7c4ffdd5" }
}