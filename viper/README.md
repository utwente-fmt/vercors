# Viper API

This project contains the Viper back-end for the VerCors tool.
Since there are no published versions of the Viper modules, the inner carbon-, silicon- and silver projects are one-to-one copies of the versions of the [carbon repository](https://bitbucket.org/viperproject/carbon), [Silicon repository](https://bitbucket.org/viperproject/silicon) and [silver repository](https://bitbucket.org/viperproject/silver) as used in this version of VerCors.
The root project contains the "viper-api" source files, for the communication between VerCors and Viper.

## Linking the Viper modules

In order to use the Viper modules the carbon and silicon project must have a symbolic link to the silver project. Creating symbolic links via Scala (or Java) is not supported on all platforms. Also the symbolic links for Unix systems differ from symbolic links on Windows systems. Therefore these links must be created manually the first time the project is build.

For Unix systems execute the following command in both the carbon and  the silicon directory:
''' ln -s ../silver silver '''

For Windows systems execute the following command both in the carbon and the silicon directory:
''' mklink /d silver ..\silver '''

## Updating Viper modules

Since the inner projects are one-to-one copies of their Viper counterparts, updating Viper is no more than replacing the the respective module with the new version, and if necessary updating the root project to reflect the changes in the updated module(s). (Don't forget to (re)create the symbolic links for carbon and silicon if needed.)
