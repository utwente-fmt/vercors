This is the boogie distribution that ships with viper 20.07.

The reason there is only one and not one for mac/windows/unix is because for each version of viper (mac, windows, linux) the same boogie is shipped. 

## Changes made

- Changed the shebang in `Boogie` from `#!/bin/sh` to `#!/bin/bash`. This is because the dirname substitution trick it does there is bash specific. Furthermore, on e.g. ubuntu, `/bin/sh` is (apparently) dash, and not bash. So that causes problems (some substitution error). Better be specific about what type of executables are expected. 
