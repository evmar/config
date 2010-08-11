set substitute-path /build/buildd /work/deb
set print pretty
set print static-members off

python
import os
import sys
sys.path.insert(0, os.path.expanduser('~/.gdb'))
from stl import register_libstdcxx_printers
register_libstdcxx_printers(None)
end
