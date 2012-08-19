set print pretty
set print static-members off

python
import sys
sys.path.insert(0, '/usr/share/gcc-4.6.2/python')
sys.path.insert(0, '/usr/share/gcc-4.7.1/python')
from libstdcxx.v6.printers import register_libstdcxx_printers
register_libstdcxx_printers(None)
