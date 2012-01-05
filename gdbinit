set substitute-path /build/buildd /work/deb
set print pretty
set print static-members off

python
import os
import sys
sys.path.insert(0, os.path.expanduser('~/.gdb'))
if os.path.exists('/work/chrome'):
    sys.path.insert(0, '/work/chrome/src/third_party/WebKit/Tools/gdb/')
    sys.path.insert(0, '/work/chrome/src/tools/gdb/')
    #import gdb_webkit
    #import gdb_chrome
#from stl import register_libstdcxx_printers
#register_libstdcxx_printers(None)
