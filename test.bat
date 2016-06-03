set path=%path%;c:\mingw\bin;
appveyor DownloadFile "https://raw.githubusercontent.com/HostsTools/test-tool/master/haskellconsoleruntest.cpp" -FileName "runtest.cpp" 
g++ -o runtest.exe runtest.cpp -DONLINERUN
runtest "C:\projects\haskell-console\.stack-work"
