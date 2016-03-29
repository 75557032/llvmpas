# a toy pascal compiler

## 命令行

lpc [options] filename

-c 编译为.obj
-S 编译为.s
-E 不生成.obj .s
-dump 输出内部信息
-O<n> 优化级别
-emit-llvm 生成LLVM
-sys-unit 编译system单元
-Fi<path> 添加.inc文件的路径
-Fl<path> 添加unit查找路径
-FU<path> 设置.cu文件输出路径
-FE<path> 设置EXE输出路径
-llvm-target LLVM目标
-target <C++/LLVM>  生成C++代码或LLVM代码（计划中）

## 目标工具

lpc只把.pas转成llvm的汇编代码或C++代码，需要使用对应的命令行工具再次编译。如果需要编译LLVM代码，则需要安装llvm包(linux)或clang(windows)。

在Windows上可以使用预编译的LLVM安装包，需要3.5或以上的版本。同时需要mingw32，因为LLVM的某些指令和异常处理仍然需要GCC代码库的支持，并且也需要GCC来处理连接。

下载 http://llvm.org/releases/3.5.0/LLVM-3.5.0-win32.exe 进行安装。
下载 https://sourceforge.net/projects/mingw-w64/files/Toolchains%20targetting%20Win32/Personal%20Builds/rubenvb/gcc-4.7-release/i686-w64-mingw32-gcc-dw2-4.7.4-release-win32_rubenvb.7z/download
 解压到某个目录中。注意不要下载非dw2的。

修改lpc.cfg文件，把路径换成自己的:
```
# command for compile .ll to .bc
-tools-ll2bc"""e:\software\llvm\llvm35\bin\clang.exe"" %%input -O%%opt -c -emit-llvm -o %%output"

# command for compile .ll to .o
-tools-ll2obj"""e:\software\llvm\llvm35\bin\clang.exe"" %%input -O%%opt -c -o %%output"

# command for compile .ll to .asm
-tools-ll2asm"""e:\software\llvm\llvm35\bin\clang.exe"" %%input -O%%opt -c -S -o %%output"

# .bc to .asm
-tools-bc2asm"""e:\software\llvm\llvm35\bin\clang.exe"" %%input -O%%opt -c -o %%output"

# link tool
-tools-link"e:\software\mingw32\mingw32-dw2-4.7\bin\g++ -static-libgcc -static-libstdc++"

```
注意上面几个变量%%input, %%opt, %%output

## 生成 system 单元
system 单元包括system.pas和ex.ll，ex.ll包含一些pascal语法不能完成的代码。
```
lpc -sys-unit -c system.pas
clang ex.ll -o ex.o
```

## 未完成

类型RTTI生成
string,variant,interface,dynarray等自动转型
set相关代码
open array
...等等
