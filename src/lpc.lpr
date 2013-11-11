program lpc;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, ast, ptrhashtable, cntx, cupersist, err, lex,
  llvmemit, parser, start, fileutils
  { you can add units after this };

begin

end.

