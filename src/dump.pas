unit dump;
// 导出表达式
{$ifdef FPC}
{$mode delphi}{$H+}
{$endif}

interface
uses Classes, SysUtils, ast;

procedure DumpExpr(E: TExpr);

implementation

const
  OpStr: array[TExprOpCode] of string = (
    'none',
//  opNE, opEQ, opLT, opLE, opGT, opGE, opIN, opIS, opAS,
    '<>', '=', '<', '<=', '>', '>=', 'in', 'is', 'as',
//  opADD, opSUB, opOR, opXOR, opMUL, opDIV, opIDIV,
    '+', '-', 'or', 'xor', '*', '/', 'div',
//  opMOD, opAND, opSHL, opSHR,
    'mod', 'and', 'shl', 'shr',
//  opMEMBER, opCAST, opCALL, opRANGE, opINDEX, opNOT, opNEG, opPOS,
    'memberof', 'cast', 'call', 'range', 'index', 'not', 'neg', 'pos',
// opINHERITED, opSET, opLIST, opADDR, opINST, opNIL,
    'inherited', 'set', 'list', '@', '^', 'nil',
// opBOOLCONST, opINTCONST, opREALCONST, opSTRCONST, opCHARCONST, opSYMBOL
    'bool', 'int', 'real', 'str', 'char', 'sym'
  );

function SymDesc(Sym: TSymbol): string;
begin
end;

function ExprDesc(Indent: Integer; E: TExpr): string;
begin
  case E.OpCode of
    opSYMBOL: Result := Format('(sym:%s)', [SymDesc(TSymbolExpr(E).Reference)]);
    opMEMBER: 
  end;
end;

procedure DumpExpr(E: TExpr);
begin

end;

end.
