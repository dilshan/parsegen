{===============================================================================
  Simple Parser Generator for FPC+LCL.

  Copyright (C) 2013 Dilshan R Jayakody. (dilshan@users.sourceforge.net)

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
===============================================================================}

unit uParsGen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TParsGen = class(TObject)
  private
    InputFile: TStringList;
    GenFile: TStringList;
    FuncTable: TStringList;
    ConstTable: TStringList;
    IncUnits : TStringList;
    ConstDict : TList;
    KeywordList: TStringList;
    OutputFileName: string;
    LinePos, ColPos: integer;
    ConstCounter: integer;
    TabPos: integer;
    ClsName: string;
    RootFunction: string;
    CommantSymbol : string;
    procedure ShowMessage(Msg : String; IsHalt : Boolean);
    function IsMultiCheckToken(InToken: string): boolean;
    procedure ProcessToken(InToken: string);
    function GetNextToken(InStr: string): string;
    function TrimConst(ConstVal: string): string;
    function CheckKeyword(InStr: string; Key: string): boolean;
    function GetCheckRoutine(ChkToken: string; ExpectType: boolean;
      MoveNextToken: boolean = True): string;
    function AddConstant(ConstID: string; ConstValue: string): string;
    procedure AddKeyword(Keyname: string);
    function GetMultiCheckRoutine(ChkToken: string; IsExpect: boolean): string;
    procedure ProcessCmd(InToken: string);
    function GenerateTab(): string;
    procedure CreateOutput();
    function AddFunction(UnitClsName: string; FuncName: string;
      ReturnType: string): string;
  public
    constructor Create(InFile: string);
    procedure Execute();
  end;

  PPassGenConst = ^TParsGenConst;
  TParsGenConst = record
    ConstName : String;
    ConstVal : String;
  end;

implementation

constructor TParsGen.Create(InFile: string);
begin
  inherited Create();

  // load source file and create output file buffer.
  InputFile := TStringList.Create;
  GenFile := TStringList.Create;
  FuncTable := TStringList.Create;
  ConstTable := TStringList.Create;
  ConstDict := TList.Create;
  KeywordList := TStringList.Create;
  IncUnits := TStringList.Create;
  InputFile.LoadFromFile(InFile);
  OutputFileName := ChangeFileExt(InFile, '.pas');

  // setup default entries
  ConstTable.Add('const');
  IncUnits.Add('Classes');
  IncUnits.Add('SysUtils');
  IncUnits.Add('uParsProc');

  //Create unit classname
  ClsName := 'T' + ExtractFileName(ChangeFileExt(InFile, ''));
  if (Length(ClsName) > 1) then
    ClsName[2] := UpperCase(ClsName[2])[1];
end;

procedure TParsGen.ShowMessage(Msg : String; IsHalt : Boolean);
begin
  Writeln(Msg);
  if(IsHalt) then
    Halt(0);
end;

function TParsGen.CheckKeyword(InStr: string; Key: string): boolean;
begin
  InStr := trim(InStr);
  Result := (Length(InStr) >= Length(Key)) and
    (Lowercase(Copy(InStr, 1, Length(Key))) = Lowercase(Key));
end;

function TParsGen.GetNextToken(InStr: string): string;
var
  InsideStr: boolean;
begin
  InStr := trim(InStr);
  InsideStr := False;
  if (ColPos = 0) then
    ColPos := 1;
  Result := '';
  while (ColPos < Length(InStr) + 1) do
  begin

    // do not break strings into token
    if (InStr[ColPos] = #34) then
    begin
      InsideStr := not InsideStr;
    end;

    if ((InStr[ColPos] = #32) and (not InsideStr)) then
    begin
      if (trim(Result) <> '') then
        break;
    end;
    Result := Result + InStr[ColPos];
    Inc(ColPos);
  end;
end;

function TParsGen.IsMultiCheckToken(InToken: string): boolean;
var
  IsInside: boolean;
  SPos: integer;
begin
  Result := False;
  IsInside := False;
  for SPos := 1 to Length(InToken) do
  begin
    if (InToken[SPos] = #34) then
    begin
      IsInside := not IsInside;
      Continue;
    end;

    if ((not IsInside) and (InToken[SPos] = #44)) then
      Result := True;
  end;
end;

function TParsGen.TrimConst(ConstVal: string): string;
begin
  Result := ConstVal;
  if (Trim(ConstVal) <> '') then
  begin
    if (ConstVal[1] = #34) then
      Result := Copy(Result, 2, Length(ConstVal) - 1);

    if (ConstVal[Length(ConstVal)] = #34) then
      Result := Copy(Result, 1, Length(Result) - 1);
  end;
end;

function TParsGen.AddConstant(ConstID: string; ConstValue: string): string;
var
  ConstName : String;
  ConstData : PPassGenConst;

  function IsConstExists() : String;
  var
    ConstTablePos : Integer;
  begin
    ConstTablePos := 0;
    result := '';
    while(ConstTablePos < (ConstDict.Count - 1)) do
    begin
      if(trim(ConstValue) = PPassGenConst(ConstDict.Items[ConstTablePos])^.ConstVal) then
      begin
        result := trim(PPassGenConst(ConstDict.Items[ConstTablePos])^.ConstName);
        break;
      end;
      inc(ConstTablePos);
    end;
  end;

begin

  ConstName := IsConstExists;
  if(ConstName <> '') then
    result := ConstName
  else
  begin
    Result := trim(ConstID) + IntToStr(ConstCounter);

    // avoid pascal syntax error with ' symbol
    if(TrimConst(ConstValue) <> #39) then
      ConstTable.Add(#9 + Result + ' = ''' + TrimConst(ConstValue) + ''';')
    else
      ConstTable.Add(#9 + Result + ' = #39;');

    new(ConstData);
    ConstData^.ConstName := Result;
    ConstData^.ConstVal := trim(ConstValue);
    ConstDict.Add(ConstData);
    Inc(ConstCounter);
  end;
end;

function TParsGen.AddFunction(UnitClsName: string; FuncName: string;
  ReturnType: string): string;
begin
  Result := '';
  if (Length(FuncName) > 1) then
    FuncName[2] := UpperCase(FuncName[2])[1];

  // find and keep root function name
  if (RootFunction = '') then
    RootFunction := trim(FuncName) + '()';

  if (ReturnType <> '') then
  begin
    Result := 'function ' + UnitClsName + '.' + trim(FuncName) +
      '() : ' + ReturnType + ';';
    FuncTable.Add(#9#9#9'function ' + trim(FuncName) + '() : ' + ReturnType + ';');
  end
  else
  begin
    Result := 'procedure ' + UnitClsName + '.' + trim(FuncName) + '();';
    FuncTable.Add(#9#9#9'procedure ' + trim(FuncName) + '();');
  end;
end;

procedure TParsGen.AddKeyword(Keyname: string);
begin
  KeywordList.Add(Keyname);
end;

function TParsGen.GetCheckRoutine(ChkToken: string; ExpectType: boolean;
  MoveNextToken: boolean = True): string;
var
  IsAdvanceToNext, ContName: string;
begin
  if (MoveNextToken) then
    IsAdvanceToNext := 'true'
  else
    IsAdvanceToNext := 'false';

  ContName := AddConstant('CHK_VAL', ChkToken);
  if (ExpectType) then
    Result := 'ExpectSymbol(' + ContName + ')'
  else
    Result := 'AcceptSymbol(' + ContName + ', ' + IsAdvanceToNext + ')';

  // const are also need to be include in the keyword list.
  AddKeyword(ContName);
end;

function TParsGen.GenerateTab(): string;
var
  TabLvl: integer;
begin
  Result := '';
  if (TabPos > 0) then
  begin
    for TabLvl := 1 to TabPos do
      Result := Result + #9;
  end
  else
    TabPos := 0;
end;

function TParsGen.GetMultiCheckRoutine(ChkToken: string; IsExpect: boolean): string;
var
  ScanPos, LPos: integer;
  IsInside, IsFirstChk: boolean;
  TmpToken: string;
begin
  IsInside := False;
  Result := '';
  ScanPos := 1;
  LPos := 1;
  IsFirstChk := true;

  if (Length(ChkToken) > 0) then
  begin

    while (ScanPos < Length(ChkToken)) do
    begin
      if (ChkToken[ScanPos] = #34) then
      begin
        Inc(ScanPos);
        IsInside := not IsInside;
        Continue;
      end;

      // start split process and combine all expressions using OR operator.
      if ((not IsInside) and (ChkToken[ScanPos] = #44)) then
      begin
        TmpToken := trim(Copy(ChkToken, LPos, (ScanPos - LPos)));
        if (Result = '') then
          Result := GetCheckRoutine(TmpToken, IsExpect, IsFirstChk)
        else
          Result := Result + ' or ' + GetCheckRoutine(TmpToken, IsExpect, IsFirstChk);
        LPos := ScanPos + 1;
        IsFirstChk := false;
      end;

      Inc(ScanPos);
    end;

    TmpToken := Copy(ChkToken, LPos, Length(ChkToken));
    if (Result = '') then
      Result := GetCheckRoutine(TmpToken, IsExpect, IsFirstChk)
    else
      Result := Result + ' or ' + GetCheckRoutine(TmpToken, IsExpect, IsFirstChk);

  end;
end;

procedure TParsGen.ProcessToken(InToken: string);
begin
  if (Length(InToken) > 1) then
  begin
    // is check entry?
    if (InToken[1] = #34) then
    begin
      if (IsMultiCheckToken(InToken)) then
      begin
        // multipal items to check.
        GenFile.Add(GenerateTab() + 'if (' + GetMultiCheckRoutine(InToken, False) +
          ') then');
        GenFile.Add(GenerateTab() + 'begin');
        GenFile.Add(GenerateTab() + 'end');
        GenFile.Add(GenerateTab() + 'else');
        GenFile.Add(GenerateTab() + 'begin');
        GenFile.Add(GenerateTab() + #9'RaiseError(0);');
        GenFile.Add(GenerateTab() + 'end;');
      end
      else
      begin
        // singal item check.
        GenFile.Add(GenerateTab() + GetCheckRoutine(InToken, True) + ';');
      end;
    end
    else if (InToken[1] = #126) then
      // call for internal commands.
      ProcessCmd(InToken)
    else
    begin
      // call to other routine.
      GenFile.Add(GenerateTab() + InToken + '();');
    end;
  end;
end;

procedure TParsGen.ProcessCmd(InToken: string);
var
  InCmd, TmpStr: string;

  function IsCommand(CmdName: string): boolean;
  begin
    Result := lowercase(Copy(InCmd, 1, Length(CmdName))) = lowercase(CmdName);
  end;

  function ExtractStringValue(CmdName : String) : String;
  var
    TmpCmdProc : String;
  begin
    TmpCmdProc := Copy(InCmd, Length(CmdName) + 1, Length(InCmd) - Length(CmdName));
    if((Length(TmpCmdProc) > 4) and (TmpCmdProc[1] = #40) and (TmpCmdProc[2] = #34) and (TmpCmdProc[Length(TmpCmdProc)] = #41) and (TmpCmdProc[Length(TmpCmdProc)-1] = #34)) then
      result := Copy(TmpCmdProc, 3, Length(TmpCmdProc) - 4)
    else
      ShowMessage('Error: Invalid parameters in command ' + trim(InCmd), true);
  end;

begin
  InCmd := trim(InToken);
  if ((Length(InCmd) > 1) and (InCmd[1] = #126)) then
  begin
    // ~error command
    if (IsCommand('~error')) then
    begin
      if (Length(InCmd) > 7) then
        GenFile.Add(GenerateTab() + 'RaiseError' +
          Copy(InCmd, 7, Length(InCmd) - 6) + ';')
      else
        GenFile.Add(GenerateTab() + 'RaiseError(0);');
    end
    // ~element command
    else if (IsCommand('~element')) then
    begin
      TmpStr := ExtractStringValue('~element');
      if(TmpStr <> '' ) then
        FuncTable.Add(#9#9#9 + TmpStr)
      else
        ShowMessage('Error: Element name is missing or invalid ' + trim(InCmd), true);
    end
    // ~keyword command
    else if (IsCommand('~keyword')) then
    begin
      TmpStr :=  ExtractStringValue('~keyword');
      if(TmpStr <> '' ) then
        AddKeyword('''' + TmpStr + '''')
      else
        ShowMessage('Error: Keyword name is missing or invalid ' + trim(InCmd), true);
    end
    // ~nextline command
    else if (IsCommand('~nextline')) then
    begin
      GenFile.Add(GenerateTab() + 'SkipToNextLine();');
    end
    // ~linecomment command
    else if (IsCommand('~linecomment')) then
    begin
      TmpStr := ExtractStringValue('~linecomment');
      if(TmpStr <> '' ) then
        CommantSymbol := TmpStr
      else
        ShowMessage('Error: Comment symbol is missing or invalid ' + trim(InCmd), true);
    end
    // ~addunit command
    else if (IsCommand('~addunit')) then
    begin
      TmpStr := ExtractStringValue('~addunit');
      if(TmpStr <> '' ) then
        IncUnits.Add(TmpStr)
      else
        ShowMessage('Error: Unit name is missing or invalid ' + trim(InCmd), true);
    end
    else
      ShowMessage('Error: Unknown command "' + InCmd + '"', true);
  end;
end;

procedure TParsGen.Execute();
var
  SrcLine, TokenID, TmpStr: string;
  IsInlineBlock: boolean;

  // fill conditional source blocks.
  procedure GenerateConditionalSrcBlock(Key: string; StartBlock: string;
    EndBlock: string);
  begin
    TmpStr := trim(Copy(SrcLine, Length(Key) + 1, Length(SrcLine) - Length(Key)));
    if (TmpStr[1] = #34) then
    begin
      if (IsMultiCheckToken(TmpStr)) then
        GenFile.Add(GenerateTab() + StartBlock +
          GetMultiCheckRoutine(TmpStr, False) + EndBlock)
      else
        GenFile.Add(GenerateTab() + StartBlock +
          GetCheckRoutine(TmpStr, False) + EndBlock);
    end
    else
    begin
      ShowMessage('Hint: Function "' + TmpStr + '" must have Boolean return type.', false);
      GenFile.Add(GenerateTab() + StartBlock + TmpStr + '()' + EndBlock);
    end;

    GenFile.Add(GenerateTab() + 'begin');
    Inc(LinePos);
    Inc(TabPos);
  end;

  // combine else block with previous if-else statement
  procedure CombineElseIf();
  var
    LastPos: integer;
  begin
    LastPos := GenFile.Count - 1;
    while (LastPos > 0) do
    begin
      if (trim(GenFile.Strings[LastPos]) <> '') then
      begin
        // no any correction is needed
        if (lowercase(trim(GenFile.Strings[LastPos])) = 'end') then
          break;

        // correction needed.
        if (lowercase(trim(GenFile.Strings[LastPos])) = 'end;') then
          GenFile.Strings[LastPos] :=
            Copy(GenFile.Strings[LastPos], 1, Length(GenFile.Strings[LastPos]) - 1);
        break;
      end
      else
        Dec(LastPos);
    end;

  end;

begin
  // start file processing.
  LinePos := 0;
  ConstCounter := 1;
  TabPos := 0;
  IsInlineBlock := False;
  RootFunction := '';

  while (LinePos < InputFile.Count) do
  begin
    SrcLine := trim(InputFile.Strings[LinePos]);

    // check for end inline block.
    if ((SrcLine <> '') and CheckKeyword(SrcLine, 'inline-stop')) then
    begin
      IsInlineBlock := False;
      Inc(LinePos);
      Continue;
    end;

    // add inline codes to output
    if (IsInlineBlock) then
    begin
      GenFile.Add(GenerateTab() + InputFile.Strings[LinePos]);
      Inc(LinePos);
      Continue;
    end;

    // skip empty lines and comments.
    if ((SrcLine = '') or ((Length(SrcLine) > 0) and (SrcLine[1] = #35))) then
    begin
      Inc(LinePos);
      Continue;
    end;

    // check for start inline block.
    if (CheckKeyword(SrcLine, 'inline-start:')) then
    begin
      GenFile.Add('');
      IsInlineBlock := True;
      Inc(LinePos);
      Continue;
    end;

    // check for define: block.
    if (CheckKeyword(SrcLine, 'define:')) then
    begin
      GenFile.Add(GenerateTab());
      GenFile.Add(GenerateTab() + AddFunction(ClsName,
        Copy(SrcLine, 8, Length(SrcLine) - 7), ''));
      GenFile.Add(GenerateTab() + 'begin');
      Inc(TabPos);
      Inc(LinePos);
      Continue;
    end;

    // check for optional: block.
    if (CheckKeyword(SrcLine, 'optional:')) then
    begin
      GenerateConditionalSrcBlock('optional:', 'if (', ') then');
      Continue;
    end;

    // check for loop: block.
    if (CheckKeyword(SrcLine, 'loop:')) then
    begin
      GenerateConditionalSrcBlock('loop:', 'while (', ') do');
      Continue;
    end;

    // check for start-case: block.
    if (CheckKeyword(SrcLine, 'start-case:')) then
    begin
      GenerateConditionalSrcBlock('start-case:', 'if (', ') then');
      Continue;
    end;

    // check for next-case: block.
    if (CheckKeyword(SrcLine, 'next-case:')) then
    begin
      CombineElseIf;
      GenerateConditionalSrcBlock('next-case:', 'else if (', ') then');
      Continue;
    end;

    // check for end-case: block.
    if (CheckKeyword(SrcLine, 'stop-case:')) then
    begin
      CombineElseIf;
      GenFile.Add(GenerateTab() + 'else');
      GenFile.Add(GenerateTab() + 'begin');
      Inc(TabPos);
      ProcessCmd(trim(Copy(SrcLine, 11, Length(SrcLine) - 10)));
      Inc(LinePos);
      Continue;
    end;

    // check for other-case: block.
    if (CheckKeyword(SrcLine, 'other-case:')) then
    begin
      CombineElseIf;
      GenFile.Add(GenerateTab() + 'else');
      GenFile.Add(GenerateTab() + 'begin');
      Inc(TabPos);
      TmpStr := trim(Copy(SrcLine, 12, Length(SrcLine) - 11));
      if (TmpStr[1] = #34) then
      begin
        if (IsMultiCheckToken(TmpStr)) then
          GenFile.Add(GenerateTab() + GetMultiCheckRoutine(TmpStr, False))
        else
          GenFile.Add(GenerateTab() + GetCheckRoutine(TmpStr, False));
      end
      else
        GenFile.Add(GenerateTab() + TmpStr + '();');
      Inc(LinePos);
      Continue;
    end;

    // check for repeat: block.
    if (CheckKeyword(SrcLine, 'repeat:')) then
    begin
      GenFile.Add(GenerateTab() + 'repeat');
      Inc(TabPos);
      Inc(LinePos);
      Continue;
    end;

    // check for-all: block.
    if (CheckKeyword(SrcLine, 'for-all:')) then
    begin
      Dec(TabPos);
      TmpStr := trim(Copy(SrcLine, 9, Length(SrcLine) - 8));
      if (TmpStr[1] = #34) then
      begin
        if (IsMultiCheckToken(TmpStr)) then
          GenFile.Add(GenerateTab() + 'until (not ' +
            GetMultiCheckRoutine(TmpStr, False) + ');')
        else
          GenFile.Add(GenerateTab() + 'until (not ' +
            GetCheckRoutine(TmpStr, False) + ');');
      end
      else
        GenFile.Add(GenerateTab() + 'until (not ' + TmpStr + '());');
      Inc(LinePos);
      Continue;
    end;

    // check end blocks.
    if (Lowercase(trim(SrcLine)) = 'end') then
    begin
      Dec(TabPos);
      GenFile.Add(GenerateTab() + 'end;');
      Inc(LinePos);
      Continue;
    end;

    // column level scanning.
    ColPos := 1;
    while (ColPos < Length(SrcLine)) do
    begin
      TokenID := trim(GetNextToken(SrcLine));
      ProcessToken(TokenID);
    end;

    Inc(LinePos);
  end;

  CreateOutput();
end;

procedure TParsGen.CreateOutput();
var
  TmpOutputBuffer: TStringList;
  TmpSupportCode: TStringList;
  CntPos, UnitPos: integer;
  CmdStr, ConstSymbolName, IncList: string;
begin
  TmpOutputBuffer := TStringList.Create;
  TmpSupportCode := TStringList.Create;

  // add support codes to generated code
  TmpSupportCode.LoadFromFile(IncludeTrailingPathDelimiter(
    ExtractFilePath(ParamStr(0))) + 'parsegen.f.lan');
  CntPos := 0;
  ConstSymbolName := AddConstant('CHK_VAL', CommantSymbol);
  while (CntPos < TmpSupportCode.Count - 1) do
  begin
    CmdStr := TmpSupportCode.Strings[CntPos];
    CmdStr := StringReplace(CmdStr, '$MAINFUNC', RootFunction, [rfReplaceAll, rfIgnoreCase]);
    CmdStr := StringReplace(CmdStr, '$CLASSNAME', ClsName, [rfReplaceAll, rfIgnoreCase]);
    CmdStr := StringReplace(CmdStr, '$COMMENTSYMBOL', ConstSymbolName, [rfReplaceAll, rfIgnoreCase]);
    TmpSupportCode.Strings[CntPos] := CmdStr;
    Inc(CntPos);
  end;
  GenFile.AddStrings(TmpSupportCode);

  // add const. list to the source code
  GenFile.Add('');
  GenFile.Add(GenerateTab() + 'procedure ' + ClsName + '.MapKeyWordList();');
  GenFile.Add(GenerateTab() + 'begin');
  Inc(TabPos);
  CntPos := 0;
  while (CntPos < KeywordList.Count - 1) do
  begin
    GenFile.Add(GenerateTab() + 'ParsProc.AddKeyWord(' +
      KeywordList.Strings[CntPos] + ');');
    Inc(CntPos);
  end;
  Dec(TabPos);
  GenFile.Add(GenerateTab() + 'end;');

  // process language header file.
  TmpOutputBuffer.LoadFromFile(IncludeTrailingPathDelimiter(
    ExtractFilePath(ParamStr(0))) + 'parsegen.h.lan');
  CntPos := 0;
  while (CntPos < TmpOutputBuffer.Count - 1) do
  begin
    CmdStr := lowercase(trim(TmpOutputBuffer.Strings[CntPos]));

    // unit name.
    if (CmdStr = '.unitname') then
      TmpOutputBuffer.Strings[CntPos] :=
        'unit ' + ExtractFileName(ChangeFileExt(OutputFileName, '') + ';');

    // include pascal/delphi units.
    if(CmdStr = '.inclist') then
    begin
      IncList := '';
      if(IncUnits.Count > 0) then
      begin
        UnitPos := 0;
        IncList := IncUnits.Strings[UnitPos];
        inc(UnitPos);
        while(UnitPos < IncUnits.Count) do
        begin
          IncList := IncList + ', ' + IncUnits.Strings[UnitPos];
          inc(UnitPos);
        end;
      end;
      TmpOutputBuffer.Strings[CntPos] := #9 + IncList + ';';
    end;

    // add constants.
    if (CmdStr = '.const') then
    begin
      if (ConstTable.Count > 0) then
        TmpOutputBuffer.Strings[CntPos] := ConstTable.Text
      else
        TmpOutputBuffer.Strings[CntPos] := '';
    end;

    // add main code block
    if (CmdStr = '.code') then
    begin
      TmpOutputBuffer.Strings[CntPos] := GenFile.Text;
    end;

    // add class defination
    if (CmdStr = '.classdef') then
    begin
      FuncTable.Insert(0, 'type');
      FuncTable.Insert(1, #9 + ClsName + ' = class(TObject)');
      FuncTable.Insert(2, #9#9'private');
      FuncTable.Insert(3, #9#9#9'ParsProc : TParsProc;');
      FuncTable.Insert(4, #9#9#9'Src : String;');
      FuncTable.Add(#9#9#9'function ExpectSymbol(s: String) : Boolean;');
      FuncTable.Add(#9#9#9'function AcceptSymbol(s : String; b: Boolean) : Boolean;');
      FuncTable.Add(#9#9#9'procedure RaiseError(i : Integer);');
      FuncTable.Add(#9#9#9'procedure MapKeyWordList();');
      FuncTable.Add(#9#9#9'procedure SkipToNextLine();');
      FuncTable.Add(#9#9'public');
      FuncTable.Add(#9#9#9'procedure Exec();');
      FuncTable.Add(#9#9#9'procedure AssignSource(S : String);');
      FuncTable.Add('end;');
      TmpOutputBuffer.Strings[CntPos] := FuncTable.Text;
    end;

    Inc(CntPos);
  end;

  // write output to the file.
  TmpOutputBuffer.SaveToFile(OutputFileName);
  ShowMessage(#10'Output file "'+ ExtractFileName(OutputFileName) +'" is generated successfully.', false);
end;

end.
