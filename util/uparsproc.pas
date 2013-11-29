{===============================================================================
  Parser Coprocessor Module.

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

unit uParsProc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TParsProc = class(TObject)
  private
    Sym: string;
    SPos, PrevSPos: integer;
    CaseSens: boolean;
    SkipToNewLine : boolean;
    Src: string;
    CommentSymbol : string;
    KeyWordList: TStringList;
    ActLine, ActCol : Integer;
    procedure ShowParserMessage(ErrIndex: integer; MsgStr : String);
  public
    constructor Create(CaseSupport: boolean; Source: string);
    procedure AddKeyWord(KeyWordName: string);
    procedure GetNextSymbol();
    procedure RaiseError(ErrIndex: integer; Param : String = '');
    function AcceptSymbol(InStr: string; IsFirstNode: boolean): boolean;
    function ExpectSymbol(InStr: string): boolean;
    function IsStringIsNumber(Str: string): boolean;
    function IsNumber(Chr: char): boolean;
    function IsAlpha(Chr: char): boolean;
    function IsAlphaNumeric(Chr: char): boolean;
    function IsStringIsAlphaNumeric(Str: string): boolean;
    function IsStringIsKeyword(Str: string): boolean;
    function IsStringIdentical(Str1: string; Str2: string): boolean;
    function GetSymbol: string;
    procedure SetLineComment(Symbol: string);
    procedure NextLine();
  end;

implementation

// parser coprocessor constructor.
constructor TParsProc.Create(CaseSupport: boolean; Source: string);
begin
  inherited Create();
  Sym := '';
  CaseSens := CaseSupport;
  SPos := 1;
  ActLine := 1;
  ActCol := 1;
  Src := Source;
  SkipToNewLine := false;
  // PrevSPos is used to avoid line counting errors.
  PrevSPos := SPos;
  KeyWordList := TStringList.Create;
end;

// Check specified character is number [0..9]
function TParsProc.IsNumber(Chr: char): boolean;
begin
  Result := ((Ord(Chr) > 47) and (Ord(Chr) < 58));
end;

// Check specified character is an element of English alphabet [a..z][A..Z]
function TParsProc.IsAlpha(Chr: char): boolean;
begin
  Result := ((Ord(Chr) > 64) and (Ord(Chr) < 91)) or
    ((Ord(Chr) > 96) and (Ord(Chr) < 123));
end;

// Check specified character is either number or English character.
function TParsProc.IsAlphaNumeric(Chr: char): boolean;
begin
  Result := (IsNumber(Chr) or IsAlpha(Chr));
end;

// Check specified string is number.
function TParsProc.IsStringIsNumber(Str: string): boolean;
var
  ScanPos: integer;
begin
  ScanPos := 1;
  Result := True;
  while (ScanPos <= Length(Str)) do
  begin
    if (not IsNumber(Str[ScanPos])) then
    begin
      Result := False;
      break;
    end;
    Inc(ScanPos);
  end;
end;

// Check similarity of the specified strings.
function TParsProc.IsStringIdentical(Str1: string; Str2: string): boolean;
begin
  if (CaseSens) then
    Result := (Str1 = Str2)
  else
    Result := (lowercase(Str1) = lowercase(Str2));
end;

// Check specified string is a language keyword.
function TParsProc.IsStringIsKeyword(Str: string): boolean;
var
  ScanPos: integer;
begin
  ScanPos := 0;
  Result := False;
  while (ScanPos < KeyWordList.Count) do
  begin
    if (IsStringIdentical(Str, KeyWordList.Strings[ScanPos])) then
    begin
      Result := True;
      break;
    end;
    Inc(ScanPos);
  end;
end;

// Check specified string is consist with alpha-numeric characters.
function TParsProc.IsStringIsAlphaNumeric(Str: string): boolean;
var
  ScanPos: integer;
begin
  ScanPos := 1;
  Result := True;
  while (ScanPos <= Length(Str)) do
  begin
    if (not IsAlphaNumeric(Str[ScanPos])) then
    begin
      Result := False;
      break;
    end;
    Inc(ScanPos);
  end;
end;

// Add keyword to the processor.
procedure TParsProc.AddKeyWord(KeyWordName: string);
begin
  KeyWordList.Add(KeyWordName);
end;

// Set line comment symbol.
procedure TParsProc.SetLineComment(Symbol: string);
begin
  CommentSymbol := Symbol;
end;

// Load next node or token into the processor.
procedure TParsProc.GetNextSymbol();
begin
  Sym := '';
  while (SPos < Length(Src)) do
  begin

    // update position information.
    if(SPos <> PrevSPos) then
    begin
    	if(Src[SPos] = #10) then
    	begin
    		Inc(ActLine);
      	ActCol := 0;
    	end
    	else if(Src[SPos] <> #10) then
      	Inc(ActCol);
      PrevSPos := SPos;
    end;

    // if this flag is active, skip all symbols till new line.
    if(SkipToNewLine) then
    begin
      if((Src[SPos] = #10) or (Src[SPos] = #13)) then
      begin
        Sym := '';
        SkipToNewLine := false
      end
      else
      begin
        Inc(SPos);
        Continue;
      end;
    end;

    if (
      // --- define special symbol patterns in here ---
      ((trim(Sym) = ':') and (Src[SPos] = '=')) or   // :=
      ((trim(Sym) = '>') and (Src[SPos] = '=')) or   // >=
      ((trim(Sym) = '<') and (Src[SPos] = '=')) or   // <=
      ((trim(Sym) = '/') and (Src[SPos] = '/'))      // comment (//)
      // --- end special symbol definitions ---
      ) then
    begin
      Sym := Sym + Src[SPos];
      Inc(SPos);
      Continue;
    end;

    // check new character pattern is valid as a symbol.
    if ((trim(Sym) <> '') and (IsAlphaNumeric(Src[SPos])) and (not IsStringIsAlphaNumeric(Sym))) then
    begin
      break;
    end;

    // split based on spaces and symbols.
    if ((Src[SPos] = #32) or (not IsAlphaNumeric(Src[SPos]))) then
    begin
      if (trim(Sym) <> '') then
        break;
    end;

    Sym := trim(Sym + Src[SPos]);
    Inc(SPos);
  end;
  Sym := trim(Sym);

  // if new symbol is for line comment?
  if((CommentSymbol <> '') and (CommentSymbol = Sym)) then
  begin
    SkipToNewLine := true;
    GetNextSymbol;
  end;
end;

function TParsProc.AcceptSymbol(InStr: string; IsFirstNode: boolean): boolean;
begin
  Result := IsStringIdentical(InStr, Sym);
  if (Result) then
    GetNextSymbol;
end;

function TParsProc.ExpectSymbol(InStr: string): boolean;
begin
  if IsStringIdentical(InStr, Sym) then
  begin
    GetNextSymbol;
    Result := True;
  end
  else
  begin
    RaiseError(2, InStr);
  end;
end;

// Skip current position and move processor into next line.
procedure TParsProc.NextLine();
begin
  SkipToNewLine := true;
  GetNextSymbol;
end;

procedure TParsProc.RaiseError(ErrIndex: integer; Param : String = '');
var
  MsgStr : String;
begin
  // error message table.
  case(ErrIndex) of
    0: MsgStr := 'Syntax error.';
    1: MsgStr := Format('Syntax error near "%s".', [Sym]);
    2: MsgStr := Format('Expecting "%s", but "%s" is found.', [Param, Sym]);
  end;

  ShowParserMessage(ErrIndex, MsgStr);
  halt(0);
end;

procedure TParsProc.ShowParserMessage(ErrIndex: integer; MsgStr : String);
begin
  // if column information is missing, then the error is in previous line.
  if((ActCol = 0) and (ActLine > 0)) then
    Writeln('[' + IntToStr(ActLine - 1) + '] Error (' + InttoStr(ErrIndex) + '): ' + MsgStr)
  else
    Writeln('[' + IntToStr(ActLine) + ', ' + InttoStr(ActCol) + '] Error (' + InttoStr(ErrIndex) + '): ' + MsgStr);
end;

// Current node or token.
function TParsProc.GetSymbol: string;
begin
  Result := Sym;
end;

end.
