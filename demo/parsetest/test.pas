unit test;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, uParsProc;
	
type
	TTest = class(TObject)
		private
			ParsProc : TParsProc;
			Src : String;
			procedure Main();
			procedure Block();
			procedure Statement();
			procedure Condition();
			procedure Expression();
			procedure Term();
			procedure Factor();
			function ident() : Boolean;
			function number() : Boolean;
			function ExpectSymbol(s: String) : Boolean;
			function AcceptSymbol(s : String; b: Boolean) : Boolean;
			procedure RaiseError(i : Integer);
			procedure MapKeyWordList();
			procedure SkipToNextLine();
		public
			procedure Exec();
			procedure AssignSource(S : String);
end;


const
	CHK_VAL1 = '.';
	CHK_VAL2 = 'const';
	CHK_VAL3 = '=';
	CHK_VAL4 = ',';
	CHK_VAL5 = ';';
	CHK_VAL6 = 'var';
	CHK_VAL7 = 'procedure';
	CHK_VAL8 = ':=';
	CHK_VAL9 = 'call';
	CHK_VAL10 = 'begin';
	CHK_VAL11 = 'end';
	CHK_VAL12 = 'if';
	CHK_VAL13 = 'then';
	CHK_VAL14 = 'while';
	CHK_VAL15 = 'do';
	CHK_VAL16 = 'odd';
	CHK_VAL17 = '#';
	CHK_VAL18 = '<';
	CHK_VAL19 = '<=';
	CHK_VAL20 = '>';
	CHK_VAL21 = '>=';
	CHK_VAL22 = '-';
	CHK_VAL23 = '+';
	CHK_VAL24 = '+';
	CHK_VAL25 = '*';
	CHK_VAL26 = '/';
	CHK_VAL27 = '(';
	CHK_VAL28 = ')';
	CHK_VAL29 = '';


implementation


procedure TTest.Main();
begin
	block();
	ExpectSymbol(CHK_VAL1);
end;

procedure TTest.Block();
begin
	if (AcceptSymbol(CHK_VAL2, true)) then
	begin
		ident();
		ExpectSymbol(CHK_VAL3);
		number();
		while (AcceptSymbol(CHK_VAL4, true)) do
		begin
			ident();
			ExpectSymbol(CHK_VAL3);
			number();
		end;
		ExpectSymbol(CHK_VAL5);
	end;
	if (AcceptSymbol(CHK_VAL6, true)) then
	begin
		ident();
		while (AcceptSymbol(CHK_VAL4, true)) do
		begin
			ident();
		end;
		ExpectSymbol(CHK_VAL5);
	end;
	while (AcceptSymbol(CHK_VAL7, true)) do
	begin
		ident();
		ExpectSymbol(CHK_VAL5);
		block();
		ExpectSymbol(CHK_VAL5);
	end;
	statement();
end;

procedure TTest.Statement();
begin
	if (ident()) then
	begin
		ExpectSymbol(CHK_VAL8);
		expression();
	end
	else if (AcceptSymbol(CHK_VAL9, true)) then
	begin
		ident();
	end
	else if (AcceptSymbol(CHK_VAL10, true)) then
	begin
		repeat
			statement();
		until (not AcceptSymbol(CHK_VAL5, true));
		ExpectSymbol(CHK_VAL11);
	end
	else if (AcceptSymbol(CHK_VAL12, true)) then
	begin
		condition();
		ExpectSymbol(CHK_VAL13);
		statement();
	end
	else if (AcceptSymbol(CHK_VAL14, true)) then
	begin
		condition();
		ExpectSymbol(CHK_VAL15);
		statement();
	end
	else
	begin
		RaiseError(1);
	end;
end;

procedure TTest.Condition();
begin
	if (AcceptSymbol(CHK_VAL16, true)) then
	begin
		expression();
	end
	else
	begin
		expression();
		if (AcceptSymbol(CHK_VAL3, true) or AcceptSymbol(CHK_VAL17, false) or AcceptSymbol(CHK_VAL18, false) or AcceptSymbol(CHK_VAL19, false) or AcceptSymbol(CHK_VAL20, false) or AcceptSymbol(CHK_VAL21, false)) then
		begin
		end
		else
		begin
			RaiseError(0);
		end;
		expression();
	end;
end;

procedure TTest.Expression();
begin
	if (AcceptSymbol(CHK_VAL22, true) or AcceptSymbol(CHK_VAL23, false)) then
	begin
	end;
	term();
	while (AcceptSymbol(CHK_VAL22, true) or AcceptSymbol(CHK_VAL24, false)) do
	begin
		term();
	end;
end;

procedure TTest.Term();
begin
	factor();
	while (AcceptSymbol(CHK_VAL25, true) or AcceptSymbol(CHK_VAL26, false)) do
	begin
		factor();
	end;
end;

procedure TTest.Factor();
begin
	if (ident()) then
	begin
	end
	else if (number()) then
	begin
	end
	else if (AcceptSymbol(CHK_VAL27, true)) then
	begin
		expression();
		ExpectSymbol(CHK_VAL28);
	end
	else
	begin
		RaiseError(1);
	end;
end;

function TTest.ident() : Boolean;
begin
  result := true;
  result := result and not ParsProc.IsStringIsKeyword(ParsProc.GetSymbol);
  result := result and ParsProc.IsAlphaNumeric(ParsProc.GetSymbol[1]);  
  result := result and ParsProc.IsStringIsAlphaNumeric(ParsProc.GetSymbol);
  
  if(result) then
  begin
    ParsProc.GetNextSymbol;
  end;
end;

function TTest.number() : Boolean;
begin
  result := ParsProc.IsStringIsNumber(ParsProc.GetSymbol);
  
  if(result) then
  begin
    ParsProc.GetNextSymbol;
  end;
end;
function TTest.ExpectSymbol(S: String) : Boolean;
begin
	result := ParsProc.ExpectSymbol(S);
end;

function TTest.AcceptSymbol(S : String; b: Boolean) : Boolean;
begin
	result := ParsProc.AcceptSymbol(S, b);
end; 

procedure TTest.RaiseError(I : Integer);
begin
	ParsProc.RaiseError(I);
end;

procedure TTest.SkipToNextLine();
begin
	ParsProc.NextLine();
end;

procedure TTest.Exec();
begin
	ParsProc := TParsProc.Create(false, Src);
	ParsProc.SetLineComment(CHK_VAL29);
	MapKeyWordList();
	
	ParsProc.GetNextSymbol;	
	Main();
end;

procedure TTest.AssignSource(S : String);
begin
	Src := S;
end;

procedure TTest.MapKeyWordList();
begin
	ParsProc.AddKeyWord(CHK_VAL1);
	ParsProc.AddKeyWord(CHK_VAL2);
	ParsProc.AddKeyWord(CHK_VAL3);
	ParsProc.AddKeyWord(CHK_VAL4);
	ParsProc.AddKeyWord(CHK_VAL3);
	ParsProc.AddKeyWord(CHK_VAL5);
	ParsProc.AddKeyWord(CHK_VAL6);
	ParsProc.AddKeyWord(CHK_VAL4);
	ParsProc.AddKeyWord(CHK_VAL5);
	ParsProc.AddKeyWord(CHK_VAL7);
	ParsProc.AddKeyWord(CHK_VAL5);
	ParsProc.AddKeyWord(CHK_VAL5);
	ParsProc.AddKeyWord(CHK_VAL8);
	ParsProc.AddKeyWord(CHK_VAL9);
	ParsProc.AddKeyWord(CHK_VAL10);
	ParsProc.AddKeyWord(CHK_VAL5);
	ParsProc.AddKeyWord(CHK_VAL11);
	ParsProc.AddKeyWord(CHK_VAL12);
	ParsProc.AddKeyWord(CHK_VAL13);
	ParsProc.AddKeyWord(CHK_VAL14);
	ParsProc.AddKeyWord(CHK_VAL15);
	ParsProc.AddKeyWord(CHK_VAL16);
	ParsProc.AddKeyWord(CHK_VAL3);
	ParsProc.AddKeyWord(CHK_VAL17);
	ParsProc.AddKeyWord(CHK_VAL18);
	ParsProc.AddKeyWord(CHK_VAL19);
	ParsProc.AddKeyWord(CHK_VAL20);
	ParsProc.AddKeyWord(CHK_VAL21);
	ParsProc.AddKeyWord(CHK_VAL22);
	ParsProc.AddKeyWord(CHK_VAL23);
	ParsProc.AddKeyWord(CHK_VAL22);
	ParsProc.AddKeyWord(CHK_VAL24);
	ParsProc.AddKeyWord(CHK_VAL25);
	ParsProc.AddKeyWord(CHK_VAL26);
	ParsProc.AddKeyWord(CHK_VAL27);
end;


end.
