{===============================================================================
  Simple Parser Generator for FPC+LCL - Demo Application.

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

program parsetest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, test, CustApp, uParsProc;

type

  { TPassGenTest }

  TPassGenTest = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TPassGenTest }

procedure TPassGenTest.DoRun;
var
  DemoPass : TTest;
  str1 : TStringList;
begin
  if (ParamCount > 0) then
  begin
    str1 := TStringList.Create;
    str1.LoadFromFile(ParamStr(1));

    DemoPass := TTest.Create;
    DemoPass.AssignSource(str1.Text);
    DemoPass.Exec();
  end
  else
  begin
    Writeln('Error: Input script file is missing');
    Writeln('Usage: ' + ExtractFileName(ParamStr(0)) + ' <filename>');
  end;
  Terminate;
end;

constructor TPassGenTest.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TPassGenTest.Destroy;
begin
  inherited Destroy;
end;

var
  Application: TPassGenTest;
begin
  Application:=TPassGenTest.Create(nil);
  Application.Run;
  Application.Free;
end.

