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

program parsegen;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, uParsGen;

type

  { TEBNFDec }

  TParseApp = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TEBNFDec }

procedure TParseApp.DoRun;
var
  ErrorMsg: String;
  PassGen : TParsGen;
begin
  // quick check parameters.
  ErrorMsg:=CheckOptions('h','help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters.
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  // check for input ebnf file.
  if((ParamCount >= 1) and (FileExists(ParamStr(1)))) then
  begin
    PassGen := TParsGen.Create(ParamStr(1));
    PassGen.Execute();
    FreeAndNil(PassGen);
  end
  else
  begin
    // no any commandline options?
    WriteHelp;
    Terminate;
    Exit;
  end;

  // stop program loop.
  Terminate;
end;

constructor TParseApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TParseApp.Destroy;
begin
  inherited Destroy;
end;

procedure TParseApp.WriteHelp;
begin
  writeln('Simple Parser Generator for FPC+LCL');
  writeln('Copyright (c) 2013 Dilshan Jayakody [dilshan@users.sourceforge.net]'#10);
  writeln('Usage: ' + ExtractFileName(ExeName) + ' <structure-file>');
  writeln('       ' + ExtractFileName(ExeName) + ' -h');
end;

var
  Application: TParseApp;
begin
  Application:=TParseApp.Create(nil);
  Application.Title:='ParseGen';
  Application.Run;
  Application.Free;
end.

