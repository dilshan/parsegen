program parsegen;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, uPassGen;

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
  PassGen : TPassGen;
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
    PassGen := TPassGen.Create(ParamStr(1));
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
  writeln('Simple Parser Generator for FPC/LCL');
  writeln('Copyright (c) 2013 Dilshan Jayakody [jayakody2000lk@gmail.com]'#10);
  writeln('Usage: ' + ExtractFileName(ExeName) + ' <structure-file>');
  writeln('       ' + ExtractFileName(ExeName) + ' -h');
end;

var
  Application: TParseApp;
begin
  Application:=TParseApp.Create(nil);
  Application.Title:='Simple Parser Generator for FPC/LCL';
  Application.Run;
  Application.Free;
end.

