program info_c;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, functions
  { you can add units after this };

type

  { info }

  info = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteToConsole;
    procedure WriteToLog(FileName: String);
    procedure Log(FileName, Text: String);
  end;

  TInfo = Record
    ComputerName : String[24];
    Description  : String[250];
    LoginNetwork : String[24];
    IpAddress    : String[15];
    CPU          : String[250];
    Memory       : String[10];
    OS           : String[250];
    Bit          : String[2];
    OSVersion    : String[10];
    Resolution   : String[15];
  end;

var
  i: TInfo;
  FullPathToLogFile: String;

{ info }

procedure info.DoRun;
begin
  i.ComputerName := SysUtils.GetEnvironmentVariable('COMPUTERNAME');
  i.Description := GetComputerNetDescription;
  i.LoginNetwork := SysUtils.GetEnvironmentVariable('USERNAME');
  i.IpAddress := GetIpAddress;
  i.CPU := GetProcessorInfo;
  i.Memory := FloatToStr(GetMemory);
  i.OS := GetOS;
  i.Bit := GetBit;
  i.OSVersion := GetOSVersion;
  i.Resolution := GetResolution;

  if HasOption('l', 'log') then begin
    FullPathToLogFile := GetOptionValue('l', 'log');
  end else
    WriteToConsole;

  if FullPathToLogFile <> '' then WriteToLog(FullPathToLogFile);

  Terminate;
end;

procedure info.WriteToConsole;
begin
  Writeln('ComputerName: ' + i.ComputerName);
  Writeln('Description: ' + i.Description);
  Writeln('LoginNetwork: ' + i.LoginNetwork);
  Writeln('IP address: ' + i.IpAddress);
  Writeln('CPU: ' + i.CPU);
  Writeln('Memory: ' + i.Memory);
  Writeln('OS: ' + i.OS);
  Writeln('Bit: ' + i.Bit);
  Writeln('OSVersion: ' + i.OSVersion);
  Writeln('Resolution: ' + i.Resolution);
end;

procedure info.Log(FileName, Text: String);
var
  F: TextFile;
  dt: String;
begin
  AssignFile(F, FileName);
  if FileExists(FileName) then
    Append(F)
  else
    Rewrite(F);
    dt := DateToStr(Date) + ' ' + TimeToStr(Time);
  WriteLn(F, dt + ';' + text);
  CloseFile(F);
end;

procedure info.WriteToLog(FileName: String);
var s: String;
begin
  s := i.ComputerName + ';' + i.Description + ';' + i.LoginNetwork +
    i.IpAddress + ';' + i.CPU + ';' + i.Memory + ';' + i.OS + ';' +
    i.Bit + ';' + i.OSVersion + ';' + i.Resolution;
  Log(FileName, s);
end;

constructor info.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor info.Destroy;
begin
  inherited Destroy;
end;

var
  Application: info;
begin
  Application:=info.Create(nil);
  Application.Title:='info';
  Application.Run;
  Application.Free;
end.

