program info_c;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, functions, fphttpclient, fpjson, jsonparser,
  LConvEncoding, LazUTF8
  { you can add units after this };

const url = 'http://10.19.19.121/json/get.php';

type

  { info }

  info = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteToConsole;
    procedure HttpPost;
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

  if HasOption('h', 'help') then begin
    WriteLn('Use parameters -l (-log) <path to log file>, -p (-post)');
    Terminate;
    Exit;
  end;

  if HasOption('l', 'log') then begin
    FullPathToLogFile := GetOptionValue('l', 'log');
  end else
    WriteToConsole;

  if HasOption('p', 'post') then HttpPost;

  if FullPathToLogFile <> '' then WriteToLog(FullPathToLogFile);

  Terminate;
end;

procedure info.WriteToConsole;
begin
  WriteLn('ComputerName: ' + i.ComputerName);
  WriteLn('Description: ' + i.Description);
  WriteLn('LoginNetwork: ' + i.LoginNetwork);
  WriteLn('IP address: ' + i.IpAddress);
  WriteLn('CPU: ' + i.CPU);
  WriteLn('Memory: ' + i.Memory + ' Gb');
  WriteLn('OS: ' + i.OS + ' x' + i.Bit + ' ' + i.OSVersion);
  WriteLn('Resolution: ' + i.Resolution);
end;

procedure info.HttpPost;
var
  json: TJSONObject;
  response: String;
begin
  json := TJSONObject.Create;
  json.Add('key', '687F1AE6195AC76694EC6D94B29D7DE708CE2739');
  json.Add('pc', i.ComputerName);
  json.Add('description', i.Description);
  json.Add('net_login', i.LoginNetwork);
  json.Add('ip', i.IpAddress);
  json.Add('cpu', i.CPU);
  json.Add('memory', i.Memory);
  json.Add('os', i.OS);
  json.Add('bit', i.Bit);
  json.Add('ver', i.OSVersion);
  json.Add('resolution', i.Resolution);

  With TFPHttpClient.Create(Nil) do
  try
    AddHeader('Content-Type', 'application/json');
    RequestBody := TStringStream.Create(json.AsJSON);
    response := Post(url);
  finally
    Free;
  end;
  WriteLn(response);
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
  s := i.ComputerName + ';' + i.Description + ';' + i.LoginNetwork + ';' +
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

