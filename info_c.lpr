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
//const url = 'http://alisa.loc/json/get.php';

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
    procedure WriteToBimoidLog;
  end;

  TInfo = Record
    ComputerName : String[24];
    Description  : String[250];
    LoginNetwork : String[24];
    IpAddress    : String[15];
    IpAddresses  : String[250];
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
  WriteLn('info_c Version 1.0 Build 2020-10-03');
  WriteLn;

  i.ComputerName := UpperCase(SysUtils.GetEnvironmentVariable('COMPUTERNAME'));
  i.Description := GetComputerNetDescription;
  i.LoginNetwork := LowerCase(SysUtils.GetEnvironmentVariable('USERNAME'));
  i.IpAddresses := GetIpAddresses;
  i.IpAddress := GetIpAddress(i.IpAddresses);
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

  if i.LoginNetwork = 'administrator' then begin
    Terminate;
    Exit;
  end;

  if HasOption('l', 'log') then begin
    FullPathToLogFile := GetOptionValue('l', 'log');
    if FullPathToLogFile <> '' then WriteToLog(FullPathToLogFile);
    WriteToBimoidLog;
  end else WriteToConsole;

  if HasOption('p', 'post') then HttpPost;

  Terminate;
end;

procedure info.WriteToConsole;
begin
  WriteLn('ComputerName: ' + i.ComputerName);
  WriteLn('Description: ' + i.Description);
  WriteLn('LoginNetwork: ' + i.LoginNetwork);
  WriteLn('IP address: ' + i.IpAddress);
  WriteLn('IP addresses: ' + i.IpAddresses);
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
  json.Add('KEY', '687F1AE6195AC76694EC6D94B29D7DE708CE2739');
  json.Add('PC', i.ComputerName);
  json.Add('DESCRIPTION', i.Description);
  json.Add('NET_LOGIN', i.LoginNetwork);
  json.Add('IP', i.IpAddress);
  json.Add('CPU', i.CPU);
  json.Add('MEMORY', i.Memory);
  json.Add('OS', i.OS);
  json.Add('BIT', i.Bit);
  json.Add('VER', i.OSVersion);
  json.Add('RESOLUTION', i.Resolution);
  json.Add('IP_ADDRESSES', i.IpAddresses);

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
  else begin
    Rewrite(F);
    WriteLn(F, 'DT;PC;DESCRIPTION;NET_LOGIN;IP;CPU;MEMORY;OS;BIT;VER;RESOLUTION;IP_ADRESSES');
  end;
  dt := DateToStr(Date) + ' ' + TimeToStr(Time);
  WriteLn(F, dt + ';' + text);
  CloseFile(F);
end;

procedure info.WriteToLog(FileName: String);
var s: String;
begin
  s := i.ComputerName + ';' + i.Description + ';' + i.LoginNetwork + ';' +
    i.IpAddress + ';' + i.CPU + ';' + i.Memory + ';' + i.OS + ';' +
    i.Bit + ';' + i.OSVersion + ';' + i.Resolution + ';';
  Log(FileName, UTF8ToCP1251(s) + UTF8ToCP1251(i.IpAddresses));
end;

procedure info.WriteToBimoidLog;
var s: String;
  qty: Integer;
begin
  s := i.ComputerName + ';' + i.Description + ';' + i.LoginNetwork + ';' + i.IpAddress;

  if DirectoryExists('C:\Bimoid\Smilies\Default') then begin
    qty := GetFileCount('C:\Bimoid\Smilies\Default');
    if (qty <> 122) and (qty <> 124) then
      Log('\\10.19.19.121\log\bimoid1.csv', UTF8ToCP1251(s + ';' + IntToStr(qty)));
    if DirectoryExists('C:\Bimoid\Smilies\QIP_Smiles') then
      Log('\\10.19.19.121\log\bimoid2.csv', UTF8ToCP1251(s));
  end;
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

