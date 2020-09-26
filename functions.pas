unit functions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, Registry, Math, USock;

type
    MEMORYSTATUSEX = record
    dwLength: DWORD;
    dwMemoryLoad: DWORD;
    ullTotalPhys: uint64;
    ullAvailPhys: uint64;
    ullTotalPageFile: uint64;
    ullAvailPageFile: uint64;
    ullTotalVirtual: uint64;
    ullAvailVirtual: uint64;
    ullAvailExtendedVirtual: uint64;
  end;

function GetMemory: Double;
function GetComputerNetDescription: String;
function GetRegData(Key, Name: String): String;
function GetEnvironment(Name: String): String;
function GetOS: String;
function GetBit: String;
function GetOSVersion: String;
function GetProcessorInfo: String;
function IsWindows64: Boolean;
function GetIpAddress: String;
function GetResolution: String;

function GlobalMemoryStatusEx(var Buffer: MEMORYSTATUSEX): Boolean;
  stdcall; external 'kernel32' Name 'GlobalMemoryStatusEx';

implementation

function GetMemory: Double;
var
  PhysRam: MEMORYSTATUSEX;
  MemSize: Double;
begin
  PhysRam.dwLength := SizeOf(PhysRam);
  GlobalMemoryStatusEx(PhysRam);
  MemSize := PhysRam.ullTotalPhys / 1024 / 1024 / 1024;
  if MemSize < 1
    then MemSize := RoundTo(MemSize, -1)
    else MemSize := Round(MemSize);
  Result := MemSize;
end;

function IsWindows64: Boolean;
  {
  Detect if we are running on 64 bit Windows or 32 bit Windows,
  independently of bitness of this program.
  Original source:
  http://www.delphipraxis.net/118485-ermitteln-ob-32-bit-oder-64-bit-betriebssystem.html
  modified for FreePascal in German Lazarus forum:
  http://www.lazarusforum.de/viewtopic.php?f=55&t=5287
  }
{$ifdef WIN32} //Modified KpjComp for 64bit compile mode
  type
    TIsWow64Process = function( // Type of IsWow64Process API fn
        Handle: Windows.THandle; var Res: Windows.BOOL): Windows.BOOL; stdcall;
  var
    IsWow64Result: Windows.BOOL; // Result from IsWow64Process
    IsWow64Process: TIsWow64Process; // IsWow64Process fn reference
  begin
    IsWow64Result := false;
    // Try to load required function from kernel32
    IsWow64Process := TIsWow64Process(Windows.GetProcAddress(
      Windows.GetModuleHandle('kernel32'), 'IsWow64Process'));
    if Assigned(IsWow64Process) then
    begin
      // Function is implemented: call it
      if not IsWow64Process(Windows.GetCurrentProcess, IsWow64Result) then
        raise SysUtils.Exception.Create('IsWindows64: bad process handle');
      // Return result of function
      Result := IsWow64Result;
    end
    else
      // Function not implemented: can't be running on Wow64
      Result := False;
{$else} //if were running 64bit code, OS must be 64bit :)
  begin
   Result := True;
{$endif}
end;

function GetRegData(Key, Name: String): String;
var
  Reg: TRegistry;
begin
  Result := '';
  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_LOCAL_MACHINE;
  Reg.OpenKeyReadOnly(Key);
  Result := Reg.ReadString(Name);
  Reg.CloseKey;
  Reg.Free;
end;

function GetComputerNetDescription: String;
begin
  Result := GetRegData('SYSTEM\CurrentControlSet\Services\LanmanServer\Parameters', 'SrvComment');
end;

function GetOS: String;
begin
  Result := GetRegData('SOFTWARE\Microsoft\Windows NT\CurrentVersion', 'ProductName');
end;

function GetBit: String;
begin
  Result := '32';
  if IsWindows64 = true then Result := '64';
end;

function GetOSVersion: String;
begin
  Result := GetRegData('SOFTWARE\Microsoft\Windows NT\CurrentVersion', 'ReleaseId');
end;

function GetProcessorInfo: String;
begin
  Result := GetRegData('\HARDWARE\DESCRIPTION\System\CentralProcessor\0', 'ProcessorNameString');
end;

function GetEnvironment(Name: String): String;
begin
  Result := GetRegData('SYSTEM\CurrentControlSet\Control\Session manager\Environment', Name);
end;

function GetIpAddress: String;
var
  ipAddress: String;
begin
  ipAddress := '';
  EnumInterfaces(ipAddress);
  Result := ipAddress;
end;

function GetResolution: String;
begin
  Result := IntToStr(GetSystemMetrics(SM_CXSCREEN)) + 'x' +
    IntToStr(GetSystemMetrics(SM_CYSCREEN));
end;

end.

