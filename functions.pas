unit functions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, Registry, Math, USock, StrUtils;

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
function GetIpAddresses: String;
function GetIpAddress(ipAddresses: String): String;
function GetResolution: String;
function str_explode(const delim, str: string): TStringList;
function str_implode(delim: string; const list:TStringlist): string;
function GlobalMemoryStatusEx(var Buffer: MEMORYSTATUSEX): Boolean;
  stdcall; external 'kernel32' Name 'GlobalMemoryStatusEx';
function GetFileCount(Directory: String): Integer;
function GetDirCount(const DirName: String): Integer;

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

function GetIpAddresses: String;
var
  ipAddresses: String;
  list, new_list: TStringList;
  i: Integer;
begin
  ipAddresses := '';
  EnumInterfaces(ipAddresses);
  Delete(ipAddresses, Length(ipAddresses), 1);
  list := str_explode(',', ipAddresses);
  list.Sort;
  new_list := TStringList.Create;
  for i:=0 to list.Count-1 do begin
    if (list[i] <> '127.0.0.1') then new_list.Add(list[i]);
  end;
  Result := str_implode(',', new_list);
end;

function GetIpAddress(ipAddresses: String): String;
var
  ipAddress: String;
  list: TStringList;
  i: Integer;
begin
  ipAddress := '';
  list := str_explode(',', ipAddresses);
  for i:=0 to list.Count-1 do begin
    if (copy(list[i],1,3) = '10.') then ipAddress := list[i];
  end;
  Result := ipAddress;
end;

function GetResolution: String;
begin
  Result := IntToStr(GetSystemMetrics(SM_CXSCREEN)) + 'x' +
    IntToStr(GetSystemMetrics(SM_CYSCREEN));
end;

function str_explode(const delim, str: string): TStringList;
    var offset: integer;
        cur: integer;
        dl: integer;
begin
    Result := TStringList.Create;
    dl := Length(delim);
    offset := 1;
    while True do begin
        cur := PosEx(delim, str, offset);
        if cur > 0 then
            Result.Add(Copy(str, offset, cur - offset))
        else begin
            Result.Add(Copy(str, offset, Length(str) - offset + 1));
            Break
        end;
        offset := cur + dl;
    end;
end;

function str_implode(delim: string; const list: TStringlist): string;
    var i: integer;
begin
    result := '';
    if list.Count = 0 then exit;
    for i := 0 to list.Count - 2 do result := result + list[i] + delim;
    result := result + list[list.Count-1];
end;

function GetFileCount(Directory: String): Integer;
var fs: TSearchRec;
begin
  Result := 0;
  if FindFirst(Directory + '/*.*', faAnyFile-faDirectory-faVolumeID, fs) = 0 then
  repeat
    inc(Result);
  until FindNext(fs) <> 0;
  SysUtils.FindClose(fs);
end;

function GetDirCount(const DirName: String): Integer;
var
  Path, SubDirName: String;
  F: TSearchRec;
begin
  Path := DirName + '\*.*';
  Result := 0;
  if FindFirst(Path, faAnyFile, F) = 0 then begin
    try
      repeat
        if (F.Attr and faDirectory <> 0) then begin
          if (F.Name <> '.') and (F.Name <> '..') then begin
            SubDirName := IncludeTrailingPathDelimiter(DirName) + F.Name;
            //Result:= Result + 1 + ListFolders(SubDirName); /* Recursion */
            Inc(Result);
          end;
        end;
      until FindNext(F) <> 0;
    finally
      SysUtils.FindClose(F);
    end;
  end;
end;

end.

