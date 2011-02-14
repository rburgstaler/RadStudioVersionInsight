{******************************************************************************}
{                                                                              }
{ RAD Studio Version Insight                                                   }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License          }
{ Version 1.1 (the "License"); you may not use this file except in compliance  }
{ with the License. You may obtain a copy of the License at                    }
{ http://www.mozilla.org/MPL/                                                  }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is HgClient.pas.                                           }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2010 Uwe Schuster. All      }
{ Rights Reserved.                                                             }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit HgClient;

interface

uses
  Windows, SysUtils, Classes, Generics.Collections;

type
  THgItem = class;
  THgBlameItem = class;

  THgHistoryItem = class(TObject)
  private
    FAuthor: string;
    FAuthorEmail: string;
    FBlameItems: TObjectList<THgBlameItem>;
    FChangedFiles: TStringList;
    FDate: TDateTime;
    FChangeSet: string;
    FChangeSetID: Integer;
    FDescription: string;
    FParent: THgItem;
    FSummary: string;
    function GetBlameCount: Integer;
    function GetBlameItems(AIndex: Integer): THgBlameItem;
  public
    constructor Create(AParent: THgItem);
    destructor Destroy; override;
    function GetFile: TBytes;
    procedure LoadBlame;
    property Author: string read FAuthor;
    property AuthorEmail: string read FAuthorEmail;
    property BlameCount: Integer read GetBlameCount;
    property BlameItems[AIndex: Integer]: THgBlameItem read GetBlameItems;
    property ChangedFiles: TStringList read FChangedFiles;
    property Date: TDateTime read FDate;
    property ChangeSet: string read FChangeSet;
    property ChangeSetID: Integer read FChangeSetID;
    property Parent: THgItem read FParent;
    property Description: string read FDescription;
    property Summary: string read FSummary;
  end;

  THgBlameItem = class(TObject)
  private
    FLineStr: string;
    FHistoryIndex: Integer;
    FHistoryItem: THgHistoryItem;
  public
    property LineStr: string read FLineStr write FLineStr;
    property HistoryIndex: Integer read FHistoryIndex write FHistoryIndex;
    property HistoryItem: THgHistoryItem read FHistoryItem write FHistoryItem;
  end;

  THgStatus = (gsAdded, gsModified, gsNormal, gsUnknown);

  THgClient = class;

  THgItem = class(TObject)
  private
    FFileName: string;
    FHgClient: THgClient;
    FHistoryItems: TObjectList<THgHistoryItem>;
    FStatus: THgStatus;
    FLogLimit: Integer;
    FLogFirstRev: Integer;
    FLogLastRev: Integer;
    FIncludeChangedFiles: Boolean;
    function GetHistoryCount: Integer;
    function GetHistoryItems(AIndex: Integer): THgHistoryItem;
  public
    constructor Create(AHgClient: THgClient; const AFileName: string);
    destructor Destroy; override;
    procedure LoadHistory(AOnlyLast: Boolean = False);
    procedure LoadStatus;
    property HistoryCount: Integer read GetHistoryCount;
    property HistoryItems[AIndex: Integer]: THgHistoryItem read GetHistoryItems;
    property IncludeChangedFiles: Boolean read FIncludeChangedFiles write FIncludeChangedFiles;
    property LogLimit: Integer read FLogLimit write FLogLimit;
    property LogFirstRev: Integer read FLogFirstRev write FLogFirstRev;
    property LogLastRev: Integer read FLogLastRev write FLogLastRev;
    property Status: THgStatus read FStatus;
  end;

  THgClient = class(TObject)
  private
    FHgExecutable: string;
  public
    constructor Create;
    function IsVersioned(const AFileName: string): Boolean;
    property HgExecutable: string read FHgExecutable write FHgExecutable;
  end;

implementation

uses
  SHFolder;

//--- JclBase and JclSysUtils --------------------------------------------------
const
  // line delimiters for a version of Delphi/C++Builder
  NativeLineFeed       = Char(#10);
  NativeCarriageReturn = Char(#13);

function CharIsReturn(const C: Char): Boolean;
begin
  Result := (C = NativeLineFeed) or (C = NativeCarriageReturn);
end;

// memory initialization
procedure ResetMemory(out P; Size: Longint);
begin
  if Size > 0 then
  begin
    Byte(P) := 0;
    FillChar(P, Size, 0);
  end;
end;

const
  ABORT_EXIT_CODE = {$IFDEF MSWINDOWS} ERROR_CANCELLED {$ELSE} 1223 {$ENDIF};

type
  // e.g. TStrings.Append
  TTextHandler = procedure(const Text: string) of object;

function MuteCRTerminatedLines(const RawOutput: string): string;
const
  Delta = 1024;
var
  BufPos, OutPos, LfPos, EndPos: Integer;
  C: Char;
begin
  SetLength(Result, Length(RawOutput));
  OutPos := 1;
  LfPos := OutPos;
  EndPos := OutPos;
  for BufPos := 1 to Length(RawOutput) do
  begin
    if OutPos >= Length(Result)-2 then
      SetLength(Result, Length(Result) + Delta);
    C := RawOutput[BufPos];
    case C of
      NativeCarriageReturn:
        OutPos := LfPos;
      NativeLineFeed:
        begin
          OutPos := EndPos;
          Result[OutPos] := NativeCarriageReturn;
          Inc(OutPos);
          Result[OutPos] := C;
          Inc(OutPos);
          EndPos := OutPos;
          LfPos := OutPos;
        end;
    else
      Result[OutPos] := C;
      Inc(OutPos);
      EndPos := OutPos;
    end;
  end;
  SetLength(Result, OutPos - 1);
end;

function InternalExecute(CommandLine: string; var Output: string; OutputLineCallback: TTextHandler;
  RawOutput: Boolean; AbortPtr: PBoolean): Cardinal;

const
  BufferSize = 255;
type
  TBuffer = array [0..BufferSize] of AnsiChar;

  procedure ProcessLine(const Line: string; LineEnd: Integer);
  begin
    if RawOutput or (Line[LineEnd] <> NativeCarriageReturn) then
    begin
      while (LineEnd > 0) and CharIsReturn(Line[LineEnd]) do
        Dec(LineEnd);
      OutputLineCallback(Copy(Line, 1, LineEnd));
    end;
  end;

  procedure ProcessBuffer(var Buffer: TBuffer; var Line: string; PipeBytesRead: Cardinal);
  var
    CR, LF: Integer;
  begin
    Buffer[PipeBytesRead] := #0;
    Line := Line + string(Buffer);
    if Assigned(OutputLineCallback) then
    repeat
      CR := Pos(NativeCarriageReturn, Line);
      if CR = Length(Line) then
        CR := 0;        // line feed at CR + 1 might be missing
      LF := Pos(NativeLineFeed, Line);
      if (CR > 0) and ((LF > CR + 1) or (LF = 0)) then
        LF := CR;       // accept CR as line end
      if LF > 0 then
      begin
        ProcessLine(Line, LF);
        Delete(Line, 1, LF);
      end;
    until LF = 0;
  end;

var
  Buffer: TBuffer;
  Line: string;
  PipeBytesRead: Cardinal;
{$IFDEF MSWINDOWS}
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  SecurityAttr: TSecurityAttributes;
  PipeRead, PipeWrite: THandle;
begin
  Result := $FFFFFFFF;
  SecurityAttr.nLength := SizeOf(SecurityAttr);
  SecurityAttr.lpSecurityDescriptor := nil;
  SecurityAttr.bInheritHandle := True;
  PipeWrite := 0;
  PipeRead := 0;
  Line := '';
  ResetMemory(Buffer, SizeOf(Buffer));
  if not CreatePipe(PipeRead, PipeWrite, @SecurityAttr, 0) then
  begin
    Result := GetLastError;
    Exit;
  end;
  ResetMemory(StartupInfo, SizeOf(TStartupInfo));
  StartupInfo.cb := SizeOf(TStartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
  StartupInfo.wShowWindow := SW_HIDE;
  StartupInfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
  StartupInfo.hStdOutput := PipeWrite;
  StartupInfo.hStdError := PipeWrite;
  UniqueString(CommandLine); // CommandLine must be in a writable memory block
  ProcessInfo.dwProcessId := 0;
  try
    if CreateProcess(nil, PChar(CommandLine), nil, nil, True, NORMAL_PRIORITY_CLASS,
      nil, nil, StartupInfo, ProcessInfo) then
    begin
      CloseHandle(PipeWrite);
      PipeWrite := 0;
      if AbortPtr <> nil then
        {$IFDEF FPC}
        AbortPtr^ := 0;
        {$ELSE ~FPC}
        AbortPtr^ := False;
        {$ENDIF ~FPC}
      PipeBytesRead := 0;
      while ((AbortPtr = nil) or not LongBool(AbortPtr^)) and
        ReadFile(PipeRead, Buffer, BufferSize, PipeBytesRead, nil) and (PipeBytesRead > 0) do
        ProcessBuffer(Buffer, Line, PipeBytesRead);
      if (AbortPtr <> nil) and LongBool(AbortPtr^) then
        TerminateProcess(ProcessInfo.hProcess, Cardinal(ABORT_EXIT_CODE));
      if (WaitForSingleObject(ProcessInfo.hProcess, INFINITE) = WAIT_OBJECT_0) and
        not GetExitCodeProcess(ProcessInfo.hProcess, Result) then
          Result := $FFFFFFFF;
      CloseHandle(ProcessInfo.hThread);
      ProcessInfo.hThread := 0;
      CloseHandle(ProcessInfo.hProcess);
      ProcessInfo.hProcess := 0;
    end
    else
    begin
      CloseHandle(PipeWrite);
      PipeWrite := 0;
    end;
    CloseHandle(PipeRead);
    PipeRead := 0;
  finally
    if PipeRead <> 0 then
      CloseHandle(PipeRead);
    if PipeWrite <> 0 then
      CloseHandle(PipeWrite);
    if ProcessInfo.hThread <> 0 then
      CloseHandle(ProcessInfo.hThread);
    if ProcessInfo.hProcess <> 0 then
    begin
      TerminateProcess(ProcessInfo.hProcess, Cardinal(ABORT_EXIT_CODE));
      WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
      GetExitCodeProcess(ProcessInfo.hProcess, Result);
      CloseHandle(ProcessInfo.hProcess);
    end;
  end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
var
  Pipe: PIOFile;
  Cmd: string;
begin
  Cmd := Format('%s 2>&1', [CommandLine]);
  Pipe := nil;
  try
    Pipe := Libc.popen(PChar(Cmd), 'r');
    { TODO : handle Abort }
    repeat
      PipeBytesRead := fread_unlocked(@Buffer, 1, BufferSize, Pipe);
      if PipeBytesRead > 0 then
        ProcessBuffer(Buffer, Line, PipeBytesRead);
    until PipeBytesRead = 0;
    Result := pclose(Pipe);
    Pipe := nil;
    wait(nil);
  finally
    if Pipe <> nil then
      pclose(Pipe);
    wait(nil);
  end;
{$ENDIF UNIX}
  if Line <> '' then
    if Assigned(OutputLineCallback) then
      // output wasn't terminated by a line feed...
      // (shouldn't happen, but you never know)
      ProcessLine(Line, Length(Line))
    else
      if RawOutput then
        Output := Output + Line
      else
        Output := Output + MuteCRTerminatedLines(Line);
end;

function Execute(const CommandLine: string; var Output: string; RawOutput: Boolean = False;
  AbortPtr: PBoolean = nil): Cardinal;
begin
  Result := InternalExecute(CommandLine, Output, nil, RawOutput, AbortPtr);
end;

//------------------------------------------------------------------------------

const
  map_cmdline_verins =
    'header = ''''' + #13#10 +
    'header_verbose = ''''' + #13#10 +
    'changeset = ''changeset:   {rev}:{node|short}\nuser:        {author|person}\nuser email:  <{author|email}>\ndate:        {date|date}\ndesc:        \n{desc}\n\n''' + #13#10 +
    'changeset_quiet = ''\t* {desc|firstline|fill68|tabindent|strip}\n\n''' + #13#10 +
    'changeset_verbose = ''changeset:   {rev}:{node|short}\nuser:        {author|person}\nuser email:  <{author|email}>\ndate:        {date|date}\ndesc:        \n{desc}\nfiles:       \n{file_adds}{file_dels}{files}\n''' + #13#10 +
    'start_tags = '' [''' + #13#10 +
    'tag = ''{tag}, ''' + #13#10 +
    'last_tag = ''{tag}]''' + #13#10 +
    'start_branches = '' <''' + #13#10 +
    'branch = ''{branch}, ''' + #13#10 +
    'last_branch = ''{branch}>''' + #13#10 +
    'file = ''M {file}\n''' + #13#10 +
    'last_file = ''M {file}\n''' + #13#10 +
    'file_add = ''A {file_add}\n''' + #13#10 +
    'last_file_add = ''A {file_add}\n''' + #13#10 +
    'file_del = ''D {file_del}\n''' + #13#10 +
    'last_file_del = ''D {file_del}\n '''
    ;

function GetMapCmdlineVerInsFileName: string;
var
  LStr: array [0..MAX_PATH] of Char;
  SL: TStringList;
begin
  Result := '';
  SetLastError(ERROR_SUCCESS);
  if SHGetFolderPath(0, CSIDL_COMMON_APPDATA, 0, 0, @LStr) = S_OK then
  begin
    Result := LStr + '\VerIns\map-cmdline.verins';
    if not FileExists(Result) then
    begin
      ForceDirectories(ExtractFilePath(Result));
      SL := TStringList.Create;
      try
        SL.Text := map_cmdline_verins;
        SL.SaveToFile(Result);
      finally
        SL.Free;
      end;
    end;
  end;
end;

{ THgHistoryItem }

constructor THgHistoryItem.Create(AParent: THgItem);
begin
  inherited Create;
  FParent := AParent;
  FBlameItems := TObjectList<THgBlameItem>.Create;
  FChangedFiles := TStringList.Create;
end;

destructor THgHistoryItem.Destroy;
begin
  FChangedFiles.Free;
  FBlameItems.Free;
  inherited Destroy;
end;

function THgHistoryItem.GetBlameCount: Integer;
begin
  Result := FBlameItems.Count;
end;

function THgHistoryItem.GetBlameItems(AIndex: Integer): THgBlameItem;
begin
  Result := FBlameItems[AIndex];
end;

function THgHistoryItem.GetFile: TBytes;
var
  Res: Integer;
  CmdLine, Output: string;
  CurrentDir: string;
  FileContent: AnsiString;
begin
  CurrentDir := GetCurrentDir;
  try
    SetCurrentDir(ExtractFilePath(FParent.FFileName));
    CmdLine := FParent.FHgClient.HgExecutable + ' cat -r ' + FChangeSet + ' ' + FParent.FFileName;
    Res := Execute(CmdLine, Output);
    FileContent := Output;
    SetLength(Result,  Length(FileContent));
    Move(FileContent[1], Result[0],  Length(FileContent));
  finally
    SetCurrentDir(CurrentDir);
  end;
end;

procedure THgHistoryItem.LoadBlame;
var
  I, J, P, ID, Idx, Res: Integer;
  CmdLine, Output: string;
  OutputStrings: TStringList;
  BlameItem: THgBlameItem;
  S, {S2, }CurrentDir, Hash: string;
begin
  CurrentDir := GetCurrentDir;
  try
    SetCurrentDir(ExtractFilePath(FParent.FFileName));
    CmdLine := FParent.FHgClient.HgExecutable + Format(' annotate -r %d ', [FChangeSetID]);
    CmdLine := CmdLine + ExtractFileName(FParent.FFileName);
    Res := Execute(CmdLine, Output);
  finally
    SetCurrentDir(CurrentDir);
  end;
  FBlameItems.Clear;
  if Res = 0 then
  begin
    OutputStrings := TStringList.Create;
    try
      OutputStrings.Text := Output;
      I := 0;
      while I < OutputStrings.Count do
      begin
        S := OutputStrings[I];
        BlameItem := FBlameItems[FBlameItems.Add(THgBlameItem.Create)];
        P := Pos(': ', S);
        if P > 0 then
        begin
          ID := StrToIntDef(Copy(S, 1, P - 1), -1);
          Idx := -1;
          for J := 0 to FParent.HistoryCount - 1 do
            if ID = FParent.HistoryItems[J].ChangeSetID then
            begin
              Idx := J;
              Break;
            end;
          BlameItem.HistoryIndex := Idx;
          if Idx <> -1 then
            BlameItem.HistoryItem := FParent.HistoryItems[Idx]
          else
            BlameItem.HistoryItem := nil;
          BlameItem.LineStr := Copy(S, P + 2, Length(S));
        end;
        Inc(I);
      end;
    finally
      OutputStrings.Free;
    end;
  end;
end;

{ THgItem }

constructor THgItem.Create(AHgClient: THgClient; const AFileName: string);
begin
  inherited Create;
  FHgClient := AHgClient;
  FHistoryItems := TObjectList<THgHistoryItem>.Create;
  FFileName := AFileName;
  FStatus := gsUnknown;
  FLogLimit := -1;
  FLogFirstRev := -1;
  FLogLastRev := -1;
  FIncludeChangedFiles := False;
end;

destructor THgItem.Destroy;
begin
  FHistoryItems.Free;
  inherited Destroy;
end;

function THgItem.GetHistoryCount: Integer;
begin
  Result := FHistoryItems.Count;
end;

function THgItem.GetHistoryItems(AIndex: Integer): THgHistoryItem;
begin
  Result := FHistoryItems[AIndex];
end;

function ConvertDate(const AStr: string): TDateTime;
const
  MonthNames: array [1..12] of string = ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul',
    'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
var
  S: string;
  I, M, D, Y: Integer;
  TimePart: TDateTime;
begin
  Result := 0;
  S := AStr;
  Delete(S, 1, 4);
  M := 0;
  for I := 1 to 12 do
    if Pos(MonthNames[I], S) = 1 then
    begin
      M := I;
      Break;
    end;
  if M <> 0 then
  begin
    Delete(S, 1, 4);
    D := StrToIntDef(Copy(S, 1, 2), 0);
    if D <> 0 then
    begin
      Delete(S, 1, 3);
      TimePart := StrToTimeDef(Copy(S, 1, 8), -1);
      if TimePart >= 0 then
      begin
        Delete(S, 1, 9);
        Y := StrToIntDef(Copy(S, 1, 4), -1);
        if Y > 0 then
          Result := EncodeDate(Y, M, D) + Frac(TimePart);
      end;
    end;
  end;
end;

procedure THgItem.LoadHistory(AOnlyLast: Boolean = False);
var
  I, P, Res: Integer;
  CmdLine, Output: string;
  OutputStrings: TStringList;
  HistoryItem: THgHistoryItem;
  S, CurrentDir: string;
  StyleFileName: string;
  UseStyleFile: Boolean;
begin
  StyleFileName := GetMapCmdlineVerInsFileName;
  UseStyleFile := (StyleFileName <> '') and FileExists(StyleFileName);
  CurrentDir := GetCurrentDir;
  try
    SetCurrentDir(ExtractFilePath(FFileName));
    CmdLine := FHgClient.HgExecutable + ' log ';
    if FLogFirstRev > 0 then
      CmdLine := CmdLine + Format(' -r%d:%d', [FLogFirstRev, FLogFirstRev - FLogLimit + 1]);
    if FLogLimit > 0 then
      CmdLine := CmdLine + Format(' -l %d', [FLogLimit]);
    CmdLine := CmdLine + ExtractFileName(FFileName);
    if UseStyleFile then
    begin
      CmdLine := CmdLine + Format(' --style=%s', [StyleFileName]);
      if FIncludeChangedFiles then
        CmdLine := CmdLine + ' -v';
    end;
    Res := Execute(CmdLine, Output);
  finally
    SetCurrentDir(CurrentDir);
  end;
  FHistoryItems.Clear;
  if Res = 0 then
  begin
    OutputStrings := TStringList.Create;
    try
      OutputStrings.Text := Output;
      I := 0;
      HistoryItem := nil;
      while I < OutputStrings.Count do
      begin
        S := OutputStrings[I];
        if Pos('changeset: ', OutputStrings[I]) = 1 then
          HistoryItem := FHistoryItems[FHistoryItems.Add(THgHistoryItem.Create(Self))];
        if Assigned(HistoryItem) then
        begin
          if Pos('changeset:', S) = 1 then
          begin
            Delete(S, 1, 10);
            S := Trim(S);
            P := Pos(':', S);
            if P > 0 then
            begin
              HistoryItem.FChangeSet := Copy(S, P + 1, Length(S) - P);
              HistoryItem.FChangeSetID := StrToIntDef(Copy(S, 1, P - 1), -1);
            end;
          end
          else
          if Pos('date:', S) = 1 then
          begin
            Delete(S, 1, 5);
            S := Trim(S);
            HistoryItem.FDate := ConvertDate(S);
          end
          else
          if Pos('user:', S) = 1 then
          begin
            Delete(S, 1, 5);
            S := Trim(S);
            HistoryItem.FAuthor := S;
          end
          else
          if Pos('user email:', S) = 1 then
          begin
            Delete(S, 1, 11);
            S := Trim(S);
            HistoryItem.FAuthorEmail := S;
          end
          else
          if Pos('summary:', S) = 1 then
          begin
            Delete(S, 1, 8);
            S := Trim(S);
            HistoryItem.FSummary := S;
            HistoryItem.FDescription := S;
          end
          else
          if Pos('desc:', S) = 1 then
          begin
            Inc(I);
            S := '';
            while (I < OutputStrings.Count) and (Pos('files:', OutputStrings[I]) = 0) and
              (Pos('changeset:', OutputStrings[I]) = 0) do
            begin
              S := S + OutputStrings[I] + #13#10;
              Inc(I);
            end;
            if (Pos('files:', OutputStrings[I]) = 1) or (Pos('changeset:', OutputStrings[I]) = 1) then
              Dec(I);
            S := Trim(S);
            if Pos(#13, S) > 0 then
              HistoryItem.FSummary := Copy(S, 1, Pos(#13, S) - 1)
            else
              HistoryItem.FSummary := S;
            HistoryItem.FDescription := S;
          end
          else
          if Pos('files:', S) = 1 then
          begin
            Inc(I);
            while (I < OutputStrings.Count) and (Pos('changeset:', OutputStrings[I]) = 0) do
            begin
              S := OutputStrings[I];
              if (Length(S) > 2) and CharInSet(S[1], ['M', 'A', 'D']) and (S[2] = ' ') then
              begin
                Delete(S, 2, 1);
                HistoryItem.FChangedFiles.Add(S);
              end;
              Inc(I);
            end;
            if Pos('changeset:', OutputStrings[I]) = 1 then
              Dec(I);
          end;
        end;
        Inc(I);
      end;
    finally
      OutputStrings.Free;
    end;
  end;
end;

procedure THgItem.LoadStatus;
begin
  FStatus := gsUnknown;
end;

{ THgClient }

constructor THgClient.Create;
begin
  inherited Create;
  //FHgExecutable := 'hg.exe';
end;

function THgClient.IsVersioned(const AFileName: string): Boolean;
var
  Res: Integer;
  CmdLine, Output: string;
  CurrentDir: string;
begin
  CurrentDir := GetCurrentDir;
  try
    SetCurrentDir(ExtractFilePath(AFileName));
    CmdLine := FHgExecutable + ' status ' + ExtractFileName(AFileName);
    Res := Execute(CmdLine, Output);
    Result := {(Res = 0) and }(Pos('abort: There is no Mercurial repository', Output) = 0);
  finally
    SetCurrentDir(CurrentDir);
  end;
end;

end.
