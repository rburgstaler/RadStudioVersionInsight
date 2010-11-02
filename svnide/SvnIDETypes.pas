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
{ The Original Code is SvnIDETypes.pas.                                        }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2010 Uwe Schuster. All      }
{ Rights Reserved.                                                             }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit SvnIDETypes;

interface

uses
  Classes, Graphics, svn_client, SvnIDEClient, SvnClientUpdate;

type
  TCustomUpdateThread = class(TThread)
  protected
    FAborted: Boolean;
    FSvnIDEClient: TSvnIDEClient;
    FSyncPath: string;
    FSyncAction: string;
    FSyncConflicted: Boolean;
    FSyncTextColor: TColor;
    FUpdateDialog: TUpdateDialog;
    FExceptionMessage: string;
    procedure AbortCallBack;
    procedure CancelCallback(Sender: TObject; var Cancel: Boolean);
    procedure Add(const Path, Action: string; Conflicted: Boolean; TextColor: TColor);
    procedure SyncAdd;
    procedure SyncCompleted;
    function ConflictCallback(Sender: TObject; var ResultFileName: string;
      var Choice: TSvnWcConflictChoice; description: PSvnWcConflictDescription): Boolean;
    procedure UpdateCallBack(Sender: TObject; const Path, MimeType: string; Action: TSvnWcNotifyAction;
      Kind: TSvnNodeKind; ContentState, PropState: TSvnWCNotifyState; Revision: TSvnRevNum; var Cancel: Boolean);
  public
    constructor Create(SvnIDEClient: TSvnIDEClient); reintroduce;
  end;

  TMergeThread = class(TCustomUpdateThread)
  protected
    FURL: string;
    FPath: string;
    FRevision1: Integer;
    FRevision2: Integer;
    procedure Execute; override;
  public
    constructor Create(SvnIDEClient: TSvnIDEClient; AURL, APath: string; ARevision1, ARevision2: Integer); reintroduce;
  end;

implementation

uses
  SysUtils, SvnConst, SvnClient, SvnIDEConst;

{ TCustomUpdateThread }

procedure TCustomUpdateThread.AbortCallBack;
begin
  FAborted := True;
end;

procedure TCustomUpdateThread.Add(const Path, Action: string; Conflicted: Boolean; TextColor: TColor);
begin
  FSyncPath := StringReplace(Path, '/', '\', [rfReplaceAll]);
  FSyncAction := Action;
  FSyncConflicted := Conflicted;
  FSyncTextColor := TextColor;
  Synchronize(nil, SyncAdd);
end;

procedure TCustomUpdateThread.CancelCallback(Sender: TObject; var Cancel: Boolean);
begin
  Cancel := FAborted;
end;

function TCustomUpdateThread.ConflictCallback(Sender: TObject;
  var ResultFileName: string; var Choice: TSvnWcConflictChoice;
  description: PSvnWcConflictDescription): Boolean;
var
  TextColor: TColor;
begin
  ResultFileName := UTF8ToString(description.path);
  Choice := SvnWcConflictChoosePostpone;
  TextColor := IDEClient.Colors.GetNotifyActionColor(svnWcNotifyUpdateUpdate, svnWcNotifyStateConflicted);
  Add(ResultFileName, sWcNotifyStateConflicted, True, TextColor);
  Result := False;
end;

constructor TCustomUpdateThread.Create(SvnIDEClient: TSvnIDEClient);
begin
  inherited Create(True);
  FSvnIDEClient := SvnIDEClient;
  FAborted := False;
  FreeOnTerminate := True;
end;

procedure TCustomUpdateThread.SyncAdd;
begin
  FUpdateDialog.Add(FSyncPath, FSyncAction, FSyncConflicted, FSyncTextColor);
end;

procedure TCustomUpdateThread.SyncCompleted;
begin
  if FExceptionMessage <> '' then
    ShowSvnExceptionMessage(FExceptionMessage);
  FUpdateDialog.Completed;
end;

procedure TCustomUpdateThread.UpdateCallBack(Sender: TObject; const Path,
  MimeType: string; Action: TSvnWcNotifyAction; Kind: TSvnNodeKind;
  ContentState, PropState: TSvnWCNotifyState; Revision: TSvnRevNum;
  var Cancel: Boolean);
var
  ActionStr: string;
  TextColor: TColor;
begin
  TextColor := IDEClient.Colors.GetNotifyActionColor(Action, ContentState);
  if ContentState = svnWcNotifyStateConflicted then
    ActionStr := sWcNotifyStateConflicted
  else
  if ContentState = svnWcNotifyStateMerged then
    ActionStr := sWcNotifyStateMerged
  else
    ActionStr := NotifyActionStr(Action);
  if Action = svnWcNotifyUpdateCompleted then
    Add(Format(sUpdateCompletedAtRevision, [Revision]), ActionStr, False, TextColor)
  else
    Add(Path, ActionStr, False, TextColor);
end;

{ TMergeThread }

constructor TMergeThread.Create(SvnIDEClient: TSvnIDEClient; AURL, APath: string; ARevision1, ARevision2: Integer);
var
  Revision1Str, Revision2Str: string;
begin
  inherited Create(SvnIDEClient);
  FURL := AURL;
  FPath := APath;
  FRevision1 := ARevision1;
  FRevision2 := ARevision2;
  if ARevision1 <> -1 then
    Revision1Str := IntToStr(ARevision1)
  else
    Revision1Str := sHead;
  if ARevision2 <> -1 then
    Revision2Str := IntToStr(ARevision2)
  else
    Revision2Str := sHead;
  FUpdateDialog := GetUpdateDialog('', AbortCallBack, nil, nil);
  FUpdateDialog.Caption := Format(sMergeDialogCaption,
    [Revision1Str, Revision2Str, AURL, StringReplace(APath, '/', '\', [rfReplaceAll])]);
  FUpdateDialog.Show;
  Resume;
end;

procedure TMergeThread.Execute;
begin
  NameThreadForDebugging('DelphiSVN Merge');
  try
    FExceptionMessage := '';
    FSvnIDEClient.SvnClient.Merge(FURL, FRevision1, FURL, FRevision2, FPath, UpdateCallBack, CancelCallback);
  except
    if not GetSvnExceptionMessage(ExceptObject, FExceptionMessage) then
      raise;
  end;
  Synchronize(nil, SyncCompleted);
end;

end.
