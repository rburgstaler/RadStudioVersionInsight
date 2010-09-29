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
{ The Original Code is delphisvn: Subversion plugin for CodeGear Delphi.       }
{                                                                              }
{ The Initial Developer of the Original Code is Embarcadero Technologies.      }
{ Portions created by Ondrej Kelle are Copyright Ondrej Kelle. All rights      }
{ reserved.                                                                    }
{                                                                              }
{ Portions created or modified by Embarcadero Technologies are                 }
{ Copyright © 2010 Embarcadero Technologies, Inc. All Rights Reserved          }
{ Modifications include a major re-write of delphisvn. New functionality for   }
{ diffing, international character support, asynchronous gathering of data,    }
{ check-out and import, usability, tighter integration into RAD Studio, and    }
{ other new features.  Most original source files not used or re-written.      }
{                                                                              }
{ Contributors:                                                                }
{ Ondrej Kelle (tondrej)                                                       }
{ Uwe Schuster (uschuster)                                                     }
{ Embarcadero Technologies                                                     }
{                                                                              }
{******************************************************************************}
unit SvnClientLog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Generics.Collections, ToolWin, ImgList,
  SvnUITypes;

const
  DefaultRange = 100;

type
  TRevision = class
  protected
    FRevision: string;
    FTime: string;
    FAuthor: string;
    FComment: string;
    FFiles: TStringList;
  public
    constructor Create(const Revision, Time, Author, Comment: string;
      const Files: TStringList);
    destructor Destroy; override;
  end;

  TLoadRevisionsCallBack = procedure(FirstRevision: Integer; Count: Integer) of object;

  TSvnLogFrame = class(TFrame)
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    CenterPanel: TPanel;
    BottomPanel: TPanel;
    TopPanel: TPanel;
    Revisions: TListView;
    Files: TListView;
    Comment: TMemo;
    ToolBar1: TToolBar;
    Refresh: TToolButton;
    Next: TToolButton;
    Search: TButtonedEdit;
    ImageList: TImageList;
    procedure RevisionsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure SearchKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SearchKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure RefreshClick(Sender: TObject);
    procedure NextClick(Sender: TObject);
    procedure SearchKeyPress(Sender: TObject; var Key: Char);
    procedure SearchRightButtonClick(Sender: TObject);
    procedure FilesData(Sender: TObject; Item: TListItem);
  protected
    FCount: Integer;
    FDoingSearch: Boolean;
    FLoadRevisionsCallBack: TLoadRevisionsCallBack;
    FRevisionList: TList<TRevision>;
    FSaveCursor: TCursor;
    class var FUseCount: Integer;
    procedure AddRevisionToListView(ARevision: TRevision);
    procedure DoCancelSearch;
    procedure DoSearch(const Text: string);
    function GetCommentColumn: Integer;
    function GetSvnEditState: TSvnEditState;
    procedure RestoreRevisions;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddRevisions(Revision: Integer; Time: TDateTime; const Author: string;
      const Comment: string; const Files: TStringList);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure StartAsync;
    procedure NextCompleted;
    function PerformEditAction(AEditAction: TSvnEditAction): Boolean;
    property LoadRevisionsCallBack: TLoadRevisionsCallBack read FLoadRevisionsCallBack write FLoadRevisionsCallBack;
    property SvnEditState: TSvnEditState read GetSvnEditState;
  end;

implementation

uses SvnUIConst, SvnUIUtils, Clipbrd;

{$R *.dfm}

{ TRevision }

constructor TRevision.Create(const Revision, Time, Author, Comment: string;
      const Files: TStringList);
begin
  FRevision := Revision;
  FTime := Time;
  FAuthor := Author;
  FComment := Comment;
  if Files <> nil then
  begin
    FFiles := TStringList.Create;
    FFiles.Assign(Files);
  end;
end;

destructor TRevision.Destroy;
begin
  if FFiles <> nil then
    FFiles.Free;
  inherited;
end;

{ TSvnLogFrame }

procedure TSvnLogFrame.AddRevisions(Revision: Integer; Time: TDateTime;
  const Author, Comment: string; const Files: TStringList);
var
  TempRev: string;
  TempTime: string;
  FirstItem: Boolean;
begin
  FirstItem := Revisions.Items.Count = 0;
  TempRev := IntToStr(Revision);
  TempTime := DateTimeToStr(Time);
  FRevisionList.Add(TRevision.Create(TempRev, TempTime, Author, Comment, Files));
  AddRevisionToListView(FRevisionList.Last);
  if FirstItem then
    Revisions.Items[0].Selected := True;
end;

procedure TSvnLogFrame.AddRevisionToListView(ARevision: TRevision);
var
  ListItem: TListItem;
  I: Integer;
begin
  if Revisions.Items.Count = 0 then
  begin
    for I := 1 to Revisions.Columns.Count - 1 do
      Revisions.Columns[I].Width := -1;
  end;
  ListItem := Revisions.Items.Add;
  ListItem.Caption := ARevision.FRevision;
  ListItem.SubItems.Add(ARevision.FAuthor);
  ListItem.SubItems.Add(ARevision.FTime);
  ListItem.SubItems.Add(ARevision.FComment);
  ListItem.Data := ARevision.FFiles;
end;

procedure TSvnLogFrame.BeginUpdate;
begin
  Revisions.Items.BeginUpdate
end;

procedure TSvnLogFrame.SearchKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
    begin
      DoSearch(Search.Text);
      Key := 0;
    end;
    VK_ESCAPE:
    begin
      DoCancelSearch;
      Key := 0;
    end;
  end;
end;

procedure TSvnLogFrame.SearchKeyPress(Sender: TObject; var Key: Char);
begin
  // Prevent the windows beep.
  if (Key = #27) or (Key = #13) then
    Key := #0;
end;

procedure TSvnLogFrame.SearchKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Search.Text = '' then
    RestoreRevisions;
end;

procedure TSvnLogFrame.StartAsync;
begin
end;

procedure TSvnLogFrame.SearchRightButtonClick(Sender: TObject);
begin
  DoCancelSearch;
end;

constructor TSvnLogFrame.Create(AOwner: TComponent);
begin
  inherited;
  Name := Format('%s_%d', [Name, FUseCount]);
  Inc(FUseCount);
  FRevisionList := TList<TRevision>.Create;
  FDoingSearch := False;
  FCount := DefaultRange;
end;

destructor TSvnLogFrame.Destroy;
begin
  FRevisionList.Free;
  inherited;
end;

procedure TSvnLogFrame.DoCancelSearch;
begin
  Search.Clear;
  RestoreRevisions;
  Revisions.SetFocus;
end;

procedure TSvnLogFrame.DoSearch(const Text: string);

  function CheckItem(const Item: TRevision): Boolean;
  var
    I: Integer;
    S: string;
  begin
    S := AnsiLowerCase(Text);
    Result := Pos(S, AnsiLowerCase(Item.FAuthor)) <> 0;
    if Result then
      Exit;
    Result := Pos(S, AnsiLowerCase(Item.FComment)) <> 0;
    if Result then
      Exit;
    if Item.FFiles <> nil then
    begin
      for I := 0 to Item.FFiles.Count - 1 do
      begin
        Result := Pos(S, AnsiLowerCase(Item.FFiles[I])) <> 0;
        if Result then
          Exit;
      end;
    end;
  end;

var
  I: Integer;
begin
  if Text <> '' then
  begin
    Revisions.Items.BeginUpdate;
    try
      FDoingSearch := True;
      Revisions.Clear;
      Files.Clear;
      Comment.Lines.Text := '';
      for I := 0 to FRevisionList.Count - 1 do
        if CheckItem(FRevisionList[I]) then
          AddRevisionToListView(FRevisionList[I]);
    finally
      Revisions.Items.EndUpdate
    end;
  end;
  Search.RightButton.Visible := True;
end;

procedure TSvnLogFrame.EndUpdate;
begin
  Revisions.Items.EndUpdate;
end;

procedure TSvnLogFrame.FilesData(Sender: TObject; Item: TListItem);
var
  S: string;
  FilesSL: TStringList;
begin
  if (Revisions.Selected <> nil) and (Revisions.Selected.Data <> nil) then
  begin
    FilesSL := TStringList(Revisions.Selected.Data);
    S := System.Copy(FilesSL[Item.Index], 2, MaxInt);
    case FilesSL[Item.Index][1] of
      'M': Item.Caption := sModified;
      'A': Item.Caption := sAdded;
      'D': Item.Caption := sDeleted;
      'R': Item.Caption := sReplaced;
    end;
    Item.SubItems.Add(S);
  end;
end;

function TSvnLogFrame.GetCommentColumn: Integer;
begin
  Result := 3;
end;

function TSvnLogFrame.GetSvnEditState: TSvnEditState;
begin
  if Search.Focused then
    Result := ControlToSvnEditState(Search)
  else
  if Comment.Focused then
    Result := ControlToSvnEditState(Comment)
  else
  if Revisions.Focused and (Revisions.SelCount > 0) then
  begin
    Result := [sesCanCopy];
    if Revisions.MultiSelect then
      Include(Result, sesCanSelectAll);
  end
  else
  if Files.Focused and (Files.SelCount > 0) then
  begin
    Result := [sesCanCopy];
    if Files.MultiSelect then
      Include(Result, sesCanSelectAll);
  end
  else
    Result := [];
end;

procedure TSvnLogFrame.NextClick(Sender: TObject);
var
  First: Integer;
begin
  FCount := FCount + DefaultRange;
  if not TryStrToInt(FRevisionList[FRevisionList.Count - 1].FRevision, First) then
    First := -1;
  Next.Enabled := False;
  Refresh.Enabled := False;
  FLoadRevisionsCallBack(First, DefaultRange);
end;

procedure TSvnLogFrame.NextCompleted;
begin
  Next.Enabled := True;
  Refresh.Enabled := True;
end;

function TSvnLogFrame.PerformEditAction(AEditAction: TSvnEditAction): Boolean;
var
  I, J: Integer;
  S, ColumnContent: string;
  SL, FilesSL: TStringList;
  FirstItem: Boolean;
begin
  if Search.Focused then
    Result := PerformDefaultSvnEditAction(Search, AEditAction)
  else
  if Comment.Focused then
    Result := PerformDefaultSvnEditAction(Comment, AEditAction)
  else
  if Revisions.Focused then
  begin
    if AEditAction = seaCopy then
    begin
      SL := TStringList.Create;
      try
        FirstItem := True;
        for I := 0 to Revisions.Items.Count - 1 do
          if Revisions.Items[I].Selected then
          begin
            if not FirstItem then
              SL.Add('');
            FirstItem := False;
            for J := 0 to Revisions.Columns.Count - 1 do
            begin
              if J = 0 then
                ColumnContent := Revisions.Items[I].Caption
              else
                ColumnContent := Revisions.Items[I].SubItems[J - 1];
              //the logmessage should start on it's own line and needs a line break correction
              if J = GetCommentColumn then
                ColumnContent := #13#10 + AdjustLineBreaks(ColumnContent);
              SL.Add(Format('%s: %s', [Revisions.Columns[J].Caption, ColumnContent]));
            end;
            SL.Add('----');
            if Assigned(Revisions.Items[I].Data) then
            begin
              FilesSL := TStringList(Revisions.Items[I].Data);
              //TODO: Duplicate code
              for J := 0 to Pred(FilesSL.Count) do
              begin
                ColumnContent := System.Copy(FilesSL[J], 2, MaxInt);
                if FilesSL[J] <> '' then
                begin
                  case FilesSL[J][1] of
                    'M': S := sModified;
                    'A': S := sAdded;
                    'D': S := sDeleted;
                    'R': S := sReplaced;
                    else
                      S := '';
                  end;
                end;
                SL.Add(Format('%s: %s', [S, ColumnContent]));
              end;
            end;
          end;
        Clipboard.AsText := SL.Text;
      finally
        SL.Free;
      end;
      Result := True;
    end
    else
    if AEditAction = seaSelectAll then
    begin
      Revisions.SelectAll;
      Result := True;
    end
    else
      Result := False;
  end
  else
  if Files.Focused then
  begin
    if AEditAction = seaCopy then
    begin
      SL := TStringList.Create;
      try
        for I := 0 to Files.Items.Count - 1 do
          if Files.Items[I].Selected then
            SL.Add(Files.Items[I].SubItems[0]);
        Clipboard.AsText := SL.Text;
      finally
        SL.Free;
      end;
      Result := True;
    end
    else
    if AEditAction = seaSelectAll then
    begin
      Files.SelectAll;
      Result := True;
    end
    else
      Result := False;
  end
  else
    Result := False;
end;

procedure TSvnLogFrame.RefreshClick(Sender: TObject);
var
  I: Integer;
begin
  Revisions.Clear;
  for I := 1 to Revisions.Columns.Count - 1 do
    Revisions.Columns[I].Width := -2;
  Application.ProcessMessages;
  Files.Clear;
  Comment.Lines.Text := '';
  FRevisionList.Free;
  FRevisionList := TList<TRevision>.Create;
  Next.Enabled := False;
  Refresh.Enabled := False;
  FLoadRevisionsCallBack(-1, FCount);
end;

procedure TSvnLogFrame.RestoreRevisions;
var
  I: Integer;
begin
  if FDoingSearch then
  begin
    FDoingSearch := False;
    Revisions.Items.BeginUpdate;
    try
      Revisions.Items.Clear;
      for I := 0 to FRevisionList.Count - 1 do
        AddRevisionToListView(FRevisionList[I]);
    finally
      Revisions.Items.EndUpdate;
    end;
    Search.RightButton.Visible := False;
  end;
end;

procedure TSvnLogFrame.RevisionsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  I, W: Integer;
  FilesSL: TStringList;
  S: string;
  ColumnWidths: array [0..1] of Integer;
begin
  Files.Items.BeginUpdate;
  try
    Files.Items.Count := 0;
    if Revisions.Selected <> nil then
    begin
      Comment.Lines.Text := Revisions.Selected.SubItems[2];
      if Revisions.Selected.Data <> nil then
      begin
        FilesSL := TStringList(Revisions.Selected.Data);
        Files.Items.Count := FilesSL.Count;
        //get max column widths
        for I := Low(ColumnWidths) to High(ColumnWidths) do
          ColumnWidths[I] := 0;
        for I := 0 to FilesSL.Count - 1 do
        begin
          S := System.Copy(FilesSL[I], 2, MaxInt);
          W := Files.StringWidth(S);
          if W > ColumnWidths[1] then
            ColumnWidths[1] := W;
          case FilesSL[I][1] of
            'M': S := sModified;
            'A': S := sAdded;
            'D': S := sDeleted;
            'R': S := sReplaced;
          end;
          W := Files.StringWidth(S);
          if W > ColumnWidths[0] then
            ColumnWidths[0] := W;
        end;
        //set columns widths including a margin (using 14 as margin, because this is the observed
        // margin with themes (no themes: first column 8, other columns 12; themes: first 10, other 14)
        for I := Low(ColumnWidths) to High(ColumnWidths) do
          Files.Columns[I].Width := ColumnWidths[I] + 14;
      end;
    end;
  finally
    Files.Items.EndUpdate;
  end;
end;

initialization
  TSvnLogFrame.FUseCount := 0;
end.
