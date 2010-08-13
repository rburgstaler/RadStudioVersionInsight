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
unit SvnIDEMenus;

interface

uses Classes, ToolsApi, SvnIDEClient, SvnClient;

const
  sPMVSvnParent = 'SvnParent';

//  Menu Positions
const
  // Project Menu Items
  // Commit
  pmmpParentCommitSvnMenu = pmmpUserVersionControl + 10;
  pmmpRootDirCommitSvnMenu = pmmpUserVersionControl + 20;
  pmmpProjectDirCommitSvnMenu = pmmpUserVersionControl + 30;
  pmmpExpicitFilesCommitSvnMenu = pmmpUserVersionControl + 40;
  // Update
  pmmpParentUpdateSvnMenu = pmmpUserVersionControl + 50;
  pmmpRootDirUpdateSvnMenu = pmmpUserVersionControl + 60;
  pmmpProjectDirUpdateSvnMenu = pmmpUserVersionControl + 70;
  pmmpExpicitFilesUpdateSvnMenu = pmmpUserVersionControl + 80;
  // Log
  pmmpParentLogSvnMenu = pmmpUserVersionControl + 90;
  pmmpRootDirLogSvnMenu = pmmpUserVersionControl + 100;
  pmmpProjectDirLogSvnMenu = pmmpUserVersionControl + 110;
  // Clean
  pmmpParentCleanSvnMenu = pmmpUserVersionControl + 120;
  pmmpRootDirCleanSvnMenu = pmmpUserVersionControl + 130;
  pmmpProjectDirCleanSvnMenu = pmmpUserVersionControl + 140;
  // Repository Browser
  pmmpParentRepoSvnMenu = pmmpUserVersionControl + 150;
  pmmpRootDirRepoSvnMenu = pmmpUserVersionControl + 160;
  pmmpProjectDirRepoSvnMenu = pmmpUserVersionControl + 170;
  // File Menu Items
  pmmpFileCommitSvnMenu = pmmpUserVersionControl + 1010;
  pmmpFileUpdateSvnMenu = pmmpUserVersionControl + 1020;
  pmmpFileRepoSvnMenu = pmmpUserVersionControl + 1030;


type
  TSvnMenu = class(TInterfacedObject, IOTALocalMenu, IOTAProjectManagerMenu)
  protected
    FCaption: string;
    FChecked: Boolean;
    FEnabled: Boolean;
    FHelpContext: Integer;
    FIsMultiSelectable: Boolean;
    FName: string;
    FParent: string;
    FPosition: Integer;
    FSvnIDEClient: TSvnIDEClient;
    FVerb: string;

    {IOTANotifier}
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;

    {IOTALocalMenu}
    function GetCaption: string;
    function GetChecked: Boolean;
    function GetEnabled: Boolean;
    function GetHelpContext: Integer;
    function GetName: string;
    function GetParent: string;
    function GetPosition: Integer;
    function GetVerb: string;
    procedure SetCaption(const Value: string);
    procedure SetChecked(Value: Boolean);
    procedure SetEnabled(Value: Boolean);
    procedure SetHelpContext(Value: Integer);
    procedure SetName(const Value: string);
    procedure SetParent(const Value: string);
    procedure SetPosition(Value: Integer);
    procedure SetVerb(const Value: string);

    { IOTAProjectManagerMenu }
    function GetIsMultiSelectable: Boolean;
    procedure SetIsMultiSelectable(Value: Boolean);
    procedure Execute(const MenuContextList: IInterfaceList); virtual;
    function PreExecute(const MenuContextList: IInterfaceList): Boolean;
    function PostExecute(const MenuContextList: IInterfaceList): Boolean;
  public
    constructor Create(ASvnIDEClient: TSvnIDEClient);
  end;

  TRootType = (rtRootDir, rtProjectDir, rtExpicitFiles);

procedure BuildFileList(const MenuContextList: IInterfaceList;
  const DirectoryList: TStringList; const SvnClient: TSvnClient;
  RootType: TRootType; var ProjectFound: Boolean);
procedure RegisterMenus(ASvnIDEClient: TSvnIDEClient);
procedure UnRegisterMenus;
function RootDirectory(const SvnClient: TSvnClient; const Path: string): string;


implementation

uses SysUtils, SvnIDEConst, SvnIDECommit, SvnIDEUpdate, SvnIDEClean, SvnIDELog,
  SvnIDEImport, SvnIDECheckout, SvnIDERepoBrowser;

const
  sSubversionName = 'embarcadero.subversion';

type
  TExecuteProc = procedure(SvnIDEClient: TSvnIDEClient;
    const MenuContextList: IInterfaceList);

  TSvnNotifier = class(TInterfacedObject, IOTAVersionControlNotifier,
    IOTAVersionControlNotifier150)
    { IOTANotifier }
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    { IOTAVersionControlNotifier }
    function GetDisplayName: string;
    function IsFileManaged(const Project: IOTAProject; const IdentList: TStrings): Boolean;
    procedure ProjectManagerMenu(const Project: IOTAProject; const IdentList: TStrings;
      const ProjectManagerMenuList: IInterfaceList; IsMultiSelect: Boolean);
    function AddNewProject(const Project: IOTAProject): Boolean;
    {  IOTAVersionControlNotifier150 }
    function CheckoutProject(var ProjectName: string): Boolean;
    function CheckoutProjectWithConnection(var ProjectName: string;
      const Connection: string): Boolean;
    function GetName: string;
  protected
    FSvnIDEClient: TSvnIDEClient;
  public
    constructor Create(const SvnIDEClient: TSvnIDEClient);
  end;

  TParentSvnMenu = class(TSvnMenu)
  public
    constructor Create;
  end;

var
  PMMSvnParent, PMMParentCommit, PMMRootDirCommit, PMMProjectDirCommit,
  PMMExpicitFilesCommit, PMMFileCommit, PMMParentUpdate, PMMRootDirUpdate,
  PMMProjectDirUpdate, PMMExpicitFilesUpdate, PMMFileUpdate,
  PMMParentCleanSvnMenu, PMMRootDirCleanSvnMenu, PMMProjectDirCleanSvnMenu,
  PMMParentLogSvnMenu, PMMRootDirLogSvnMenu, PMMProjectDirLogSvnMenu,
  PMMParentRepo, PMMRootDirRepo, PMMProjectDirRepo, PMMFileRepoSvnMenu: IOTAProjectManagerMenu;

function RootDirectory(const SvnClient: TSvnClient; const Path: string): string;
var
  RepoPath: string;
  TempPath: string;
begin
  Result := ExtractFilePath(Path);
  RepoPath := SvnClient.FindRepositoryRoot(Result);
  if RepoPath = '' then
  else
  begin
    TempPath := ExtractFilePath(ExcludeTrailingPathDelimiter(Result));
    while RepoPath = SvnClient.FindRepositoryRoot(TempPath) do
    begin
      Result := TempPath;
      TempPath := ExtractFilePath(ExcludeTrailingPathDelimiter(Result));
    end;
  end;
 end;

procedure BuildFileList(const MenuContextList: IInterfaceList;
  const DirectoryList: TStringList; const SvnClient: TSvnClient;
  RootType: TRootType; var ProjectFound: Boolean);
var
  I: Integer;
  MenuContext: IOTAMenuContext;
  Project: IOTAProject;
  TempProject: IOTAProject;
  Path: string;
  Module: IOTAModule;
begin
  ProjectFound := False;
  for I := 0 to MenuContextList.Count - 1 do
  begin
    Project := (MenuContextList[I] as IOTAProjectMenuContext).Project;
    if Supports(MenuContextList[I], IOTAMenuContext, MenuContext) then
      if FileExists(MenuContext.Ident) then
      begin
        // If it is a project
        if Supports((BorlandIDEServices as IOTAModuleServices).FindModule(MenuContext.Ident), IOTAProject, TempProject) then
        begin
          ProjectFound := True;
          case RootType of
            rtRootDir:
              begin
                Path := RootDirectory(SvnClient, MenuContext.Ident);
                if Path = '' then
                  Path := ExtractFilePath(MenuContext.Ident);
                DirectoryList.Add(Path);
              end;
            rtProjectDir: DirectoryList.Add(ExtractFilePath(MenuContext.Ident));
            rtExpicitFiles: TempProject.GetCompleteFileList(DirectoryList);
          end;
        end
        else
        begin
          if Project <> nil then
            Project.GetAssociatedFiles(MenuContext.Ident, DirectoryList)
          else
          begin
            Module := (BorlandIDEServices as IOTAModuleServices).FindModule(MenuContext.Ident);
            if Module <> nil then
              Module.GetAssociatedFilesFromModule(DirectoryList)
            else
              DirectoryList.Add(MenuContext.Ident);
          end;
        end;
      end;
  end;
end;

{ TSvnMenu }

procedure TSvnMenu.AfterSave;
begin

end;

procedure TSvnMenu.BeforeSave;
begin

end;

constructor TSvnMenu.Create(ASvnIDEClient: TSvnIDEClient);
begin
  inherited Create;
  FSvnIDEClient := ASvnIDEClient;
  FParent := '';
  FChecked := False;
  FEnabled := True;
  FIsMultiSelectable := True;
  FName := '';
end;

procedure TSvnMenu.Destroyed;
begin

end;

procedure TSvnMenu.Execute(const MenuContextList: IInterfaceList);
begin

end;

function TSvnMenu.GetCaption: string;
begin
  Result := FCaption;
end;

function TSvnMenu.GetChecked: Boolean;
begin
  Result := FChecked;
end;

function TSvnMenu.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TSvnMenu.GetHelpContext: Integer;
begin
  Result := FHelpContext;
end;

function TSvnMenu.GetIsMultiSelectable: Boolean;
begin
  Result := FIsMultiSelectable;
end;

function TSvnMenu.GetName: string;
begin
  Result := FName;
end;

function TSvnMenu.GetParent: string;
begin
  Result := FParent;
end;

function TSvnMenu.GetPosition: Integer;
begin
  Result := FPosition;
end;

function TSvnMenu.GetVerb: string;
begin
  Result := FVerb;
end;

procedure TSvnMenu.Modified;
begin

end;

function TSvnMenu.PostExecute(const MenuContextList: IInterfaceList): Boolean;
begin
  Result := True;
end;

function TSvnMenu.PreExecute(const MenuContextList: IInterfaceList): Boolean;
begin
  Result := True;
end;

procedure TSvnMenu.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TSvnMenu.SetChecked(Value: Boolean);
begin
  FChecked := Value;
end;

procedure TSvnMenu.SetEnabled(Value: Boolean);
begin
  FEnabled := Value;
end;

procedure TSvnMenu.SetHelpContext(Value: Integer);
begin
  FHelpContext := Value;
end;

procedure TSvnMenu.SetIsMultiSelectable(Value: Boolean);
begin
  FIsMultiSelectable := Value;
end;

procedure TSvnMenu.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TSvnMenu.SetParent(const Value: string);
begin
  FParent := Value;
end;

procedure TSvnMenu.SetPosition(Value: Integer);
begin
  FPosition := Value;
end;

procedure TSvnMenu.SetVerb(const Value: string);
begin
  FVerb := Value;
end;

{ TSvnNotifier }

function TSvnNotifier.AddNewProject(const Project: IOTAProject): Boolean;
begin
  Result := ImportProject(FSvnIDEClient, Project);
end;

procedure TSvnNotifier.AfterSave;
begin

end;

procedure TSvnNotifier.BeforeSave;
begin

end;

function TSvnNotifier.CheckoutProject(var ProjectName: string): Boolean;
begin
  Result := DoCheckOutProject(ProjectName);
end;

function TSvnNotifier.CheckoutProjectWithConnection(var ProjectName: string;
  const Connection: string): Boolean;
begin
  Result := DoCheckOutProject(ProjectName, Connection);
end;

constructor TSvnNotifier.Create(const SvnIDEClient: TSvnIDEClient);
begin
  inherited Create;
  FSvnIDEClient := SvnIDEClient;
end;

procedure TSvnNotifier.Destroyed;
begin

end;

function TSvnNotifier.GetDisplayName: string;
begin
  Result := sSubversion;
end;

function TSvnNotifier.GetName: string;
begin
  Result := sSubversionName;
end;

function TSvnNotifier.IsFileManaged(const Project: IOTAProject;
  const IdentList: TStrings): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to IdentList.Count - 1 do
    if FileExists(IdentList[I]) then
    begin
      try
        Result := IDEClient.SvnClient.IsPathVersioned(IdentList[I]);
      except
        Result := False;
        Exit;
      end;
      if Result then
        Break;
    end;
end;

procedure TSvnNotifier.Modified;
begin

end;

procedure TSvnNotifier.ProjectManagerMenu(const Project: IOTAProject;
  const IdentList: TStrings; const ProjectManagerMenuList: IInterfaceList;
  IsMultiSelect: Boolean);

  function ContainersProject: Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to IdentList.Count - 1 do
      if Supports((BorlandIDEServices as IOTAModuleServices).FindModule(IdentList[I]), IOTAProject) then
      begin
        Result := True;
        Break;
      end;
  end;

begin
  ProjectManagerMenuList.Add(PMMSvnParent);
  if ContainersProject then
  begin
    ProjectManagerMenuList.Add(PMMParentCommit);
    ProjectManagerMenuList.Add(PMMRootDirCommit);
    ProjectManagerMenuList.Add(PMMProjectDirCommit);
    ProjectManagerMenuList.Add(PMMExpicitFilesCommit);
    ProjectManagerMenuList.Add(PMMParentUpdate);
    ProjectManagerMenuList.Add(PMMRootDirUpdate);
    ProjectManagerMenuList.Add(PMMProjectDirUpdate);
    ProjectManagerMenuList.Add(PMMExpicitFilesUpdate);
    ProjectManagerMenuList.Add(PMMParentLogSvnMenu);
    ProjectManagerMenuList.Add(PMMRootDirLogSvnMenu);
    ProjectManagerMenuList.Add(PMMProjectDirLogSvnMenu);
    ProjectManagerMenuList.Add(PMMParentCleanSvnMenu);
    ProjectManagerMenuList.Add(PMMRootDirCleanSvnMenu);
    ProjectManagerMenuList.Add(PMMProjectDirCleanSvnMenu);
    ProjectManagerMenuList.Add(PMMParentRepo);
    ProjectManagerMenuList.Add(PMMRootDirRepo);
    ProjectManagerMenuList.Add(PMMProjectDirRepo);
  end
  else
  begin
    ProjectManagerMenuList.Add(PMMFileCommit);
    ProjectManagerMenuList.Add(PMMFileUpdate);
    ProjectManagerMenuList.Add(PMMFileRepoSvnMenu);
  end;
end;

var
  NotifierIndex: Integer;

procedure RegisterMenus(ASvnIDEClient: TSvnIDEClient);
begin
  NotifierIndex := (BorlandIDEServices as IOTAVersionControlServices).AddNotifier(TSvnNotifier.Create(ASvnIDEClient));
  PMMSvnParent := TParentSvnMenu.Create;
  PMMParentCommit := TParentCommitSvnMenu.Create;
  PMMRootDirCommit := TRootDirCommitSvnMenu.Create(ASvnIDEClient);
  PMMProjectDirCommit := TProjectDirCommitSvnMenu.Create(ASvnIDEClient);
  PMMExpicitFilesCommit := TExpicitFilesCommitSvnMenu.Create(ASvnIDEClient);
  PMMFileCommit := TFileCommitSvnMenu.Create(ASvnIDEClient);
  PMMParentUpdate := TParentUpdateSvnMenu.Create;
  PMMRootDirUpdate := TRootDirUpdateSvnMenu.Create(ASvnIDEClient);
  PMMProjectDirUpdate := TProjectDirUpdateSvnMenu.Create(ASvnIDEClient);
  PMMExpicitFilesUpdate := TExpicitFilesUpdateSvnMenu.Create(ASvnIDEClient);
  PMMFileUpdate := TFileUpdateSvnMenu.Create(ASvnIDEClient);
  PMMParentCleanSvnMenu := TParentCleanSvnMenu.Create;
  PMMRootDirCleanSvnMenu := TRootDirCleanSvnMenu.Create(ASvnIDEClient);
  PMMProjectDirCleanSvnMenu := TProjectDirCleanSvnMenu.Create(ASvnIDEClient);
  PMMParentLogSvnMenu := TParentLogSvnMenu.Create;
  PMMRootDirLogSvnMenu := TRootDirLogSvnMenu.Create(ASvnIDEClient);
  PMMProjectDirLogSvnMenu := TProjectDirLogSvnMenu.Create(ASvnIDEClient);
  PMMParentRepo := TParentRepoSvnMenu.Create;
  PMMRootDirRepo := TRootDirRepoSvnMenu.Create(ASvnIDEClient);
  PMMProjectDirRepo := TProjectDirRepoSvnMenu.Create(ASvnIDEClient);
  PMMFileRepoSvnMenu := TFileRepoSvnMenu.Create(ASvnIDEClient);
end;

procedure UnRegisterMenus;
begin
  (BorlandIDEServices as IOTAVersionControlServices).RemoveNotifier(NotifierIndex);
  PMMSvnParent := nil;
  PMMParentCommit := nil;
  PMMRootDirCommit := nil;
  PMMProjectDirCommit := nil;
  PMMExpicitFilesCommit := nil;
  PMMFileCommit := nil;
  PMMParentUpdate := nil;
  PMMRootDirUpdate := nil;
  PMMProjectDirUpdate := nil;
  PMMExpicitFilesUpdate := nil;
  PMMFileUpdate := nil;
  PMMParentCleanSvnMenu := nil;
  PMMRootDirCleanSvnMenu := nil;
  PMMProjectDirCleanSvnMenu := nil;
  PMMParentLogSvnMenu := nil;
  PMMRootDirLogSvnMenu := nil;
  PMMProjectDirLogSvnMenu := nil;
  PMMParentRepo := nil;
  PMMRootDirRepo := nil;
  PMMProjectDirRepo := nil;
  PMMFileRepoSvnMenu := nil;
end;

{ TParentSvnMenu }

constructor TParentSvnMenu.Create;
begin
  inherited Create(nil);
  FCaption := sPMMSvnParent;
  FVerb := sPMVSvnParent;
  FPosition := pmmpUserVersionControl;
  FHelpContext := 0;
end;

end.
