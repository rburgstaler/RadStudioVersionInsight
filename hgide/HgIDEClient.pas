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
{ The Original Code is HgIDEClient.pas.                                        }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2010 Uwe Schuster. All      }
{ Rights Reserved.                                                             }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit HgIDEClient;

interface

uses
  SysUtils, HgClient;

type
  THgIDEClient = class(TObject)
  private
    FHistoryProviderIndex: Integer;
    FHgClient: THgClient;
    FHgInitialized: Boolean;
    procedure Initialize;
    procedure Finalize;
    function GetHgClient: THgClient;
  public
    constructor Create;
    destructor Destroy; override;
    function HgInitialize: Boolean;
    property HgClient: THgClient read GetHgClient;
  end;

procedure Register;

function BaseRegKey: string;

var
  IDEClient: THgIDEClient;

implementation

uses
  Registry, ToolsAPI, FileHistoryAPI, HgIDEHistory, HgIDEAddInOptions, HgIDEMenus,
  HgImages, HgIDEConst;

procedure Register;
begin
  IDEClient := THgIDEClient.Create;
  RegisterMenus(IDEClient);
  RegisterAddInOptions;
end;

{ THgIDEClient }

constructor THgIDEClient.Create;
begin
  inherited Create;
  Initialize;
end;

destructor THgIDEClient.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

procedure THgIDEClient.Finalize;
var
  FileHistoryManager: IOTAFileHistoryManager;
begin
  if (FHistoryProviderIndex <> -1) and Assigned(BorlandIDEServices)
    and BorlandIDEServices.GetService(IOTAFileHistoryManager, FileHistoryManager)
  then
    FileHistoryManager.UnregisterHistoryProvider(FHistoryProviderIndex);
  FHistoryProviderIndex := -1;
  FreeAndNil(HgImageModule);
end;

function THgIDEClient.GetHgClient: THgClient;
begin
  if not FHgInitialized then
    HgInitialize;
  Result := FHgClient;
end;

function THgIDEClient.HgInitialize: Boolean;
var
  RegIniFile: TRegIniFile;
  Key: string;
begin
  Result := True;
  if FHgInitialized then
    Exit;
  FHgInitialized := True;

  HgImageModule := THgImageModule.Create(nil);

  FHgClient := THgClient.Create;

  Key := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey + '\VersionInsight';
  RegIniFile := TRegIniFile.Create(Key);
  try
    FHgClient.HgExecutable := RegIniFile.ReadString('Mercurial', 'Executable', '');
  finally
    RegIniFile.Free;
  end;
end;

procedure THgIDEClient.Initialize;
var
  FileHistoryManager: IOTAFileHistoryManager;
begin
  if Assigned(BorlandIDEServices) then
  begin
    if BorlandIDEServices.GetService(IOTAFileHistoryManager, FileHistoryManager) then
      FHistoryProviderIndex := FileHistoryManager.RegisterHistoryProvider(THgFileHistoryProvider.Create(Self));
  end;
end;

function BaseRegKey: string;
begin
  Result := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey + '\' + sMercurial + '\';
end;

end.
